;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2023 Jiwei YANG <yangjiwei@protonmail.com>

(define-module (ac services usbguard)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services base)
  #:use-module (gnu services dbus)
  #:use-module (gnu services avahi)
  #:use-module (gnu services xorg)
  #:use-module (gnu services networking)
  #:use-module (gnu services sound)
  #:use-module ((gnu system file-systems)
                #:select (%control-groups
                          %elogind-file-systems
                          file-system))
  #:autoload   (gnu services sddm) (sddm-service-type)
  #:use-module (gnu system)
  #:use-module (gnu system setuid)
  #:use-module (gnu system shadow)
  #:use-module (gnu system uuid)
  #:use-module (gnu system pam)
  #:use-module (gnu packages hardware)
  #:use-module (guix deprecation)
  #:use-module (guix records)
  #:use-module (guix packages)
  #:use-module (guix store)
  #:use-module (guix ui)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:export (<usbguard-configuration>
            usbguard-configuration
            usbguard-configuration?
            usbguard-configuration-usbguard
            usbguard-service-type))

(define-record-type* <usbguard-configuration>
  usbguard-configuration make-usbguard-configuration
  usbguard-configuration?
  (usbguard usbguard-configuration-usbguard  ;file-like
               (default usbguard))
  (config-file usbguard-configuration-config-file ;file-like
               (default (local-file "aux-files/usbguard/usbguard-daemon.conf"))))

(define %usbguard-activation
  #~(begin
      (use-modules (guix build utils))
      (mkdir-p "/var/lib/usbguard")))

(define (usbguard-dbus-service config)
  (list (wrapped-dbus-service (usbguard-configuration-usbguard config)
                              "sbin/usbguard-daemon"
                              `())))

(define (usbguard-shepherd-service config)
  "Return a shepherd service for usbguard with CONFIG."
  (let ((usbguard (usbguard-configuration-usbguard config))
        (config (usbguard-configuration-config-file config)))
    (list (shepherd-service
           (documentation "Run the usbguard.")
           (provision '(usbguard-daemon))
           (requirement '(dbus-system udev))

           (start #~(make-forkexec-constructor
                     (list (string-append #$usbguard "/sbin/usbguard-daemon")
                           (string-append "-c" #$config)
                           "-k" "-P")))
           (stop #~(make-kill-destructor))
           (actions (list (shepherd-configuration-action config)))))))

(define usbguard-service-type
  (let ((usbguard-package (compose list usbguard-configuration-usbguard)))
    (service-type (name 'usbguard)
                  (description
                   "Run @command{usbguard-daemon}}.")
                  (extensions
                   (list (service-extension dbus-root-service-type
                                            usbguard-dbus-service)
                         (service-extension shepherd-root-service-type
                                            usbguard-shepherd-service)
                         (service-extension activation-service-type
                                            (const %usbguard-activation))
                         (service-extension udev-service-type
                                            usbguard-package)

                         ;; Make the 'usbguard' command visible.
                         (service-extension profile-service-type
                                            usbguard-package)))
                  (default-value (usbguard-configuration)))))

