;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2023 Jiwei YANG <yangjiwei@protonmail.com>

(define-module (ac services ntp)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services configuration)
  #:use-module (gnu services linux)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services dbus)
  #:use-module (gnu services admin)
  #:use-module (gnu services networking)
  #:use-module (gnu system shadow)
  #:use-module (gnu system pam)
  #:use-module ((gnu system file-systems) #:select (file-system-mapping))
  #:use-module (gnu packages admin)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages cluster)
  #:use-module (gnu packages connman)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages tor)
  #:use-module (gnu packages usb-modeswitch)
  #:use-module (gnu packages messaging)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages ntp)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages ipfs)
  #:use-module (gnu build linux-container)
  #:autoload   (guix least-authority) (least-authority-wrapper)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (guix modules)
  #:use-module (guix packages)
  #:use-module (guix deprecation)
  #:use-module (guix diagnostics)
  #:autoload   (guix ui) (display-hint)
  #:use-module (guix i18n)
  #:use-module (rnrs enums)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-43)
  #:use-module (ice-9 match)
  #:use-module (json)
  #:export (chronyd-service-type))

(define-record-type* <chronyd-configuration>
  chronyd-configuration make-chronyd-configuration
  chronyd-configuration?
  (chronyd  chronyd-configuration-chronyd  ;file-like
               (default chrony))
  (config-file chronyd-configuration-config-file ;file-like
               (default (local-file "aux-files/chronyd.conf"))))

(define chronyd-shepherd-service
  (match-lambda
    (($ <chronyd-configuration> chronyd config-file)
        (list (shepherd-service
            (provision '(ntpd))
            (documentation "Run the Network Time Protocol (NTP) daemon.")
            (requirement '(user-processes networking))
            (start #~(make-forkexec-constructor
                        (list (string-append #$chrony "/sbin/chronyd")
                              (string-append "-f" #$config-file)
                              "-d" "-F 1")
                        #:log-file "/var/log/ntpd.log"))
            (stop #~(make-kill-destructor)))))))

(define %chrony-accounts
  (list (user-account
         (name "chrony")
         (group "nogroup")
         (system? #t)
         (comment "chrony daemon user")
         (home-directory "/var/empty")
         (shell (file-append shadow "/sbin/nologin")))))

(define (chronyd-service-activation config)
  "Return the activation gexp for CONFIG."
  (with-imported-modules '((guix build utils))
    #~(begin
        (use-modules (guix build utils))
        (define %user
          (getpw "chrony"))

        (let ((directory "/var/lib/chrony"))
          (mkdir-p directory)
          (chown directory (passwd:uid %user) (passwd:gid %user))))))

(define chronyd-service-type
  (service-type (name 'chronyd)
                (extensions
                 (list (service-extension shepherd-root-service-type
                                          chronyd-shepherd-service)
                       (service-extension account-service-type
                                          (const %chrony-accounts))
                       (service-extension activation-service-type
                                          chronyd-service-activation)
                       (service-extension rottlog-service-type
                                          (const (@@ (gnu services networking) %ntp-log-rotation)))))
                (description
                 "Run the @command{ntpd}, the Network Time Protocol (NTP)
daemon, as implemented by @uref{https://chrony.tuxfamily.org, chrony}.  The
daemon will keep the system clock synchronized with that of the given servers.")
                (default-value (chronyd-configuration))))
