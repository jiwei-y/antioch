;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2023 Hilton Chain <hako@ultrarare.space>

;;; SPDX-License-Identifier: AGPL-3.0-or-later
;;; Copyright © 2023 Jiwei YANG <yangjiwei@protonmail.com>

(define-module (ac packages bootloaders)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages bootloaders)
  #:use-module (gnu packages python))

;; Patches obtained from:
;; <https://leo3418.github.io/collections/gentoo-config-luks2-grub-systemd/packages.html>

(define-public grub-efi-luks2
  (let ((base grub-efi))
    (package
      (inherit base)
      (name "grub-efi-luks2")
      (source
       (let ((base (package-source base)))
         (origin
           (inherit base)
           (patches
            (append (origin-patches base)
                    (list (local-file "patches/4500-grub-2.06-runtime-memregion-alloc.patch")
                          (local-file "patches/5000-grub-2.06-luks2-argon2-v4.patch")
                          (local-file "patches/9500-grub-AUR-improved-luks2.patch")))))))
      (arguments
       (substitute-keyword-arguments (package-arguments base)
         ((#:configure-flags flags ''())
          #~(append #$flags '("--disable-werror")))
         ((#:phases phases '%standard-phases)
          #~(modify-phases #$phases
              (add-after 'unpack 'delete-configure-script
                (lambda _
                  (delete-file "configure")))))))
      (native-inputs
       (modify-inputs (package-native-inputs base)
         (append autoconf automake python-minimal-wrapper))))))
