;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2013 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2016 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2019–2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2023 Petr Hodina <phodina@protonmail.com>
;;; Copyright © 2024 Jiwei YANG <yangjiwei@protonmail.com>

(define-module (ac packages cryptsetup)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages cryptsetup)
  #:use-module (guix gexp)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages password-utils)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages popt)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages web))

(define-public cryptsetup-stable
  (package
   (inherit cryptsetup)
   (name "cryptsetup-stable")
   (version "2.7.2")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://kernel.org/linux/utils/cryptsetup/v"
                                (version-major+minor version)
                                "/cryptsetup-" version ".tar.xz"))
            ;; guix download https://cdn.kernel.org/pub/linux/utils/cryptsetup/v2.7/cryptsetup-2.7.2.tar.xz -o /tmp/ac/cryptsetup-2.7.2.tar.xz
            (sha256
             (base32
              "1x7y795xj55immynxwxacw53b3vgb9z4fxh399i9dpzdx1sbz7i1"))))
   (arguments
    (substitute-keyword-arguments (package-arguments cryptsetup)
      ((#:phases phases '%standard-phases)
       #~(modify-phases #$phases
           (delete 'check)))))
   (native-inputs
    (modify-inputs (package-native-inputs cryptsetup)
      (prepend ruby-asciidoctor)))
   (inputs
    (modify-inputs (package-inputs cryptsetup)
      (prepend libssh)))))