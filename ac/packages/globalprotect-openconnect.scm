;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2023 Jiwei YANG <yangjiwei@protonmail.com>

(define-module (ac packages globalprotect-openconnect)
  #:use-module (gnu packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix build-system qt)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix modules)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages vpn)
  #:use-module (gnu packages pkg-config))

(define-public globalprotect-openconnect
  (package
    (name "globalprotect-openconnect")
    (version "1.4.8")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/yuezk/GlobalProtect-openconnect")
                    (commit (string-append "v" version))
                    (recursive? #t)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1h4w4sk88dgam591bq3c9c7k3xy0jbykchm954k24symw4cja01x"))))
    (build-system qt-build-system)
    (arguments
     `(#:tests? #f))
    (native-inputs (list pkg-config qttools))
    (inputs (list qtbase-5
                  qtwebengine-5
                  qtwebsockets-5
                  qtwebchannel-5
                  qtdeclarative-5
                  openconnect))
    (home-page "https://github.com/yuezk/GlobalProtect-openconnect/")
    (synopsis "A GlobalProtect VPN client (GUI) for Linux")
    (description
     "A GlobalProtect VPN client (GUI) for Linux based on Openconnect and built with Qt5,
 supports SAML auth mode, inspired by gp-saml-gui")
    (license license:gpl3)))
