;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2023 Jiwei YANG <yangjiwei@protonmail.com>

(define-module (ac packages pypi)
  #:use-module (guix build-system python)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (gnu packages python-xyz))

(define-public python-depix
  (package
    (name "python-depix")
    (version "1.0.2")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "depix" version))
              (sha256
               (base32
                "12rj1m75mlbh21ssjmfjbf670smxyb30zw3an00f0j39j73cd6b4"))))
    (build-system python-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'sanity-check))))
    (propagated-inputs (list python-pillow))
    (home-page "https://github.com/beurtschipper/Depix")
    (synopsis
     "Depix is a tool for recovering passwords from pixelized screenshots.")
    (description
     "Depix is a tool for recovering passwords from pixelized screenshots.")
    (license #f)))
