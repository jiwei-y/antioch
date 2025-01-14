;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2025 Jiwei YANG <yangjiwei@protonmail.com>

(define-module (ac packages cran)
  #:use-module (gnu packages cran)
  #:use-module (gnu packages statistics)
  #:use-module (guix-cran packages g)
  #:use-module (guix build-system r)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module ((guix licenses) :prefix license:))

(define-public r-gpinter
  (let ((commit "e6ba8b7375fac8c8940b37ec7fc05c77b0402a2c")
        (revision "1"))
    (package
      (name "r-gpinter")
      (version (git-version "0.0.0.9001" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/jiwei-y/gpinter")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "15bwg78wy1a4s05c80m8cisnjd1wh94qffxyd296dkms1ijc2jqk"))))
      (properties `((upstream-name . "gpinter")))
      (build-system r-build-system)
      (propagated-inputs
       (list r-nloptr
             r-ggplot2
             r-scales
             r-emdbook
             r-randtoolbox
             r-gumbel
             r-pbapply
             r-shiny
             r-mass))
      (native-inputs
       (list r-knitr r-testthat))
      (home-page "https://github.com/jiwei-y/gpinter")
      (synopsis "R package for generalized Pareto interpolation")
      (description
       "Generalized Pareto Interpolation is a method for reconstructing complete distributions based on tabulations that only contain information on a few thresholds and bracket shares. It is usually applied in the study of income and wealth, but it can also be used with other types of data.")
      (license license:agpl3))))