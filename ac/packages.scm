;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2023 Jiwei YANG <yangjiwei@protonmail.com>

(define-module (ac packages)
  #:use-module (gnu packages)
  #:use-module (guix packages)
  #:use-module (guix ui)
  #:use-module (guix utils)
  #:use-module (guix diagnostics)
  #:use-module (guix discovery)
  #:use-module (guix memoization)
  #:use-module ((guix build utils)
                #:select ((package-name->name+version
                           . hyphen-separated-name->name+version)
                          mkdir-p))
  #:use-module (guix profiles)
  #:use-module (guix describe)
  #:use-module (guix deprecation)
  #:use-module (ice-9 vlist)
  #:use-module (ice-9 match)
  #:use-module (ice-9 binary-ports)
  #:autoload   (system base compile) (compile)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (srfi srfi-39)
  #:use-module (srfi srfi-71)
  #:export (search-ac-auxiliary-file
            %ac-auxiliary-files-path))

(define %ac-auxiliary-files-path
  (make-parameter
   (map (cut string-append <> "/ac/packages/aux-files")
        %load-path)))

(define (search-ac-auxiliary-file file-name)
  "Search the auxiliary FILE-NAME.  Return #f if not found."
  (search-path (%ac-auxiliary-files-path) file-name))