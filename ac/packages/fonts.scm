;;; SPDX-License-Identifier: AGPL-3.0-or-later
;;; Copyright © 2023 Jiwei YANG <yangjiwei@protonmail.com>


(define-module (ac packages fonts)
  #:use-module (ice-9 regex)
  #:use-module (guix utils)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix build-system font)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages c)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages xorg))

(define-public font-my-noto-core
  (package
    (name "font-my-noto-core")
    (version "20201206-phase3")
    (source
     (origin
       (method url-fetch/zipbomb)
       (uri (string-append
             ; https://github.com/notofonts/noto-fonts/archive/refs/tags/v20201206-phase3.zip
             "https://github.com/notofonts/noto-fonts/archive/refs/tags/v"
             version ".zip"))
       (sha256
        (base32 "19l6a5cvzj16klyx6lf1hvc4ldvnjiyr5fy7vjj9iihdc91k4vip"))))
    (build-system font-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (font-dir (string-append out "/share/fonts"))
                    (truetype-dir (string-append font-dir "/truetype")))
               (with-directory-excursion "noto-fonts-20201206-phase3/hinted/ttf"
                 (for-each (lambda (f) (install-file f truetype-dir))
                           (find-files "." "\\.ttf$"))
               #t)))))))
    (home-page "https://www.google.com/get/noto/")
    (synopsis "Fonts to cover all languages")
    (description "Google Noto Fonts is a family of fonts designed to support
all languages with a consistent look and aesthetic.  Its goal is to properly
display all Unicode symbols.")
    (license license:silofl1.1)))

(define-public font-my-noto-sans-cjk
  (package
    (name "font-my-noto-sans-cjk")
    (version "20221019")
    (source
     (origin
       (method url-fetch)
       (uri "https://github.com/googlefonts/noto-cjk/raw/main/Sans/SuperOTC/NotoSansCJK.ttc.zip")
       (file-name (string-append name "-" version ".zip"))
       (sha256
        (base32 "02zfyshyc7zjkx24n555n83l2y3cf10hqz56ia5ldp10fi6vf06l"))))
    (build-system font-build-system)
    (home-page "https://www.google.com/get/noto/")
    (synopsis "Fonts to cover all languages")
    (description "Google Noto Fonts is a family of fonts designed to support
all languages with a consistent look and aesthetic.  Its goal is to properly
display all Unicode symbols.  This package provides the Sans Serif variant of
CJK fonts.")
    (license license:silofl1.1)))

(define-public font-my-noto-serif-cjk
  (package
    (name "font-my-noto-serif-cjk")
    (version "2.001")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             ; https://github.com/googlefonts/noto-cjk/releases/download/Serif2.001/01_NotoSerifCJK.ttc.zip
             "https://github.com/googlefonts/noto-cjk/releases/download/Serif"
             version "/01_NotoSerifCJK.ttc.zip"))
       (file-name (string-append name "-" version ".zip"))
       (sha256
        (base32 "1mmc639bjzq06wxffjgznwj38lq50414742z7n7h3g9995mw1gh6"))))
    (build-system font-build-system)
    (home-page "https://www.google.com/get/noto/")
    (synopsis "Fonts to cover all languages")
    (description "Google Noto Fonts is a family of fonts designed to support
all languages with a consistent look and aesthetic.  Its goal is to properly
display all Unicode symbols.  This package provides the Serif variant of CJK
fonts.")
    (license license:silofl1.1)))

(define-public font-my-noto-emoji
  (package
    (name "font-my-noto-emoji")
    (version "20221019")
    (source
     (origin
       (method url-fetch)
       (uri "https://github.com/googlefonts/noto-emoji/raw/main/fonts/NotoColorEmoji.ttf")
       (sha256
        (base32 "0z1sa9bxw11yj79kvjcb29pg92krbznp9szq179cc0w8l1awisif"))))
    (build-system font-build-system)
    (home-page "https://www.google.com/get/noto/")
    (synopsis "Fonts to cover all languages")
    (description "Google Noto Fonts is a family of fonts designed to support
all languages with a consistent look and aesthetic.  Its goal is to properly
display all Unicode symbols.  This package provides the Sans Serif variant of
CJK fonts.")
    (license license:silofl1.1)))
