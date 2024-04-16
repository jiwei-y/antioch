;;; SPDX-License-Identifier: GPL-3.0-or-later
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
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages xorg))

(define-public font-google-noto-git
  (package
    (inherit font-google-noto)
    (name "font-google-noto-git")
    (version "24.4.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/notofonts/notofonts.github.io")
             (commit (string-append "noto-monthly-release-" version))))
       (file-name (git-file-name name version))
       (sha256
        ;; git clone -b noto-monthly-release-24.4.1 --depth 1 https://github.com/notofonts/notofonts.github.io /tmp/ac/noto && guix hash --serializer=nar -x /tmp/ac/noto && rm -rf /tmp/ac/noto
        (base32 "1lm9brd61dsmjpr40s4iznqjqpz9giayfyfhyzxnnxzcj5lqjfgv"))))))

(define-public font-google-noto-sans-cjk-superotc
  (package
    (inherit font-google-noto-sans-cjk)
    (name "font-google-noto-sans-cjk-superotc")
    (version "2.004")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             ;; https://github.com/notofonts/noto-cjk/releases/download/Sans2.004/00_NotoSansCJK.ttc.zip
             ;; guix download https://github.com/notofonts/noto-cjk/releases/download/Sans2.004/00_NotoSansCJK.ttc.zip -o /tmp/ac/00_NotoSansCJK.ttc.zip && rm -rf /tmp/ac/00_NotoSansCJK.ttc.zip
             "https://github.com/googlefonts/noto-cjk/releases/download/Sans"
             version "/00_NotoSansCJK.ttc.zip"))
       (file-name (string-append name "-" version ".zip"))
       (sha256
        (base32 "1z4bsdqb09srckgxzw0bri4x0arsf6r1arqybcx5z1rqc1rpfq55"))))
    (arguments '())))

(define-public font-google-noto-serif-cjk-superotc
  (package
    (inherit font-google-noto-serif-cjk)
    (name "font-google-noto-serif-cjk-superotc")
    (version "2.002")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             ;; https://github.com/googlefonts/noto-cjk/releases/download/Serif2.002/01_NotoSerifCJK.ttc.zip
             ;; guix download https://github.com/googlefonts/noto-cjk/releases/download/Serif2.002/01_NotoSerifCJK.ttc.zip -o /tmp/ac/01_NotoSerifCJK.ttc.zip && rm -rf /tmp/ac/01_NotoSerifCJK.ttc.zip
             "https://github.com/googlefonts/noto-cjk/releases/download/Serif"
             version "/01_NotoSerifCJK.ttc.zip"))
       (file-name (string-append name "-" version ".zip"))
       (sha256
        (base32 "11ag68imf7crfm5035zz2fvk64bfal536vg6xrv86gc0kzm272z7"))))
    (arguments '())
    (outputs '("out"))))

(define-public font-google-noto-emoji-git
  (package
    (inherit font-google-noto-emoji)
    (name "font-google-noto-emoji-git")
    (version "2.042")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googlefonts/noto-emoji")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        ;; git clone -b v2.042 --depth 1 https://github.com/googlefonts/noto-emoji /tmp/ac/noto-emoji && guix hash --serializer=nar -x /tmp/ac/noto-emoji && rm -rf /tmp/ac/noto-emoji
        (base32
         "17i7awyqz9jv0j2blcf0smmpas375c3pdhjv1zqzl861g8qm1lm2"))))))