;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2013, 2014, 2015, 2016, 2018, 2019 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014, 2017 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2014 Joshua Grant <tadni@riseup.net>
;;; Copyright © 2014 Alex Kost <alezost@gmail.com>
;;; Copyright © 2015, 2023 Sou Bunnbu <iyzsong@envs.net>
;;; Copyright © 2015 Eric Dvorsak <eric@dvorsak.fr>
;;; Copyright © 2015, 2017 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2015, 2016 Leo Famulari <leo@famulari.name>
;;; Copyright © 2016, 2017, 2018 Nikita <nikita@n0.is>
;;; Copyright © 2016 Jookia <166291@gmail.com>
;;; Copyright © 2016 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2016 Dmitry Nikolaev <cameltheman@gmail.com>
;;; Copyright © 2016-2023 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016, 2020 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2016 Toni Reina <areina@riseup.net>
;;; Copyright © 2017–2022 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2017 José Miguel Sánchez García <jmi2k@openmailbox.com>
;;; Copyright © 2017 Alex Griffin <a@ajgrf.com>
;;; Copyright © 2017 Clément Lassieur <clement@lassieur.org>
;;; Copyright © 2017 Brendan Tildesley <mail@brendan.scot>
;;; Copyright © 2017–2023 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2017 Mohammed Sadiq <sadiq@sadiqpk.org>
;;; Copyright © 2018 Charlie Ritter <chewzerita@posteo.net>
;;; Copyright © 2018 Gabriel Hondet <gabrielhondet@gmail.com>
;;; Copyright © 2019, 2020 Jens Mølgaard <jens@zete.tk>
;;; Copyright © 2019, 2020 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2019 Baptiste Strazzulla <bstrazzull@hotmail.fr>
;;; Copyright © 2019 Alva <alva@skogen.is>
;;; Copyright © 2019 Alexandros Theodotou <alex@zrythm.org>
;;; Copyright © 2020 Damien Cassou <damien@cassou.me>
;;; Copyright © 2020 Amin Bandali <bandali@gnu.org>
;;; Copyright © 2020 Michael Rohleder <mike@rohleder.de>
;;; Copyright © 2020 John Soo <jsoo1@asu.edu>
;;; Copyright © 2020 Raghav Gururajan <raghavgururajan@disroot.org>
;;; Copyright © 2020, 2021 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2020 Zhu Zihao <all_but_last@163.com>
;;; Copyright © 2020, 2021, 2022 Simen Endsjø <simendsjo@gmail.com>
;;; Copyright © 2020 Tim Van den Langenbergh <tmt_vdl@gmx.com>
;;; Copyright © 2020 Nicolò Balzarotti <nicolo@nixo.xyz>
;;; Copyright © 2021 Antoine Côté <antoine.cote@posteo.net>
;;; Copyright © 2021 Sergiu Ivanov <sivanov@colimite.fr>
;;; Copyright © 2021 Sarah Morgensen <iskarian@mgsn.dev>
;;; Copyright © 2021-2023 Paul A. Patience <paul@apatience.com>
;;; Copyright © 2021, 2022 Taiju HIGASHI <higashi@taiju.info>
;;; Copyright © 2022-2023 Philip McGrath <philip@philipmcgrath.com>
;;; Copyright © 2022 Kitzman <kitzman@disroot.org>
;;; Copyright © 2021 Wamm K. D. <jaft.r@outlook.com>
;;; Copyright © 2022 Jai Vetrivelan <jaivetrivelan@gmail.com>
;;; Copyright © 2022, 2023 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2021 Liliana Marie Prikler <liliana.prikler@gmail.com>
;;; Copyright © 2022 Jose G Perez Taveras <josegpt27@gmail.com>
;;; Copyright © 2022 Hilton Chain <hako@ultrarare.space>
;;; Copyright © 2022 Nguyễn Gia Phong <mcsinyx@disroot.org>
;;; Copyright © 2023 Nicolas Graves <ngraves@ngraves.fr>
;;; Copyright © 2023 Ahmad Draidi <a.r.draidi@redscript.org>
;;; Copyright © 2023 Arnaud Lechevallier <arnaud.lechevallier@free.fr>
;;; Copyright © 2023 gemmaro <gemmaro.dev@gmail.com>
;;; Copyright © 2023 Denis 'GNUtoo' Carikli <GNUtoo@cyberdimension.org>
;;; Copyright © 2023 chris <chris@bumblehead.com>
;;; Copyright © 2023 Luis Felipe López Acevedo <sirgazil@zoho.com>

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
    (version "24.2.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/notofonts/notofonts.github.io")
             (commit (string-append "noto-monthly-release-" version))))
       (file-name (git-file-name name version))
       (sha256
        ;; git clone -b noto-monthly-release-24.2.1 --depth 1 https://github.com/notofonts/notofonts.github.io /tmp/noto && guix hash --serializer=nar -x /tmp/noto && rm -rf /tmp/noto
        (base32 "087jg8ahpq35xwyrmvm9ivxl0wjic2j4r28bbrwqmgdva9brms40"))))
      (arguments
      `(#:phases
        (modify-phases %standard-phases
          (replace 'install
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (font-dir (string-append out "/share/fonts")))
                (for-each (lambda (notofont)
                            (with-directory-excursion notofont
                              (if (directory-exists? "full")
                                  (for-each (lambda (fulldir)   ;; check availability in order of variable -> otf -> ttf
                                              (with-directory-excursion fulldir
                                                (if (directory-exists? "variable-ttf")
                                                    (copy-recursively "variable-ttf" font-dir)
                                                    (if (directory-exists? "otf")
                                                        (copy-recursively "otf" font-dir)
                                                        (copy-recursively "ttf" font-dir)))))
                                            (find-files "." "full" #:directories? #t)) ;; searching /full in current Noto font folder
                                  (if (directory-exists? "googlefonts")
                                      (for-each (lambda (googledir)   ;; check availability in order of variable -> otf -> ttf
                                                  (with-directory-excursion googledir
                                                    (if (directory-exists? "variable-ttf")
                                                        (copy-recursively "variable-ttf" font-dir)
                                                        (if (directory-exists? "otf")
                                                            (copy-recursively "otf" font-dir)
                                                            (copy-recursively "ttf" font-dir)))))
                                                (find-files "." "googlefonts" #:directories? #t))
                                      (if (directory-exists? "unhinted")
                                          (for-each (lambda (unhinteddir)   ;; check availability in order of variable -> otf -> ttf
                                                      (with-directory-excursion unhinteddir
                                                        (if (directory-exists? "variable-ttf")
                                                            (copy-recursively "variable-ttf" font-dir)
                                                            (if (directory-exists? "otf")
                                                                (copy-recursively "otf" font-dir)
                                                                (copy-recursively "ttf" font-dir)))))
                                                    (find-files "." "unhinted" #:directories? #t))))))) ;; searching /unhinted in current Noto font folder
                          (filter directory-exists?
                            (find-files "fonts" "Noto" #:directories? #t))) ;; searching all Noto fonts folders
                #t))))))))

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
             ;; guix download https://github.com/notofonts/noto-cjk/releases/download/Sans2.004/00_NotoSansCJK.ttc.zip -o /tmp/00_NotoSansCJK.ttc.zip && rm -rf /tmp/00_NotoSansCJK.ttc.zip
             "https://github.com/googlefonts/noto-cjk/releases/download/Sans"
             version "/00_NotoSansCJK.ttc.zip"))
       (file-name (string-append name "-" version ".zip"))
       (sha256
        (base32 "1z4bsdqb09srckgxzw0bri4x0arsf6r1arqybcx5z1rqc1rpfq55"))))))

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
             ;; guix download https://github.com/googlefonts/noto-cjk/releases/download/Serif2.002/01_NotoSerifCJK.ttc.zip -o /tmp/01_NotoSerifCJK.ttc.zip && rm -rf /tmp/01_NotoSerifCJK.ttc.zip
             "https://github.com/googlefonts/noto-cjk/releases/download/Serif"
             version "/01_NotoSerifCJK.ttc.zip"))
       (file-name (string-append name "-" version ".zip"))
       (sha256
        (base32 "11ag68imf7crfm5035zz2fvk64bfal536vg6xrv86gc0kzm272z7"))))))

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
        ;; git clone -b v2.042 --depth 1 https://github.com/googlefonts/noto-emoji /tmp/noto-emoji && guix hash --serializer=nar -x /tmp/noto-emoji && rm -rf /tmp/noto-emoji
        (base32
         "17i7awyqz9jv0j2blcf0smmpas375c3pdhjv1zqzl861g8qm1lm2"))))))

