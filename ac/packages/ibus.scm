;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2023 Jiwei YANG <yangjiwei@protonmail.com>


(define-module (ac packages ibus)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages hunspell)
  #:use-module (gnu packages ibus)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages ninja)
  #:use-module (gnu packages ocr)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages xorg)
  #:use-module (srfi srfi-1))

(define-public ibus-typing-booster
  (package
    (name "ibus-typing-booster")
    (version "2.25.7")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/mike-fabian/ibus-typing-booster/releases/download/"
                    version "/ibus-typing-booster-" version ".tar.gz"))
              (sha256
               ;; guix download https://github.com/mike-fabian/ibus-typing-booster/releases/download/2.25.7/ibus-typing-booster-2.25.7.tar.gz -o /tmp/ac/ibus-typing-booster-2.25.7.tar.gz
               (base32
                "13xfqcs57bihwm6ncgamj7w7cizavy67w5501qana341znzr1mnz"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'wrap-programs
            (lambda* (#:key inputs #:allow-other-keys)
              (for-each
               (lambda (prog)
                 (wrap-program (string-append #$output prog)
                   `("GUIX_PYTHONPATH" ":" prefix
                     (,(getenv "GUIX_PYTHONPATH")))
                   `("GI_TYPELIB_PATH" ":" prefix
                     (,(getenv "GI_TYPELIB_PATH")
                      ,(string-append #$output "/lib/girepository-1.0")))
                   `("LD_LIBRARY_PATH" ":" prefix
                     (,(string-append (assoc-ref inputs "m17n-lib") "/lib")))
                   `("DICPATH" ":" prefix
                     (,(string-append (assoc-ref inputs "hunspell-dict-fr-moderne") "/share/hunspell")
                      ,(string-append (assoc-ref inputs "hunspell-dict-pl") "/share/hunspell")
                      ,(string-append (assoc-ref inputs "hunspell-dict-de") "/share/hunspell")
                      ,(string-append (assoc-ref inputs "hunspell-dict-hu") "/share/hunspell")
                      ,(string-append (assoc-ref inputs "hunspell-dict-he-il") "/share/hunspell")
                      ,(string-append (assoc-ref inputs "hunspell-dict-it-it") "/share/hunspell")
                      ,(string-append (assoc-ref inputs "hunspell-dict-en") "/share/hunspell")
                      ,(string-append (assoc-ref inputs "hunspell-dict-en-au") "/share/hunspell")
                      ,(string-append (assoc-ref inputs "hunspell-dict-en-ca") "/share/hunspell")
                      ,(string-append (assoc-ref inputs "hunspell-dict-en-gb") "/share/hunspell")
                      ,(string-append (assoc-ref inputs "hunspell-dict-en-gb-ize") "/share/hunspell")
                      ,(string-append (assoc-ref inputs "hunspell-dict-en-us") "/share/hunspell")))
                    ))
               '("/bin/emoji-picker"
                 "/libexec/ibus-engine-typing-booster"
                 "/libexec/ibus-setup-typing-booster"))))
          (delete 'check))))
    (native-inputs
     (list pkg-config
           gobject-introspection))
    (inputs
     (list python
           python-dbus
           python-pygobject
           gtk+
           ibus
           hunspell-dict-fr-classique
           hunspell-dict-fr-moderne
           hunspell-dict-fr-réforme-1990
           hunspell-dict-fr-toutes-variantes
           hunspell-dict-pl
           hunspell-dict-de
           hunspell-dict-hu
           hunspell-dict-he-il
           hunspell-dict-it-it
           hunspell-dict-en
           hunspell-dict-en-au
           hunspell-dict-en-ca
           hunspell-dict-en-gb
           hunspell-dict-en-gb-ize
           hunspell-dict-en-us
           m17n-lib))
    (synopsis "A completion input method for faster typing")
    (description "Ibus-typing-booster is a completion input method to speed-up typing.

The project was started in 2010 for Fedora 15. The original purpose was to make typing of Indic languages easier and faster by providing completion and spell checking suggestions.

Originally it was forked from ibus-table whose developer was Yu Yuwei acevery@gmail.com, with contributions from Caius(\"kaio\") chanceme@kaio.net.

Since then ibus-typing-booster has been improved to support many other languages as well (i.e. most languages except Chinese and Japanese are supported).

Recently the capability to type different languages at the same time without having to switch between languages has been added.")
    (home-page "https://github.com/mike-fabian/ibus-typing-booster")
    (license license:gpl3+)))

(define-public ibus-mozc
  (package
    (name "ibus-mozc")
    (version "20230426")    ;; the last version supporting gyp, TODO: use bazel with guix-science
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/google/mozc")
                    (commit "cde23396637d4fffa2b35774892ae10fc0156b5f")
                    (recursive? #t)))
              (sha256
               ;; git clone --depth 1 --recurse-submodules https://github.com/google/mozc /tmp/ac/mozc && guix hash --serializer=nar -x /tmp/ac/mozc && rm -rf /tmp/ac/mozc
               (base32 "073c1mnpn1lj6i0h0qryivjcc895yx8vnc0iap49bxyw1m74a45d"))))
    (build-system python-build-system)
    (arguments
     `(#:use-setuptools? #f
       #:tests? #f
       #:modules ((ice-9 match)
                  ,@%python-build-system-modules)
       #:phases
       (modify-phases %standard-phases
;         (add-after 'unpack 'symlink
;           (lambda* (#:key inputs #:allow-other-keys)
;             (let ((gyp (assoc-ref inputs "python-gyp")))
;               (rmdir "src/third_party/gyp/")
;               (symlink gyp "src/third_party/gyp"))))
         (add-after 'unpack 'fix-gpp
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((gcc (assoc-ref inputs "gcc-toolchain")))
               (setenv "CPLUS_INCLUDE_PATH"
                       (string-append gcc "/include/c++:"
                                      gcc "/include:"
                                      gcc "/include/c++/x86_64-unknown-linux-gnu:"
                                      (getenv "CPLUS_INCLUDE_PATH"))))))
         (add-after 'fix-gpp 'preconfigure
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "src/gyp/common.gypi"
               (("-lc++") 
               "-lstdc"))))
         (replace 'configure
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((gyp (assoc-ref inputs "python-gyp"))
                   (out (assoc-ref outputs "out")))
               ;; (chdir "src")
               (add-installed-pythonpath inputs outputs)
               (setenv "GYP_DEFINES" 
                        (string-append
                          "document_dir=" out "/share/doc/mozc" " "
                          "use_libzinnia=1" " "
                          "use_libprotobuf=1" " "
                          "ibus_mozc_path=" out "/libexec/ibus-mozc/ibus-engine-mozc" " "
                          "ibus_mozc_icon_path=" out "/share/ibus-mozc/product_icon.png" " "
                          "mozc_dir=" out "/libexec/mozc" " "
                          "mozc_icons_dir=" out "/share/icons/mozc" " "
                          "ibus_component_dir=" out "/share/ibus/component" " "
                          "ibus_mozc_install_dir=" out "/share/ibus-mozc" " "
                          "emacs_helper_dir=" out "/bin" " "
                          "emacs_client_dir=" out "/share/emacs/site-lisp/emacs-mozc"))
                       (invoke "python" "src/build_mozc.py" "gyp"
                               (string-append "--gypdir=" gyp "/bin")
                               (string-append "--server_dir="
                                              out "/libexec/mozc")
                               "--target_platform=Linux"))))
         (replace 'build
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (add-installed-pythonpath inputs outputs)
             (invoke "python" "src/build_mozc.py" "build" "-c" "Release"
                     "unix/ibus/ibus.gyp:ibus_mozc"
                     "unix/emacs/emacs.gyp:mozc_emacs_helper"
                     "server/server.gyp:mozc_server"
                     "gui/gui.gyp:mozc_tool"
                     ;"renderer/renderer.gyp:mozc_renderer"
                     "--use_gyp_for_ibus_build")))
         (replace 'install
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (ibus_mozc_exec_dir (string-append out "/libexec/ibus-mozc"))
                    (ibus_component_dir (string-append out "/share/ibus/component"))
                    (ibus_mozc_install_dir (string-append out "/share/ibus-mozc"))
                    (mozc_dir (string-append out "/libexec/mozc")))
               (add-installed-pythonpath inputs outputs)
               (rename-file "src/out_linux/Release/ibus_mozc" "src/out_linux/Release/ibus-engine-mozc")
               (for-each (lambda (name)
                           (install-file name ibus_mozc_exec_dir))
                         '("src/out_linux/Release/ibus-engine-mozc"))
               (for-each (lambda (name)
                           (install-file name ibus_component_dir))
                         '("src/out_linux/Release/gen/unix/ibus/mozc.xml"))
               (rename-file "src/data/images/unix/ime_product_icon_opensource-32.png" "src/data/images/unix/product_icon.png")
               (rename-file "src/data/images/unix/ui-tool.png" "src/data/images/unix/tool.png")
               (rename-file "src/data/images/unix/ui-properties.png" "src/data/images/unix/properties.png")
               (rename-file "src/data/images/unix/ui-dictionary.png" "src/data/images/unix/dictionary.png")
               (rename-file "src/data/images/unix/ui-direct.png" "src/data/images/unix/direct.png")
               (rename-file "src/data/images/unix/ui-hiragana.png" "src/data/images/unix/hiragana.png")
               (rename-file "src/data/images/unix/ui-katakana_half.png" "src/data/images/unix/katakana_half.png")
               (rename-file "src/data/images/unix/ui-katakana_full.png" "src/data/images/unix/katakana_full.png")
               (rename-file "src/data/images/unix/ui-alpha_half.png" "src/data/images/unix/alpha_half.png")
               (rename-file "src/data/images/unix/ui-alpha_full.png" "src/data/images/unix/alpha_full.png")
               (for-each (lambda (name)
                           (install-file name ibus_mozc_install_dir))
                         '("src/data/images/unix/product_icon.png"
                           "src/data/images/unix/tool.png"
                           "src/data/images/unix/properties.png"
                           "src/data/images/unix/dictionary.png"
                           "src/data/images/unix/direct.png"
                           "src/data/images/unix/hiragana.png"
                           "src/data/images/unix/katakana_half.png"
                           "src/data/images/unix/katakana_full.png"
                           "src/data/images/unix/alpha_half.png"
                           "src/data/images/unix/alpha_full.png"))
               (for-each (lambda (name)
                           (install-file name mozc_dir))
                         '("src/out_linux/Release/mozc_server"
                           "src/out_linux/Release/mozc_tool"
                           "src/out_linux/Release/mozc_emacs_helper")))))
         (add-after 'install 'wrap
           (lambda* (#:key outputs #:allow-other-keys)
             (wrap-program (string-append (assoc-ref outputs "out")
                                          "/libexec/ibus-mozc/ibus-engine-mozc")
               `("GUIX_PYTHONPATH" ":" prefix (,(getenv "GUIX_PYTHONPATH")))
               ;`("GI_TYPELIB_PATH" ":" prefix (,(getenv "GI_TYPELIB_PATH")))
              )
             #t))
         (delete 'check)
      )))
    (inputs
      (list protobuf
            ibus
            gtk+-2
            libxcb
            qtbase-5
            zinnia))
    (native-inputs
      (list clang
            gcc-toolchain
            python
            python-six
            python-gyp
            ninja
            pkg-config))
    (synopsis "A Japanese Input Method Editor designed for multi-platform")
    (description
     "Mozc is a Japanese Input Method Editor (IME) designed for multi-platform such as Android OS, Apple OS X, Chromium OS, GNU/Linux and Microsoft Windows. This OpenSource project originates from Google Japanese Input.")
    (home-page "https://github.com/google/mozc")
    (license license:bsd-3)))

(define-public ibus-mozc-ut
  (package
    (inherit ibus-mozc)
    (name "ibus-mozc-ut")
    (version "20230426")
    (arguments
        (substitute-keyword-arguments (package-arguments ibus-mozc)
          ((#:modules modules %python-build-system-modules)
           `((ice-9 match) ,@modules))
          ((#:phases phases)
           #~(modify-phases #$phases
               (add-after 'fix-gpp 'add-ut
                 (lambda* (#:key inputs #:allow-other-keys)
                   (let ((target "src")
                         (mergeut (assoc-ref inputs "merge-ut-dictionaries")))
                     (copy-recursively
                       (string-append mergeut "/src") target)
                     (for-each
                       (match-lambda
                         ((name . path)
                          (if (string-prefix? "mozcdic-ut-" name)
                              (let ((dict (string-append path "/" name ".txt.tar.bz2")))
                                (install-file dict target)))))
                       inputs)
                     (install-file
                       (assoc-ref inputs "jawiki-titles") target)
                     (for-each (lambda (file)
                                 (rename-file file "src/jawiki-latest-all-titles-in-ns0.gz"))
                               (find-files target ".*jawiki-latest-all-titles-.*"))
                     (substitute* (string-append target "/make.sh")
                       (("#alt_cannadic") "alt_cannadic")
                       (("#edict") "edict")
                       (("#skk_jisyo") "skk_jisyo")
                       (("#sudachidict") "sudachidict")
                       (("^git clone.*") "printf done\n")
                       (("ruby apply_word_hits.rb mozcdic-ut.txt") "ruby apply_word_hits.rb mozcdic-ut.txt\n cat mozcdic-ut.txt >> data/dictionary_oss/dictionary00.txt"))
                     (substitute* (string-append target "/count_word_hits.rb")
                       (("^`wget.*$") ""))
                     (substitute* (string-append target "/remove_duplicate_ut_entries.rb")
                       (("https://raw.githubusercontent.com/google/mozc/master/src/") 
                        ""))
                     (for-each make-file-writable (find-files target "mozcdic-ut-"))
                     (for-each make-file-writable (find-files target "jawiki-latest-all-titles-"))
                     (with-directory-excursion target
                       (invoke "bash" "make.sh")))
                   #t))))))
    (inputs
     `(("protobuf" ,protobuf)
       ("ibus" ,ibus)
       ("gtk+-2" ,gtk+-2)
       ("libxcb" ,libxcb)
       ("qtbase-5" ,qtbase-5)
       ("zinnia" ,zinnia)
       ("mozcdic-ext"
         ,(origin
           (method git-fetch)
           (uri (git-reference
                 (url "https://github.com/reasonset/mozcdict-ext")
                 (commit "01490262fe58576b4356734468c973bc46acdd33")))
           (file-name "mozcdic-ext")
           (sha256
             ;; git clone --depth 1 https://github.com/reasonset/mozcdict-ext /tmp/ac/mozcdic-ext && guix hash  --serializer=nar -x /tmp/ac/mozcdic-ext && rm -rf /tmp/ac/mozcdic-ext
             (base32
               "17463m3rnfgmj6yndym4avhnjpk0ivr1kmbfdawga6j1fq0m5468"))))
       ("merge-ut-dictionaries"
         ,(origin
           (method git-fetch)
           (uri (git-reference
                 (url "https://github.com/utuhiro78/merge-ut-dictionaries")
                 (commit "c4daa91f69ee6d45eaec510cbaa58075d29ba820")))
           (file-name "merge-ut-dictionaries")
           (sha256
             ;; git clone --depth 1 https://github.com/utuhiro78/merge-ut-dictionaries /tmp/ac/merge-ut-dictionaries &&  guix hash --serializer=nar -x /tmp/ac/merge-ut-dictionaries && rm -rf /tmp/ac/merge-ut-dictionaries
             (base32
               "0ck0hc93z5v3vp8dqymczgpfvcw0pym0npxlixhazrv91dyrcy56"))))
       ("mozcdic-ut-alt-cannadic"
         ,(origin
           (method git-fetch)
           (uri (git-reference
                 (url "https://github.com/utuhiro78/mozcdic-ut-alt-cannadic")
                 (commit "f59287e569db3e226378380a34e71275654b46d0")))
           (file-name "mozcdic-ut-alt-cannadic")
           (sha256
             ;; git clone --depth 1 https://github.com/utuhiro78/mozcdic-ut-alt-cannadic /tmp/ac/mozcdic-ut-alt-cannadic && guix hash --serializer=nar -x /tmp/ac/mozcdic-ut-alt-cannadic && rm -rf /tmp/ac/mozcdic-ut-alt-cannadic
             (base32
               "1rmiahhrc8gmcsa2y5agchaw3pkfiajh3pl2q3r1c0s6d6c3mmbb"))))
       ("mozcdic-ut-edict2"
         ,(origin
           (method git-fetch)
           (uri (git-reference
                 (url "https://github.com/utuhiro78/mozcdic-ut-edict2")
                 (commit "b976b6720a9ce38bea26b457af4678ab3d76b971")))
           (file-name "mozcdic-ut-edict2")
           (sha256
             ;; git clone --depth 1 https://github.com/utuhiro78/mozcdic-ut-edict2 /tmp/ac/mozcdic-ut-edict2 && guix hash  --serializer=nar -x /tmp/ac/mozcdic-ut-edict2 && rm -rf /tmp/ac/mozcdic-ut-edict2
             (base32
               "0aayj1ydav3p6bajjd85gd7jzdx724kq4565njf9966slnv19z8f"))))
       ("mozcdic-ut-jawiki"
         ,(origin
           (method git-fetch)
           (uri (git-reference
                 (url "https://github.com/utuhiro78/mozcdic-ut-jawiki")
                 (commit "25d82b3def00c3d8b5ccff660952d317a8f5c6c6")))
           (file-name "mozcdic-ut-jawiki")
           (sha256
             ;; git clone --depth 1 https://github.com/utuhiro78/mozcdic-ut-jawiki /tmp/ac/mozcdic-ut-jawiki && guix hash  --serializer=nar -x /tmp/ac/mozcdic-ut-jawiki && rm -rf /tmp/ac/mozcdic-ut-jawiki
             (base32
               "0z37s5gdsnk8rf6mfg3p3kn50svny6mxs9mzw6acbb6zsawx5qkr"))))
       ("mozcdic-ut-neologd"
         ,(origin
           (method git-fetch)
           (uri (git-reference
                 (url "https://github.com/utuhiro78/mozcdic-ut-neologd")
                 (commit "90e59c7707a5fe250c992c10c6ceb08a7ce7e652")))
           (file-name "mozcdic-ut-neologd")
           (sha256
             ;; git clone --depth 1 https://github.com/utuhiro78/mozcdic-ut-neologd /tmp/ac/mozcdic-ut-neologd && guix  hash --serializer=nar -x /tmp/ac/mozcdic-ut-neologd && rm -rf /tmp/ac/mozcdic-ut-neologd
             (base32
               "04d3z9sbrrxvb8qp670n99mfl4zk5igb1z5q3v819k0fkvycm3nd"))))
       ("mozcdic-ut-personal-names"
         ,(origin
           (method git-fetch)
           (uri (git-reference
                 (url "https://github.com/utuhiro78/mozcdic-ut-personal-names")
                 (commit "c96f2a22aaf2bddcca0dcb0b028ed6c7188f6e17")))
           (file-name "mozcdic-ut-personal-names")
           (sha256
             ;; git clone --depth 1 https://github.com/utuhiro78/mozcdic-ut-personal-names /tmp/ac/mozcdic-ut-personal-names && guix hash --serializer=nar -x /tmp/ac/mozcdic-ut-personal-names && rm -rf /tmp/ac/mozcdic-ut-personal-names
             (base32
               "11b586adhjfrzf986rjd61k5nwnqz4py6qania02x220lssyibpv"))))
       ("mozcdic-ut-place-names"
         ,(origin
           (method git-fetch)
           (uri (git-reference
                 (url "https://github.com/utuhiro78/mozcdic-ut-place-names")
                 (commit "754722f01544c06a6bb5bb590e704d86334dc6b1")))
           (file-name "mozcdic-ut-place-names")
           (sha256
             ;; git clone --depth 1 https://github.com/utuhiro78/mozcdic-ut-place-names /tmp/ac/mozcdic-ut-place-names &&  guix hash --serializer=nar -x /tmp/ac/mozcdic-ut-place-names && rm -rf /tmp/ac/mozcdic-ut-place-names
             (base32
               "1qgdh9vxwcsgpy32svyrz2m1hfi1g55rky71g2px7jxbam2p0y2g"))))
       ("mozcdic-ut-skk-jisyo"
         ,(origin
           (method git-fetch)
           (uri (git-reference
                 (url "https://github.com/utuhiro78/mozcdic-ut-skk-jisyo")
                 (commit "43518e6ea033681580a515281668c85eb74a5b14")))
           (file-name "mozcdic-ut-skk-jisyo")
           (sha256
             ;; git clone --depth 1 https://github.com/utuhiro78/mozcdic-ut-skk-jisyo /tmp/ac/mozcdic-ut-skk-jisyo && guix  hash --serializer=nar -x /tmp/ac/mozcdic-ut-skk-jisyo && rm -rf /tmp/ac/mozcdic-ut-skk-jisyo
             (base32
               "0idr6zdlkg01vybisf1kd3q2kxpgpi0kipd373pr2jvcx7zgz56k"))))
       ("mozcdic-ut-sudachidict"
         ,(origin
           (method git-fetch)
           (uri (git-reference
                 (url "https://github.com/utuhiro78/mozcdic-ut-sudachidict")
                 (commit "39c8c2c16b326a5d9ba63a9d2f13032533ae1634")))
           (file-name "mozcdic-ut-sudachidict")
           (sha256
             ;; git clone --depth 1 https://github.com/utuhiro78/mozcdic-ut-sudachidict /tmp/ac/mozcdic-ut-sudachidict &&  guix hash --serializer=nar -x /tmp/ac/mozcdic-ut-sudachidict && rm -rf /tmp/ac/mozcdic-ut-sudachidict
             (base32
               "0ns4alj8pfprp6bncngss94swsrgxxi7zqa05zw34qg6sjch2gbm"))))
       ("jawiki-titles"
         ,(origin
           (method url-fetch)
           (uri "https://dumps.wikimedia.org/jawiki/latest/jawiki-latest-all-titles-in-ns0.gz")
           (file-name "jawiki-latest-all-titles-in-ns0.gz")
           (sha256
             ;; guix download https://dumps.wikimedia.org/jawiki/latest/jawiki-latest-all-titles-in-ns0.gz -o /tmp/ac/jawiki-latest-all-titles-in-ns0.gz && rm -rf /tmp/ac/jawiki-latest-all-titles-in-ns0.gz
             (base32
               "0l0vx44n7ns0ir273faj51cfp83nsnxqnlxj7yw1cmdxdlsi0zab"))))))
    (native-inputs
      (modify-inputs (package-native-inputs ibus-mozc)
        (append coreutils ruby tar)))))
