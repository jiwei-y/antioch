;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2023, 2024 Giacomo Leidi <goodoldpaul@autistici.org>
;;; Copyright © 2024 Jiwei YANG <yangjiwei@protonmail.com>

(define-module (ac packages editors)
  #:use-module (gnu packages base)
  #:use-module (gnu packages gtk)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module ((guix licenses) :prefix license:)
  #:use-module (nonguix build-system chromium-binary)
  #:use-module (ice-9 match))

(define-public positron
  (package
    (name "positron")
    (version "2025.01.0-159")
    (source
     (let ((arch (match (or (%current-target-system) (%current-system))
                   (_ "x64")))
           (hash (match (or (%current-target-system) (%current-system))
                   (_
                    "0pk3dhxp7nhrkgdypshfacmx76ks9wz03dsdlhxa6zim95j3nzb2"))))
       (origin
        (method url-fetch)
        (uri
         (string-append
          "https://github.com/posit-dev/positron/releases/download/" version
          "/Positron-" version ".deb"))
        (sha256
         (base32 hash)))))
    (build-system chromium-binary-build-system)
    (arguments
     (list #:validate-runpath? #f ; TODO: fails on wrapped binary and included other files
           #:substitutable? #f
           #:wrapper-plan
           #~'("usr/share/positron/positron")
           #:phases
           #~(modify-phases %standard-phases
               (replace 'unpack
                 (lambda* (#:key source #:allow-other-keys)
                   (invoke "ar" "x" source)
                   (invoke "tar" "-xvf" "data.tar.xz")
                   (invoke "rm" "-rfv" "control.tar.xz"
                           "data.tar.xz"
                           "debian-binary")))
               (add-before 'install-wrapper 'install-entrypoint
                 (lambda _
                   (let* ((bin (string-append #$output "/bin")))
                     (delete-file (string-append #$output "/environment-variables"))
                     (mkdir-p bin)
                     (symlink (string-append #$output "/usr/share/positron/positron")
                              (string-append bin "/positron")))))
               (add-after 'install-entrypoint 'install-resources
                 (lambda _
                   (let* ((icons
                           (string-append #$output
                                          "/share/icons/hicolor/512x512/apps"))
                          (icon.png
                           (string-append #$output
                                          "/usr/share/pixmaps/co.posit.positron.png"))
                          (apps (string-append #$output "/share/applications")))
                     (mkdir-p icons)
                     (symlink icon.png
                              (string-append icons "/co.posit.positron.png"))
                     (mkdir-p apps)
                     (make-desktop-entry-file
                      (string-append apps "/" #$name ".desktop")
                      #:name "Positron"
                      #:generic-name "Text Editor"
                      #:exec (string-append #$output "/bin/positron --ozone-platform-hint=auto")
                      #:icon "co.posit.positron"
                      #:type "Application"
                      #:actions '("new-empty-window")
                      #:keywords '("vscode")
                      #:categories '("TextEditor" "Development"
                                     "IDE")
                      #:startup-notify #t
                      #:startup-w-m-class "Positron"
                      #:comment
                      '(("en" "Code Editing. Redefined.")
                        (#f "Code Editing. Redefined."))))))
               (add-after 'install-wrapper 'wrap-where-patchelf-does-not-work
                 (lambda _
                   (wrap-program (string-append #$output "/bin/positron")
                     `("LD_LIBRARY_PATH" ":"
                       prefix
                       (,(string-join
                          (list (string-append #$output "/usr/share/positron"))
                          ":")))))))))
    (supported-systems '("x86_64-linux"))
    (native-inputs
     (list tar))
    (inputs
     (list gdk-pixbuf))
    (home-page "https://positron.posit.co/")
    (synopsis "A next-generation data science IDE")
    (description "Positron is a next-generation data science IDE built by Posit PBC, an extensible, polyglot tool for writing code and exploring data, and a familiar environment for reproducible authoring and publishing.")
    (license license:expat)))
