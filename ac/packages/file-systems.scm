;;; SPDX-License-Identifier: AGPL-3.0-or-later
;;; Copyright © 2023 Jiwei YANG <yangjiwei@protonmail.com>

(define-module (ac packages file-systems)
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system go)
  #:use-module (guix build-system linux-module)
  #:use-module (guix build-system python)
  #:use-module (guix build-system trivial)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages acl)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages attr)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages cyrus-sasl)
  #:use-module (gnu packages datastructures)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages kerberos)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages nfs)
  #:use-module (gnu packages onc-rpc)
  #:use-module (gnu packages openldap)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages photo)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages rsync)
  #:use-module (gnu packages sssd)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages time)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages valgrind)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xml))

(define-public snapper
  (package
    (name "snapper")
    (version "0.10.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/openSUSE/snapper")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        ; git clone -b v0.10.3 --depth 1 https://github.com/openSUSE/snapper /tmp/snapper
        ; guix hash --serializer=nar -x /tmp/snapper
        ; rm -rf /tmp/snapper
        (base32 "17g02vfjj3w38mssmf1mcpjrxlpkzfd4inh80minl1wlsbjr4bd6"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           (delete-file-recursively "dists")
           (delete-file-recursively "zypp-plugin")
           (substitute* '("configure.ac" "doc/Makefile.am")
             ((".*dists.*") "")
             ((".*zypp-plugin.*") ""))
           (substitute* "Makefile.am"
             (("zypp-plugin") ""))))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'relative-file-locations
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out")))
               (substitute* (list "scripts/Makefile.am" "data/Makefile.am")
                 (("/usr/share") (string-append out "/share"))
                 (("/usr/lib") (string-append out "/lib"))
                 (("/etc/") (string-append out "/etc/"))))
               (substitute* "client/Makefile.am"
                 (("/usr/lib") "@libdir@")))))))
    (home-page "https://snapper.io")
    (native-inputs
     (list glibc-locales autoconf automake libtool pkg-config dbus))
    (inputs
    `(("btrfs" ,btrfs-progs)
      ("e2fs" ,e2fsprogs)
      ("libmount" ,util-linux "lib")
      ("dbus" ,dbus)
      ("libxml" ,libxml2)
      ("json-c" ,json-c)
      ("libacl" ,acl)
      ("boost" ,boost)
      ("libxslt" ,libxslt)
      ("docbook-xsl" ,docbook-xsl)
      ("gettext" ,gettext-minimal)
      ("pam" ,linux-pam)
      ("ncurses" ,ncurses/tinfo)))
    (synopsis "Manage filesystem snapshots and allow roll-backs")
    (description "\
This package provides Snapper, a tool that helps with managing
snapshots of Btrfs subvolumes and thin-provisioned LVM volumes.  It
can create and compare snapshots, revert differences between them, and
supports automatic snapshots timelines.")
    (license license:gpl2)))