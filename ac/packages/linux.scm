;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2023 Jiwei YANG <yangjiwei@protonmail.com>

(define-module (ac packages linux)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cpio)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages python-xyz)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system linux-module)
  #:use-module (guix build-system python)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix licenses)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (ac packages)
  #:use-module (ac utils download)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex))

(define %upstream-linux-source
  (@@ (gnu packages linux) %upstream-linux-source))

(define source-with-patches
  (@@ (gnu packages linux) source-with-patches))

(define %default-extra-linux-options
  (@@ (gnu packages linux) %default-extra-linux-options))

(define config->string
  (@@ (gnu packages linux) config->string))

(define-public upstream-version "6.9.1")
(define-public upstream-major-version
  (version-major+minor upstream-version))
(define-public xanmod-hardened-version  "6.9.1")
(define-public xanmod-version "6.9.1")
(define-public xanmod-revision "xanmod1")
(define-public hardened-version "6.9.1")
(define-public hardened-revision "hardened1")

(define-public linux-pristine-source
  (let ((version upstream-major-version)
        ;; mirror://kernel.org/linux/kernel/v6.x/linux-6.9.tar.xz
        ;; guix download https://cdn.kernel.org/pub/linux/kernel/v6.x/linux-6.9.tar.xz -o /tmp/ac/linux-6.9.tar.xz
        (hash (base32 "0jc14s7z2581qgd82lww25p7c4w72scpf49z8ll3wylwk3xh3yi4")))
    (%upstream-linux-source version hash)))

(define %xanmod-patch-main
  (origin
    (method url-fetch/xz-file)
    ;; guix download https://sourceforge.net/projects/xanmod/files/releases/main/6.9.1-xanmod1/patch-6.9.1-xanmod1.xz -o /tmp/ac/6.9.1-xanmod1.xz
    (file-name (string-append "linux-" xanmod-version "-" xanmod-revision ".patch"))
    (uri (string-append "https://sourceforge.net/projects/xanmod/files"
                        "/releases/main/" xanmod-version "-" xanmod-revision
                        "/patch-" xanmod-version "-" xanmod-revision ".xz"))
    (sha256 (base32
             "1wgrqvh52r0nwws21qj9fpkh21i45f6qs3dc00918w0xqnsnqmar"))))

(define %xanmod-patch-edge
  (origin
    (method url-fetch/xz-file)
    ;; guix download https://sourceforge.net/projects/xanmod/files/releases/edge/6.9.1-xanmod1/patch-6.9.1-xanmod1.xz -o /tmp/ac/6.9.1-xanmod1.xz
    (file-name (string-append "linux-" xanmod-version "-" xanmod-revision ".patch"))
    (uri (string-append "https://sourceforge.net/projects/xanmod/files"
                        "/releases/edge/" xanmod-version "-" xanmod-revision
                        "/patch-" xanmod-version "-" xanmod-revision ".xz"))
    (sha256 (base32
             "1231mqkdiiw1x7hg54x5brjlnd1dhzz8blk9fiad1dmjhdhba6mj"))))

(define %hardened-patch
  (origin
    (method url-fetch)
    ;; guix download https://github.com/anthraxx/linux-hardened/releases/download/6.9.1-hardened1/linux-hardened-6.9.1-hardened1.patch -o ~/all/antioch/ac/packages/patches/linux-6.9.1-hardened1.patch 
    (file-name (string-append "linux-" hardened-version "-" hardened-revision ".patch"))
    (uri (string-append
          "https://github.com/anthraxx/linux-hardened/releases/download/"
          hardened-version "-" hardened-revision "/linux-hardened-" hardened-version "-" hardened-revision ".patch"))
    (sha256 (base32
             "1zp0qwri43v4h234x1vqbwcbd50hryshi7i717xandzkpxvq72l2"))))

(define-public xanmod-hardened-source
  (origin
    (inherit (source-with-patches
              linux-pristine-source
              (list ;%xanmod-patch-main
                    %xanmod-patch-edge
                    ;%hardened-patch
                    ;; find ".procname	= "unprivileged_userns_clone",", delete that trunk
                    (local-file "patches/linux-6.9.1-hardened1.patch"))))
    (modules '((guix build utils)))))

;(define-public xanmod-source
;  (origin
;    (inherit (source-with-patches
;              linux-pristine-source
;              (list ;; %vfio-pci-pm-patch
;                    %xanmod-patch)))
;    (modules '((guix build utils)))))
;
;(define-public hardened-source
;  (origin
;    (inherit (source-with-patches
;              linux-pristine-source
;              (list ;; %vfio-pci-pm-patch
;                    %hardened-patch)))
;    (modules '((guix build utils)))))
  
(define %personal-extra-options
  `(  ;; kheaders module to avoid building failure
      ;("CONFIG_IKHEADERS" . #f)
      ;; modules required for initird
      ("CONFIG_CRYPTO_XTS" . m)
      ("CONFIG_VIRTIO_CONSOLE" . m)
      ;; built in VFIO modules for pci passthrough
      ;("CONFIG_VFIO_PCI" . #t)
      ;("CONFIG_VFIO_VIRQFD" . #t)
      ;; modprobe path on guix
      ("CONFIG_MODPROBE_PATH" . "/run/current-system/profile/bin/modprobe")

      ;; adjustment to khc
      ("CONFIG_MODULES" . #t)
      ("CONFIG_FB" . #t)
      ("CONFIG_VT" . #t)
      ("CONFIG_STATIC_USERMODEHELPER" . #f)
      ("CONFIG_SECURITY_LOADPIN" . #f)
      ("CONFIG_SECURITY_LOADPIN_ENFORCE" . #f)
      ("CONFIG_WERROR" . #f)
      ("CONFIG_LOCK_DOWN_KERNEL_FORCE_CONFIDENTIALITY" . #f)
      ("CONFIG_MODULE_SIG_FORCE" . #f)
      ("CONFIG_HIBERNATION" . #t)

      ;; Required by LKRG:
;      ("CONFIG_KALLSYMS" . #t)
;      ("CONFIG_KPROBES" . #t)
;      ("CONFIG_TRIM_UNUSED_KSYMS" . #f)   ;; required if lkrg should be built as an out-of-tree kernel module
;      ("CONFIG_SECURITY_SELINUX_BOOTPARAM" . #t)
;      ("CONFIG_SECURITY_SELINUX_DEVELOP" . #t)
      ;; LKRG in-tree module (not supported yet)
      ;("CONFIG_SECURITY_LKRG" . #t)

      ;; trimming
      ("CONFIG_DRM_XE" . #f)  ;; temporary solution to https://lkml.org/lkml/2024/1/24/1138

      ;; cpu specified optimisation
      ("CONFIG_GENERIC_CPU" . #f)
      ("CONFIG_GENERIC_CPU2" . #f)
      ("CONFIG_MZEN3" . #t)
      ("CONFIG_MNATIVE_INTEL" . #f)
      ("CONFIG_MNATIVE_AMD" . #f)))

(define-public linux-xanmod-hardened
  (let ((base (customize-linux #:name "linux-xanmod-hardened"
                               #:source xanmod-hardened-source
                               #:defconfig "config_x86-64-v3"
                               ;; Extraversion is used instead.
                               #:configs (config->string
                                          '(("CONFIG_LOCALVERSION" . "")))
                               #:extra-version xanmod-revision)))
    (package
      (inherit base)
      (version xanmod-hardened-version)
      (arguments
       (substitute-keyword-arguments (package-arguments base)
         ((#:configure-flags flags)
          `(append '("CFLAGS=-O3")
                  ,flags))
         ((#:modules modules)
          `((ice-9 popen)
            (ice-9 textual-ports)
            ,@modules))
         ((#:phases phases)
          #~(modify-phases #$phases
              (add-after 'unpack 'remove-localversion
                (lambda _
                  (when (file-exists? "localversion")
                    (delete-file "localversion"))))
              (add-after 'patch-source-shebangs 'patch-randstruct
              ;; customize the kernel RANDSTRUCT seed
                (lambda* (#:key inputs target #:allow-other-keys)
                          (substitute* "scripts/gen-randstruct-seed.sh"
                            (("od -A n -t x8 -N 32 /dev/urandom") 
                              "echo $ARCH $EXTRAVERSION $KBUILD_BUILD_USER $PATH $C_INCLUDE_PATH $CPLUS_INCLUDE_PATH | sha256sum | cut -d ' ' -f 1")
                            (("tr -d ' ") 
                              "tr -d '"))))
              (add-before 'configure 'hardening-config
                (lambda* (#:key outputs #:allow-other-keys)
                  (let* ((hardening-fragment "kernel-hardening-checker -g X86_64")
                         (port (open-input-pipe hardening-fragment))
                         (str (get-string-all port)))
                    (close-pipe port)
                    (call-with-output-file "fragment"
                      (lambda (port)
                        (format port "~a" str))))
                  (copy-file "CONFIGS/xanmod/gcc/config_x86-64-v3" ".config")
                  ;; Adapted from `make-linux-libre*'.
                  (chmod ".config" #o666)
                  (invoke "scripts/kconfig/merge_config.sh" "-m" ".config" "fragment")
                  #t))
              (add-before 'configure 'customize-config
                (lambda _
                  (let ((port (open-file ".config" "a"))
                        (extra-configuration #$(config->string
                                                ;; FIXME: There might be other
                                                ;; support missing.
                                                (append ;%waydroid-extra-linux-options
                                                        %personal-extra-options
                                                        %default-extra-linux-options))))
                    (display extra-configuration port)
                    (close-port port))
                  (invoke "make" "oldconfig")
                  (rename-file ".config" "arch/x86/configs/config_x86-64-v3")))
              (add-after 'configure 'fix-config
              ;; fix problem in guix
                (lambda* (#:key inputs #:allow-other-keys)
                  (substitute* '(".config" "arch/x86/configs/guix_defconfig")
                    (("CONFIG_LOCK_DOWN_KERNEL_FORCE_CONFIDENTIALITY=y") 
                    "# CONFIG_LOCK_DOWN_KERNEL_FORCE_CONFIDENTIALITY is not set"))))))))
      (native-inputs
       (modify-inputs (package-native-inputs base)
         ;; cpio is needed for CONFIG_IKHEADERS.
         (append gcc-13 cpio zstd kernel-hardening-checker-git)))
      (home-page "https://github.com/anthraxx/linux-hardened")
      (supported-systems '("x86_64-linux"))
      (synopsis
       "Xanmod + hardened")
      (description
       "Linux kernel with Xanmod and hardened patches"))))

(define-public lkrg-git
  (package 
    (inherit lkrg)
    (name "lkrg-git")
    (version "20231108")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/lkrg-org/lkrg")
                    (commit "3760e0e1bd1f05a4e5bcb0d1c91dfe40595e4d15")))
                    ;; https://github.com/lkrg-org/lkrg/commits/main
              (file-name (git-file-name name version))
              (sha256
               ; git clone --depth 1 https://github.com/lkrg-org/lkrg /tmp/ac/lkrg && guix hash --serializer=nar -x /tmp/ac/lkrg
               (base32
                "1wnzlbwrvg19v45sc8lbjvyha2554m21qpdx916dg7v2hbdgzkr1"))))
    (arguments
        (substitute-keyword-arguments (package-arguments lkrg)
          ((#:linux linux) linux-xanmod-hardened)
          ((#:phases phases '%standard-phases)
             `(modify-phases ,phases
                (add-after 'unpack 'patch-source
                  (lambda* (#:key inputs #:allow-other-keys)
                    (substitute* "src/modules/exploit_detection/syscalls/p_call_usermodehelper/p_call_usermodehelper.c"
                      ;; Use path to /gnu/store/*-kmod in actual path that is
                      ;; exec'ed.
                      (("\"/sbin/modprobe\"")
                      (string-append "\""
                                      (search-input-file inputs "/bin/modprobe")
                                      "\""))
                      ; (("/bin/true") (search-input-file inputs "/bin/true"))
                      ; (("/bin/false") (search-input-file inputs "/bin/false"))
                    )))))))
    (inputs (modify-inputs (package-inputs lkrg)
              (prepend coreutils
                       kmod)))))

(define-public lkrg-stable
  (package 
    (inherit lkrg-git)
    (name "lkrg-stable")
    (version "0.9.8")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/lkrg-org/lkrg")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               ; git clone -b v0.9.8 --depth 1 https://github.com/lkrg-org/lkrg /tmp/ac/lkrg && guix hash --serializer=nar -x /tmp/ac/lkrg
               (base32
                "0vwsbp57bsp3fj4kap791vj7swqpkb4jv18w8dcizp4dxnl4lvrp"))))))

(define-public kernel-hardening-checker-git
  (package
    (inherit kconfig-hardened-check)
    (name "kernel-hardening-checker-git")
    (version "20240514")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/a13xp0p0v/kernel-hardening-checker")
             ; https://github.com/a13xp0p0v/kernel-hardening-checker/commits/master
             (commit "35f7574150940bc88eb3192f64c2dbfc650cf3e3")))
       (file-name (git-file-name name version))
       (sha256
        ; git clone --depth 1 https://github.com/a13xp0p0v/kernel-hardening-checker /tmp/ac/kernel-hardening-checker && guix hash --serializer=nar -x /tmp/ac/kernel-hardening-checker
        (base32 "0m5n7v7xaqvhrw5qh2l1wkr79kfs8vl7chhq5a2zbwgizckav7gr"))))
    (license gpl3)))

(define-public tlp-git
  (package
    (inherit tlp)
    (name "tlp-git")
    (version "20240303")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/linrunner/TLP")
             (commit "3a12e12fd2bea8e7662c3dd7b646e124f8c60168")))
       (file-name (git-file-name name version))
       (sha256
        ;; git clone --depth 1 https://github.com/linrunner/TLP /tmp/ac/TLP && guix hash --serializer=nar -x /tmp/ac/TLP && rm -rf /tmp/ac/TLP
        (base32 "1gyhx1i2z2yipgpnjylmspzhv2cxmf5f0rx9si9l28zpjwan3g2y"))))
    (arguments
        (substitute-keyword-arguments (package-arguments tlp)
          ((#:phases phases)
          #~(modify-phases #$phases
              (add-after 'setenv 'setenv-extra
                (lambda* (#:key outputs #:allow-other-keys)
                  (let ((out (assoc-ref outputs "out")))
                    (setenv "TLP_ZSHCPL" 
                            (string-append out "/share/zsh/site-functions"))
                    (setenv "TLP_CONFDPR" 
                            (string-append out "/share/tlp/deprecated.conf")))))))))))

(define-public tlp-stable
  (package
    (inherit tlp)
    (name "tlp-stable")
    (version "1.6.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/linrunner/TLP")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        ;; git clone -b 1.6.1 --depth 1 https://github.com/linrunner/TLP /tmp/ac/TLP && guix hash --serializer=nar -x /tmp/ac/TLP && rm -rf /tmp/ac/TLP
        (base32 "0ybhcpx5vap4pv9j2id84xrg2c5nlzljiqyb1zww9sn59qlva4qb"))))
    (arguments
        (substitute-keyword-arguments (package-arguments tlp)
          ((#:phases phases)
          #~(modify-phases #$phases
              (add-after 'setenv 'setenv-extra
                (lambda* (#:key outputs #:allow-other-keys)
                  (let ((out (assoc-ref outputs "out")))
                    (setenv "TLP_ZSHCPL" 
                            (string-append out "/share/zsh/site-functions"))
                    (setenv "TLP_CONFDPR" 
                            (string-append out "/share/tlp/deprecated.conf")))))))))))

(define-public tuxedo-keyboard
  (package
    (name "tuxedo-keyboard")
    (version "3.1.1")
    (source (origin
              (method git-fetch)
              (uri
               (git-reference
                (url "https://github.com/tuxedocomputers/tuxedo-keyboard")
                (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               ; git clone -b v3.1.1 --depth 1 https://github.com/tuxedocomputers/tuxedo-keyboard /tmp/ac/tuxedo-keyboard
               ; guix hash --serializer=nar -x /tmp/ac/tuxedo-keyboard
               ; rm -rf /tmp/ac/tuxedo-keyboard
               (base32
                "17n14yh55yrxx4qbx4ph9drbzx2ll4kdsfmlngrdgizhyzk7z7zv"))))
    (build-system linux-module-build-system)
    (arguments
     (list #:linux linux-xanmod-hardened
           #:tests? #f
;            #:phases
;              #~(modify-phases %standard-phases
;                 (add-after 'unpack 'patch-randstruct
;                 ;; customize the kernel RANDSTRUCT seed
;                   (lambda* (#:key inputs target #:allow-other-keys)
;                     (let ((config (search-input-file linux-module-builder "lib/modules/build/CONFIGS/xanmod/gcc/config_x86-64-v3")))
;                             (setenv "RANDSTRUCT_CFG" config)
;                             (substitute* (search-input-file linux-module-builder "gen-randstruct-seed.sh")
;                               (("od -A n -t x8 -N 32 /dev/urandom") 
;                               "echo $ARCH $EXTRAVERSION $RANDSTRUCT_CFG $PATH $C_INCLUDE_PATH $CPLUS_INCLUDE_PATH | sha256sum | cut -d ' ' -f 1"))))))
))
    (home-page "https://github.com/tuxedocomputers/tuxedo-keyboard")
    (synopsis "Tuxedo computers kernel module for keyboard
backlighting")
    (description "Tuxedo computer kernel module for keyboard
backlighting.")
    (license gpl2)))