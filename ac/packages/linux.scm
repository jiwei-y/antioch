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

(define-public upstream-version "6.8.4")
(define-public upstream-major-version
  (version-major+minor upstream-version))
(define-public xanmod-hardened-version  "6.8.4")
(define-public xanmod-version "6.8.4")
(define-public xanmod-revision "xanmod1")
(define-public hardened-version "6.8.4")
(define-public hardened-revision "hardened1")

(define-public linux-pristine-source
  (let ((version upstream-major-version)
        ;; mirror://kernel.org/linux/kernel/v6.x/linux-6.8.tar.xz
        ;; guix download https://cdn.kernel.org/pub/linux/kernel/v6.x/linux-6.8.tar.xz -o /tmp/ac/linux-6.8.tar.xz && rm -rf /tmp/ac/linux-6.8.tar.xz
        (hash (base32 "1wv5x7qhcd05m8m0myyqm2il6mha1sx11h7ppf8yjsxvx2jdwsf9")))
    (%upstream-linux-source version hash)))

(define %xanmod-patch-main
  (origin
    (method url-fetch/xz-file)
    ;; guix download https://sourceforge.net/projects/xanmod/files/releases/main/6.8.4-xanmod1/patch-6.8.4-xanmod1.xz -o /tmp/ac/6.8.4-xanmod1.xz && rm -rf /tmp/ac/6.8.4-xanmod1.xz
    (file-name (string-append "linux-" xanmod-version "-" xanmod-revision ".patch"))
    (uri (string-append "https://sourceforge.net/projects/xanmod/files"
                        "/releases/main/" xanmod-version "-" xanmod-revision
                        "/patch-" xanmod-version "-" xanmod-revision ".xz"))
    (sha256 (base32
             "0jas8mn37nagd8k6qhywfch8mv7n083nzaw6kwdfiwamjccw7r5g"))))

(define %xanmod-patch-edge
  (origin
    (method url-fetch/xz-file)
    ;; guix download https://sourceforge.net/projects/xanmod/files/releases/edge/6.8.4-xanmod1/patch-6.8.4-xanmod1.xz -o /tmp/ac/6.8.4-xanmod1.xz && rm -rf /tmp/ac/6.8.4-xanmod1.xz
    (file-name (string-append "linux-" xanmod-version "-" xanmod-revision ".patch"))
    (uri (string-append "https://sourceforge.net/projects/xanmod/files"
                        "/releases/edge/" xanmod-version "-" xanmod-revision
                        "/patch-" xanmod-version "-" xanmod-revision ".xz"))
    (sha256 (base32
             "0jas8mn37nagd8k6qhywfch8mv7n083nzaw6kwdfiwamjccw7r5g"))))

(define %hardened-patch
  (origin
    (method url-fetch)
    ;; guix download https://github.com/anthraxx/linux-hardened/releases/download/6.8.4-hardened1/linux-hardened-6.8.4-hardened1.patch -o ~/all/antioch/ac/packages/patches/linux-6.8.4-hardened1.patch 
    (file-name (string-append "linux-" hardened-version "-" hardened-revision ".patch"))
    (uri (string-append
          "https://github.com/anthraxx/linux-hardened/releases/download/"
          hardened-version "-" hardened-revision "/linux-hardened-" hardened-version "-" hardened-revision ".patch"))
    (sha256 (base32
             "0jgax2x3qicic8r8dm6bfzh0r6v1xxrzdfycw4wgg7m12d0imxbl"))))

; (define %adjusted-hardened-patch
;   (let* ((version hardened-version)
;          (patch (string-append "linux-" version ".patch"))
;          (source (origin
;                    (method url-fetch)
;                    ;; guix download https://github.com/anthraxx/linux-hardened/releases/download/6.8.4-hardened1/linux-hardened-6.8.4-hardened1.patch -o /tmp/ac/linux-6.8.4-hardened1.patch
;                    (uri (string-append
;                          "https://github.com/anthraxx/linux-hardened/releases/download/"
;                          version "/linux-hardened-" version ".patch"))
;                    (sha256 (base32
;                             "12si2gy6maxbvf252ircp94ci0ihqlxv3l9sf4xwxrs66gn3z2fa")))))
;     (origin
;       (method computed-origin-method)
;       (file-name patch)
;       (sha256 #f)
;       (uri
;         (delay
;           (with-imported-modules '((guix build utils))
;             #~(begin
;                 (use-modules (guix build utils)
;                              (srfi srfi-1)
;                              (ice-9 match)
;                              (ice-9 ftw))
;                 (copy-file #+source #$patch)
;                 (make-file-writable #$patch)
;                 (chmod #$patch #o755)
;                 (substitute* #$patch
;                   (("SUBLEVEL = 15") "SUBLEVEL = 0")
;                   (("EXTRAVERSION = -hardened1") "EXTRAVERSION ="))
;                 (copy-file #$patch
;                            #$output))))))))

(define-public xanmod-hardened-source
  (origin
    (inherit (source-with-patches
              linux-pristine-source
              (list ;%xanmod-patch-main
                    %xanmod-patch-edge
                    ;%hardened-patch
                    ;; find ".procname	= "unprivileged_userns_clone",", delete that trunk
                    (local-file "patches/linux-6.8.4-hardened1.patch"))))
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

(define %waydroid-extra-linux-options
  `( ;Modules required for waydroid:
      ("CONFIG_ASHMEM" . #t)
      ("CONFIG_ANDROID" . #t)
      ("CONFIG_ANDROID_BINDER_IPC" . #t)
      ("CONFIG_ANDROID_BINDERFS" . #t)
      ("CONFIG_ANDROID_BINDER_DEVICES" . "binder,hwbinder,vndbinder")))

;(define %khc-extra-linux-options
;  `(  ;; kernel-hardening-checker (remember to apply the configs in the comments)
;      ;; wget -O ~/all/antioch/ac/packages/aux-files/config_x86-64-v3 https://github.com/xanmod/linux/raw/6.8/CONFIGS/;xanmod/gcc/config_x86-64-v3
;      ;; kernel-hardening-checker -m show_fail -c ~/all/antioch/ac/packages/aux-files/config_x86-64-v3
;      ;; CONFIG_LOCK_DOWN_KERNEL_FORCE_CONFIDENTIALITY MUST be not set
;
;      ;; dependencies of hardened config (UBSAN)
;      ("CONFIG_CC_HAS_UBSAN_BOUNDS" . #t)
;      ("CONFIG_CC_HAS_UBSAN_ARRAY_BOUNDS" . #t)
;
;      ("CONFIG_GCC_PLUGINS" . #t)
;      ("CONFIG_INIT_STACK_ALL_ZERO" . #t)
;      ("CONFIG_WERROR" . #t)
;      ("CONFIG_X86_KERNEL_IBT" . #t)
;      ("CONFIG_BUG_ON_DATA_CORRUPTION" . #t)
;      ("CONFIG_DEBUG_LIST" . #t)
;      ("CONFIG_DEBUG_VIRTUAL" . #t)
;      ("CONFIG_DEBUG_SG" . #t)
;      ("CONFIG_DEBUG_CREDENTIALS" . #t)
;      ("CONFIG_DEBUG_NOTIFIERS" . #t)
;      ("CONFIG_STATIC_USERMODEHELPER" . #t)
;      ("CONFIG_RANDSTRUCT_FULL" . #t)
;      ("CONFIG_RANDSTRUCT_PERFORMANCE" . #f)
;      ("CONFIG_GCC_PLUGIN_LATENT_ENTROPY" . #t)
;      ("CONFIG_MODULE_SIG_FORCE" . #t)
;      ("CONFIG_INIT_ON_FREE_DEFAULT_ON" . #t)
;      ("CONFIG_EFI_DISABLE_PCI_DMA" . #t)
;      ("CONFIG_UBSAN_BOUNDS" . #t)
;      ("CONFIG_UBSAN_LOCAL_BOUNDS" . #t)
;      ("CONFIG_UBSAN_TRAP" . #t)
;      ("CONFIG_UBSAN_SANITIZE_ALL" . #t)
;      ("CONFIG_GCC_PLUGIN_STACKLEAK" . #t)
;      ("CONFIG_STACKLEAK_METRICS" . #f)
;      ("CONFIG_STACKLEAK_RUNTIME_DISABLE" . #f)
;      ("CONFIG_CFI_CLANG" . #t)
;      ("CONFIG_CFI_PERMISSIVE" . #f)
;      ("CONFIG_IOMMU_DEFAULT_DMA_STRICT" . #t)
;      ("CONFIG_INTEL_IOMMU_DEFAULT_ON" . #t)
;      ("CONFIG_AMD_IOMMU_V2" . #t)
;      ("CONFIG_SLAB_MERGE_DEFAULT" . #f)
;      ("CONFIG_LIST_HARDENED" . #t)
;      ("CONFIG_RANDOM_KMALLOC_CACHES" . #t)
;      ("CONFIG_SECURITY_SELINUX_BOOTPARAM" . #f)
;      ("CONFIG_SECURITY_SELINUX_DEVELOP" . #f)
;      ;("CONFIG_LOCK_DOWN_KERNEL_FORCE_CONFIDENTIALITY" . #t)
;      ("CONFIG_BINFMT_MISC" . #f)
;      ("CONFIG_INET_DIAG" . #f)
;      ("CONFIG_KEXEC" . #f)
;      ("CONFIG_PROC_KCORE" . #f)
;      ("CONFIG_LEGACY_PTYS" . #f)
;      ("CONFIG_HIBERNATION" . #f)
;      ("CONFIG_COMPAT" . #f)
;      ("CONFIG_IA32_EMULATION" . #f)
;      ("CONFIG_MODIFY_LDT_SYSCALL" . #f)
;      ("CONFIG_X86_MSR" . #f)
;      ("CONFIG_LEGACY_TIOCSTI" . #f)
;      ("CONFIG_MODULES" . #f)
;      ("CONFIG_DEVMEM" . #f)
;      ("CONFIG_IO_STRICT_DEVMEM" . #t)
;      ("CONFIG_LDISC_AUTOLOAD" . #f)
;      ("CONFIG_X86_VSYSCALL_EMULATION" . #f)
;      ("CONFIG_PROC_VMCORE" . #f)
;      ("CONFIG_PROC_PAGE_MONITOR" . #f)
;      ("CONFIG_USELIB" . #f)
;      ("CONFIG_CHECKPOINT_RESTORE" . #f)
;      ("CONFIG_USERFAULTFD" . #f)
;      ("CONFIG_HWPOISON_INJECT" . #f)
;      ("CONFIG_MEM_SOFT_DIRTY" . #f)
;      ("CONFIG_DEVPORT" . #f)
;      ("CONFIG_DEBUG_FS" . #f)
;      ("CONFIG_NOTIFIER_ERROR_INJECTION" . #f)
;      ("CONFIG_PUNIT_ATOM_DEBUG" . #f)
;      ("CONFIG_ACPI_CONFIGFS" . #f)
;      ("CONFIG_MTD_SLRAM" . #f)
;      ("CONFIG_MTD_PHRAM" . #f)
;      ("CONFIG_IO_URING" . #f)
;      ("CONFIG_KCMP" . #f)
;      ("CONFIG_RSEQ" . #f)
;      ("CONFIG_SUNRPC_DEBUG" . #f)
;      ("CONFIG_FB" . #f)
;      ("CONFIG_VT" . #f)
;      ("CONFIG_BLK_DEV_FD" . #f)
;      ("CONFIG_STAGING" . #f)
;      ("CONFIG_KSM" . #f)
;      ("CONFIG_KALLSYMS" . #f)
;      ("CONFIG_MAGIC_SYSRQ" . #f)
;      ("CONFIG_KEXEC_FILE" . #f)
;      ("CONFIG_USER_NS" . #f)
;      ("CONFIG_X86_CPUID" . #f)
;      ("CONFIG_X86_IOPL_IOPERM" . #f)
;      ("CONFIG_ACPI_TABLE_UPGRADE" . #f)
;      ("CONFIG_EFI_CUSTOM_SSDT_OVERLAYS" . #f)
;      ("CONFIG_AIO" . #f)
;      ("CONFIG_EFI_TEST" . #f)
;      ("CONFIG_KPROBES" . #f)
;      ("CONFIG_BPF_SYSCALL" . #f)
;      ("CONFIG_IP_DCCP" . #f)
;      ("CONFIG_IP_SCTP" . #f)
;      ("CONFIG_VIDEO_VIVID" . #f)
;      ("CONFIG_KGDB" . #f)
;      ("CONFIG_XFS_SUPPORT_V4" . #f)
;      ("CONFIG_TRIM_UNUSED_KSYMS" . #t)
;      ("CONFIG_COREDUMP" . #f)
;    ))
  
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
      ("CONFIG_DRM_XE" . #f)  ;; temporary solution to https://lkml.org/lkml/2024/1/24/1138
      
      ;; Required by LKRG:
      ("CONFIG_KALLSYMS" . #t)
      ("CONFIG_KPROBES" . #t)
      ("CONFIG_TRIM_UNUSED_KSYMS" . #f)   ;; required if lkrg should be built as an out-of-tree kernel module
      ("CONFIG_SECURITY_SELINUX_BOOTPARAM" . #t)
      ("CONFIG_SECURITY_SELINUX_DEVELOP" . #t)
      ;; LKRG in-tree module (not supported yet)
      ;("CONFIG_SECURITY_LKRG" . #t)

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
                                                        ;%khc-extra-linux-options   ;; automated, no longer needed
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
    (version "20240330")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/a13xp0p0v/kernel-hardening-checker")
             ; https://github.com/a13xp0p0v/kernel-hardening-checker/commits/master
             (commit "74963559e760568c1d4b11e4c120e792efd428e4")))
       (file-name (git-file-name name version))
       (sha256
        ; git clone --depth 1 https://github.com/a13xp0p0v/kernel-hardening-checker /tmp/ac/kernel-hardening-checker && guix hash --serializer=nar -x /tmp/ac/kernel-hardening-checker
        (base32 "15jijf28a2qbpcz7znkaraxygz2g69yaqv0cnpwrrzp7w2cvdd94"))))
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