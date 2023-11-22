;;; SPDX-License-Identifier: AGPL-3.0-or-later
;;; Copyright © 2023 Jiwei YANG <yangjiwei@protonmail.com>

(define-module (ac packages networking)
  #:use-module (gnu packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix build-system go)
  #:use-module (guix build-system qt)
  #:use-module (nonguix build-system binary)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix modules)
  #:use-module (gnu packages adns)
  #:use-module (gnu packages aidc)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages regex)
  #:use-module (gnu packages rpc)
  #:use-module (gnu packages serialization)
)

(define-public v2ray
  (package
    (name "v2ray")
    (version "5.1.0")
    (source (origin
              (method url-fetch)
              ;; https://github.com/v2fly/v2ray-core/releases/download/v5.1.0/v2ray-linux-64.zip
              (uri (string-append
                    "https://github.com/v2fly/v2ray-core/releases/download/v"
                    version "/v2ray-linux-64.zip"))
              (sha256
               (base32
                "0slfa8bdphhhwwd2ycjnjl7lp549x5wjvwd1ymhp7crl2x536w76"))))
    (build-system binary-build-system)
    (arguments
     `(#:install-plan '(("v2ray" "/bin/")
                        ("geoip.dat" "/bin/")
                        ("geosite.dat" "/bin/"))
       #:phases (modify-phases %standard-phases
                  (replace 'unpack
                    (lambda* (#:key source #:allow-other-keys)
                      (invoke "unzip" source))))))
    (native-inputs (list unzip))
    (home-page "https://github.com/v2fly/v2ray-core")
    (synopsis
     "A platform for building proxies to bypass network restrictions.")
    (description
     "Project V is a set of network tools that help you to build your
own computer network. It secures your network connections and thus protects your
privacy. See our website for more information.")
    (license license:expat)))

(define-public v2ray-sn
  (package
    (inherit v2ray)
    (name "v2ray-sn")
    (version "5.0.16")
    (source (origin
              (method url-fetch)
              ;; https://github.com/SagerNet/v2ray-core/releases/download/v5.0.16/v2ray-linux-64.zip
              (uri (string-append
                    "https://github.com/SagerNet/v2ray-core/releases/download/v"
                    version "/v2ray-linux-64.zip"))
              (sha256
               (base32
                "0f5aikgc2hnnx93wp9b1z0ms2rsyx9yyv1db4p7gjprn2dnkv3v6"))))
    (build-system binary-build-system)
    (arguments
     `(#:install-plan '(("v2ray" "/bin/")
                        ("geoip.dat" "/bin/")
                        ("geosite.dat" "/bin/"))
       #:phases (modify-phases %standard-phases
                  (replace 'unpack
                    (lambda* (#:key source #:allow-other-keys)
                      (invoke "unzip" source))))))
    (native-inputs (list unzip))
    (home-page "https://github.com/SagerNet/v2ray-core")
    (synopsis
     "A platform for building proxies to bypass network restrictions (for SagerNet :)")
    (description
     "Project V is a set of network tools that help you to build your
own computer network. It secures your network connections and thus protects your
privacy. See our website for more information.")
    (license license:expat)))

(define-public qv2ray
  (package
    (name "qv2ray")
    (version "fb44fb1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/Qv2ray/Qv2ray")
                    ; (commit (string-append "v" version))
                    (commit "fb44fb1421941ab192229ff133bc28feeb4a8ce5")
                    (recursive? #t)))
              (file-name (git-file-name name version))
              ; git clone https://github.com/Qv2ray/Qv2ray /tmp/Qv2ray
              ; guix hash --serializer=nar -x /tmp/Qv2ray
              ; rm -rf /tmp/Qv2ray
              (sha256
               (base32
                "1dg7i1488pdbq682nrqzbj8xy271ngb09sbm2q80mj6ann006y2f"))))
    (build-system qt-build-system)
    (arguments
     `(#:configure-flags '("-DQV2RAY_DISABLE_AUTO_UPDATE=ON"
                           "-DCMAKE_BUILD_TYPE=Release"
                           "-DQV2RAY_BUILD_INFO=Qv2ray for GNU Guix"
                           "-DQV2RAY_HAS_BUILTIN_THEMES=OFF"
                           ; "-DQV2RAY_QT6=ON"
                           "-DQV2RAY_USE_V5_CORE=ON")
       #:tests? #f))
    (native-inputs (list pkg-config
                         qttools-5
                         zlib
                         c-ares
                         abseil-cpp
                         re2
                         curl
                         grpc
                         protobuf
                         openssl
                         qtbase-5
                         qtsvg-5))
    (propagated-inputs (list v2ray-sn qtwayland-5))
    (home-page "https://github.com/Qv2ray/Qv2ray/")
    (synopsis
     "A cross-platform connection manager for V2Ray and other backends")
    (description "Binary version of Qv2ray")
    (license license:gpl3)))

(define-public v2ray-rules-dat
  (package
    (name "v2ray-rules-dat")
    (version "202305312208")
    (source (origin
              (method url-fetch)
              ;; guix download https://github.com/Loyalsoldier/v2ray-rules-dat/releases/download/202305312208/rules.zip
              (uri (string-append
                    "https://github.com/Loyalsoldier/v2ray-rules-dat/releases/download/"
                    version "/rules.zip"))
              (sha256
               (base32
                "1kx6dv79c9ca6byz49ar5b00mpg2h419sp23dq3nqf9f5h9shc9z"))))
    (build-system binary-build-system)
    (arguments
     `(#:install-plan '(("geoip.dat" "/share/")
                        ("geosite.dat" "/share/"))
       #:phases (modify-phases %standard-phases
                  (replace 'unpack
                    (lambda* (#:key source #:allow-other-keys)
                      (invoke "unzip" source))))))
    (native-inputs (list unzip))
    (home-page "https://github.com/Loyalsoldier/v2ray-rules-dat")
    (synopsis "V2ray route rules")
    (description
     "V2ray route rules.")
    (license license:gpl3)))

(define-public sing-box-bin
  (package
    (name "sing-box-bin")
    (version "1.2.7")
    (source (origin
              (method url-fetch)
              ;; https://github.com/SagerNet/sing-box/releases/download/v1.2.7/sing-box-1.2.7-linux-amd64.tar.gz
              (uri (string-append
                    "https://github.com/SagerNet/sing-box/releases/download/v"
                    version "/sing-box-" version "-linux-amd64.tar.gz"))
              (sha256
               (base32
                "04pwa6nmj314kqfgfmj7jgyps524s1w9wp7hdv413xldz7cml5ay"))))
    (build-system binary-build-system)
    (arguments
     `(#:install-plan '(("sing-box" "/bin/"))))
    (home-page "https://sing-box.sagernet.org")
    (synopsis "The universal proxy platform")
    (description
     "The universal proxy platform.")
    (license license:gpl3+)))

(define-public sing-geoip
  (package
    (name "sing-geoip")
    (version "202306010018")
    (source (origin
              (method url-fetch)
              ;; guix download https://github.com/soffchen/sing-geoip/releases/download/202306010018/geoip.db
              (uri (string-append
                    "https://github.com/soffchen/sing-geoip/releases/download/"
                    version "/geoip.db"))
              (sha256
               (base32
                "0ph8ki91wl0jijjidsfbgxxmw6b6k52kppmzn7z3i18c5r8g1vs6"))))
    (build-system binary-build-system)
    (arguments
     `(#:install-plan '(("geoip.db" "/share/"))))
    (home-page "https://github.com/soffchen/sing-geoip")
    (synopsis "sing-box route rules")
    (description
     "sing-box route rules.")
    (license license:gpl3)))

(define-public sing-geosite
  (package
    (name "sing-geosite")
    (version "202305312208")
    (source (origin
              (method url-fetch)
              ;; guix download https://github.com/soffchen/sing-geosite/releases/download/202305312208/geosite.db
              (uri (string-append
                    "https://github.com/soffchen/sing-geosite/releases/download/"
                    version "/geosite.db"))
              (sha256
               (base32
                "1xhc5bizhl1liafh9sb7bfh0kmvhqidxbwaq1n2n6hsd0zq1jx52"))))
    (build-system binary-build-system)
    (arguments
     `(#:install-plan '(("geosite.db" "/share/"))))
    (home-page "https://github.com/soffchen/sing-geosite")
    (synopsis "sing-box route rules")
    (description
     "sing-box route rules.")
    (license license:gpl3)))

;(define-public nekoray
;  (package
;    (name "nekoray")
;    (version "3.6")
;    (source (origin
;              (method git-fetch)
;              (uri (git-reference
;                    (url "https://github.com/MatsuriDayo/nekoray")
;                    (commit version)
;                    (recursive? #t)))
;              (file-name (git-file-name name version))
;              ; git clone https://github.com/MatsuriDayo/nekoray /tmp/nekoray && guix hash --serializer=nar -x /tmp/nekoray && rm -rf /tmp/nekoray
;              (sha256
;               (base32
;                "1dqhkydwy8qqyjlhgg7bm5hfnw2h9346cd2nwwy94y3hccqmr2ww"))))
;    (build-system qt-build-system)
;    (arguments
;     `(#:configure-flags '("-DQT_VERSION_MAJOR=5"
;                           "-DNKR_PACKAGE=ON"
;                           "-DNKR_NO_ZXING=1")
;       #:tests? #f))
;    (native-inputs (list pkg-config
;                         protobuf
;                         qtbase-5
;                         qtsvg-5
;                         qttools-5
;                         qtx11extras
;                         yaml-cpp
;                         ;zxing-cpp
;                      ))
;    (propagated-inputs (list sing-box-bin sing-geoip sing-geosite))
;    (home-page "https://matsuridayo.github.io")
;    (synopsis
;     "Qt based cross-platform GUI proxy configuration manager")
;    (description "Qt based cross-platform GUI proxy configuration manager (backend: v2ray / sing-box)
;
;  Support Windows / Linux out of the box now.")
;    (license license:gpl3)))