;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2019 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2020 Zhu Zihao <all_but_last@163.com>
;;; Copyright © 2022 Fredrik Salomonsson <plattfot@posteo.net>
;;; Copyright © 2022 Jonathan Brielmaier <jonathan.brielmaier@web.de>
;;; Copyright © 2023 Hilton Chain <hako@ultrarare.space>
;;; Copyright © 2023 Jiwei YANG <yangjiwei@protonmail.com>


(define-module (ac bootloader grub)
  #:use-module (guix build union)
  #:use-module (guix records)
  #:use-module (guix store)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (gnu artwork)
  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader grub)
  #:use-module (gnu system uuid)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system keyboard)
  #:use-module (gnu system locale)
  #:use-module (gnu packages bootloaders)
  #:autoload   (gnu packages gtk) (guile-cairo guile-rsvg)
  #:autoload   (gnu packages xorg) (xkeyboard-config)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (ac packages bootloaders)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  #:export (grub-efi-luks2-bootloader
            grub-efi-bootloader-luks2))


(define grub-efi-luks2-bootloader
  (bootloader
   (inherit grub-efi-bootloader)
   ;; NOTE: Don't change the name.  Generation switching code only knows
   ;; bootloaders defined in (gnu bootloader grub).
   (name 'grub-efi)
   (package grub-efi-luks2)))