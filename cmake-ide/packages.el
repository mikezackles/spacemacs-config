;;; packages.el --- rtags layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `rtags-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `rtags/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `rtags/pre-init-PACKAGE' and/or
;;   `rtags/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst cmake-ide-packages
  '(rtags cmake-ide cmake-mode))

;;;
;;; Adapted from comment:
;;; https://github.com/syl20bnr/spacemacs/issues/2327#issuecomment-153283156
;;; by user
;;; https://github.com/autosquid
;;;
(defun cmake-ide-major-mode-keybindings (mode)
  (spacemacs/set-leader-keys-for-major-mode mode
    "R." 'rtags-find-symbol-at-point
    "R," 'rtags-find-references-at-point
    "Rv" 'rtags-find-virtuals-at-point
    "RV" 'rtags-print-enum-value-at-point
    "R/" 'rtags-find-all-references-at-point
    "RY" 'rtags-cycle-overlays-on-screen
    "R>" 'rtags-find-symbol
    "R<" 'rtags-find-references
    "R[" 'rtags-location-stack-back
    "R]" 'rtags-location-stack-forward
    "RD" 'rtags-diagnostics
    "RG" 'rtags-guess-function-at-point
    "Rp" 'rtags-set-current-project
    "RP" 'rtags-print-dependencies
    "Re" 'rtags-reparse-file
    "RE" 'rtags-preprocess-file
    "RR" 'rtags-rename-symbol
    "RM" 'rtags-symbol-info
    "RS" 'rtags-display-summary
    "RO" 'rtags-goto-offset
    "R;" 'rtags-find-file
    "RF" 'rtags-fixit
    "RL" 'rtags-copy-and-print-current-location
    "RX" 'rtags-fix-fixit-at-point
    "RB" 'rtags-show-rtags-buffer
    "RI" 'rtags-imenu
    "RT" 'rtags-taglist
    "Rh" 'rtags-print-class-hierarchy
    "Ra" 'rtags-print-source-arguments
    )
  )

(defun cmake-ide/init-rtags ()
  "Initialize rtags"
  (use-package rtags
    :init
    ;;(evil-set-initial-state 'rtags-mode 'emacs)
    ;;(rtags-enable-standard-keybindings c-mode-base-map)
    :ensure company
    :config
    (progn
      (require 'company-rtags)
      (add-to-list 'company-backends 'company-rtags)
      (setq company-rtags-begin-after-member-access t)

      (require 'rtags-ac)
      (setq rtags-completions-enabled t)
      (rtags-diagnostics)

      (define-key evil-normal-state-map (kbd "RET") 'rtags-select-other-window)
      (define-key evil-normal-state-map (kbd "M-RET") 'rtags-select)
      (define-key evil-normal-state-map (kbd "q") 'rtags-bury-or-delete)

      ;; see https://github.com/Andersbakken/rtags/issues/559
      (add-hook 'rtags-jump-hook 'evil-set-jump)

      (cmake-ide-major-mode-keybindings 'c-mode)
      (cmake-ide-major-mode-keybindings 'c++-mode)

      ;; see https://github.com/atilaneves/cmake-ide/issues/9#issuecomment-68705901
      ;; see https://github.com/redguardtoo/cpputils-cmake#avoid-scanning-system-header-files
      (require 'semantic/bovine/gcc)
      (setq cmake-ide-flags-c++ (append '("-std=c++14")
                                        (mapcar (lambda (path) (concat "-I" path)) (semantic-gcc-get-include-paths "c++"))))
      (setq cmake-ide-flags-c (append (mapcar (lambda (path) (concat "-I" path)) (semantic-gcc-get-include-paths "c"))))
      )
    )
  )

(defun cmake-ide/init-cmake-ide ()
  "Initialize cmake-ide"
  (use-package cmake-ide
   :config
   (progn
     (cmake-ide-setup)
    )
   )
  )

(defun cmake-ide/init-cmake-mode ()
  (use-package cmake-mode
    :mode (("CMakeLists\\.txt\\'" . cmake-mode) ("\\.cmake\\'" . cmake-mode))
    :init (push 'company-cmake company-backends-cmake-mode)))

; packages.el ends here
