(setq rtags-packages
      '(
        cc-mode
        company
        company-c-headers
        gdb-mi
        rtags
        ))

;; List of packages to exclude.
(setq rtags-excluded-packages '())

(defun rtags/init-cc-mode ()
  (use-package cc-mode
    :defer t
    :init
    (add-to-list 'auto-mode-alist `("\\.h$" . c++-mode))
    :config
    (progn
      (require 'compile)
      (c-toggle-auto-newline 1)
      (evil-leader/set-key-for-mode 'c-mode
        "m g a" 'projectile-find-other-file
        "m g A" 'projectile-find-other-file-other-window)
      (evil-leader/set-key-for-mode 'c++-mode
        "m g a" 'projectile-find-other-file
        "m g A" 'projectile-find-other-file-other-window)
      )
    )
  )

(defun rtags/post-init-company ()
  (spacemacs|add-company-hook c-mode-common))

(when (configuration-layer/layer-usedp 'auto-completion)
  (defun rtags/init-company-c-headers ()
    (use-package company-c-headers
      :if (configuration-layer/package-usedp 'company)
      :defer t
      :init (add-to-list 'company-backends-c-mode-common 'company-c-headers))))

(defun rtags-evil-standard-keybindings (mode)
  (evil-leader/set-key-for-mode mode
    "m r ." 'rtags-find-symbol-at-point
    "m r ," 'rtags-find-references-at-point
    "m r v" 'rtags-find-virtuals-at-point
    "m r V" 'rtags-print-enum-value-at-point
    "m r /" 'rtags-find-all-references-at-point
    "m r Y" 'rtags-cycle-overlays-on-screen
    "m r >" 'rtags-find-symbol
    "m r <" 'rtags-find-references
    "m r [" 'rtags-location-stack-back
    "m r ]" 'rtags-location-stack-forward
    "m r D" 'rtags-diagnostics
    "m r G" 'rtags-guess-function-at-point
    "m r p" 'rtags-set-current-project
    "m r P" 'rtags-print-dependencies
    "m r e" 'rtags-reparse-file
    "m r E" 'rtags-preprocess-file
    "m r R" 'rtags-rename-symbol
    "m r M" 'rtags-symbol-info
    "m r S" 'rtags-display-summary
    "m r O" 'rtags-goto-offset
    "m r ;" 'rtags-find-file
    "m r F" 'rtags-fixit
    "m r L" 'rtags-copy-and-print-current-location
    "m r X" 'rtags-fix-fixit-at-point
    "m r B" 'rtags-show-rtags-buffer
    "m r I" 'rtags-imenu
    "m r T" 'rtags-taglist
    "m r h" 'rtags-print-class-hierarchy
    "m r a" 'rtags-print-source-arguments
    "m r j" 'rtags-next-match
    "m r k" 'rtags-previous-match
    )
  )

;; For each package, define a function rtags/init-<package-name>
;;
(defun rtags/init-rtags ()
  (use-package rtags
    :init
    ;;(evil-set-initial-state 'rtags-mode 'emacs)
    ;;(rtags-enable-standard-keybindings c-mode-base-map)
    :ensure company
    :config
    (progn
      (require 'company-rtags)
      (add-to-list 'company-backends-c-mode-common 'company-rtags)
      (setq company-rtags-begin-after-member-access t)
      (setq rtags-completions-enabled t)

      (define-key evil-normal-state-map (kbd "RET") 'rtags-select-other-window)
      (define-key evil-normal-state-map (kbd "M-RET") 'rtags-select)
      (define-key evil-normal-state-map (kbd "q") 'rtags-bury-or-delete)

      (rtags-evil-standard-keybindings 'c-mode)
      (rtags-evil-standard-keybindings 'c++-mode)

      (rtags-diagnostics)
      )
    )
  )

(defun rtags/init-gdb-mi ()
  (use-package gdb-mi
    :defer t
    :init
    (setq
     ;; use gdb-many-windows by default when `M-x gdb'
     gdb-many-windows t
     ;; Non-nil means display source file containing the main routine at startup
     gdb-show-main t)
    )
  )
