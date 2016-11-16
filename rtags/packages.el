(setq rtags-packages
      '(
        cc-mode
        company
        gdb-mi
        cmake-mode
        rtags
        ))

;; List of packages to exclude.
(setq rtags-excluded-packages '())

(defun rtags/init-modern-cpp-font-lock ()
  (use-package modern-cpp-font-lock
    :init
    :defer t
    :ensure cc-mode
    :config
    (modern-c++-font-lock-global-mode t)
    )
  )

(defun rtags/init-cc-mode ()
  (use-package cc-mode
    :defer t
    :init
    (add-to-list 'auto-mode-alist `("\\.h$" . c++-mode))
    :config
    (require 'compile)
    (c-toggle-auto-newline 1)

    ;; turn off auto-indentation
    (add-to-list 'c-mode-common-hook
      (lambda () (setq c-syntactic-indentation nil)))
    )
  )

(defun rtags/init-cmake-mode ()
  (use-package cmake-mode
    :mode (("CMakeLists\\.txt\\'" . cmake-mode) ("\\.cmake\\'" . cmake-mode))
    :init (push 'company-cmake company-backends-cmake-mode)))

(defun rtags/post-init-company ()
  (spacemacs|add-company-hook c-mode-common)
  (spacemacs|add-company-hook cmake-mode))

(defun rtags-evil-standard-keybindings (mode)
  (spacemacs/declare-prefix-for-mode mode "mg" "goto")
  (spacemacs/declare-prefix-for-mode mode "mr" "rtags")
  (spacemacs/set-leader-keys-for-major-mode mode
    "ga" 'projectile-find-other-file
    "gA" 'projectile-find-other-file-other-window
    "r." 'rtags-find-symbol-at-point
    "r," 'rtags-find-references-at-point
    "rv" 'rtags-find-virtuals-at-point
    "rV" 'rtags-print-enum-value-at-point
    "r/" 'rtags-find-all-references-at-point
    "rY" 'rtags-cycle-overlays-on-screen
    "r>" 'rtags-find-symbol
    "r<" 'rtags-find-references
    "r[" 'rtags-location-stack-back
    "r]" 'rtags-location-stack-forward
    "rD" 'rtags-diagnostics
    "rG" 'rtags-guess-function-at-point
    "rp" 'rtags-set-current-project
    "rP" 'rtags-print-dependencies
    "re" 'rtags-reparse-file
    "rE" 'rtags-preprocess-file
    "rR" 'rtags-rename-symbol
    "rM" 'rtags-symbol-info
    "rS" 'rtags-display-summary
    "rO" 'rtags-goto-offset
    "r;" 'rtags-find-file
    "rF" 'rtags-fixit
    "rL" 'rtags-copy-and-print-current-location
    "rX" 'rtags-fix-fixit-at-point
    "rB" 'rtags-show-rtags-buffer
    "rI" 'rtags-imenu
    "rT" 'rtags-taglist
    "rh" 'rtags-print-class-hierarchy
    "ra" 'rtags-print-source-arguments
    "rj" 'rtags-next-match
    "rk" 'rtags-previous-match
    )
  )

;; For each package, define a function rtags/init-<package-name>
;;
(defun rtags/init-rtags ()
  (use-package rtags
    :init
    :ensure company
    :config
    (progn
      (require 'company-rtags)
      (add-to-list 'company-backends-c-mode-common 'company-rtags)
      (setq company-rtags-begin-after-member-access t)
      (setq rtags-autostart-diagnostics t) ;; added after checking rtags docs
      (rtags-diagnostics)
      (setq rtags-completions-enabled t)

      (define-key evil-normal-state-map (kbd "RET") 'rtags-select-other-window)
      (define-key evil-normal-state-map (kbd "M-RET") 'rtags-select)
      (define-key evil-normal-state-map (kbd "q") 'rtags-bury-or-delete)

      (rtags-evil-standard-keybindings 'c-mode)
      (rtags-evil-standard-keybindings 'c++-mode)
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
