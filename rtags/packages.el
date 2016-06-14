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

(defun line-has-leading-comma-p ()
  (save-excursion
    (beginning-of-line)
    (c-forward-token-2 0 nil (c-point 'eol))
    (eq (char-after) ?,)))

(defun my-c-backward-template-prelude ()
  "Back up over expressions that end with a template argument list.
Examples include:
        typename foo<bar>::baz::mumble
        foo(bar, baz).template bing
"
  (while
      (save-excursion
        ;; Inspect the previous token or balanced pair to
        ;; see whether to skip backwards over it
        (c-backward-syntactic-ws)
        (or
         ;; is it the end of a nested template argument list?
         (and
          (eq (char-before) ?>)
          (c-backward-token-2 1 t) ;; skips over balanced "<>" pairs
          (eq (char-after) ?<))

         (and
          (c-backward-token-2 1 t)
          (looking-at "[A-Za-z_\\[(.]\\|::\\|->"))))

    (c-backward-token-2 1 t)))

(defun my-lineup-more-template-args (langelem)
  "Line up template argument lines under the first argument,
adjusting for leading commas. To allow this function to be used in
a list expression, nil is returned if there's no template
argument on the first line.
Works with: template-args-cont."
  (let ((result (c-lineup-template-args langelem)))
    (if (not (eq result nil))
        (if (line-has-leading-comma-p)
            (vector (- (aref result 0) c-basic-offset))
          result))))

(defun my-lineup-template-close (langelem)
  (save-excursion
    (c-with-syntax-table c++-template-syntax-table
      (beginning-of-line)
      (c-forward-syntactic-ws (c-point 'eol))
      (if (and
           (eq (char-after) ?>)
           (progn
             (forward-char)
             (c-backward-token-2 1 t) ;; skips over balanced "<>" pairs
             (eq (char-after) ?<)))
          (progn
            (my-c-backward-template-prelude)
            (vector (current-column)))))))

(defun my-lineup-first-template-args (langelem)
  "Align lines beginning with the first template argument.
To allow this function to be used in a list expression, nil is
returned if we don't appear to be in a template argument list.
Works with: template-args-cont."
  (let ((leading-comma (line-has-leading-comma-p)))
    (save-excursion
      (c-with-syntax-table c++-template-syntax-table
        (beginning-of-line)
        (backward-up-list 1)
        (if (eq (char-after) ?<)
            (progn
              (my-c-backward-template-prelude)
              (vector
               (+ (current-column)
                  (if leading-comma 0 c-basic-offset)))))))))

(defun my-innamespace (x)
  "Be smart about indenting namespaces if multiple namespaces are opened on one
line."
  (defun followed-by (cases)
    (cond ((null cases) nil)
          ((assq (car cases)
                 (cdr (memq c-syntactic-element c-syntactic-context))) t)
          (t (followed-by (cdr cases)))))
  (if (followed-by '(innamespace namespace-close)) 0 '+))

(defun rtags/init-cc-mode ()
  (use-package cc-mode
    :defer t
    :init
    (add-to-list 'auto-mode-alist `("\\.h$" . c++-mode))
    :config
    (require 'compile)
    (c-toggle-auto-newline 1)
    (evil-leader/set-key-for-mode 'c-mode
      "m g a" 'projectile-find-other-file
      "m g A" 'projectile-find-other-file-other-window)
    (evil-leader/set-key-for-mode 'c++-mode
      "m g a" 'projectile-find-other-file
      "m g A" 'projectile-find-other-file-other-window)

    (c-add-style "zam++"
      '("c++_guessed"
        (c-basic-offset . 2)
        (c-offsets-alist
          (access-label . 0)
          (block-close . 0)
          (class-close . 0)
          (defun-block-intro . +)
          (inclass . +)
          (inline-close . 0)
          (innamespace . my-innamespace)
          (namespace-close . 0)
          (statement . 0)
          (statement-block-intro . +)
          (topmost-intro . 0)
          (topmost-intro-cont . 0)
          (annotation-top-cont . 0)
          (annotation-var-cont . +)
          (arglist-close . c-lineup-close-paren)
          (arglist-cont c-lineup-gcc-asm-reg 0)
          (arglist-cont-nonempty . c-lineup-arglist)
          (arglist-intro . +)
          (block-open . 0)
          (brace-entry-open . 0)
          (brace-list-close . 0)
          (brace-list-entry . 0)
          (brace-list-intro . +)
          (brace-list-open . 0)
          (c . c-lineup-C-comments)
          (case-label . 0)
          (catch-clause . 0)
          (class-open . 0)
          (comment-intro . c-lineup-comment)
          (composition-close . 0)
          (composition-open . 0)
          (cpp-define-intro c-lineup-cpp-define +)
          (cpp-macro . -1000)
          (cpp-macro-cont . +)
          (defun-close . 0)
          (defun-open . 0)
          (do-while-closure . 0)
          (else-clause . 0)
          (extern-lang-close . 0)
          (extern-lang-open . 0)
          (friend . 0)
          (func-decl-cont . +)
          (incomposition . +)
          (inexpr-class . +)
          (inexpr-statement . +)
          (inextern-lang . +)
          (inher-cont . c-lineup-multi-inher)
          (inher-intro . +)
          (inlambda . c-lineup-inexpr-block)
          (inline-open . +)
          (inmodule . +)
          (knr-argdecl . 0)
          (knr-argdecl-intro . +)
          (label . 2)
          (lambda-intro-cont . +)
          (member-init-cont . c-lineup-multi-inher)
          (member-init-intro . +)
          (module-close . 0)
          (module-open . 0)
          (namespace-open . 0)
          (objc-method-args-cont . c-lineup-ObjC-method-args)
          (objc-method-call-cont c-lineup-ObjC-method-call-colons c-lineup-ObjC-method-call +)
          (objc-method-intro . [0])
          (statement-case-intro . +)
          (statement-case-open . 0)
          (statement-cont . +)
          (stream-op . c-lineup-streamop)
          (string . -1000)
          (substatement . +)
          (substatement-label . 2)
          (substatement-open . +)
          ;(template-args-cont c-lineup-template-args +)
          (template-args-cont
           my-lineup-more-template-args
           my-lineup-template-close
           my-lineup-first-template-args
           +)
          )
        )
      )
    (add-to-list 'c-mode-common-hook
      (lambda () (c-set-style "zam++")))
    ;; turn off auto-indentation
    ;(add-to-list 'c-mode-common-hook
    ;  (lambda () (setq c-syntactic-indentation nil)))
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
  (spacemacs/set-leader-keys-for-major-mode mode
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
  ;(evil-leader/set-key-for-mode mode
  ;  "m r ." 'rtags-find-symbol-at-point
  ;  "m r ," 'rtags-find-references-at-point
  ;  "m r v" 'rtags-find-virtuals-at-point
  ;  "m r V" 'rtags-print-enum-value-at-point
  ;  "m r /" 'rtags-find-all-references-at-point
  ;  "m r Y" 'rtags-cycle-overlays-on-screen
  ;  "m r >" 'rtags-find-symbol
  ;  "m r <" 'rtags-find-references
  ;  "m r [" 'rtags-location-stack-back
  ;  "m r ]" 'rtags-location-stack-forward
  ;  "m r D" 'rtags-diagnostics
  ;  "m r G" 'rtags-guess-function-at-point
  ;  "m r p" 'rtags-set-current-project
  ;  "m r P" 'rtags-print-dependencies
  ;  "m r e" 'rtags-reparse-file
  ;  "m r E" 'rtags-preprocess-file
  ;  "m r R" 'rtags-rename-symbol
  ;  "m r M" 'rtags-symbol-info
  ;  "m r S" 'rtags-display-summary
  ;  "m r O" 'rtags-goto-offset
  ;  "m r ;" 'rtags-find-file
  ;  "m r F" 'rtags-fixit
  ;  "m r L" 'rtags-copy-and-print-current-location
  ;  "m r X" 'rtags-fix-fixit-at-point
  ;  "m r B" 'rtags-show-rtags-buffer
  ;  "m r I" 'rtags-imenu
  ;  "m r T" 'rtags-taglist
  ;  "m r h" 'rtags-print-class-hierarchy
  ;  "m r a" 'rtags-print-source-arguments
  ;  "m r j" 'rtags-next-match
  ;  "m r k" 'rtags-previous-match
  ;  )
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
