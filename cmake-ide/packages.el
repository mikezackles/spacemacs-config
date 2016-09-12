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
            (vector (- (aref result 0) 2))
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
                  (if leading-comma (- c-basic-offset 2) c-basic-offset)))))))))

(defun my-innamespace (x)
  "Be smart about indenting namespaces if multiple namespaces are opened on one
line."
  (defun followed-by (cases)
    (cond ((null cases) nil)
          ((assq (car cases)
                 (cdr (memq c-syntactic-element c-syntactic-context))) t)
          (t (followed-by (cdr cases)))))
  (if (followed-by '(innamespace namespace-close)) 0 '+))

(defun my-arglist-cont (x)
  (if (line-has-leading-comma-p) -2 0))

(defun cmake-ide/init-cmake-ide ()
  "Initialize cmake-ide"
  (use-package cmake-ide
   :config
   (progn
     (cmake-ide-setup)
     (c-add-style "zam++"
                  '("c++_guessed"
                    (c-basic-offset . 2)
                    (c-offsets-alist
                     (access-label . -)
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
                     ;(arglist-cont c-lineup-gcc-asm-reg 0)
                     (arglist-cont my-arglist-cont 0)
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
                     (inline-open . 0)
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
                      +))))
    (add-to-list 'c-mode-common-hook
      (lambda () (c-set-style "zam++")))
    )
   )
  )

(defun cmake-ide/init-cmake-mode ()
  (use-package cmake-mode
    :mode (("CMakeLists\\.txt\\'" . cmake-mode) ("\\.cmake\\'" . cmake-mode))
    :init (push 'company-cmake company-backends-cmake-mode)))

(defun cmake-ide/post-init-company ()
  (spacemacs|add-company-hook cmake-mode))

; packages.el ends here
