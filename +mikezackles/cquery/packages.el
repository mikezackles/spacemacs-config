(defconst cquery-packages
  '(
    cquery
    evil
    lsp-mode
    company
    cmake-mode
    meson-mode
    ))

(defun cquery/post-init-evil ()
  (add-to-list 'evil-emacs-state-modes 'xref--xref-buffer-mode)
  )

(defun cquery/init-cmake-mode ()
  (use-package cmake-mode
    :mode (("CMakeLists\\.txt\\'" . cmake-mode) ("\\.cmake\\'" . cmake-mode))
    ))

(defun cquery/init-meson-mode ()
  (use-package meson-mode
    :mode (("meson\\.build\\'" . meson-mode))))

;; See also https://github.com/cquery-project/cquery/wiki/Emacs
(defun cquery/init-cquery ()
  (use-package cquery
    :init
    (add-hook 'c-mode-common-hook #'cquery//enable)
    :config
    ;; overlay is slow
    ;; Use https://github.com/emacs-mirror/emacs/commits/feature/noverlay
    (setq cquery-sem-highlight-method 'overlay)
    (cquery-use-default-rainbow-sem-highlight)
    (setq cquery-extra-init-params
          '(:cacheFormat "msgpack" :completion (:detailedLabel t) :xref (:container t)))

    ;; Tell projectile to ignore files in the cquery cache
    (require 'projectile)
    (add-to-list 'projectile-globally-ignored-directories ".cquery_cached_index")

    ;(setq cquery-project-roots '("~/Dev/llvm-project" "~/Dev/llvm"))
    ))

(defun cquery/post-init-company ()
  (spacemacs|add-company-backends :backends company-lsp :modes c-mode-common))

(defun cquery/post-init-lsp-mode ()
  (use-package lsp-mode
    :config
    ;;(add-to-list 'spacemacs-jump-handlers-d-mode 'company-dcd-goto-definition)
    (setq-default flycheck-disabled-checkers '(c/c++-clang c/c++-gcc)) ;; in flycheck.el

    (setq company-quickhelp-delay 0)
    (setq company-show-numbers t)

    (require 'lsp-imenu)
    (add-hook 'lsp-after-open-hook #'lsp-enable-imenu)

    (advice-add 'spacemacs/jump-to-definition :before #'my-advice/xref-set-jump)
    (advice-add 'spacemacs/jump-to-reference :before #'my-advice/xref-set-jump)

    ;;; Override
    ;(dolist (mode '("c" "c++" "go" "haskell" "javascript" "python" "rust"))
    (dolist (mode '("c" "c++"))
      (let ((handler (intern (format "spacemacs-jump-handlers-%s-mode" mode))))
        (add-to-list handler 'lsp-ui-peek-find-definitions))
      (let ((handler (intern (format "spacemacs-reference-handlers-%s-mode" mode))))
        (add-to-list handler 'lsp-ui-peek-find-references)))

    (defun cquery/base () (interactive) (lsp-ui-peek-find-custom 'base "$cquery/base"))
    (defun cquery/callers () (interactive) (lsp-ui-peek-find-custom 'callers "$cquery/callers"))
    (defun cquery/derived () (interactive) (lsp-ui-peek-find-custom 'derived "$cquery/derived"))
    (defun cquery/vars () (interactive) (lsp-ui-peek-find-custom 'vars "$cquery/vars"))
    (defun cquery/random () (interactive) (lsp-ui-peek-find-custom 'random "$cquery/random"))

    (spacemacs/set-leader-keys-for-minor-mode 'lsp-mode
      "a" #'lsp-ui-find-workspace-symbol
      "A" #'lsp-ui-peek-find-workspace-symbol
      "f" #'lsp-format-buffer
      "l" #'lsp-ui-sideline-mode
      "D" #'lsp-ui-doc-mode
      "n" #'lsp-ui-find-next-reference
      "p" #'lsp-ui-find-prev-reference
      "r" #'lsp-rename
      "l" #'lsp-ui-peek-jump-forward
      "h" #'lsp-ui-peek-jump-backward
      "," #'my-xref/find-references
      "." #'my-xref/find-definitions
      )

    ;(dolist (mode '("c-mode" "c++-mode"))
      (spacemacs/set-leader-keys-for-major-mode 'c++-mode
        "b" #'cquery/base
        "c" #'cquery/callers
        "d" #'cquery/derived
        "R" #'cquery-freshen-index
        "v" #'cquery/vars
        "SPC" #'cquery/random
        )
      ;)
    ))
