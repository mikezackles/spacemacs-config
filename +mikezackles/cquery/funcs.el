(require 'cl-lib)

(defun cquery//enable ()
  (when
      (and buffer-file-name
           (or (locate-dominating-file default-directory "compile_commands.json")
               (locate-dominating-file default-directory ".cquery")))
    (lsp-cquery-enable)))


;;; xref

(defun my-advice/xref-set-jump (&optional args)
  (lsp-ui-peek--with-evil-jumps (evil-set-jump)))

(defun my-xref/find-definitions ()
  (interactive)
  (if lsp-mode (lsp-ui-peek-find-definitions) (spacemacs/jump-to-definition)))

(defun my-xref/find-references ()
  (interactive)
  (if lsp-mode (lsp-ui-peek-find-references) (spacemacs/jump-to-definition)))

;;; Override
;; This function is transitively called by xref-find-{definitions,references,apropos}
(require 'xref)
(defun xref--show-xrefs (xrefs display-action &optional always-show-list)
  (cond
   ((cl-some (lambda (x) (string-match-p x buffer-file-name))
             my-xref-blacklist)
    nil)
   (t
    ;; PATCH
    (lsp-ui-peek--with-evil-jumps (evil-set-jump))

    ;; PATCH Jump to the first candidate
    ;; (when (not (cdr xrefs))
      ;; (xref--pop-to-location (car xrefs) display-action))

    (funcall xref-show-xrefs-function xrefs
             `((window . ,(selected-window)))))))


;; https://github.com/syl20bnr/spacemacs/pull/9911

(defmacro spacemacs|define-reference-handlers (mode &rest handlers)
  "Defines reference handlers for the given MODE.
This defines a variable `spacemacs-reference-handlers-MODE' to which
handlers can be added, and a function added to MODE-hook which
sets `spacemacs-reference-handlers' in buffers of that mode."
  (let ((mode-hook (intern (format "%S-hook" mode)))
        (func (intern (format "spacemacs//init-reference-handlers-%S" mode)))
        (handlers-list (intern (format "spacemacs-reference-handlers-%S" mode))))
    `(progn
       (defvar ,handlers-list ',handlers
         ,(format (concat "List of mode-specific reference handlers for %S. "
                          "These take priority over those in "
                          "`spacemacs-default-reference-handlers'.")
                  mode))
       (defun ,func ()
         (setq spacemacs-reference-handlers
               (append ,handlers-list
                       spacemacs-default-reference-handlers)))
       (add-hook ',mode-hook ',func)
       (with-eval-after-load 'bind-map
         (spacemacs/set-leader-keys-for-major-mode ',mode
           "gr" 'spacemacs/jump-to-reference)))))

(defun spacemacs/jump-to-reference ()
  "Jump to reference around point using the best tool for this action."
  (interactive)
  (catch 'done
    (let ((old-window (selected-window))
          (old-buffer (current-buffer))
          (old-point (point)))
      (dolist (-handler spacemacs-reference-handlers)
        (let ((handler (if (listp -handler) (car -handler) -handler))
              (async (when (listp -handler)
                       (plist-get (cdr -handler) :async))))
          (ignore-errors
            (call-interactively handler))
          (when (or (eq async t)
                    (and (fboundp async) (funcall async))
                    (not (eq old-point (point)))
                    (not (equal old-buffer (window-buffer old-window))))
            (throw 'done t)))))
    (message "No reference handler was able to find this symbol.")))

(defun spacemacs/jump-to-reference-other-window ()
  "Jump to reference around point in other window."
  (interactive)
  (let ((pos (point)))
    ;; since `spacemacs/jump-to-reference' can be asynchronous we cannot use
    ;; `save-excursion' here, so we have to bear with the jumpy behavior.
    (switch-to-buffer-other-window (current-buffer))
    (goto-char pos)
    (spacemacs/jump-to-reference)))



