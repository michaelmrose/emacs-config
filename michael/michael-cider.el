;;; michael-cider.el personal functions for cider/clojure/smartparens
;;; Commentary:
;;; Code:


(defun my-cider-debug-toggle-insert-state ()
  (if cider--debug-mode    ;; Checks if you're entering the debugger   
      (evil-insert-state)  ;; If so, turn on evil-insert-state
    (evil-normal-state)))  ;; Otherwise, turn on normal-state

(defun my-setup-grim ()
  (interactive) 
  (progn (clojure-mode)
         (buffer-local-set-key "q" 'cider-popup-buffer-quit)
         (evil-insert-state))
  ())
(defun setup-cider-popup ()
  (interactive)
  (progn (clojure-mode)
         (buffer-local-set-key "q" 'cider-popup-buffer-quit)
         (evil-insert-state)))


(defun cider-right-repl ()
  (interactive)
  (let ((current-prefix-arg 4)) ;; emulate C-u
    (call-interactively 'cider-switch-to-repl-buffer)
    )
  )
(defun cider-eval-current-sexp ()
  (interactive)
  (save-excursion
    (sp-after-sexp)
    (cider-eval-last-sexp)))

(defun cider-debug-form ()
  "This produces the same result as C-u C-c C-c."
  (interactive)
  (cider-eval-defun-at-point 0))


(defun mmr-macroexpand-1-in-place ()
  (interactive)
  (save-excursion
    (sp-end-of-sexp)
    (sp-up-sexp)
    (cider-macroexpand-1-inplace)))

(defun mmr-macroexpand-1 ()
  (interactive)
  (save-excursion
    (sp-end-of-sexp)
    (sp-up-sexp)
    (cider-macroexpand-1)))

(defun mmr-macroexpand-all()
  (interactive)
  (save-excursion
    (forward-line)
    (cider-macroexpand-all)))

(defun cider-eval-next-sexp ()
  (interactive)
  (save-excursion
    (sp-topmost-sexp)
    (sp-next-sexp)
    (cider-eval-current-sexp)))

(defun elisp-eval-next-sexp ()
  (interactive)
  (save-excursion
    (sp-topmost-sexp)
    (sp-next-sexp)
    (sp-after-sexp)
    (eval-defun)))

(defun mmr-macroexpand-all-inplace()
  (interactive)
  (save-excursion
    (forward-line)
    (cider-macroexpand-all-inplace))
  (forward-char)
  (sp-end-of-sexp)
  (forward-char)
  (newline)
  (sp-end-of-previous-sexp))



;;-----------------------------------------------
;; smartparens related
;;-----------------------------------------------

(defun mmr-myinsert ()
  "Appends STRING without smartparens enabled"
  (interactive)
  (insert (read-from-minibuffer "prompt: "))
  )

(defun toggle-slurp-direction ()
  (interactive)
  (setq michaelr-slurp-direction
        (toggle-pair michaelr-slurp-direction :forward :backward)))

(defun bidirectional-slurp ()
  (interactive)
  (pcase michaelr-slurp-direction
    (:forward (sp-forward-slurp-sexp))
    (:backward (sp-backward-slurp-sexp))))

(defun bidirectional-barf ()
  (interactive)
  (pcase michaelr-slurp-direction
    (:forward (sp-forward-barf-sexp))
    (:backward (sp-backward-barf-sexp))))


(defun mmr-commentary (target op)
  (interactive)
  (save-excursion
    (pcase target
      (:top (sp-topmost-sexp))
      (:current (sp-backward-up-sexp)))
    (pcase op
      (:comment (insert "#_"))
      (:uncomment (kill-backward-chars 2)))))

(defun mmr-sexp-is-commented-p (fn)
  (interactive)
  (save-excursion
    (funcall fn)
    (backward-char)
    (if (looking-at "_")
        t nil)))

(defun mmr-topmost-sexp-is-commented-p ()
  (mmr-sexp-is-commented-p 'sp-topmost-sexp))

(defun mmr-current-sexp-is-commented-p ()
  (mmr-sexp-is-commented-p 'sp-backward-up-sexp))
(defun mmr-toggle-comment-top-sexp ()
  (interactive)
  (save-excursion
    (if (mmr-topmost-sexp-is-commented-p)
        (mmr-commentary :top :uncomment)
      (mmr-commentary :top :comment))))

(defun mmr-toggle-comment-current-sexp ()
  (interactive)
  (save-excursion
    (if (mmr-current-sexp-is-commented-p)
        (mmr-commentary :current :uncomment)
      (mmr-commentary :current :comment))))


(defun wrap-sexp()
  (interactive)
  (sp-backward-up-sexp)
  (insert "()")
  (sp-end-of-previous-sexp)
  (sp-slurp-hybrid-sexp)
  (evil-insert-state)
  )

(defun sexp-wrap-around-symbol()
  (interactive)
  (insert "()")
  (backward-char)
  (sp-forward-slurp-sexp))

(defun sp-select-sexp ()
  (interactive)
  (sp-beginning-of-sexp)
  (sp-up-sexp)
  (left-char)
  (sp-select-next-thing))


(defun sp-comment-sexp ()
  (interactive)
  (mark-sexp)
  (save-excursion
    (comment-or-uncomment-region (point) (mark))))


(defun sp-topmost-sexp ()
  (interactive)
  (while (sp-backward-up-sexp)))

(defun sp-after-sexp ()
  (interactive)
  (sp-topmost-sexp)
  (right-char)
  (sp-end-of-sexp)
  (right-char))

(defun slime-eval-current-sexp ()
  (interactive)
  (save-excursion
    (sp-after-sexp)
    (slime-eval-last-expression)))

(defun geiser-eval-current-sexp ()
  (interactive)
  (save-excursion
    (sp-after-sexp)
    (geiser-eval-last-sexp)))

(defun cider-find-dwim-immediate ()
  (interactive)
  (let ((current-prefix-arg 4))
    (call-interactively 'cider-find-dwim)))

(defun cider-find-dwim-other-window-immediate ()
  (interactive)
  (let ((current-prefix-arg 4))
    (call-interactively 'cider-find-dwim-other-window)))

(defun mmr-eval-current-sexp ()
  (interactive)
  (save-excursion
    (sp-after-sexp)
    (when (equal major-mode 'emacs-lisp-mode)
      (call-interactively 'eval-last-sexp))
    (when (equal major-mode 'clojure-mode)
      (cider-eval-current-sexp))))
(provide 'michael-cider)
;;; michael-cider.el ends here
