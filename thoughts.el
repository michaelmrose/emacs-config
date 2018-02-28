(global-set-key (kbd "M-/") 'hippie-expand)


;; spelling correction
(setq ispell-program-name "/usr/bin/hunspell")
(eval-after-load 'flyspell
  '(define-key flyspell-mode-map (kbd "<escape>") 'keyboard-quit))
(setq flyspell-consider-dash-as-word-delimiter-flag t)

;;bookmark related

;; Lisp-friendly hippie expand
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))

;;------------------------------------------------------------
;; - lets play with occur
;;------------------------------------------------------------
(add-hook 'occur-mode-hook
          (lambda () (local-set-key (kbd "SPC") #'occur-mode-display-occurrence)))
(add-hook 'occur-mode-hook
          (lambda () (local-set-key (kbd "n") #'occur-next)))
(add-hook 'occur-mode-hook
          (lambda () (local-set-key (kbd "n") #'occur-prev)))

;; "When several buffers visit identically-named files,
;; Emacs must give the buffers distinct names. The usual method
;; for making buffer names unique adds ‘<2>’, ‘<3>’, etc. to the end
;; of the buffer names (all but one of them).
;; The forward naming method includes part of the file's directory
;; name at the beginning of the buffer name
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Uniquify.html
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(setq zeal-at-point-zeal-version "0.4.0")

;;------------------------------------------------------------
;; - org mode
;;------------------------------------------------------------
(setq org-hide-emphasis-markers t)
;; (font-lock-add-keywords 'org-mode
;;                        '(("^ +\\([-*]\\) "
;;                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;; now the evil leader mappings



(evil-define-key 'normal org-mode-map (kbd "<space>") 'org-ctrl-c-ctrl-c)


;; Configure Org-mode supported languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((clojure . t)
   (emacs-lisp . t)))

;; Let's have pretty source code blocks
(setq org-edit-src-content-indentation 0
      org-src-tab-acts-natively t
      org-src-fontify-natively t
      org-confirm-babel-evaluate nil
      org-support-shift-sclect 'always)

;; Use cider as the Clojure execution backend
(setq org-babel-clojure-backend 'cider)

;; (define-clojure-indent
;;   (alet 'defn)
;;   (mlet 'defn)
;;   (into 'defn))
;; (put-clojure-indent 'into 1)
;; (put-clojure-indent '-> 3)
;; (define-clojure-indent
;;   (fdef     '(1 ((:defn)) nil)))
;; (define-clojure-indent
;;   (-> 'defn)
;;   (->> 5)
;;   (as-> 'defn))

(define-key key-translation-map (kbd "<f13>") (kbd "C-g"))

(defhydra cbm-hydra ()
  "Cycle Similar Bufffers"
  ("n" next-buffer-same-mode "next")
  ("p" previous-buffer-same-mode "prev")
  ("q" nil "Quit" :color blue))


(mapc #'diminish '(auto-revert-mode
                   company-mode
                   eldoc-mode
                   evil-commentary-mode
                   evil-smartparens-mode
                   which-key-mode
                   helm-mode
                   line-number-mode
                   rainbow-mode
                   smartparens-mode
                   undo-tree-mode))

(byte-recompile-directory (expand-file-name "~/.emacs.d") 0)
