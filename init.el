;;; init.el --- My config file
;;; Commentary:
;;; Code:
;; (require 'package)
;; (setq package-enable-at-startup nil)

;;; billpiel is added purely for sayid a clojure debugging tool
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("melpa-stable" . "http://melpa-stable.milkbox.net/packages/")
        ("billpiel" . "http://billpiel.com/emacs-packages/")))


(add-to-list 'load-path "~/.emacs.d/michael")

(setq package-archive-priorities
      '(("melpa" . 3)
        ("melpa-stable" . 2)
        ("gnu" . 1)
        ("billpiel" . 0)))

(package-initialize)

(setq initial-buffer-choice nil)
;; ----------------------------------------------
;; ensure use package is available and set it up
;; ----------------------------------------------
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; always install missing packages
(setq use-package-always-ensure t)


(require 'michael)
;; (require 'ibuffer-hydra)
(require 'michael-cider)
(require 'prettify-utils)


;; ----------------------------------------------
;; look and feel
;; ----------------------------------------------

;; consider _ and - part of a word
;; interesting discussion on that http://emacs.stackexchange.com/questions/9583/how-to-treat-underscore-as-part-of-the-word
(modify-syntax-entry ?_ "w")
(modify-syntax-entry ?- "w")

;; hide uneeded elements
(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))


;; No cursor blinking, it's distracting
(blink-cursor-mode 0)

;; Go straight to scratch buffer on startup
(setq inhibit-startup-message t)

;; Don't show native OS scroll bars for buffers because they're redundant
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;; no bell
(setq ring-bell-function 'ignore)

;; Highlights matching parenthesis
(show-paren-mode 1)

(column-number-mode)

;; themes
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(add-to-list 'load-path "~/.emacs.d/themes")

(set-face-attribute  
 'default nil 
 :font "Sauce Code Powerline Plus Nerd File Types Mono")

;; increase font size for better readability
(set-face-attribute 'default nil :height 110)
(load-theme 'hemisu-dark t)
(set-face-background 'cursor "#EFEFEF")


;; ----------------------------------------------
;; recentf
;; ----------------------------------------------
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 1000)
(setq recentf-max-saved-items 10000)

;; ----------------------------------------------
;; search
;; ----------------------------------------------

(global-set-key (kbd "C-s") 'isearch-forward-regexp)
;;------------------------------------------------------------
;; - clipboard settings
;;------------------------------------------------------------

;; makes killing/yanking interact with the clipboard
(setq x-select-enable-clipboard t)

;; Use primary selection, remember that linux has 2 different buffers
(setq x-select-enable-primary t)

;; Save clipboard strings into kill ring before replacing them.
;; When one selects something in another program to paste it into Emacs,
;; but kills something in Emacs before actually pasting it,
;; this selection is gone unless this variable is non-nil
(setq save-interprogram-paste-before-kill t)

;; Mouse yank commands yank at point instead of at click.
(setq mouse-yank-at-point t)

;;------------------------------------------------------------
;; indentation and tabs
;;------------------------------------------------------------

;; Define how tab key works
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)
(setq-default c-basic-offset 2)
(defun fucking-tab ()
  (define-key conf-mode-map (kbd "<tab>") 'tab-to-tab-stop))  
(add-hook 'conf-unix-mode-hook 'fucking-tab)
;; automatically indent on return
(electric-indent-mode +1)
;; shell scripts
(setq-default sh-basic-offset 2)
(setq-default sh-indentation 2)


;;------------------------------------------------------------
;; saveplace and backups
;;------------------------------------------------------------

;; When you visit a file, point goes to the last place where it
;; was when you previously visited the same file.
;; http://www.emacswiki.org/emacs/SavePlace
(require 'saveplace)
(setq-default save-place t)
;keep track of saved places in ~/.emacs.d/places
(setq save-place-file (concat user-emacs-directory "places"))

;; Emacs can automatically create backup files. This tells Emacs to
;; put all backups in ~/.emacs.d/backups. More info:
;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Backup-Files.html
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory
                                               "backups"))))
(setq auto-save-default nil)
;; No need for ~ files when editing
(setq create-lockfiles nil)


;;------------------------------------------------------------
;;  automatically load conf mode in appropriate files
;;------------------------------------------------------------
(add-to-list 'auto-mode-alist '(".*package\.use$" . conf-mode))
(add-to-list 'auto-mode-alist '(".*package\.accept_keywords$" . conf-mode))
(add-to-list 'auto-mode-alist '(".*package\.license$" . conf-mode))
(add-to-list 'auto-mode-alist '(".*package\.mask$" . conf-mode))
(add-to-list 'auto-mode-alist '(".*package\.unmask$" . conf-mode))
(add-to-list 'auto-mode-alist '(".*i3config$" . conf-mode))
(add-to-list 'auto-mode-alist '(".*personalconfig$" . conf-mode))
(add-to-list 'auto-mode-alist '(".*zathurarc" . conf-mode))
;; (add-to-list 'auto-mode-alist '(".*\.ebuild$" . conf-mode))
;; now do the same for rkt files
(add-to-list 'auto-mode-alist '(".*\.rkt$" . scheme-mode))
(add-to-list 'auto-mode-alist '(".*cider-grimoire.*" . clojure-mode))

;;------------------------------------------------------------
;; eldoc
;;------------------------------------------------------------
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
( add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)

;;------------------------------------------------------------
;; - dired
;;------------------------------------------------------------
(defun dired-open-file ()
  "In dired, open the file named on this line."
  (interactive)
  (let* ((file (dired-get-filename nil t)))
    (message "Opening %s..." file)
    (call-process "xdg-open" nil 0 nil file)
    (message "Opening %s done" file)))

;; (define-key dired-mode-map (kbd "C-o") #'dired-open-file)


;;------------------------------------------------------------
;; - better looking symbols
;;------------------------------------------------------------
(defun my-symbols-alist ()
  (interactive)
  (setq prettify-symbols-alist
        (prettify-utils-generate
         ("defn" "𝑓")
         ("defun" "𝑓")
         (">=" "⪰")
         ("<=" "⪯")
         (">>" ">>")
         ("%1" "⍺")
         ("%2" "β")
         ("%3" "γ")
         ("%4" "δ")
         ("%5" "ε")
         ("!=" "≠")
         ("<>" "♦")
         ("||" "∥")
         ("=>" "⇒")
         ;; ("->" "⟶")
         ("<-" "←")
         ("::" "∷")
         ("nil" "∅")
         )))

(add-hook 'prog-mode-hook 'my-symbols-alist)
(global-prettify-symbols-mode 1)

;;------------------------------------------------------------
;;  various
;;------------------------------------------------------------
;; Changes all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)

;; tramp should default to ssh
(setq tramp-default-method "ssh")

;;given compiled or noncompiled always prefer the newer version
(setq load-prefer-newer t)

;; Shows all options when running apropos. For more info,
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Apropos.html
(setq apropos-do-all t)

;; opening links in browser
(global-set-key (kbd "s-u") 'browse-url)
(setq browse-url-browser-function 'browse-url-firefox)




(add-hook 'kill-emacs-hook 'silently-save-recentf-and-bookmarks)
;; ----------------------------------------------
;; configure various packages with use package
;; ----------------------------------------------


;; ----------------------------------------------
;;email
;; ----------------------------------------------

(require 'myemail)

;; ----------------------------------------------

(use-package org-bullets)
(use-package fish-mode)
(use-package diminish)
(use-package cider-hydra
  :after (cider hydra))
(use-package projectile)
(use-package helm-projectile)
;; -----------------------------------------------------
;; treemacs
;; -----------------------------------------------------
(use-package treemacs
  :ensure t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (use-package treemacs-evil
      :demand t)
    (setq treemacs-change-root-without-asking nil
          treemacs-collapse-dirs              (if (executable-find "python") 3 0)
          treemacs-file-event-delay           5000
          treemacs-follow-after-init          t
          treemacs-follow-recenter-distance   0.1
          treemacs-goto-tag-strategy          'refetch-index
          treemacs-indentation                2
          treemacs-indentation-string         " "
          treemacs-is-never-other-window      nil
          treemacs-never-persist              nil
          treemacs-no-png-images              nil
          treemacs-recenter-after-file-follow nil
          treemacs-recenter-after-tag-follow  nil
          treemacs-show-hidden-files          t
          treemacs-silent-filewatch           nil
          treemacs-silent-refresh             nil
          treemacs-sorting                    'alphabetic-desc
          treemacs-tag-follow-cleanup         t
          treemacs-tag-follow-delay           1.5
          treemacs-width                      35)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null (executable-find "python3"))))
      (`(t . t)
       (treemacs-git-mode 'extended))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  )

(use-package treemacs-projectile
  :config
  (setq treemacs-header-function #'treemacs-projectile-create-header))

;; -----------------------------------------------------
(use-package which-key
  :config
  (which-key-setup-side-window-bottom)
  (which-key-mode)
  (global-set-key (kbd "<F1>") 'which-key-show-top-level)
  (setq which-key-idle-delay 1.0))

(use-package async)
(use-package helm
  :config
  (require 'helm-config)
  (helm-mode 1)
  (setq helm-split-window-in-side-p t
        ;;open helm buffer inside current window, not occupy whole other window
        helm-move-to-line-cycle-in-source     t
        ;; move to end or beginning of source when reaching top or bottom of source.
        helm-ff-search-library-in-sexp        t
        ;; search for library in `require' and `declare-function' sexp.
        helm-scroll-amount                    8
        ;; scroll 8 lines other window using M-<next>/M-<prior>
        helm-ff-file-name-history-use-recentf t
        helm-M-x-fuzzy-match t)
  (define-key helm-map (kbd "C-o") #'helm-ff-run-switch-other-window)
  (define-key helm-map (kbd "C-f") #'helm-ff-run-switch-other-frame)
  (define-key helm-map (kbd "C-t") #'helm-ff-run-switch-other-tab)
  (define-key helm-map (kbd "<escape>") #'helm-keyboard-quit)
  (define-key helm-buffer-map (kbd "C-o") #'helm-buffer-switch-other-window)
  (define-key helm-buffer-map (kbd "C-f") #'helm-buffer-switch-other-frame)
  (define-key helm-buffer-map (kbd "C-t") #'helm-buffer-switch-other-tab)
  (define-key helm-buffer-map (kbd "<escape>") #'helm-keyboard-quit)


  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
  (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
  (define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "<f22>") 'helm-M-x)
  )
(use-package undo-tree
  :config
  (global-undo-tree-mode)
  (setq undo-tree-auto-save-history t)
  (setq undo-tree-history-dir (concat user-emacs-directory "undotree/"))
  (setq undo-tree-history-directory-alist `(("." . ,undo-tree-history-dir)))
  )
(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
  )

(use-package rainbow-mode
  :config
  (add-hook 'prog-mode-hook #'rainbow-mode)
  )
(use-package elscreen
  :config
  (elscreen-start)

  (setq elscreen-display-screen-number nil)
  (setq elscreen-tab-display-control nil)
  (setq elscreen-tab-display-kill-screen nil)
  (setq elscreen-tab-display-kill-screen nil)
  (setq elscreen-display-screen-number nil)

  ;; bindings
  (global-set-key (kbd "M-<left>") 'elscreen-previous)
  (global-set-key (kbd "M-<right>") 'elscreen-next)
  (global-set-key (kbd "C-t") 'elscreen-new-tab)
  (global-set-key (kbd "C-Q") 'elscreen-kill-tab-and-renumber)
  (global-set-key (kbd "<f21>") 'elscreen-previous)
  (global-set-key (kbd "<f23>") 'create-or-switch-tabs)
  (global-set-key (kbd "<f24>") 'elscreen-previous)
  (global-set-key (kbd "<f27>") 'create-or-switch-tabs)
  (global-set-key (kbd "<XF86Word>") 'windmove-left)
  (global-set-key (kbd "<XF86Messenger>") 'windmove-down)
  (global-set-key (kbd "<XF86WebCam>") 'windmove-right)
  (global-set-key (kbd "<XF86Calculater>") 'windmove-up)
  (global-set-key (kbd "<XF86iTouch>") 'elscreen-previous)
  (global-set-key (kbd "<XF86Support>") 'create-or-switch-tabs)
  (global-set-key (kbd "<f26>") 'windmove-up)
  (global-set-key (kbd "<f25>") 'windmove-down)
  (global-set-key (kbd "<f24>") 'windmove-left)
  (global-set-key (kbd "<f27>") 'windmove-right)
  (global-set-key (kbd "s-0") '(elscreen-jump 0))
  (global-set-key (kbd "<f2>") 'universal-argument)
  )
(use-package paradox
  :config (setq paradox-execute-asynchronously t
                ;; paradox-github-token t
                ;; paradox-github-token "bf0bdfb4f8fba519064f2b52ba69a7ca52a14b2d"
                ;; paradox-automatically-star t
                paradox-display-download-count t))

(use-package evil
  :init
  (progn
    (use-package evil-leader
      :config
      (progn
        (setq evil-leader/in-all-states t)
        (setq evil-leader/no-prefix-mode-rx '(".*"))
        (evil-leader/set-leader "<f14>")
        (evil-leader/set-key
          "!"   'delete-other-windows
          ")"   'delete-window
          ":" 'helm-M-x
          "<f14>"  'treemacs-delete-other-windows
          "b" 'helm-buffers-list
          "B" 'helm-bookmarks
          "F" 'helm-find-files
          "P"     'paradox-list-packages
          "SPC" 'other-window
          "a" 'align-regexp
          "c a" 'clojure-align
          "c B"  'cbm-hydra/body
          "c D"     'cider-doc
          "c b" 'cider-eval-buffer
          "c c"    'cider-restart
          "c d"   'cider-hydra-doc/body
          "c e" 'cider-hydra-eval/body
          "c f"   'cider-debug-defun-at-point
          "c g"     'cider-grimoire
          "C h" 'nil
          "c h r" 'cider-hydra-repl/body
          "c h e" 'cider-hydra-eval/body
          "c i" 'nil
          "c i 1" 'mmr-macroexpand-1-in-place
          "c i a" 'mmr-macroexpand-all-inplace
          "c j d" 'cider-find-dwim-immediate
          "c j i" 'cider-jack-in
          "c j n" 'cider-find-ns
          "c m"   'nil
          "c m 1"   'mmr-macroexpand-1
          "c m a"  'mmr-macroexpand-all
          "c n"   'cider-repl-set-ns
          "c N" 'cider-eval-next-sexp
          "c o j d" 'cider-find-dwim-other-window-immediate
          "c o j" 'nil
          "c o" 'nil
          "c p"   'cider-pprint-eval-last-sexp-to-repl
          "c q"   'cider-quit
          "c q"   'cider-quit
          "c c" 'nil
          "c c r"   'nil
          "c c r b" 'cider-format-buffer
          "c c r n" 'cljr-clean-ns
          "c r" 'cider-hydra-repl/body
          "c s" 'nil
          "c s r" 'cider-switch-to-repl-buffer
          "c s s" 'cider-switch-to-last-clojure-buffer
          "c t" 'cider-hydra-test/body
          ;; "c c t a" 'cider-test-run-project-tests
          ;; "c c t n" 'cider-test-run-ns-tests
          ;; "c c t p" 'cider-test-run-test
          ;; "c c x"     'cider-eval-current-sexp
          ;; "c p"     'flyspell-correct-previous-word-generic
          ;; "c w"     'flyspell-correct-word-generic
          "d"       'dashboard-startmeup
          "d" 'kill-this-buffer
          "e b" 'eval-buffer
          "e n" 'elisp-eval-next-sexp
          "e" 'nil
          "f" 'nil
          "f p" 'helm-projectile
          "f f" 'helm-semantic-or-imenu
          "f l" 'flycheck-list-errors
          "g" 'nil
          "gs" 'magit-status
          "gu" 'magit-push-current-to-upstream
          "gd" 'magit-dispatch-popup
          "i" 'ibuffer
          "k" 'nil
          "k b" 'kill-buffer-and-window
          "k f" 'delete-frame
          "k m" 'kill-matching-buffers-without-prompt
          "k o" 'delete-other-windows
          "k t" 'elscreen-kill-tab-and-renumber
          "k w" 'delete-window
          "l l" 'nil
          "m" 'mu4e
          ;; "m b" 'toggle-menu-bar-mode-from-frame
          ;; "ml"  'bookmark-bmenu-list
          ;; "ms" 'bookmark-set
          "n"     'evil-next-buffer
          "o"   'other-window
          ;; "o o" 'other-window
          ;; "o d" 'org-cut-subtree
          ;; "o i" 'nil
          ;; "o i c" 'org-insert-todo-heading
          ;; "o t" 'org-ctrl-c-ctrl-c
          "p"     'evil-prev-buffer
          "q" 'delete-window
          "q" 'kill-buffer-and-window
          "r" 'helm-recentf
          "s"       'nil
          "s u"   'sudo
          "s s" 'sayid-trace-hydra/body
          ;; "s s t" 'nil
          ;; "s s t d" 'nil
          ;; "s s t n" 'sayid-trace-ns-in-file
          ;; "s s t r" 'sayid-trace-ns-by-pattern
          ;; "s s t f" 'sayid-trace-fn-enable
          ;; "s s t d a" 'sayid-trace-disable-all
          ;; "s s t p" 'sayid-query-form-at-point
          ;; "s s t d f" 'sayid-trace-fn-disable
          "t" 'nil
          ;; "t c" 'elscreen-kill-tab-and-renumber
          ;; "t n" 'elscreen-create
          "t" 'treemacs-projectile
          "q" 'treemacs-projectile-toggle
          "x"         'cider-eval-defun-at-point
          "z"       'nil
          "z z"     'cider-right-repl
          "z p"     'zeal-at-point
          "z s"     'zeal-at-point-search
          ;; "c c i"   'cider-switch-to-repl-buffer
          ;; "c c s"   'cider-switch-to-last-clojure-buffer
          ;; "c e "   'cider-eval-last-sexp
          ;; "f"  'helm-find-files
          ;; "x"       'cider-eval-sexp-at-point
          ;; bindings for common lisp
          ;; possibly remove these later as other bindings are more consistent
          )
        

        (global-evil-leader-mode)
        (evil-mode 1))
    )
    )
  :config
  (setq evil-move-beyond-eol t
        evil-want-fine-undo t
        evil-esc-delay 0
        evil-shift-width 2)
  (evil-mode 1)
  (evil-set-initial-state 'pdf-view-mode 'emacs)
  (evil-set-initial-state 'paradox-menu-mode 'emacs)
  (evil-set-initial-state 'pdf-outline-buffer-mode 'emacs)
  (evil-set-initial-state 'bm-show-mode 'emacs)
  (evil-set-initial-state 'sayid-traced-mode 'emacs)
  (evil-set-initial-state 'sayid-mode 'emacs)
  (evil-set-initial-state 'mu4e:main 'motion)
  ;;bindings
  (define-key evil-visual-state-map (kbd ">") 'djoyner/evil-shift-right-visual)
  (define-key evil-visual-state-map (kbd "<") 'djoyner/evil-shift-left-visual)
  (define-key evil-visual-state-map [tab] 'djoyner/evil-shift-right-visual)
  (define-key evil-visual-state-map [S-tab] 'djoyner/evil-shift-left-visual)
  (define-key evil-normal-state-map (kbd "C-t") 'elscreen-new-tab)
  (define-key evil-normal-state-map (kbd "[")   'evil-prev-buffer)
  (define-key evil-normal-state-map (kbd "]")   'evil-next-buffer)
  (define-key evil-normal-state-map "s" 'smartparens-hydra/body)
  (define-key evil-normal-state-map "x" 'mmr-eval-current-sexp)
  (define-key evil-normal-state-map "(" 'sp-backward-up-sexp)
  (define-key evil-normal-state-map ")" 'sp-end-of-sexp)
  (define-key evil-normal-state-map (kbd "-")
    (lambda ()
      (interactive)
      (splitwin "v")))

  (define-key evil-normal-state-map (kbd "|")
    (lambda ()
      (interactive)
      (splitwin "h")))


  ;; textobj for lines and entire files
  ;; first the macro that makes it possible
  (defmacro define-and-bind-text-object (key start-regex end-regex)
    (let ((inner-name (make-symbol "inner-name"))
          (outer-name (make-symbol "outer-name")))
      `(progn
         (evil-define-text-object ,inner-name (count &optional beg end type)
           (evil-select-paren ,start-regex ,end-regex beg end type count nil))
         (evil-define-text-object ,outer-name (count &optional beg end type)
           (evil-select-paren ,start-regex ,end-regex beg end type count t))
         (define-key evil-inner-text-objects-map ,key (quote ,inner-name))
         (define-key evil-outer-text-objects-map ,key (quote ,outer-name)))))

  ;; now some new text objects

  ;; create "il"/"al" (inside/around) line text objects:
  (define-and-bind-text-object "l" "^\\s-*" "\\s-*$")
  ;; create "ie"/"ae" (inside/around) entire buffer text objects:
  (define-and-bind-text-object "e" "\\`\\s-*" "\\s-*\\'")


  (evil-global-set-key 'normal (kbd "<up>") 'michael-increase-font)
  (evil-global-set-key 'normal (kbd "<down>") 'michael-decrease-font)
  )


(use-package evil-commentary
  :after (evil)
  :config (evil-commentary-mode))

(use-package evil-tabs
  :after (evil)
  :config (global-evil-tabs-mode))

(use-package helpful
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)))

(use-package telephone-line
  :demand t
  :config
  (setq telephone-line-lhs
        '((evil . (telephone-line-evil-tag-segment))
          (nil   . (telephone-line-position-segment))
          (accent . (telephone-line-vc-segment))
          (nil    . (telephone-line-minor-mode-segment
                   telephone-line-buffer-segment))))

  (setq telephone-line-rhs
        '((nil    . (telephone-line-misc-info-segment))
          (accent . (telephone-line-major-mode-segment))))

  (setq telephone-line-primary-left-separator 'telephone-line-cubed-left
        telephone-line-secondary-left-separator 'telephone-line-cubed-hollow-left
        telephone-line-primary-right-separator 'telephone-line-cubed-right
        telephone-line-secondary-right-separator 'telephone-line-cubed-hollow-right)
  (setq telephone-line-height 24
        telephone-line-evil-use-short-tag nil)
  (telephone-line-mode t))

(use-package hydra
  :after (smartparens)
  :config (setq hydra-verbose t)
  (defhydra sayid-trace-hydra (:hint nil :exit t)
    "
  ^Functions^  |     ^All^    |    ^Trace^   |      ^Show^       |
--^---------^--+-----^---^----+----^-----^---+------^----^--------
  _i_: inner   | _K_: kill    | _f_: file    |   _l_: list
  _o_: outer   | _D_: disable | _p_: pattern |   _c_: current
  _e_: enable  | _E_: enable  |            |   _w_: workspace
  _d_: disable |            |            |   _s_: symbol
  _r_: remove  |            |            |
--^---------^--+-----^---^----+----^-----^---+------^----^--------
"
    ("i" sayid-inner-trace-fn)
    ("K" sayid-kill-all-traces)
    ("o" sayid-outer-trace-fn)
    ("r" sayid-remove-trace-fn)
    ("D" sayid-trace-disable-all)
    ("E" sayid-trace-enable-all)
    ("e" sayid-trace-fn-enable)
    ("d" sayid-trace-fn-disable)
    ("p" sayid-trace-ns-by-pattern)
    ("f" sayid-trace-ns-in-file)
    ("l" sayid-show-traced)
    ("c" sayid-show-traced-ns)
    ("s" sayid-query-form-at-point)
    ("w" sayid-get-workspace))
  (defhydra smartparens-hydra (:foreign-keys run
                                             :body-pre (setq michaelr-slurp-direction :forward))
    "Smartparens"
    ;; slurping and barfing
    ("s" bidirectional-slurp "Slurp")
    ("b" bidirectional-barf "Barf")
    ("r" toggle-slurp-direction "toggle direction")
    ;; navigation
    ("j" sp-down-sexp "Down")
    ("k" sp-backward-up-sexp "Up")
    ("h" sp-beginning-of-sexp "Beginning")
    ("i" mmr-myinsert  :exit t)
    ("l" sp-end-of-sexp "End")
    ("e" sp-end-of-sexp "End")
    ("n" sp-beginning-of-next-sexp "Next")
    ("p" sp-beginning-of-previous-sexp "Previous")
    ("a" sp-after-sexp)
    ("K" sp-topmost-sexp)
    ;; Kill/copy
    ("y" sp-copy-sexp "copy sexp" :exit t)
    ("d" sp-kill-sexp "kill sexp")
    ;; Misc
    ("t" sp-transpose-sexp "transpose")
    ("T" smartparens-mode "toggle smartparens" :exit t)
    ("J" sp-join-sexp)
    ;; ("c" sp-comment-sexp :exit t)
    ("c" mmr-toggle-comment-current-sexp :exit t)
    ("C" mmr-toggle-comment-top-sexp :exit t)
    ;; splice and split
    ("S" sp-splice-sexp)
    ("A" sp-splice-sexp-killing-around)
    ("<down>" sp-splice-sexp-killing-backward)
    ("<up>" sp-splice-sexp-killing-forward)
    ("X" sp-split-sexp)
    ;; rewrap wrap and unwrap
    ("(" (lambda (_) (interactive "P") (sp-wrap-with-pair "(")))
    ("{" (lambda (_) (interactive "P") (sp-wrap-with-pair "{")))
    ("[" (lambda (_) (interactive "P") (sp-wrap-with-pair "[")))
    ("w" sp-rewrap-sexp "rewrap sexp" :exit t)
    ("U" sp-unwrap-sexp "unwrap")
    ("'" (lambda (_) (interactive "P") (sp-wrap-with-pair "'")) :exit t)
    ("\"" (lambda (_) (interactive "P") (sp-wrap-with-pair "\"")) :exit t)
    ("q" nil "Quit" :color blue))
  )

(use-package evil-smartparens
  :after (smartparens evil))
(use-package smartparens
  :config
  (require 'smartparens-config)
  (evil-define-key 'normal "s" 'evil-smartparens-mode-map 'smartparens-hydra/body)
  (setq sp-autoskip-closing-pair '(quote always))
  (smartparens-global-strict-mode)
  (add-hook 'smartparens-enabled-hook #'evil-smartparens-mode)
  )
(use-package cider-hydra)
(use-package sayid
  :after (cider))
(use-package cider
  :commands (cider cider-connect cider-jack-in)
  :config
  (add-hook 'cider-mode-hook 'evil-normal-state)
  (add-hook 'cider--debug-mode-hook 'my-cider-debug-toggle-insert-state)
  (add-hook 'cider-macroexpansion-mode-hook 'evil-insert-state)
  (add-hook 'cider-repl-mode-hook 'evil-normal-state)

  (add-hook 'clojure-mode-hook 'cider-mode)
  (add-hook 'cider-repl-mode-hook 'eldoc-mode)
  (add-hook 'clojure-mode-hook 'eldoc-mode)
  (evil-set-initial-state 'cider-popup-buffer-mode 'insert)
  (evil-set-initial-state 'cider-inspector-mode 'emacs)
  (evil-set-initial-state 'cider-stacktrace-mode 'emacs)
  (evil-set-initial-state 'cider-test-report-mode 'emacs)
  (setq cider-auto-select-error-buffer t
        cider-repl-pop-to-buffer-on-connect nil
        cider-repl-use-clojure-font-lock t
        cider-repl-wrap-history t
        cider-repl-history-size 1000
        cider-show-error-buffer t
        nrepl-hide-special-buffers t)

  (evil-define-key 'normal cider-mode-map (kbd "x") 'cider-eval-defun-at-point)
  (evil-define-key 'normal cider-mode-map (kbd "X") 'cider-eval-last-sexp)
  (evil-define-key 'visual cider-mode-map (kbd "x") 'cider-eval-region)
  (evil-define-key 'normal emacs-lisp-mode-map (kbd "x") 'eval-defun)
  (evil-define-key 'normal emacs-lisp-mode-map (kbd "X") 'eval-last-sexp)
  (evil-define-key 'visual emacs-lisp-mode-map (kbd "x") 'eval-region)
  (evil-define-key 'normal lisp-interaction-mode-map (kbd "x") 'eval-defun)
  (evil-define-key 'normal lisp-interaction-mode-map (kbd "X") 'eval-last-sexp)
  (evil-define-key 'visual lisp-interaction-mode-map (kbd "x") 'eval-region)
  )

(use-package avy
  ;; conveinient movement via keyboard
  :config
  (setq avy-keys
        '(?q ?w ?e ?r ?t ?y ?i ?o ?p ?a ?s ?d ?f ?g ?h ?j ?k ?h ?l ?z ?x ?c ?v ?b ?n ?m))

  (define-key evil-normal-state-map "f" 'evil-avy-goto-word-or-subword-1)
  (define-key evil-normal-state-map "F" 'evil-avy-goto-char-2)
  )
(use-package zeal-at-point)

(use-package helm-flycheck
  :after (helm flycheck))
(use-package flycheck
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode)
  (eval-after-load 'flycheck '(flycheck-pos-tip-mode))
  (eval-after-load 'flycheck
    '(setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages))


  ;; global keys to cycle through the errors
  (global-set-key (kbd "s-n") 'next-error)
  (global-set-key (kbd "s-p") 'previous-error)

  )

(use-package flycheck-pos-tip
  :after (flycheck))
(use-package company
  ;;completion
  :config
  (global-company-mode)
  (setq company-idle-delay 0)
  )
(use-package magit)
(use-package gist
  )
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(async-bytecomp-allowed-packages
   (quote
    (async helm helm-core helm-ls-git helm-ls-hg magit all)))
 '(bmkp-last-as-first-bookmark-file "~/.emacs.d/bookmarks")
 '(cider-font-lock-dynamically (quote (macro function var deprecated core)))
 '(cider-lein-parameters "trampoline repl :headless :host ::")
 '(cider-repl-pop-to-buffer-on-connect nil)
 '(custom-safe-themes
   (quote
    ("7feeed063855b06836e0262f77f5c6d3f415159a98a9676d549bfeb6c49637c4" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default)))
 '(global-aggressive-indent-mode t)
 '(global-color-identifiers-mode t)
 '(global-visual-line-mode t)
 '(package-selected-packages
   (quote
    (mu4e-alert mu4e-maildirs-extension evil-mu4e oauth2 oath2 fish-mode rust-mode treemacs-evil treemacs-projectile treemacs helm-projectile evil-smartparens zeal-at-point which-key use-package telephone-line smartparens sayid rainbow-mode rainbow-delimiters projectile paradox org-bullets magit helpful helm-flycheck helm gist flycheck-pos-tip evil-tabs evil-leader evil-commentary diminish company cider-hydra avy)))
 '(paradox-github-token t)
 '(send-mail-function (quote mailclient-send-it))
 '(telephone-line-mode t)
 '(tramp-syntax (quote default) nil (tramp)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(elscreen-tab-background-face ((t (:background "black"))))
 '(elscreen-tab-control-face ((t (:background "#aed033" :foreground "black" :underline "Gray50"))))
 '(elscreen-tab-current-screen-face ((t (:background "#aed033" :foreground "black"))))
 '(elscreen-tab-other-screen-face ((t (:background "black" :foreground "#aed033"))))
 '(org-level-1 ((t (:inherit outline-1 :height 180))))
 '(org-level-2 ((t (:inherit outline-2 :height 160))))
 '(org-level-3 ((t (:inherit outline-3 :height 140))))
 '(org-level-4 ((t (:inherit outline-4 :height 120))))
 '(rainbow-delimiters-depth-1-face ((t (:foreground "dark orange"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "deep pink"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "chartreuse"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "deep sky blue"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "yellow"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "orchid"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "spring green"))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "sienna1"))))
 '(telephone-line-evil-normal ((t (:inherit telephone-line-evil :background "dark goldenrod")))))



;; bullshit done in daemon mode after frame exists
(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (load-theme 'hemisu-dark t)
                (set-face-background 'cursor "#EFEFEF")
                (set-face-attribute  
                 'default nil 
                 :font "Sauce Code Powerline Plus Nerd File Types Mono")

                (load-theme 'hemisu-dark t)
                (set-face-attribute 'default nil :height 120)

                (set-face-background 'cursor "#EFEFEF")
                (silence-bs)
              )))


(defun kill-scratch-buffer ()
  (set-buffer (get-buffer-create "*scratch*"))
  ;; Kill the current (*scratch*) buffer
  (kill-buffer (current-buffer))

  ;; Make a brand new *scratch* buffer
  (set-buffer (get-buffer-create "*scratch*"))
  (switch-to-buffer "*scratch*")
  (lisp-interaction-mode)
  (evil-normal-state)
  )

(kill-scratch-buffer)
;; (byte-recompile-directory (expand-file-name "~/.emacs.d") 0)

(defun michael-evil-bindings-cider ()
  (interactive)
  (evil-local-set-key 'normal "s" 'smartparens-hydra/body)
  (evil-local-set-key 'normal "x" 'cider-eval-current-sexp)
  (evil-local-set-key 'visual "x" 'cider-eval-region)
  )

(defun michael-evil-bindings-elisp ()
  (interactive)
  (evil-local-set-key 'normal "s" 'smartparens-hydra/body)
  (evil-local-set-key 'normal "x" 'eval-defun)
  (evil-local-set-key 'visual "x" 'eval-region)
  )

(add-hook 'cider-mode-hook 'michael-evil-bindings-cider)
(add-hook 'elisp-refs-mode-hook 'michael-evil-bindings-elisp)

(provide 'init)
;;; init.el ends here
