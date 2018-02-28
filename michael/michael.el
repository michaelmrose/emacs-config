;;; michael.el personal functions some even written by me
;;; Commentary:
;;; Code:

(defun sort-words (reverse beg end)
  "Sort words in region alphabetically, in REVERSE if negative.
    Prefixed with negative \\[universal-argument], sorts in reverse.
  
    The variable `sort-fold-case' determines whether alphabetic case
    affects the sort order.
  
    See `sort-regexp-fields'."
  (interactive "*P\nr")
  (sort-regexp-fields reverse "\\w+" "\\&" beg end))

(defun ws (target)
  "Switch i3 to TARGET workspace."
  (interactive)
  (shell-command (concat "i3 workspace " target)))

(defun djoyner/evil-shift-left-visual ()
  (interactive)
  (evil-shift-left (region-beginning) (region-end))
  (evil-normal-state)
  (evil-visual-restore))

(defun djoyner/evil-shift-right-visual ()
  (interactive)
  (evil-shift-right (region-beginning) (region-end))
  (evil-normal-state)
  (evil-visual-restore))

(defun michael-increase-font ()
  (interactive)
  (set-face-attribute 'default nil :height
                      (+ 10 (face-attribute 'default :height))))

(defun michael-decrease-font ()
  (interactive)
  (set-face-attribute 'default nil :height
                      (- (face-attribute 'default :height) 10)))
(defun fucking-tab ()
  (define-key conf-mode-map (kbd "<tab>") 'tab-to-tab-stop))

(defun splitwin (my-direction)
  (interactive)
  (if (equal my-direction "v")
      (split-window-vertically)
    (split-window-horizontally))
  (other-window 1)
  (switch-to-buffer "blank"))

;;------------------------------------------------------------
;; Elscreen related
;;------------------------------------------------------------

(defun create-or-switch-tabs ()
  "Switch to the next tab or create another if it doesn't exit."
  (interactive)
  (if (elscreen-get-screen-property (+ 1 (elscreen-get-current-screen)))
      (elscreen-next)
    (progn
      (elscreen-create)
      (switch-to-buffer (generate-new-buffer "*new*")))))

(defun elscreen-new-tab ()
  (interactive)
  (elscreen-create)
  (switch-to-buffer (generate-new-buffer "*new*"))
  (elscreen-renumber))

(defun elscreen-renumber-internal ()
  (cl-loop for i from 0 for s in (sort (elscreen-get-screen-list) '<) do
           (when (/= i s) 
             (setf (car (assoc s (elscreen-get-conf-list 'screen-property))) i
                   (car (member s (elscreen-get-conf-list 'screen-history))) i
                   (car (assoc s (elscreen-get-screen-to-name-alist-cache))) i))))

(defun elscreen-renumber ()
  (interactive)
  (elscreen-renumber-internal)
  (elscreen-tab-update t))

(defun elscreen-renumber-create ()
  (interactive)
  (if (> (elscreen-get-number-of-screens) 9)
      (message "No more screens.")
    (elscreen-renumber-internal)
    (elscreen-create)))

(defun elscreen-renumber-create-right ()
  (interactive)
  (if (> (elscreen-get-number-of-screens) 9)
      (message "No more screens.")
    (elscreen-renumber-internal)
    (let ((current-s-nth (1+ (elscreen-get-current-screen))))
      (cl-loop for i downfrom (elscreen-get-number-of-screens) to (1+ current-s-nth)
               for s in (sort (elscreen-get-screen-list) '>) do
               (when (/= i s)
                 (setf (car (assoc s (elscreen-get-conf-list 'screen-property))) i
                       (car (member s (elscreen-get-conf-list 'screen-history))) i
                       (car (assoc s (elscreen-get-screen-to-name-alist-cache))) i)))
      (elscreen-create))))
(defun elscreen-kill-tab-and-renumber ()
  "Kill current tab and renumbers."
  (interactive)
  (elscreen-kill)
  (mmr-kill-new-tabs)
  (elscreen-renumber)
  )

(defun mmr-kill-new-tabs ()
  (interactive)
  (let ((new-tab-regex "^*new"))
    (->> (elscreen-get-screen-to-name-alist)
         (cl-remove-if-not (lambda (a-buffer) (string-match new-tab-regex (cdr a-buffer))))
         (mapcar 'car)
         (mapcar 'elscreen-kill-screen-and-buffers))
    (kill-matching-buffers-without-prompt new-tab-regex)))

;;------------------------------------------------------------
;; Dired
;;------------------------------------------------------------
(defun dired-open-file ()
  "In dired, open the file named on this line."
  (interactive)
  (let* ((file (dired-get-filename nil t)))
    (message "Opening %s..." file)
    (call-process "xdg-open" nil 0 nil file)
    (message "Opening %s done" file)))
;;------------------------------------------------------------
;; helm
;;------------------------------------------------------------

(defun helm-ff-run-switch-other-frame ()
  "Run switch to other window action from `helm-source-find-files'."
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action 'helm-find-files-other-frame)))

(defun helm-find-files-other-frame (_candidate)
  "Keep current-buffer and open files in separate windows."
  (let* ((files (helm-marked-candidates))
         (buffers (mapcar 'find-file-noselect files)))
    (switch-to-buffer-other-frame (car buffers))
    (helm-aif (cdr buffers)
        (save-selected-window
          (cl-loop for buffer in it
                   do (progn
                        (select-window (split-window))
                        (switch-to-buffer buffer)))))))

(defun helm-ff-run-switch-other-tab ()
  "Run switch to other window action from `helm-source-find-files'."
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action 'helm-find-files-other-tab)))

(defun helm-find-files-other-tab (_candidate)
  "Keep current-buffer and open files in separate windows."
  (let* ((files (helm-marked-candidates))
         (buffers (mapcar 'find-file-noselect files)))
    (elscreen-find-file (buffer-file-name (car buffers)))))

(defun helm-switch-to-buffers-other-tab (buffer-or-name)
  "switch to buffer BUFFER-OR-NAME in other window.
See `helm-switch-to-buffers' for switching to marked buffers."
  (elscreen-create)
  (switch-to-buffer buffer-or-name))

(defun helm-buffer-switch-other-tab ()
  "Run switch to other window action from `helm-source-buffers-list'."
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action 'helm-switch-to-buffers-other-tab)))

(defun silence-bs ()
  (interactive)
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
                     undo-tree-mode)))

(defun kill-matching-buffers-without-prompt (regexp &optional internal-too)
  "Kill buffers whose name match the specified REGEXP without prompting.
The optional second argument indicates whether to kill internal buffers too."
  (interactive "sKill buffers matching this regular expression: \nP")
  (dolist (buffer (buffer-list))
    (let ((name (buffer-name buffer)))
      (when (and name (not (string-equal name ""))
                 (or internal-too (/= (aref name 0) ?\s))
                 (string-match regexp name))
        (kill-buffer buffer)))))

(defun silently-save-recentf-and-bookmarks ()
  "Saves recentf without fucking messaging."
  (interactive)
  (let ((message-log-max nil)
        (inhibit-message t))
    (recentf-save-list)
    (bookmark-save)))

(run-at-time nil 180 'silently-save-recentf-and-bookmarks)

(defun sudo ()
  "Use TRAMP to `sudo' the current buffer."
  (interactive)
  (when buffer-file-name
    (find-alternate-file
     (concat "/sudo:root@localhost:"
             buffer-file-name))))

(defun url ()
  (interactive)
  (browse-url))

(defun buffer-same-mode (change-buffer-fun)
  (let ((current-mode major-mode)
        (next-mode nil))
    (while (not (eq next-mode current-mode))
      (funcall change-buffer-fun)
      (setq next-mode major-mode))))

(defun previous-buffer-same-mode ()
  (interactive)
  (buffer-same-mode #'previous-buffer))

(defun next-buffer-same-mode ()
  (interactive)
  (buffer-same-mode #'next-buffer))

(provide 'michael)


;;; michael.el ends here
