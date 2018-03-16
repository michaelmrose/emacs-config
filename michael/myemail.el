;;; michael.el mu4e setup
;;; Commentary:
;;; Code:
(add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")
(require 'mu4e)
(require 'evil-mu4e)
(setq mu4e-maildir "~/mail")
;; (use-package mu4e-maildirs-extension
;;   :config (mu4e-maildirs-extension))
(mu4e-maildirs-extension)
 (setq
 user-mail-address "m@rosenetwork.net"
 user-full-name  "Michael Rose"
 mu4e-update-interval 60
 mu4e-use-fancy-chars t)
(setq mu4e-bookmarks `(("\\\\Inbox" "Inbox" ?i)
                       ("flag:flagged" "Flagged messages" ?f)
                       ("date:today..now" "Today's messages" ?t)
                       ("date:7d..now" "Last 7 days" ?7)
                       (,(concat "flag:unread AND "
                                 "NOT flag:trashed AND "
                                 "NOT maildir:/[Gmail].Spam AND "
                                 "NOT maildir:/[Gmail].Bin")
                        "Unread messages" ?u))) 
(add-hook 'mu4e-mark-execute-pre-hook
          (lambda (mark msg)
            (cond ((member mark '(refile trash)) (mu4e-action-retag-message msg "-\\Inbox"))
                  ((equal mark 'flag) (mu4e-action-retag-message msg "\\Starred"))
                  ((equal mark 'unflag) (mu4e-action-retag-message msg "-\\Starred"))))) 
(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-stream-type 'starttls
      mu4e-html2text-command 'mu4e-shr2text
      shr-color-visible-luminance-min 80
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587)

;; don't keep message buffers around
(setq message-kill-buffer-on-exit nil)
(add-function :after message-send-mail-function 'kill-buffer-and-window)

(evil-define-key 'motion 'mu4e-headers-mode-map (kbd "-") 'mu4e-headers-mark-for-unflag)

(evil-define-key 'motion 'mu4e-headers-mode-map (kbd "+") 'mu4e-headers-mark-for-flag )
(evil-define-key 'visual 'mu4e-headers-mode-map (kbd "d") 'mu4e-headers-mark-for-trash )
(evil-define-key 'visual 'mu4e-headers-mode-map (kbd "D") 'mu4e-headers-mark-for-delete )

(evil-define-key 'motion 'mu4e-headers-mode-map (kbd "~") 'michael-mu4e-select-all-messages)

(evil-define-key 'motion 'mu4e-headers-mode-map (kbd "x") 'my-mu4e-mark-execute-all-no-confirm)

(evil-define-key 'visual 'mu4e-headers-mode-map (kbd "x") 'my-mu4e-mark-execute-all-no-confirm)
(add-hook 'mu4e-headers-mode-hook 'evil-motion-state)
(defun michael-mu4e-select-all-messages ()
  (interactive)
  (evil-visual-line)
  (evil-goto-line)
  (previous-line))

(defun my-mu4e-mark-execute-all-no-confirm ()
  "Execute all marks without confirmation."
  (interactive)
  (mu4e-mark-execute-all 'no-confirm)) 
;;mu4e-alert
(setq mu4e-alert-interesting-mail-query
      (concat
       "flag:unread"
       " AND NOT flag:trashed"
       " AND NOT maildir:"
       "\"/[Gmail].All Mail\""))
(mu4e-alert-set-default-style 'libnotify)
(add-hook 'after-init-hook #'mu4e-alert-enable-mode-line-display)
(add-hook 'after-init-hook #'mu4e-alert-enable-notifications)

()
(provide 'myemail)
;;; myemail.el ends here
