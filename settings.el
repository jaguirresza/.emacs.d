;;; package --- settings

;;; Commentary:
;;; General settings.

;;; Code:
(setq auto-save-default nil
      inhibit-startup-message t
      comment-auto-fill-only-comments t
      confirm-nonexistent-file-or-buffer nil
      custom-file "~/.emacs.d/custom.el"
      disabled-command-function nil
      kill-buffer-query-functions (remq 'process-kill-buffer-query-function
                                        kill-buffer-query-functions))

(setq-default c-basic-offset 4
              fill-column 80
              indent-tabs-mode nil
              tab-width 2
              truncate-lines t)

(c-set-offset 'case-label '+)
(fset 'yes-or-no-p 'y-or-n-p)

(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

(define-key isearch-mode-map [escape] 'isearch-abort)
(define-key isearch-mode-map "\e" 'isearch-abort)
(global-set-key [escape] 'keyboard-escape-quit)

(mapc 'load (directory-files "~/.emacs.d/settings" t "^[A-Za-z][A-Za-z-0-9]*\\.el"))
;;; settings.el ends here
