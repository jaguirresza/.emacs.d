;;; package --- backup

;;; Commentary:
;;; backup configuration

;;; Code:
(if (not (file-exists-p "~/.emacs.d/backups"))
    (make-directory "~/.emacs.d/backups" t))

(setq backup-by-copying t
      backup-directory-alist '(("." . "~/.emacs.d/backups")))
;;; backup.el ends here
