;;; package --- platform

;;; Commentary:
;;; Platform specific settings.

;;; Code:
(cond
 ((eq system-type 'darwin)
  (load-file "~/.emacs.d/platform/darwin.el")))
;;; platform.el ends here
