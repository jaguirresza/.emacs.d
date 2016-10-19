;;; package --- darwin

;;; Commentary:
;;; OSX specific configuration.

;;; Code:
(setq mac-command-modifier 'super)
(setq mac-option-modifier 'meta)
(setq mac-control-modifier 'control)

(use-package exec-path-from-shell
  :init (exec-path-from-shell-initialize))
;;; darwin.el ends here
