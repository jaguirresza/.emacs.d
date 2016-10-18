;;; package --- flycheck

;;; Commentary:
;;; flycheck package configuration

;;; Code:
(use-package flycheck
  :init (add-hook 'after-init-hook #'global-flycheck-mode)

  :config
  (setq-default flycheck-disabled-checkers '(go-errcheck))

  (use-package flycheck-flow)

  (use-package flycheck-pos-tip
    :init (flycheck-pos-tip-mode)))
;;; flycheck.el ends here
