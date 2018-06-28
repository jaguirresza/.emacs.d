;;; package --- prettier

;;; Commentary:
;;; prettier configuration.

;;; Code:
(use-package prettier-js
  :after js2-mode
  :init
  (add-hook 'js2-mode-hook 'prettier-js-mode)
  (add-hook 'web-mode-hook 'prettier-js-mode)
  :config
  (setq prettier-js-args '(
                           "--tab-width" "4"
                           "--use-tabs" "true"
                           "--parser" "flow"
                           "--no-bracket-spacing"
                           "--single-quote"
                           "--trailing-comma" "es5"
                           "--bracket-spacing" "false")
        ))

;;; prettier.el ends here
