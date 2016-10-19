;;; package --- smart-parens

;;; Commentary:
;;; smartparens configuration.

;;; Code:
(use-package smartparens
  :defer 2
  :diminish smartparens-mode

  :config
  (use-package smartparens-config :ensure smartparens)

  (setq sp-highlight-pair-overlay nil
        sp-show-pair-delay 0
        sp-show-pair-from-inside t
        sp-cancel-autoskip-on-backward-movement nil)

  (smartparens-global-mode 1)
  (show-smartparens-global-mode 1)

  (when (fboundp 'sp-local-pair)
    (sp-local-pair '(go-mode js2-jsx-mode) "{" nil
                 :post-handlers '(:add my-curly-braces-newline-handle)
                 :actions '(insert))))

(defun my-curly-braces-newline-handle (id action context)
  (save-excursion
    (newline-and-indent))
  (indent-according-to-mode))
;;; smart-parens.el ends here
