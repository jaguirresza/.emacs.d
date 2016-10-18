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

  (sp-local-pair 'go-mode "{" nil
                 :post-handlers '(:add my-go-handle-breakline)
                 :actions '(insert)))

(defun my-go-handle-breakline (id action context)
  (save-excursion
    (newline-and-indent))
  (indent-according-to-mode))
