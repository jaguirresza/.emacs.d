;;; package --- company

;;; Commentary:
;;; Company configuration

;;; Code:
(use-package company
  :init
  (add-hook 'after-init-hook 'global-company-mode)

  :config
  (general-define-key
   :keymaps 'company-active-map
   "M-n" nil
   "M-p" nil
   "C-j" 'company-select-next
   "C-k" 'company-select-previous
   "C-d" (lambda ()
           (interactive)
           (when (fboundp 'company-select-next) (company-select-next 5)))
   "C-u" (lambda ()
           (interactive)
           (when (fboundp 'company-select-previous) (company-select-previous 5))))

  (setq company-tooltip-limit 20
        company-echo-delay 0
        company-begin-commands '(self-insert-command)
        company-idle-delay 0.2
        company-tooltip-minimum-width 40
        company-tooltip-align-annotations t
        company-tooltip-margin 0)

  (use-package company-flow
    :init
    (setq company-flow-modes '(js-mode js2-mode js2-jsx-mode))
    (add-to-list 'company-backends 'company-flow))

  (use-package company-go
    :init
    (add-hook 'go-mode-hook (lambda ()
                (set (make-local-variable 'company-backends) '(company-go))))
    :config
    (setq company-go-show-annotation t)))
;;; company.el ends here
