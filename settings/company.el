;;; package --- company

;;; Commentary:
;;; Company configuration

;;; Code:
(use-package company
  :init
  (add-hook 'after-init-hook 'global-company-mode)

  :general
  (:keymaps 'company-active-map
                      "M-n" nil
                      "M-p" nil
                      "C-j" 'company-select-next
                      "C-k" 'company-select-previous
                      "C-d" (lambda ()
                              (interactive) (company-select-next 5))
                      "C-u" (lambda ()
                              (interactive) (company-select-previous 5)))
  :config
  (setq company-tooltip-limit 20
        company-echo-delay 0
        company-begin-commands '(self-insert-command)
        company-idle-delay 0.2
        company-tooltip-minimum-width 40
        company-tooltip-align-annotations t
        company-tooltip-margin 0)
  
  (use-package company-go
    :init
    (add-hook 'go-mode-hook (lambda ()
                (set (make-local-variable 'company-backends) '(company-go))))
    :config
    (setq company-go-show-annotation t)))
;;; company.el ends here
