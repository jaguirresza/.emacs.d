;;; package --- go

;;; Commentary:
;;; Configuration of go-mode

;;; Code:
(use-package go-mode
  :config
  (setq-default gofmt-command "goimports")

  (general-define-key :keymaps 'go-mode-map :states 'normal
            "gd" 'godef-jump
            "SPC d" 'godoc-at-point)

  (add-hook 'go-mode-hook
            (lambda ()
              (add-hook 'before-save-hook 'gofmt-before-save)
              (when (boundp 'evil-shift-width) (setq evil-shift-width 4))
              (setq tab-width 4
                    indent-tabs-mode 1)))

  (use-package go-eldoc
    :config
    (add-hook 'go-mode-hook 'go-eldoc-setup)))
;;; go.el ends here
