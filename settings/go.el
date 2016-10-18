;;; package --- go

;;; Commentary:
;;; Configuration of go-mode

;;; Code:
(use-package go-mode
  :init
  ;; use goimports if available
  (when (executable-find "goimports")
    (setq gofmt-command "goimports"))

  :general
  (:keymaps 'go-mode-map :states 'normal
            "gd" 'godef-jump
            "SPC d" 'godoc-at-point)
  
  :config
  (add-hook 'go-mode-hook
            (lambda ()
              ;; run gofmt vefore saving file
              (add-hook 'before-save-hook 'gofmt-before-save)
              (setq tab-width 4
                    evil-shift-width 4
                    indent-tabs-mode 1)))

  (use-package go-eldoc
    :config
    (add-hook 'go-mode-hook 'go-eldoc-setup)))
;;; go.el ends here
