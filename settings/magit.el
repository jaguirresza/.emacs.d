;;; package --- magit

;;; Commentary:
;;; magit configuration.

;;; Code:
(use-package magit
  :general (:keymaps 'evil-normal-state-map
            :prefix "SPC"
            "g" '(:ignore t :which-key "Magit")
            "g b" '(magit-blame :which-key "Blame")
            "g l" '(magit-log-current :which-key "Log")
            "g L" '(magit-log-buffer-file :which-key "Log [for file]")
            "g s" '(magit-status :which-key "Status"))

  :config (use-package evil-magit)
  (add-hook 'magit-blame-mode-hook 'evil-normalize-keymaps)
  )
;;; magit.el ends here
