(use-package js2-mode
  :mode ("\\.jsx?\\'" . js2-jsx-mode)
  :interpreter ("node" . js2-jsx-mode)
  :config
  (setq js2-mode-show-parse-errors nil
        js2-mode-show-strict-warnings nil
        js2-basic-offset 2
        ))
;;; js2.el ends here
