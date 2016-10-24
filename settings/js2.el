;;; package --- js2

;;; Commentary:
;;; js2 configuration.

;;; Code:
;; (defun setup-js2-jsx-mode ()
;;   "Setup hook for js2-jsx-mode."
;;   (when (fboundp 'flycheck-select-checker)
;;     (flycheck-select-checker 'javascript-eslint)))


(use-package js2-mode
  :mode ("\\.jsx?\\'" . js2-jsx-mode)
  :interpreter ("node" . js2-jsx-mode)
  ;; :init
  ;; (add-hook 'js2-mode-hook #'setup-js2-jsx-mode)

  :config
  (setq js2-mode-show-parse-errors nil
        js2-mode-show-strict-warnings nil
        js2-highlight-level 3
        js2-basic-offset 2
        js-indent-level 2
        ))
;;; js2.el ends here
