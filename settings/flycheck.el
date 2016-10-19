;;; package --- flycheck

;;; Commentary:
;;; flycheck package configuration.

;;; Code:
(defun flycheck-list-errors-only-when-errors ()
  "Display error list when there is at least one error."
  (if (and (boundp 'flycheck-current-errors) flycheck-current-errors)
      (when (fboundp 'flycheck-list-errors) (flycheck-list-errors))
    (when (and (boundp 'flycheck-error-list-buffer) flycheck-error-list-buffer)
      (dolist (window (get-buffer-window-list flycheck-error-list-buffer))
        (quit-window nil window)))))

(use-package flycheck
  :init
  (add-hook 'after-init-hook #'global-flycheck-mode)
  (add-hook 'before-save-hook #'flycheck-list-errors-only-when-errors)

  :config
  (general-define-key
   :states 'normal
   :prefix "SPC"
   "n" '(:ignore t :which-key "Checker")
   "n l" '(flycheck-list-errors :which-key "List errors"))

  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers
                        '(go-errcheck)))
  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers
                        '(javascript-jshint)))

  (setq-default flycheck-temp-prefix ".flycheck")
  (when (fboundp 'flycheck-add-mode)
    (flycheck-add-mode 'javascript-eslint 'js2-jsx-mode))

  (use-package flycheck-flow
    :config
    (when (fboundp 'flycheck-add-mode)
      (flycheck-add-mode 'javascript-flow 'js2-jsx-mode))))
;;; flycheck.el ends here
