;;; package --- flycheck

;;; Commentary:
;;; flycheck package configuration.

;;; Code:

;; (defun flycheck-list-errors-only-when-errors ()
;;   "Display error list when there is at least one error.
;; This is intended to use as a `flycheck-after-syntax-check-hook'."
;;   (if (and (boundp 'flycheck-current-errors) flycheck-current-errors)
;;       (when (fboundp 'flycheck-list-errors) (flycheck-list-errors))
;;     (when (and (boundp 'flycheck-error-list-buffer) flycheck-error-list-buffer)
;;       (dolist (window (get-buffer-window-list flycheck-error-list-buffer))
;;         (quit-window t window)))))

;; (defcustom flycheck-error-list-window-height 8
;;   "Flycheck error list window height."
;;   :group 'flycheck)

;; (defadvice flycheck-error-list-refresh (around shrink-error-list activate)
;;   "Shrink error list window to a fixed size."
;;   ad-do-it
;;   (when (and (boundp 'flycheck-current-errors) flycheck-current-errors)
;;     (-when-let (window (flycheck-get-error-list-window t))
;;       (fit-window-to-buffer window flycheck-error-list-window-height))))

(defun use-eslint-from-node-modules ()
  "Try to use eslint executable in project dependecies."
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/eslint/bin/eslint.js"
                                        root))))
    (when (and eslint (file-executable-p eslint))
      (when (and (boundp 'flycheck-disabled-checkers) (boundp 'flycheck-javascript-eslint-executable))
        (setq-local flycheck-disabled-checkers (delete 'javascript-eslint flycheck-disabled-checkers))
        (setq-local flycheck-javascript-eslint-executable eslint)))))

(use-package flycheck
  :diminish flycheck-mode
  :init
  (add-hook 'after-init-hook #'global-flycheck-mode)
  (add-hook 'flycheck-mode-hook #'use-eslint-from-node-modules)

  :config
  (general-define-key
   :states 'normal
   :prefix "SPC"
   "n" '(:ignore t :which-key "Checker")
   "n l" '(flycheck-list-errors :which-key "List errors"))

  (setq-default flycheck-temp-prefix ".flycheck"
                ;; flycheck-after-syntax-check-hook 'flycheck-list-errors-only-when-errors
                flycheck-disabled-checkers '(javascript-jshint go-errcheck))

  (when (fboundp 'flycheck-add-mode)
    (flycheck-add-mode 'javascript-eslint 'js2-jsx-mode))

  (use-package flycheck-pos-tip
    :config  (flycheck-pos-tip-mode))

  (use-package flycheck-flow
    :config
    (when (fboundp 'flycheck-add-next-checker)
      (flycheck-add-next-checker 'javascript-flow 'javascript-eslint))
    (when (fboundp 'flycheck-add-mode)
      (flycheck-add-mode 'javascript-flow 'js2-jsx-mode))))
;;; flycheck.el ends here
