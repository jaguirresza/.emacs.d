;;; package --- cpp

;;; Commentary:
;;; C++ keybindings and configuration.

;;; Code:
(use-package flycheck
  :config
  (add-hook 'c++-mode-hook '(lambda ()
            (setq-default flycheck-clang-language-standard "c++1y"))))

(add-hook 'c++-mode-hook
          '(lambda ()
             (add-hook 'after-save-hook 'delete-trailing-whitespace nil t)))
(use-package company
  :config
  (setq-default company-clang-arguments '("-std=c++1y"))
  (setq company-backends (delete 'company-semantic company-backends))

  (use-package company-irony
    :config
    (add-to-list 'company-backends 'company-irony)))
;;; cpp.el ends here
