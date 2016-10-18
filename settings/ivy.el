;;; package --- ivy

;;; Commentary:
;;; Ivy configuration

;;; Code:
(use-package ivy
  :diminish ivy-mode
  :init
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  :general (:keymaps 'ivy-mode-map
                     "C-j" 'ivy-next-line
                     "C-k" 'ivy-previous-line
                     "C-d" (lambda () (interactive) (ivy-next-line 5))
                     "C-u" (lambda () (interactive) (ivy-previous-line 5)))
  :config
  (use-package counsel
    :config
    (use-package counsel-projectile
      :init (counsel-projectile-on)
      :general (:keymaps 'evil-normal-state-map
                :prefix "SPC"
                "p" '(:ignore t :which-key "Projectile")
                "p f" '(counsel-projectile-find-file :which-key "Find file")
                "p b" '(counsel-projectile-switch-to-buffer :which-key "Find buffer")
                "p d" '(counsel-projectile-find-dir :which-key "Find directory")
                "p s" '(counsel-projectile-ag :which-key "ag in project")
                "p p" '(counsel-projectile-switch-project :which-key "Switch project"))))

  (use-package flx)
  (use-package swiper)

  (setq ivy-extra-directories nil
        ivy-fixed-height-minibuffer t
        ivy-height 20

        ivy-ignore-buffers `("^\\*alchemist-server\\*"
                             "^\\*alchemist test report\\*"
                             "^\\*Compile-Log\\*"
                             "^\\*Completions\\*"
                             "^\\*Help\\*"
                             "^\\*Messages\\*"
                             "^\\*Warnings\\*"
                             "^\\*eshell"
                             "^\\*magit"
                             "^\\*scratch\\*"
                             "^\\*rspec-compilation\\*"
                             (lambda (name)
                               (save-excursion
                                 (equal major-mode 'dired-mode))))

        ivy-re-builders-alist '((t . ivy--regex-fuzzy))))
;;; ivy.el ends here
