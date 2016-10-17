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
                         "C-p" nil
                         "C-p f" 'counsel-projectile-find-file
                         "C-p C-f" 'counsel-projectile-find-file
                         "C-p d" 'counsel-projectile-find-dir
                         "C-p C-d" 'counsel-projectile-find-dir
                         "C-p b" 'counsel-projectile-switch-to-buffer
                         "C-p C-b" 'counsel-projectile-switch-to-buffer
                         "C-p s" 'counsel-projectile-ag
                         "C-p C-s" 'counsel-projectile-ag
                         "C-p p" 'counsel-projectile-switch-project
                         "C-p C-p" 'counsel-projectile-switch-project)))

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
