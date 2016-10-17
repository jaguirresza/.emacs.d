;;; package --- general

;;; Commentary:
;;; General/Globals keybindings.

;;; Code:
(defvar-local emacs-init-file "~/.emacs.d/init.el")

(general-define-key
 "s-=" 'text-scale-increase
 "s--" 'text-scale-decrease
 [(super return)] 'toggle-frame-fullscreen
 )

(general-define-key
 :prefix "SPC"
 "vi" (lambda () (interactive) (find-file emacs-init-file))
 "so" (lambda () (interactive) (save-buffer)
        (save-window-excursion (find-file emacs-init-file) (eval-buffer))))

(general-define-key :prefix "SPC"
                    "e" '(counsel-find-file :wich-key "Find file")
                    "s s" '(swiper :which-key "Swiper")
                    "s a" '(swiper-all :which-key "Swiper all")

                    "SPC" '(evil-switch-to-windows-last-buffer :which-key "Switch to last opened buffer")
                    "tl" 'my-linum-relative-toggle
                    "sl" 'linum-mode
                    "b" 'ivy-switch-buffer
                    "k" 'kill-buffer)

(general-define-key :prefix "SPC"
                    "h" '(:ignore t :which-key "Help")
                    "h a" '(describe-face :which-key "Describe face")
                    "h b" '(counsel-descbinds :which-key "Search bindings")
                    "h f" '(counsel-describe-function :which-key "Describe function")
                    "h k" '(describe-key :which-key "Describe key")
                    "h m" '(describe-mode :which-key "Describe mode")
                    "h p" '(describe-package :which-key "Describe package")
                    "h v" '(counsel-describe-variable :which-key "Describe variable"))
;;; general.el ends here
