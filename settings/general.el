;;; package --- general

;;; Commentary:
;;; General/Globals keybindings.

;;; Code:
(defvar-local emacs-init-file "~/.emacs.d/init.el")

(defun open-emacs-init-file ()
  "Open Emacs init configuration file."
  (interactive)
  (find-file emacs-init-file))

(defun reload-emacs-init-file ()
  "Evaluate Emacs init configuration file."
  (interactive)
  (save-window-excursion
    (save-buffer)
    (find-file emacs-init-file)
    (eval-buffer)))

(defun toggle-linum-relative ()
  "Toggle relative numbers."
  (interactive)
  (if (and (boundp 'linum-mode) linum-mode)
      (linum-relative-toggle)
    (progn
      (linum-mode)
      (linum-relative-toggle))))

(general-define-key
 "s-=" 'text-scale-increase
 "s--" 'text-scale-decrease
 )

(general-define-key
 :states 'normal
 :prefix "["
 "e" 'flycheck-previous-error
 )

(general-define-key
 :states 'normal
 :prefix "]"
 "e" 'flycheck-next-error
 )

(general-define-key
 :states 'normal
 :prefix "SPC"
 "SPC" 'evil-switch-to-windows-last-buffer
 "e" 'counsel-find-file
 "b" 'ivy-switch-buffer)

(general-define-key
 :states 'normal
 :prefix "SPC"
 "k" '(:ignore t :which-key "Kill")
 "k f" '(kill-buffer :which-key "Kill buffer")
 "k b" '(kill-this-buffer :which-key "Kill this buffer"))

(general-define-key
 :states 'normal
 :prefix "SPC"
 "c" '(:ignore t :which-key "Config")
 "c e" '(open-emacs-init-file :which-key "Edit init file")
 "c r" '(reload-emacs-init-file :which-key "Reload init file"))

(general-define-key
 :states 'normal
 :prefix "SPC"
 "s" '(:ignore t :which-key "Search")
 "s f" '(swiper :which-key "Search in buffer")
 "s a" '(swiper-all :which-key "Search in all buffers"))

(general-define-key
 :states 'normal
 :prefix "SPC"
 "t" '(:ignore t :which-key "Toggle")
 "t f" '(toggle-frame-fullscreen :which-key "Toggle fullscreen")
 "t n" '(linum-mode :which-key "Toggle show numbers")
 "t N" '(toggle-linum-relative :which-key "Toggle relative numbers"))

(general-define-key
 :states 'normal
 :prefix "SPC"
 "h" '(:ignore t :which-key "Help")
 "h a" '(describe-face :which-key "Describe face")
 "h b" '(counsel-descbinds :which-key "Search bindings")
 "h f" '(counsel-describe-function :which-key "Describe function")
 "h k" '(describe-key :which-key "Describe key")
 "h m" '(describe-mode :which-key "Describe mode")
 "h p" '(describe-package :which-key "Describe package")
 "h v" '(counsel-describe-variable :which-key "Describe variable"))
;;; general.el ends here
