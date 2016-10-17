;; (use-package helm
;;   :init
;;   (helm-mode)

;;   :general
;;   (:keymaps 'helm-map
;;             "C-n" nil
;;             "C-p" nil
;;             "C-j" 'helm-next-line
;;             "C-k" 'helm-previous-line
;;             "C-d" (lambda () (interactive) (helm-next-line 5))
;;             "C-u" (lambda () (interactive) (helm-previous-line 5))))
