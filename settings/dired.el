;;; package --- dired

;;; Commentary:
;;; dired package configuration.

;;; Code:
(use-package dired
  :ensure nil
  :bind (:map dired-mode-map ("SPC" . nil))

  :init
  (add-hook 'dired-mode-hook 'vinegar/dired-setup)

  (use-package dired+
    :defer t
    :init
    (setq diredp-hide-details-initially-flag nil
          diredp-hide-details-propagate-flag nil
          font-lock-maximum-decoration (quote ((dired-mode . 1) (t . t))))
    (when (fboundp 'toggle-diredp-find-file-reuse-dir) (toggle-diredp-find-file-reuse-dir 1)))

  :config
  (general-define-key
   :states '(normal emacs)
   :keymaps 'dired-mode-map
   "-"         'vinegar/up-directory
   "0"         'dired-back-to-start-of-files
   "="         'vinegar/dired-diff
   "I"         'vinegar/dotfiles-toggle
   "~"         '(lambda ()(interactive) (find-alternate-file "~/"))
   "RET"       'dired-find-alternate-file
   "J"         'dired-goto-file
   "C-f"       'find-name-dired
   "C-r"       'dired-do-redisplay
   "G"         'vinegar/jump-to-bottom
   "j"         'vinegar/move-down
   "k"         'vinegar/move-up
   "gg"        'vinegar/back-to-top)

  (use-package diff-hl
    :defer t
    :init
    (add-hook 'dired-mode-hook 'diff-hl-dired-mode)))

(defvar-local dired-dotfiles-show-p nil)
(defun vinegar/dired-setup ()
  "Setup custom dired settings for vinegar."
  (setq dired-omit-verbose nil)
  (make-local-variable 'dired-hide-symlink-targets)
  (setq dired-hide-details-hide-symlink-targets nil)
  (when (fboundp 'dired-omit-mode) (dired-omit-mode t)))

(defun vinegar/dotfiles-toggle ()
  "Show/hide dot-files."
  (interactive)
  (when (equal major-mode 'dired-mode)
    (if (or (not (boundp 'dired-dotfiles-show-p)) dired-dotfiles-show-p) ; if currently showing
        (progn
          (set (make-local-variable 'dired-dotfiles-show-p) nil)
          (message "h")
          (dired-mark-files-regexp "^/\\.")
          (dired-do-kill-lines))
      (progn (revert-buffer) ; otherwise just revert to re-show
             (set (make-local-variable 'dired-dotfiles-show-p) t)))))

(defun vinegar/back-to-top ()
  "Move to first file."
  (interactive)
  (goto-char (point-min))
  (dired-next-line 1))

(defun vinegar/jump-to-bottom ()
  "Move to last file."
  (interactive)
  (goto-char (point-max))
  (dired-next-line -1))

(defun vinegar/move-up ()
  "Move to previous file."
  (interactive)
  (dired-previous-line 1)
  (if (bobp)
      (dired-next-line 1)))

(defun vinegar/move-down ()
  "Move to next file."
  (interactive)
  (dired-next-line 1)
  (if (eobp)
      (dired-next-line -1)))

(defun vinegar/up-directory (&optional other-window)
  "Run Dired on parent directory of current directory.
Optionally if OTHER-WINDOW is set, runs in another window."
  (interactive "P")
  (let* ((dir (dired-current-directory))
         (orig (current-buffer))
         (up (file-name-directory (directory-file-name dir))))
    (or (dired-goto-file (directory-file-name dir))
        ;; Only try dired-goto-subdir if buffer has more than one dir.
        (and (cdr dired-subdir-alist)
             (dired-goto-subdir up))
        (progn
          (kill-buffer orig)
          (dired up)
          (dired-goto-file dir)))))

(defun vinegar/dired-diff ()
  "Ediff marked files in dired or selected files in separate window."
  (interactive)
  (let* ((marked-files (dired-get-marked-files nil nil))
         (other-win (get-window-with-predicate
                     (lambda (window)
                       (with-current-buffer (window-buffer window)
                         (and (not (eq window (selected-window)))
                              (eq major-mode 'dired-mode))))))
         (other-marked-files (and other-win
                                  (with-current-buffer (window-buffer other-win)
                                    (dired-get-marked-files nil)))))
    (cond ((= (length marked-files) 2)
           (ediff-files (nth 0 marked-files)
                        (nth 1 marked-files)))
          ((= (length marked-files) 3)
           (ediff-files3 (nth 0 marked-files)
                         (nth 1 marked-files)
                         (nth 2 marked-files)
                         ))
          ((and (= (length marked-files) 1)
                (= (length other-marked-files) 1))
           (ediff-files (nth 0 marked-files)
                        (nth 0 other-marked-files)))
          ((= (length marked-files) 1)
           (dired-diff (car marked-files)))
          (t (error "Mark exactly 2 files, at least 1 locally")))))
;;; dired.el ends here
