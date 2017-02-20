;;; custom-modeline.el --- fancy statusline

;;; Commentary:

;; This package simply provides a minor mode for fancifying the status line.

;;; Code:
(defvar-local custom-modeline-text-height 0.8)
(defun custom-modeline-modified ()
  (let* ((config-alist
          '(
            ("*" all-the-icons-octicon-family all-the-icons-octicon "diff-added" :height 1 :v-adjust -0.1)
            ("-" all-the-icons-octicon-family all-the-icons-octicon "rocket" :height 1 :v-adjust -0.1)
            ("%" all-the-icons-octicon-family all-the-icons-octicon "lock" :height 1 :v-adjust -0.1)))
         (result (cdr (assoc (format-mode-line "%*") config-alist))))
    (concat
     (unless (equal (format-mode-line "%*") "-")
       (propertize (format "  %s" (apply (cadr result) (cddr result)))
                   'face `(:family ,(funcall (car result)))
                   'display '(raise 0.1)))
     (propertize (format " %s" (format-mode-line "%b"))
                 'help-echo "Find file"
                 'display '(raise 0.1)
                 'local-map (make-mode-line-mouse-map
                             'mouse-1 (lambda () (interactive) (counsel-find-file)))))))

(defun custom-modeline-project-id (&rest args)
  (if (and (fboundp 'projectile-project-name)
           (projectile-project-name))
      (concat
       (propertize (format "%s" (all-the-icons-octicon "repo" :height 0.8 :v-adjust 0.0))
                   'display '(raise 0.1))
       (propertize (format " %s" (projectile-project-name))
                  'help-echo "Switch Project"
                  'display '(raise 0.1)
                  'local-map (make-mode-line-mouse-map
                              'mouse-1 (lambda () (interactive) (counsel-projectile-switch-project)))))
    ""))

(defun custom-modeline-window-number ()
  (propertize (format " %c " (+ 9311 (window-numbering-get-number)))
              'display '(raise 0.1)))

(defun custom-modeline-mode-icon ()
  (let ((icon (all-the-icons-icon-for-buffer)))
    (unless (symbolp icon) ;; This implies it's the major mode
      (format "%s"
              (propertize icon
                          'help-echo (format "Major-mode: `%s`" major-mode)
                          'display '(raise 0.05))))))

(defun custom-modeline-region-info ()
  (when mark-active
    (let ((words (count-lines (region-beginning) (region-end)))
          (chars (count-words (region-end) (region-beginning))))
      (concat
       (propertize (format "   %s" (all-the-icons-octicon "pencil") words chars)
                   'face `(:family ,(all-the-icons-octicon-family))
                   'display '(raise -0.0))
       (propertize (format " (%s, %s)" words chars)
                   'face `(:height 0.9))))))

(defun -custom-modeline-github-vc ()
  (let ((branch (mapconcat 'concat (cdr (split-string vc-mode "[:-]")) "-")))
    (concat
     (propertize (format " %s" (all-the-icons-alltheicon "git" :height 1.0 :v-adjust 0.1))
                 'display '(raise 0.1))
     (propertize " • ")
     (propertize (format "%s" (all-the-icons-octicon "git-branch"))
                 'display '(raise 0.1))
     (propertize (format " %s  " branch)
                 'display '(raise 0.1)))))

(defun custom-modeline-icon-vc ()
  (when vc-mode
    (cond
      ((string-match "Git[:-]" vc-mode) (-custom-modeline-github-vc))
      (t (format "%s" vc-mode)))))

(defun custom-modeline-flycheck-status ()
  (defun cm-status (icon color))

  (let* ((text (pcase flycheck-last-status-change
                (`finished (if flycheck-current-errors
                               (let ((error-count (let-alist (flycheck-count-errors flycheck-current-errors)
                                              (+ (or .error 0))))
                                     (warning-count (let-alist (flycheck-count-errors flycheck-current-errors)
                                              (+ (or .warning 0)))))
                                 (concat
                                   (propertize (all-the-icons-octicon "x")
                                               'face `(:family ,(all-the-icons-octicon-family) :height 0.9))
                                   (propertize (format " %s" error-count)
                                               'face '(:height 0.9))
                                   " "
                                   (propertize (all-the-icons-octicon "alert")
                                               'face `(:family ,(all-the-icons-octicon-family) :height 0.9))
                                   (propertize (format " %s" warning-count)
                                               'face '(:height 0.9 ))))
                              (propertize (all-the-icons-octicon "check")
                                          'face `(:family ,(all-the-icons-octicon-family) :height 0.9 ))))
                (`running     (propertize (all-the-icons-octicon "sync")
                                          'face `(:family ,(all-the-icons-octicon-family) :height 0.9)))
                (`no-checker  (propertize (all-the-icons-octicon "circle-slash")
                                          'face `(:family ,(all-the-icons-octicon-family) :height 0.9)))
                (`not-checked (propertize (all-the-icons-octicon "circle-slash")
                                          'face `(:family ,(all-the-icons-octicon-family) :height 0.9 )))
                (`errored     (propertize (all-the-icons-octicon "issue-opened")
                                          'face `(:family ,(all-the-icons-octicon-family) :height 0.9 )))
                (`interrupted (propertize (all-the-icons-octicon "alert")
                                          'face `(:family ,(all-the-icons-octicon-family) :height 0.9 )))
                (`suspicious  ""))))
     (propertize (format " %s " text)
                 'help-echo "Show Flycheck Errors"
                 'mouse-face '(:box 1)
                 'display '(raise 0.2)
                 'local-map (make-mode-line-mouse-map
                             'mouse-1 (lambda () (interactive) (flycheck-list-errors))))))

(defvar powerline/upgrades nil)

(defun powerline/count-upgrades ()
  (let ((buf (current-buffer)))
    (package-list-packages-no-fetch)
    (with-current-buffer "*Packages*"
      (setq powerline/upgrades (length (package-menu--find-upgrades))))
    (switch-to-buffer buf)))
(advice-add 'package-menu-execute :after 'powerline/count-upgrades)

(defun custom-modeline-package-updates ()
  (let ((num (or powerline/upgrades (powerline/count-upgrades))))
    (when (> num 0)
      (propertize
        (concat
         (propertize (format "%s" (all-the-icons-octicon "package"))
                     'face `(:family ,(all-the-icons-octicon-family) :height 1)
                     'display '(raise -0.1))
         (propertize (format " %d updates " num)
                     'face `(:height 0.9)))
        'help-echo "Open Packages Menu"
        'mouse-face '(:box 2)
        'local-map (make-mode-line-mouse-map
                    'mouse-1 (lambda () (interactive) (package-list-packages)))))))


(defun custom-modeline-separator (sep)
    (propertize (format " %s " sep) )
  )

;; write a function to do the spacing
(defun custom-modeline-render (left right)
  "Return a string of `window-width' length containing LEFT, and RIGHT
 aligned respectively."
  (let* ((available-width (- (window-width) (length left) 2)))
    (format (format "%%s %%%ds" available-width) left right)))

(custom-set-faces
   '(mode-line ((t (:box (:line-width 1 :color "#eee")) :background "#fff" ))))
(use-package window-numbering
  :config
  (window-numbering-mode)
  (use-package all-the-icons
    :config
    (setq-default mode-line-format '((:eval
      (custom-modeline-render
       ;; left
       (concat
        (custom-modeline-window-number)
        (custom-modeline-modified)
        (custom-modeline-separator "|")
        (custom-modeline-project-id)
        (custom-modeline-separator "|"))
       ;; right
       (concat
        (custom-modeline-mode-icon)
        (custom-modeline-separator "|")
        (propertize (format "%s" (format-mode-line "%l:%c "))
                    'face '(:height 0.9)
                    'display '(raise 0.1))
        (custom-modeline-icon-vc)
        (custom-modeline-flycheck-status))
       ))))))


;; Local Variables:
;; indent-tabs-mode: nil
;; eval: (flycheck-mode 0)
;; End:
;;; custom-modeline.el ends here
