;;; custom-modeline.el --- fancy statusline

;;; Commentary:

;; This package simply provides a minor mode for fancifying the status line.

;;; Code:
(defvar-local custom-modeline-text-height 0.8)
(defun custom-modeline-modified ()
  (let* ((config-alist
          '(
            ("*" all-the-icons-faicon-family all-the-icons-faicon "chain-broken" :height 1 :v-adjust -0.1)
            ("-" all-the-icons-faicon-family all-the-icons-faicon "link" :height 1 :v-adjust -0.1)
            ("%" all-the-icons-octicon-family all-the-icons-octicon "lock" :height 0.5 :v-adjust -0.1)))
         (result (cdr (assoc (format-mode-line "%*") config-alist))))
    (concat
     " "
     (propertize (apply (cadr result) (cddr result))
                 'face `(:family ,(funcall (car result))))
     (propertize " %b "
                 'help-echo "Find file"
                 'mouse-face '(:foreground "#6c3163")
                 'local-map (make-mode-line-mouse-map
                             'mouse-1 (lambda () (interactive) (counsel-find-file)))))))

(defun custom-modeline-project-id (&rest args)
  (if (and (fboundp 'projectile-project-name)
           (projectile-project-name))
      (format " | %s |"
              (propertize (format "%s" (concat (projectile-project-name) ))
                          'help-echo "Switch Project"
                          'mouse-face '(:foreground "#6c3163")
                          'local-map (make-mode-line-mouse-map
                                      'mouse-1 (lambda () (interactive) (counsel-projectile-switch-project)))))
    (propertize " | × |" 'face '(:height 0.8))))

(defun custom-modeline-window-number ()
  (propertize (format " %c" (+ 9311 (window-numbering-get-number)))
              'face `(:height ,(/ (* 0.90 custom-modeline-text-height) 100.0))
              'display '(raise 0.0)))

(defun custom-modeline-mode-icon ()
  (let ((icon (all-the-icons-icon-for-buffer)))
    (unless (symbolp icon) ;; This implies it's the major mode
      (format " %s"
              (propertize icon
                          'help-echo (format "Major-mode: `%s`" major-mode)
                          'face `(:height 1.1 :family ,(all-the-icons-icon-family-for-buffer)))))))

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
     (propertize "\ue0ba" 'face `(:foreground "#655370"))
     (propertize (format " %s" (all-the-icons-alltheicon "git"))
                 'face `(:foreground "#e4e4e4"
                                     :background "#655370"
                                     :height 1.0)
                 'display '(raise -0.1))
     (propertize " • "  'face '(:foreground "#e4e4e4" :background "#655370"))
     (propertize (format "%s" (all-the-icons-octicon "git-branch"))
                 'face `(:height 1.3 :foreground "#e4e4e4" :background "#655370" :family ,(all-the-icons-octicon-family))
                 'display '(raise -0.1))
     (propertize (format " %s " branch) 'face `(:foreground "#e4e4e4" :background "#655370" :height 0.9)))))

(defun -custom-modeline-svn-vc ()
  (let ((revision (cadr (split-string vc-mode "-"))))
    (concat
     (propertize (format " %s" (all-the-icons-faicon "cloud")) 'face `(:height 1.2) 'display '(raise -0.1))
     (propertize (format " · %s" revision) 'face `(:height 0.9)))))

(defun custom-modeline-icon-vc ()
  (when vc-mode
    (cond
      ((string-match "Git[:-]" vc-mode) (-custom-modeline-github-vc))
      ((string-match "SVN-" vc-mode) (-custom-modeline-svn-vc))
      (t (format "%s" vc-mode)))))

;; (defun custom-modeline-flycheck-status ()
;;   (let* ((text (pcase flycheck-last-status-change
;;                 (`finished (if flycheck-current-errors
;;                                (let ((count (let-alist (flycheck-count-errors flycheck-current-errors)
;;                                               (+ (or .warning 0) (or .error 0)))))
;;                                  (format "✖ %s Issue%s" count (unless (eq 1 count) "s")))
;;                               "✔ No Issues"))
;;                 (`running     "⟲ Running")
;;                 (`no-checker  "⚠ No Checker")
;;                 (`not-checked "✖ Disabled")
;;                 (`errored     "⚠ Error")
;;                 (`interrupted "⛔ Interrupted")
;;                 (`suspicious  ""))))
;;      (propertize text
;;                  'help-echo "Show Flycheck Errors"
;;                  'mouse-face '(:box 1)
;;                  'local-map (make-mode-line-mouse-map
;;                              'mouse-1 (lambda () (interactive) (flycheck-list-errors))))))

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
                     'face `(:family ,(all-the-icons-octicon-family) :height 1.2)
                     'display '(raise -0.1))
         (propertize (format " %d updates " num)
                     'face `(:height 0.9)))
        'help-echo "Open Packages Menu"
        'mouse-face '(:box 2)
        'local-map (make-mode-line-mouse-map
                    'mouse-1 (lambda () (interactive) (package-list-packages)))))))

;; write a function to do the spacing
(defun custom-mode-line-render (left right)
  "Return a string of `window-width' length containing LEFT, and RIGHT
 aligned respectively."
  (let* ((available-width (- (window-width) (string-bytes left) 1)))
    (format (format " %%s %%%ds " available-width) left right)))

(use-package window-numbering
  :config
  (window-numbering-mode)
  (use-package all-the-icons
    :config
    (setq mode-line-format '("%e" (:eval
      (custom-mode-line-render
       ;; left
       (concat
        (custom-modeline-window-number)
        " |"
        (custom-modeline-modified)
        (custom-modeline-project-id)
        (custom-modeline-mode-icon)
        (custom-modeline-region-info))
       ;; right
       (concat
        (custom-modeline-icon-vc))
       ))))))

;; (setq mode-line-format '("%e" (:eval
;;   (concat
;;     (custom-modeline-modified)
;;     (custom-modeline-window-number)
;;     (custom-modeline-mode-icon)
;;     (custom-modeline-icon-vc)
;;     (custom-modeline-region-info)
;;     (custom-modeline-flycheck-status)
;;     (custom-modeline-suntime)
;;     (custom-modeline-weather)
;;     (custom-modeline-time)))))

;; Local Variables:
;; indent-tabs-mode: nil
;; eval: (flycheck-mode 0)
;; End:
;;; custom-modeline.el ends here
