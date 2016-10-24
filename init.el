;;; package --- init

;;; Commentary:
;;; Init Emacs configuration

;;; Code:
(package-initialize)

(require 'package)

(defvar package-list)
(setq-default package-list '(better-defaults
                             general
                             linum-relative
                             use-package))

(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(unless package-archive-contents
  (package-refresh-contents))

(require 'diminish)
(require 'autorevert)
(eval-after-load "autorevert" '(diminish 'auto-revert-mode))
(require 'undo-tree)
(eval-after-load "undo-tree" '(diminish 'undo-tree-mode))

;;; Bootstrap `package'
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq-default use-package-always-ensure t
              use-package-verbose t)

;;; Requires
(eval-when-compile
  (require 'use-package))
(require 'general)
(require 'uniquify)

(load-file "~/.emacs.d/settings.el")
(load-file "~/.emacs.d/interface.el")
(load-file "~/.emacs.d/platform.el")

(load "~/.emacs.d/local.el" 'noerror)
(load "~/.emacs.d/custom.el" 'noerror)
;;; init.el ends here
