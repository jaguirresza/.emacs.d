;;; package --- init

;;; Commentary:
;;; Init emcas configuration

;;; Code:
(package-initialize)

(setq inhibit-startup-message t)

(require 'package)

(defvar package-list)
(setq package-list '(
  better-defaults
  general
  linum-relative
  use-package))

(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(unless package-archive-contents (package-refresh-contents))

(defvar use-package-always-ensure)
(defvar use-package-verbose)
(setq use-package-always-ensure t
      use-package-verbose t)

(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(require 'use-package)
(require 'general)
(require 'uniquify)

(load-file "~/.emacs.d/settings.el")
(load-file "~/.emacs.d/interface.el")
(load-file "~/.emacs.d/platform.el")

(load "~/.emacs.d/local.el" 'noerror)
(load "~/.emacs.d/custom.el" 'noerror)

(setq user-full-name "Jorge Aguirre"
      user-mail-address "jorge.aguirre.sza@gmail.com")
;;; init.el ends here
