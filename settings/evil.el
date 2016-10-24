;;; package --- evil

;;; Commentary:
;;; evil-mode keybindings and configuration.

;;; Code:
(use-package evil
  :diminish evil-mode
  :init
  (setq-default evil-ex-search-vim-style-regexp t
                evil-search-module 'evil-search
                evil-want-C-u-scroll t
                evil-shift-width 2
                evil-auto-balance-windows t
                evil-want-Y-yank-to-eol nil)
  (evil-mode t)

  :config
  (general-define-key
   :keymaps 'evil-normal-state-map
            "-" 'dired-jump)

  (general-define-key
   :keymaps '(evil-normal-state-map evil-motion-state-map)
   "gt" 'other-frame
   "C-h" 'evil-window-left
   "C-j" 'evil-window-down
   "C-l" 'evil-window-right
   "C-k" 'evil-window-up)

  (when (fboundp 'evil-ex-define-cmd)
    (evil-ex-define-cmd "W" "write")
    (evil-ex-define-cmd "Q" "quit"))

  (use-package evil-commentary
    :diminish evil-commentary-mode
    :init (evil-commentary-mode))

  (use-package evil-iedit-state
    :commands evil-iedit-state/iedit-mode)

  (use-package evil-matchit
    :init (global-evil-matchit-mode 1))

  (use-package evil-numbers
    :commands (evil-numbers/inc-at-pt evil-numbers/dec-at-t))

  (use-package evil-surround
    :init (global-evil-surround-mode 1)))
;;; evil.el ends here
