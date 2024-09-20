;; Priorities
(use-package package
  :config (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))

(use-package icomplete
  :init (fido-vertical-mode)
  :bind (:map icomplete-minibuffer-map ("TAB" . 'icomplete-force-complete))
  :hook (icomplete-minibuffer-setup . (lambda () (setq truncate-lines t))))
(use-package savehist :init (savehist-mode))
(use-package marginalia :ensure t :init (marginalia-mode))

;; Modes
(electric-pair-mode 1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(pixel-scroll-precision-mode 1)
(pixel-scroll-mode 1)

;; Variables
(setq electric-pair-inhibit-predicate `(lambda (c) (if (char-equal c ?\<) t (,electric-pair-inhibit-predicate c)))
      create-lockfiles nil
      backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))
      scroll-step 1
      scroll-conservatively  10000
      scroll-margin 8
      indent-line-function 'insert-tab
      use-dialog-box nil
      org-src-preserve-indentation t
      initial-scratch-message ""
      split-width-threshold 0
      split-height-threshold nil)

;; Defaults
(setq-default indent-tabs-mode nil
              tab-width 4
              truncate-lines t)

;; Hooks
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'dired-mode-hook 'dired-hide-details-mode)

;; Extras
(add-to-list 'default-frame-alist '(font . "JetBrainsMono Nerd Font"))
(add-to-list 'default-frame-alist '(alpha-background . 90))
(set-frame-parameter nil 'alpha-background 90)
(fset 'yes-or-no-p 'y-or-n-p)

;; General packages
(use-package ligature
  :ensure t
  :config
  (ligature-set-ligatures 'prog-mode
                          '("--" "---" "==" "===" "!=" "!==" "=!=" "=:=" "=/=" "<=" ">=" "&&" "&&&" "&=" "++" "+++" "***" ";;" "!!"
                            "??" "???" "?:" "?." "?=" "<:" ":<" ":>" ">:" "<:<" "<>" "<<<" ">>>" "<<" ">>" "||" "-|" "_|_" "|-" "||-"
                            "|=" "||=" "##" "###" "####" "#{" "#[" "]#" "#(" "#?" "#_" "#_(" "#:" "#!" "#=" "^=" "<$>" "<$" "$>" "<+>"
                            "<+" "+>" "<*>" "<*" "*>" "</" "</>" "/>" "<!--" "<#--" "-->" "->" "->>" "<<-" "<-" "<=<" "=<<" "<<=" "<=="
                            "<=>" "<==>" "==>" "=>" "=>>" ">=>" ">>=" ">>-" ">-" "-<" "-<<" ">->" "<-<" "<-|" "<=|" "|=>" "|->" "<->"
                            "<~~" "<~" "<~>" "~~" "~~>" "~>" "~-" "-~" "~@" "[||]" "|]" "[|" "|}" "{|" "[<" ">]" "|>" "<|" "||>" "<||"
                            "|||>" "<|||" "<|>" "..." ".." ".=" "..<" ".?" "::" ":::" ":=" "::=" ":?" ":?>" "//" "///" "/*" "*/" "/="
                            "//=" "/==" "@_" "__" "???" "<:<" ";;;")) (global-ligature-mode t))

(use-package avy :ensure t :config (avy-setup-default) :bind ("M-j" . 'avy-goto-char-timer))
(use-package apheleia :ensure t :init (apheleia-global-mode +1))
(use-package catppuccin-theme :ensure t :init (load-theme 'catppuccin :noconfirm))
(use-package corfu :ensure t :custom (corfu-auto t) :init (global-corfu-mode))
(use-package doom-modeline :ensure t :init (doom-modeline-mode 1))
(use-package eat :ensure t :config (advice-add #'project-shell :override #'eat-project))
(use-package magit :ensure t :config (advice-add #'project-vc-dir :override #'magit))
(use-package nerd-icons-completion :ensure t :init (nerd-icons-completion-mode))
(use-package nerd-icons-dired :ensure t :hook (dired-mode . nerd-icons-dired-mode))
(use-package rainbow-delimiters :ensure t :hook prog-mode org-mode)
(use-package sudo-edit :ensure t)
(use-package treesit-auto :ensure t :config (global-treesit-auto-mode))
(use-package yasnippet :ensure t)
(use-package yasnippet-snippets :ensure t :after yasnippet :config (yas-global-mode t)
  :custom (yas-snippets-dirs '("~/.config/emacs/elpaca/builds/yasnippet-snippets/")))

;; Languages
(use-package pyvenv :ensure t)
(use-package jtsx :ensure t)
(use-package web-mode :ensure t)
(use-package lua-mode :ensure t)
(use-package php-mode :ensure t)
(use-package raku-mode :ensure t)
(use-package emmet-mode :ensure t :hook web-mode tsx-ts-mode js-mode html-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(ligature eat raku-mode nerd-icons-completion nerd-icons-dired avy doom-modeline emmet-mode php-mode lua-mode web-mode jtsx treesit-auto sudo-edit rainbow-delimiters corfu catppuccin-theme apheleia yasnippet-snippets which-key pyvenv pdf-tools marginalia magit)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(match ((t (:background "#313244" :foreground "#cdd6f4")))))
