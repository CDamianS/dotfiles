(use-package package
  :config (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))

(use-package icomplete
  :init (fido-vertical-mode)
  :bind (:map icomplete-minibuffer-map ("TAB" . 'icomplete-force-complete))
  :hook (icomplete-minibuffer-setup . (lambda () (setq truncate-lines t))))
(use-package savehist :init (savehist-mode))
(use-package marginalia :ensure t :init (marginalia-mode))

(electric-pair-mode 1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(pixel-scroll-precision-mode 1)
(pixel-scroll-mode 1)

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

(setq-default indent-tabs-mode nil
              tab-width 4
              truncate-lines t)

(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'dired-mode-hook 'dired-hide-details-mode)

(add-to-list 'default-frame-alist '(font . "JetBrainsMono Nerd Font 14"))
(add-to-list 'default-frame-alist '(alpha-background . 90))
(set-frame-parameter nil 'alpha-background 90)
(fset 'yes-or-no-p 'y-or-n-p)
(defun on-after-init ()
  (unless (display-graphic-p (selected-frame))
    (set-face-background 'default "unspecified-bg" (selected-frame))))

(add-hook 'window-setup-hook 'on-after-init)

(use-package avy 
    :ensure t 
    :config (avy-setup-default) 
    :bind ("M-j" . 'avy-goto-char-timer))

(use-package apheleia 
    :ensure t 
    :init (apheleia-global-mode))

(use-package catppuccin-theme 
    :ensure t 
    :init (load-theme 'catppuccin :noconfirm))

(use-package corfu 
    :ensure t 
    :custom (corfu-auto t) 
    :init (global-corfu-mode))

(use-package doom-modeline 
    :ensure t 
    :init (doom-modeline-mode 1))

(use-package eat 
    :ensure t 
    :config (advice-add #'project-shell :override #'eat-project))

(use-package eglot-booster 
    :after eglot 
    :config (eglot-booster-mode))

(use-package enlight
  :ensure t
  :config (setopt initial-buffer-choice #'enlight)
  :custom (enlight-content
           (concat
            (propertize "Welcome to emacs!" 'face 'highlight)
            "\n"
            (enlight-menu
             '(("Actions"
                ("Find file" find-file "f"))
               ("Projects"
	            ("Open projects" project-switch-project "p"))
               ("Open config files"
	            ("Sway config" (find-file "~/dots/.config/sway/README.org") "s")
                ("Emacs config" (find-file "~/dots/.config/emacs/README.org") "e"))))
            "\n"
            (propertize "Esperanto word of the Day" 'face 'highlight)
            "\n"
            (with-temp-buffer
              (insert-file-contents "~/.local/share/esperanto")
              (let ((lines (split-string (buffer-string) "\n" t)))
                (nth (random (length lines)) lines))))))

(use-package exec-path-from-shell
  :ensure t
  :init (exec-path-from-shell-initialize)
  :custom (exec-path-from-shell-variables '("ROSWELL_HOME" "GOPATH" "WORKON_HOME" "PATH")))

(use-package pyvenv :ensure t)
(use-package jtsx :ensure t)
(use-package web-mode :ensure t)
(use-package php-mode :ensure t)
(use-package lua-mode :ensure t)
(use-package raku-mode :ensure t)
(use-package emmet-mode 
    :ensure t 
    :hook web-mode tsx-ts-mode js-mode html-mode)

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

(use-package forge 
    :ensure t 
    :after magit 
    :custom (auth-sources '("~/.local/share/authinfo")))

(use-package magit 
    :ensure t 
    :config (advice-add #'project-vc-dir :override #'magit)
    :custom epg-pinentry-mode 'loopback)

(use-package nerd-icons-completion 
    :ensure t 
    :init (nerd-icons-completion-mode))

(use-package nerd-icons-dired 
    :ensure t 
    :hook (dired-mode . nerd-icons-dired-mode))

(use-package org-auto-tangle
  :ensure t
  :defer t
  :hook (org-mode . org-auto-tangle-mode))

(use-package toc-org
  :ensure t
  :commands toc-org-enable
  :hook (org-mode . toc-org-enable))

(use-package org-modern
  :ensure t
  :init (global-org-modern-mode)
  :custom (org-modern-star 'replace))

(use-package rainbow-delimiters 
    :ensure t 
    :hook prog-mode org-mode)

(use-package sly 
    :ensure t 
    :custom (inferior-lisp-program "ros -Q run"))

(use-package sudo-edit :ensure t)

(use-package treesit-auto 
    :ensure t
    :custom (treesit-auto-install 'prompt) 
    :config (treesit-auto-add-to-auto-mode-alist 'all) (global-treesit-auto-mode))

(use-package yasnippet :ensure t)
(use-package yasnippet-snippets 
    :ensure t 
    :after yasnippet 
    :config (yas-global-mode t))
