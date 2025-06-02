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
(require 'org-tempo)

(use-package apheleia 
    :ensure t 
    :init (apheleia-global-mode))

(use-package beacon
  :ensure t
  :init (beacon-mode 1))

(use-package catppuccin-theme 
    :ensure t 
    :init (load-theme 'catppuccin :noconfirm))

(use-package completion-preview
  :ensure nil
  :hook (prog-mode . completion-preview-mode)
  :bind ( :map completion-preview-active-mode-map
          ("M-n" . completion-preview-next-candidate)
          ("M-p" . completion-preview-prev-candidate)))

(use-package doom-modeline 
    :ensure t 
    :init (doom-modeline-mode 1))

(use-package dumb-jump
  :ensure t
  :hook (xref-backend-functions . dumb-jump-xref-activate)
  :custom (xref-show-definitions-function #'xref-show-definitions-completing-read))

(use-package eat 
  :ensure t
  :hook (eshell-load . eat-eshell-visual-command-mode)
  :config (advice-add #'project-shell :override #'eat-project))

(use-package eglot-booster
  :vc (:url "https://github.com/jdtsmith/eglot-booster")
  :after eglot
  :config (eglot-booster-mode))

(defun elfeed-watch-youtube ()
  "Play youtube video in elfeed entry"
  (interactive)
  (if (string= (buffer-name) "*elfeed-entry*")
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward "^Link: \\(.*\\)$" nil t)
        (let ((link (match-string 1)))
          (when (string-match "youtube\\.com" link)
            (emms-play-url link)) link)
      (message "No link found.")
      nil))
  (message "Not in elfeed!")))

(use-package elfeed
  :ensure t
  :custom
  (elfeed-db-directory "~/.local/share/elfeed")
  (elfeed-search-filter "@1-months-ago")
  (browse-url-browser-function 'eww-browse-url)
  :bind
  ("C-x w" . elfeed)
  (:map elfeed-show-mode-map
        ("C-c o" . elfeed-watch-youtube)))

(use-package elfeed-org
  :ensure t
  :custom (rmh-elfeed-org-files (list "~/.local/share/elfeed.org"))
  :init (elfeed-org))

(use-package elfeed-tube
  :ensure t
  :after elfeed
  :demand t
  :config
  (elfeed-tube-setup)
  :bind (:map elfeed-show-mode-map
         ("F" . elfeed-tube-fetch)
         ([remap save-buffer] . elfeed-tube-save)
         :map elfeed-search-mode-map
         ("F" . elfeed-tube-fetch)
         ([remap save-buffer] . elfeed-tube-save)))

(use-package emms
  :ensure t
  :init
  (require 'emms-setup)
  (emms-all)
  (emms-default-players)
  (require 'emms-player-mpd)
  :config
  (emms-player-set emms-player-mpv
                   'regex
                   (rx (or (: "https://" (* nonl) "youtube.com" (* nonl))
                           (+ (? (or "https://" "http://")) (* nonl)
                              (regexp (eval (emms-player-simple-regexp "mp4" "mov" "wmv" "webm" "flv" "avi" "mkv"))))))))

(use-package enlight
  :ensure t
  :config (setopt initial-buffer-choice #'enlight)
  :custom (enlight-content
           (concat
            (propertize "Welcome to emacs!" 'face 'highlight)
            "\n"
            (enlight-menu
             '(("Actions"
                ("Find file" find-file "f")
                ("Eshell" eshell "e")
	            ("Open projects" project-switch-project "p"))))
            "\n"
            (propertize "Esperanto word of the Day" 'face 'highlight)
            "\n"
            (with-temp-buffer
              (insert-file-contents "~/.local/share/esperanto")
              (let ((lines (split-string (buffer-string) "\n" t)))
                (nth (random (length lines)) lines))))))

;; Eshell has a bug so this should do it
(defun eshell-keys ()
  (keymap-local-set "C-p" 'eshell-previous-matching-input-from-input)
  (keymap-local-set "C-n" 'eshell-next-matching-input-from-input))

(defun shortened-path (path max-len)
  "Return a modified version of `path', replacing some components
  with single characters starting from the left to try and get
  the path down to `max-len'"
  (let* ((components (split-string (abbreviate-file-name path) "/"))
         (len (+ (1- (length components))
              (cl-reduce '+ components :key 'length)))
         (str ""))
    (while (and (> len max-len)
                (cdr components))
      (setq str (concat str (if (= 0 (length (car components)))
                                "/"
                              (string (elt (car components) 0) ?/)))
            len (- len (1- (length (car components))))
            components (cdr components)))
    (concat str (cl-reduce (lambda (a b) (concat a "/" b)) components))))

(use-package eshell
  :ensure nil
  :init (require 'eshell)
  :custom
  (eshell-banner-message "")
  (eshell-prompt-function
  (lambda nil (concat "λ " (shortened-path (eshell/pwd) 40) (if (= (user-uid) 0) " # " " $ "))))
  :hook (eshell-mode . eshell-keys))

(use-package eshell-syntax-highlighting
  :ensure t
  :init (eshell-syntax-highlighting-global-mode))

(use-package exec-path-from-shell
  :ensure t
  :init (exec-path-from-shell-initialize)
  :custom (exec-path-from-shell-variables '("ROSWELL_HOME" "GOPATH" "WORKON_HOME" "PATH")))

(use-package god-mode
  :ensure t
  :bind ("<escape>" . god-local-mode))

(use-package pyvenv :ensure t)
(use-package web-mode :ensure t)
(use-package emmet-mode 
    :ensure t 
    :hook tsx-ts-mode js-mode html-mode php-ts-mode)

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

(use-package mpc
  :ensure nil
  :bind (:map mpc-mode-map
   ("M-p" . windmove-up)
   ("M-n" . windmove-down)
   ("M-b" . windmove-left)
   ("M-f" . windmove-right)
   ("C-<return>" . mpc-play-at-point)
   ("<SPC>" . mpc-toggle-play)
   ("s" . mpc-toggle-shuffle)
   ("n" . next-line)
   ("p" . previous-line)
   ("f" . mpc-next)
   ("b" . mpc-prev)))

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

(defun watch-movie ()
  "Select a movie to play"
  (interactive)
  (let ((movie (completing-read "Movie: " (cddr (directory-files "~/mov")))))
    (emms-play-file (concat "~/mov/" movie))))
