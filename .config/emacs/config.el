(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(use-package vertico
  :ensure t
  :init (vertico-mode))

(use-package savehist
  :ensure nil
  :init
  (savehist-mode))

(use-package vertico-directory
  :after vertico
  :ensure nil
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :ensure t
  :init
  (marginalia-mode))

(use-package all-the-icons-completion
  :ensure t
  :init
  (all-the-icons-completion-mode)
  :hook
  (marginalia-mode . all-the-icons-completion-marginalia-setup))

(electric-pair-mode 1)
(setq electric-pair-inhibit-predicate
      `(lambda (c)
         (if (char-equal c ?\<) t (,electric-pair-inhibit-predicate c))))

(set-face-attribute 'font-lock-comment-face nil :slant 'italic)
(set-face-attribute 'font-lock-keyword-face nil :slant 'italic)

(add-to-list 'default-frame-alist '(font . "FiraCode Nerd Font"))
(setq-default line-spacing 0.12)

(use-package emojify
  :ensure t
  :config
  (global-emojify-mode))

(defun sm-greek-lambda ()
    (font-lock-add-keywords nil `(("\\<lambda\\>"
        (0 (progn (compose-region (match-beginning 0) (match-end 0)
        ,(make-char 'greek-iso8859-7 107)) nil))))))

(add-hook 'prog-mode-hook 'sm-greek-lambda)
(add-hook 'org-mode-hook 'sm-greek-lambda)

(setq create-lockfiles nil)

(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backups"))))

(use-package catppuccin-theme
  :ensure t
  :custom
  (catppuccin-flavor 'mocha)
  :init
  (load-theme 'catppuccin :noconfirm))

(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(pixel-scroll-precision-mode 1)
(pixel-scroll-mode 1)
(setq scroll-step            1
      scroll-conservatively  10000
      scroll-margin 8)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)

(set-frame-parameter nil 'alpha-background 90)
(add-to-list 'default-frame-alist '(alpha-background . 90))

(setq-default truncate-lines t)

(setq use-dialog-box nil)

(fset 'yes-or-no-p 'y-or-n-p)

(org-babel-do-load-languages
 'org-babel-load-languages '((C . t)
			     (java . t)
			     (python . t)))

(require 'org-tempo)
(electric-indent-mode 1)
(setq org-src-preserve-indentation t)
(setq org-startup-with-inline-images t)

(defun reload-init-file ()
  (interactive)
  (load-file user-init-file))

(use-package all-the-icons
  :ensure t
  :if (display-graphic-p))

(use-package all-the-icons-dired
  :ensure t
  :hook (dired-mode . (lambda () (all-the-icons-dired-mode t))))

(use-package apheleia
  :ensure t
  :init
  (apheleia-global-mode +1)
  :config
  (setf (alist-get 'php-mode apheleia-formatters) "prettier"))

(use-package avy
  :ensure t
  :config (avy-setup-default)
  :bind
  ("C-:" . 'avy-goto-char)
  ("M-j" . 'avy-goto-char-timer)
  ("C-'" . 'avy-goto-char-2)
  ("M-g f" . 'avy-goto-line)
  ("M-g w" . 'avy-goto-word-1)
  ("M-g e" . 'avy-goto-word-0)
  ("C-c C-j" . 'avy-resume))

(use-package corfu
  :ensure t
  :custom (corfu-auto t)
  :init (global-corfu-mode))

(use-package cheat-sh
  :ensure t
  :bind ("C-c s" . 'cheat-sh))

(setq initial-scratch-message "")

(use-package eat
  :ensure t
  :bind ("C-c o T" . 'eat)
  :config (advice-add #'project-shell :override #'eat-project))

(use-package ein
  :ensure t)

(use-package elfeed
  :ensure t
  :bind ("C-x w" . 'elfeed)
  :custom (elfeed-db-directory "~/.local/share/elfeed"))

(use-package elfeed-goodies
  :ensure t
  :init (elfeed-goodies/setup))

(use-package elfeed-org
  :ensure t
  :init (elfeed-org)
  :custom (rmh-elfeed-org-files (list "~/Agenda/elfeed.org")))

(use-package eshell
  :ensure nil
  :config
  (defalias 'g 'magit)
  (defalias 'd 'dired)
  (defalias 'f 'find-file)
  (defalias 'gl 'magit-log)
  (defalias 'clear 'eshell/clear-scrollback)
  (defalias 'l (lambda () (eshell/ls '-la))))

(setq eshell-banner-message "")
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

(setq eshell-prompt-function (lambda nil (concat "λ "
       (shortened-path (eshell/pwd) 40)
              (if (= (user-uid) 0) " # " " $ "))))

(use-package exec-path-from-shell
  :ensure t
  :init
  (exec-path-from-shell-initialize))

(use-package god-mode
  :ensure t
  :bind
  ("<escape>" . 'god-mode-all)
  ("C-x C-1" . 'delete-other-windows)
  ("C-x C-2" . 'split-window-below)
  ("C-x C-3" . 'split-window-right)
  ("C-x C-0" . 'delete-window)
  (:map god-local-mode-map
        ("z" . 'repeat)
        ("i" . 'god-local-mode)))

(use-package sly
  :ensure t
  :custom
  (inferior-lisp-program "ros -Q run")
  (split-width-threshold 0)
  (split-height-threshold nil))

(defun python-compile ()
  (setq-local compile-command
	      (concat "python3 "
                        (when buffer-file-name
                          (shell-quote-argument buffer-file-name)))))

(add-hook 'python-mode-hook 'python-compile)

(defun perl-compile ()
  (setq-local compile-command
              (concat "perl "
                      (when buffer-file-name
                        (shell-quote-argument buffer-file-name)))))

(add-hook 'perl-mode-hook 'perl-compile)

(use-package jtsx :ensure t)
(use-package web-mode :ensure t)
(use-package lua-mode :ensure t)
(use-package clojure-mode :ensure t)
(use-package impatient-mode :ensure t)
(use-package moonscript :ensure t)
(use-package php-mode :ensure t)
(use-package emmet-mode
  :ensure t
  :hook web-mode tsx-ts-mode js-mode)
(use-package treesit-auto
  :ensure t
  :config
  (global-treesit-auto-mode))

(use-package ligature
  :ensure t
  :config  
  (ligature-set-ligatures 'prog-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
                                       ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                                       "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                                       "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                                       "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                                       "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                                       "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                                       "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                                       ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                                       "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                                       "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                                       "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                                       "\\\\" "://"))
  :init
  (global-ligature-mode t))

(use-package magit
  :ensure t
  :custom (magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  :config (advice-add #'project-vc-dir :override #'magit))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

(setq org-agenda-files '("~/Agenda/Agenda.org" "~/Agenda/Roam/"))
(setq org-agenda-span 1
      org-agenda-window-setup 'only-window
      org-agenda-start-day "+0d"
      org-agenda-skip-timestamp-if-done t
      org-agenda-skip-deadline-if-done t
      org-agenda-skip-scheduled-if-done t
      org-agenda-skip-scheduled-if-deadline-is-shown t
      org-agenda-skip-timestamp-if-deadline-is-shown t)

(setq org-agenda-hide-tags-regexp ".*")
(setq org-agenda-current-time-string "")
(setq org-agenda-time-grid '((daily) (800 1200 1600 2000) "---" "-----"))

(use-package org-modern
  :ensure t
  :init (global-org-modern-mode)
  :custom (org-modern-star 'replace))

(use-package olivetti
  :ensure t
  :hook org-agenda-mode)

(use-package org-super-agenda
  :ensure t
  :init (org-super-agenda-mode t)
  :custom
  (org-super-agenda-groups
        '((:name " Dates" :tag "dates" :time-grid t :order 2 :face 'error)
          (:name " Tasks" :tag "tasks" :order 3 :face 'error)
          (:name " Events" :tag "events" :time-grid t :order 4 :face 'error)
          (:name " Assignment" :tag "assignement" :order 2 :face 'error)
          (:name " Today " :time-grid t :date today :scheduled today :order 1 :face 'warning))))

(use-package org-auto-tangle
  :ensure t
  :defer t
  :hook (org-mode . org-auto-tangle-mode))

(defun headers ()
  (custom-set-faces
   '(org-document-title ((t (:inherit default :font "Arimo Nerd Font" :weight bold :height 1.5))))
   '(org-level-1 ((t (:inherit outline-1 :font "Arimo Nerd Font" :weight bold :height 1.5))))
   '(org-level-2 ((t (:inherit outline-2 :font "Arimo Nerd Font" :weight bold :height 1.4))))
   '(org-level-3 ((t (:inherit outline-3 :font "Arimo Nerd Font" :weight bold :height 1.3))))
   '(org-level-4 ((t (:inherit outline-4 :font "Arimo Nerd Font" :weight bold :height 1.2))))
   '(org-level-5 ((t (:inherit outline-5 :font "Arimo Nerd Font" :weight bold :height 1.1))))
   '(org-level-6 ((t (:inherit outline-6 :font "Arimo Nerd Font" :weight bold :height 1.1))))
   '(org-level-7 ((t (:inherit outline-7 :font "Arimo Nerd Font" :weight bold :height 1.1))))))

(add-hook 'org-mode-hook 'headers)

(use-package toc-org
  :ensure t
  :commands toc-org-enable
  :hook (org-mode . toc-org-enable))

(use-package pdf-tools :ensure t)

(use-package pyvenv :ensure t)

(use-package rainbow-mode
  :ensure t
  :hook org-mode prog-mode)

(use-package rainbow-delimiters
  :ensure t
  :hook prog-mode org-mode)

(use-package restclient :ensure t)

(use-package sqlite-mode-extras
  :ensure t
  :bind (:map sqlite-mode-map
         ("n" . next-line)
         ("p" . previous-line)
         ("<backtab>" . sqlite-mode-extras-backtab-dwim)
         ("<tab>" . sqlite-mode-extras-tab-dwim)
         ("RET" . sqlite-mode-extras-ret-dwim)))

(use-package sudo-edit :ensure t)

(use-package yasnippet :ensure t)
(use-package yasnippet-snippets
  :after yasnippet
  :ensure t
  :config (yas-global-mode t)
  :custom (yas-snippets-dirs '("~/.config/emacs/elpaca/builds/yasnippet-snippets/")))

(use-package which-key
  :ensure t
  :init (which-key-mode))

(keymap-global-set "C-c c c" 'compile)
(keymap-global-set "C-c c r" 'recompile)

(keymap-global-set "C-c o c" '(lambda () (interactive) (find-file "~/.cfg/.config/emacs/config.org")))
(keymap-global-set "C-c o s" '(lambda () (interactive) (find-file "~/.cfg/.config/sway/README.org")))
(which-key-add-key-based-replacements
  "C-c o c" "Open Emacs Config"
  "C-c o s" "Open Sway Config")

(keymap-global-set "C-c f a" '(lambda () (interactive) (find-file "~/Agenda/Agenda.org")))
(keymap-global-set "C-c f e" '(lambda () (interactive) (find-file "~/Agenda/elfeed.org")))
(keymap-global-set "C-c a" 'org-agenda)
(which-key-add-key-based-replacements
  "C-c f a" "Open Agenda"
  "C-c f e" "Open Elfeed Feeds")

(keymap-global-set "C-c r r" 'reload-init-file)

(defun async-shell-command-no-window (command)
  (interactive)
  (let ((display-buffer-alist
        (list
         (cons
          "\\*Async Shell Command\\*.*"
          (cons #'display-buffer-no-window nil)))))
    (async-shell-command
     command)))

(defun lisp-script ()
  "Start a Roswell script"
  (interactive)
  (let ((script-name (read-string "Script name: ")))
    (shell-command (format "ros init %s" script-name))))

(setq video-player "swayhide mpv ")

(defun scream ()
  "Play the Scream movie"
  (interactive)
  (async-shell-command-no-window
   (concat video-player
	   (shell-quote-argument
	    (expand-file-name "~/Movies/Scream (1996)/")))))

(defun watch-movie ()
  "Select a movie to watch"
  (interactive)
  (let* ((movies (directory-files "~/Movies/"))
	 (movie (completing-read "Movie: " (cddr movies)))
	 (path (shell-quote-argument (expand-file-name (concat "~/Movies/" movie)))))
    (async-shell-command-no-window
     (concat video-player path))))
