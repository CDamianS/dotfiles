(org-babel-load-file
 (expand-file-name
  "README.org"
  user-emacs-directory))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(match ((t (:background "#313244" :foreground "#cdd6f4"))))
 '(org-document-title ((t (:inherit outline-7 :font "Arimo Nerd Font" :height 1.5 :weight ultra-bold))))
 '(org-level-1 ((t (:inherit outline-1 :font "Arimo Nerd Font" :height 1.5 :weight ultra-bold))))
 '(org-level-2 ((t (:inherit outline-2 :font "Arimo Nerd Font" :height 1.4 :weight ultra-bold))))
 '(org-level-3 ((t (:inherit outline-3 :font "Arimo Nerd Font" :height 1.4 :weight ultra-bold))))
 '(org-level-4 ((t (:inherit outline-4 :font "Arimo Nerd Font" :height 1.3 :weight ultra-bold))))
 '(org-level-5 ((t (:inherit outline-5 :font "Arimo Nerd Font" :height 1.3 :weight ultra-bold))))
 '(org-level-6 ((t (:inherit outline-6 :font "Arimo Nerd Font" :height 1.2 :weight ultra-bold))))
 '(org-level-7 ((t (:inherit outline-7 :font "Arimo Nerd Font" :height 1.1 :weight ultra-bold)))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(apheleia beacon catppuccin-theme coffee-mode corfu doom-modeline
              dumb-jump eat eglot-booster elfeed-goodies elfeed-org
              emmet-mode enlight exec-path-from-shell forge ligature
              marginalia nerd-icons-completion nerd-icons-dired
              org-auto-tangle org-modern pyvenv rainbow-delimiters sly
              sudo-edit toc-org treesit-auto web-mode
              yasnippet-snippets))
 '(package-vc-selected-packages
   '((eglot-booster :url "https://github.com/jdtsmith/eglot-booster"))))
