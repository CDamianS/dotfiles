(org-babel-load-file
 (expand-file-name
  "README.org"
  user-emacs-directory))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(lsp-mode lsp-java leetcode php-mode gruvbox-theme highlight-indent-guides go-mode toc-org org-modern enlight treepy forge flycheck-inline flycheck eglot-booster ligature eat raku-mode nerd-icons-completion nerd-icons-dired avy doom-modeline emmet-mode lua-mode web-mode jtsx treesit-auto sudo-edit rainbow-delimiters corfu catppuccin-theme apheleia yasnippet-snippets which-key pyvenv pdf-tools marginalia magit))
 '(package-vc-selected-packages
   '((eglot-booster :vc-backend Git :url "https://github.com/jdtsmith/eglot-booster"))))
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
