(org-babel-load-file
 (expand-file-name
  "config.org"
  user-emacs-directory))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(sudo-edit sqlite-mode-extras rainbow-delimiters rainbow-mode toc-org org-auto-tangle doom-modeline ligature treesit-auto emmet-mode php-mode lua-mode web-mode jtsx sly exec-path-from-shell corfu avy apheleia all-the-icons-dired all-the-icons catppuccin-theme which-key pyvenv pdf-tools marginalia magit)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(match ((t (:background "#313244" :foreground "#cdd6f4"))))
 '(org-document-title ((t (:inherit default :font "Arimo Nerd Font" :weight bold :height 1.5))))
 '(org-level-1 ((t (:inherit outline-1 :font "Arimo Nerd Font" :weight bold :height 1.5))))
 '(org-level-2 ((t (:inherit outline-2 :font "Arimo Nerd Font" :weight bold :height 1.4))))
 '(org-level-3 ((t (:inherit outline-3 :font "Arimo Nerd Font" :weight bold :height 1.3))))
 '(org-level-4 ((t (:inherit outline-4 :font "Arimo Nerd Font" :weight bold :height 1.2))))
 '(org-level-5 ((t (:inherit outline-5 :font "Arimo Nerd Font" :weight bold :height 1.1))))
 '(org-level-6 ((t (:inherit outline-6 :font "Arimo Nerd Font" :weight bold :height 1.1))))
 '(org-level-7 ((t (:inherit outline-7 :font "Arimo Nerd Font" :weight bold :height 1.1)))))
