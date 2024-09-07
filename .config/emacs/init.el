(org-babel-load-file
 (expand-file-name
  "config.org"
  user-emacs-directory))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("a690924a4483c8b8e42d62bc3f62d65d9e3cb40444de0f8c6ebfbca28baad579" default))
 '(package-selected-packages
   '(orderless vertico cheat-sh plz exec-path-from-shell gruvbox-theme raku-mode markdown-mode org-roam-ui org-roam org-super-agenda org-modern olivetti yasnippet-snippets which-key web-mode treesit-auto toggle-term toc-org sudo-edit sqlite-mode-extras sly restclient rainbow-mode rainbow-delimiters pyvenv php-mode pdf-tools org-bullets org-auto-tangle moonscript marginalia magit lua-mode ligature jtsx impatient-mode god-mode emojify emmet-mode elfeed-org elfeed-goodies ein eat doom-modeline dirvish dashboard corfu clojure-mode catppuccin-theme apheleia all-the-icons-dired all-the-icons-completion))
 '(package-vc-selected-packages
   '((one-tab-per-project :vc-backend Git :url "https://github.com/abougouffa/one-tab-per-project")
     (unique-dir-name :vc-backend Git :url "https://github.com/abougouffa/unique-dir-name"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(match ((t (:background "#313244" :foreground "#cdd6f4"))))
 '(org-document-title ((t (:inherit outline-7 :font "Arimo Nerd Font" :height 1.1))))
 '(org-level-1 ((t (:inherit outline-1 :font "Arimo Nerd Font" :height 1.7))))
 '(org-level-2 ((t (:inherit outline-2 :font "Arimo Nerd Font" :height 1.6))))
 '(org-level-3 ((t (:inherit outline-3 :font "Arimo Nerd Font" :height 1.5))))
 '(org-level-4 ((t (:inherit outline-4 :font "Arimo Nerd Font" :height 1.4))))
 '(org-level-5 ((t (:inherit outline-5 :font "Arimo Nerd Font" :height 1.3))))
 '(org-level-6 ((t (:inherit outline-6 :font "Arimo Nerd Font" :height 1.2))))
 '(org-level-7 ((t (:inherit outline-7 :font "Arimo Nerd Font" :height 1.1)))))
