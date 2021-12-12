(set-frame-font "DejaVu Sans Mono-10" nil t)

(setq inhibit-startup-screen t)

(tool-bar-mode -1)

(menu-bar-mode -1)

(scroll-bar-mode -1)

(setq make-backup-files nil)

;; Configure packages
(setq package-file (expand-file-name "package.el" user-emacs-directory))
(when (file-exists-p package-file)
  (load package-file))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(evil-undo-system 'undo-tree)
 '(ispell-dictionary nil)
 '(package-selected-packages
   '(ox-reveal org-tree-slide clang-format crux yasnippet marginalia capf-autosuggest multi-vterm visual-fill-column consult dired-subtree dired-collapse lsp-pyright git-timemachine php-mode lsp-ui projectile treemacs-evil evil-visualstar evil-surround undo-tree rgi tree-sitter zenburn-theme vertico use-package orderless evil)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(vertico-current ((t (:background "#3a3f5a")))))
