;;; package --- Emacs configuration
;;; Commentary:
;;; Rainer Poisel Emacs configuration
;;; Code:

(set-frame-font "DejaVu Sans Mono-10" nil t)

(setq inhibit-startup-screen t)

(tool-bar-mode -1)

(menu-bar-mode -1)

(scroll-bar-mode -1)

(setq make-backup-files nil)

(setq ring-bell-function 'ignore)

(defun rpo-load-file-if-exists (filename)
  "Load a file FILENAME from the user's Emacs directory if it exists."
  (let ((package-file (expand-file-name filename user-emacs-directory)))
    (when (file-exists-p package-file)
      (load package-file))))

(defun rpo-load-file-from-emacs-directory (filename)
  "Load a file FILENAME from the user's Emacs directory."
  (let ((package-file (expand-file-name filename user-emacs-directory)))
    (load package-file)))

;; Configure packages
(rpo-load-file-from-emacs-directory "package.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(evil-undo-system 'undo-tree)
 '(ispell-dictionary nil)
 '(org-agenda-files (directory-files-recursively "~/Sync/org" "[a-zA-z]+.org$"))
 '(org-directory "~/Sync/org")
 '(org-trello-current-prefix-keybinding "C-c o")
 '(package-selected-packages
   '(emacsql-sqlite3 emacsql-sqlite org-noter-pdftools org-pdftools org-re-reveal shell-pop consult-flycheck dash rustic ox-hugo ox-reveal org-tree-slide clang-format crux yasnippet marginalia capf-autosuggest multi-vterm visual-fill-column consult dired-subtree dired-collapse lsp-pyright git-timemachine php-mode lsp-ui projectile treemacs-evil evil-visualstar evil-surround undo-tree rgi tree-sitter zenburn-theme vertico use-package orderless evil)))

;;; init.el ends here
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
