(require 'package)

(setq use-package-hook-name-suffix nil)

(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(use-package exec-path-from-shell
  :ensure
  :config
  (when (display-graphic-p)
    (exec-path-from-shell-initialize)))

(use-package dired
  :init
  ;; (setq delete-by-moving-to-trash t)
  ;; (setq dired-dwim-target t)
  (setq dired-listing-switches "-AFhlv")
  (setq dired-ls-F-marks-symlinks t) ;; Required b/c of -F in `dired-listing-switches'.
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq dired-use-ls-dired nil)
  :config

  ;; See https://christiantietze.de/posts/2021/06/emacs-trash-file-macos.
  ;; Requires http://hasseg.org/trash, for example via `brew install trash`.
  ;; (defun system-move-file-to-trash (path)
  ;;   "Moves file at PATH to the macOS Trash according to `move-file-to-trash' convention."
  ;;   (shell-command (concat "trash -vF \"" path "\"") nil "*Trash Error Buffer*"))

  :hook ((dired-mode-hook . dired-hide-details-mode)
         (dired-mode-hook . hl-line-mode)))

(use-package dired-x
  :demand
  :after dired
  :init
  (setq dired-omit-verbose nil)
  :hook (dired-mode-hook . dired-omit-mode)
        (dired-mode-hook . dired-hide-details-mode))

(use-package dired-aux
  :after dired
  :config
  ;; (setq dired-isearch-filenames 'dwim) ;; I use [j]ump instead
  (setq dired-create-destination-dirs 'ask))

(use-package async
  :ensure)

(use-package dired-async
  :after (dired async) ;; provided by package async
  :hook (dired-mode-hook . dired-async-mode))

(use-package wdired
  :after dired
  :config
  (with-eval-after-load 'evil
    (add-hook 'wdired-mode-hook #'evil-normal-state))
  (dolist (fn '(wdired-finish-edit wdired-abort-changes wdired-exit))
    (advice-add fn :after (lambda () (evil-local-mode -1)))))

 (use-package dired-rainbow
   :ensure
   :defer 2
   :config
   (dired-rainbow-define-chmod directory "#6cb2eb" "d.*")
   (dired-rainbow-define html "#eb5286" ("css" "less" "sass" "scss" "htm" "html" "jhtm" "mht" "eml" "mustache" "xhtml"))
   (dired-rainbow-define xml "#f2d024" ("xml" "xsd" "xsl" "xslt" "wsdl" "bib" "json" "msg" "pgn" "rss" "yaml" "yml" "rdata"))
   (dired-rainbow-define document "#9561e2" ("docm" "doc" "docx" "odb" "odt" "pdb" "pdf" "ps" "rtf" "djvu" "epub" "odp" "ppt" "pptx"))
   (dired-rainbow-define markdown "#ffed4a" ("org" "etx" "info" "markdown" "md" "mkd" "nfo" "pod" "rst" "tex" "textfile" "txt"))
   (dired-rainbow-define database "#6574cd" ("xlsx" "xls" "csv" "accdb" "db" "mdb" "sqlite" "nc"))
   (dired-rainbow-define media "#de751f" ("mp3" "mp4" "mkv" "MP3" "MP4" "avi" "mpeg" "mpg" "flv" "ogg" "mov" "mid" "midi" "wav" "aiff" "flac"))
   (dired-rainbow-define image "#f66d9b" ("tiff" "tif" "cdr" "gif" "ico" "jpeg" "jpg" "png" "psd" "eps" "svg"))
   (dired-rainbow-define log "#c17d11" ("log"))
   (dired-rainbow-define shell "#f6993f" ("awk" "bash" "bat" "sed" "sh" "zsh" "vim"))
   (dired-rainbow-define interpreted "#38c172" ("py" "ipynb" "rb" "pl" "t" "msql" "mysql" "pgsql" "sql" "r" "clj" "cljs" "scala" "js"))
   (dired-rainbow-define compiled "#4dc0b5" ("asm" "cl" "lisp" "el" "c" "h" "c++" "h++" "hpp" "hxx" "m" "cc" "cs" "cp" "cpp" "go" "f" "for" "ftn" "f90" "f95" "f03" "f08" "s" "rs" "hi" "hs" "pyc" ".java"))
   (dired-rainbow-define executable "#8cc4ff" ("exe" "msi"))
   (dired-rainbow-define compressed "#51d88a" ("7z" "zip" "bz2" "tgz" "txz" "gz" "xz" "z" "Z" "jar" "war" "ear" "rar" "sar" "xpi" "apk" "xz" "tar"))
   (dired-rainbow-define packaged "#faad63" ("deb" "rpm" "apk" "jad" "jar" "cab" "pak" "pk3" "vdf" "vpk" "bsp"))
   (dired-rainbow-define encrypted "#ffed4a" ("gpg" "pgp" "asc" "bfe" "enc" "signature" "sig" "p12" "pem"))
   (dired-rainbow-define fonts "#6cb2eb" ("afm" "fon" "fnt" "pfb" "pfm" "ttf" "otf"))
   (dired-rainbow-define partition "#e3342f" ("dmg" "iso" "bin" "nrg" "qcow" "toast" "vcd" "vmdk" "bak"))
   (dired-rainbow-define vc "#0074d9" ("git" "gitignore" "gitattributes" "gitmodules"))
   (dired-rainbow-define-chmod executable-unix "#38c172" "-.*x.*"))

(use-package dired-subtree
  :ensure
  :config
  (bind-keys :map dired-mode-map
             ("i" . dired-subtree-insert)
             ("j" . dired-subtree-remove)))

(use-package evil
  :ensure
  :demand
  :init
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-w-delete t)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join))
(evil-set-initial-state 'dired-mode 'emacs)

(use-package undo-tree
  :ensure
  :config
  (with-eval-after-load 'evil
    (custom-set-variables '(evil-undo-system 'undo-tree))) ;; requires use of custom-set; setq doesn't work here
  (global-undo-tree-mode))

(use-package evil-surround
  :ensure
  :config
  (global-evil-surround-mode 1))

(use-package evil-visualstar
  :ensure
  :config (global-evil-visualstar-mode t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ediff                                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'ediff)
;; don't start another frame
;; this is done by default in preluse
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
;; put windows side by side
(setq ediff-split-window-function (quote split-window-horizontally))
;;revert windows on exit - needs winner mode
(winner-mode)
(add-hook 'ediff-after-quit-hook-internal 'winner-undo)

(defun dw/minibuffer-backward-kill (arg)
  "When minibuffer is completing a file name delete up to parent
folder, otherwise delete a word"
  (interactive "p")
  (if minibuffer-completing-file-name
      ;; Borrowed from https://github.com/raxod502/selectrum/issues/498#issuecomment-803283608
      (if (string-match-p "/." (minibuffer-contents))
          (zap-up-to-char (- arg) ?/)
        (delete-minibuffer-contents))
      (backward-kill-word arg)))

(use-package vertico
  ;; :straight '(vertico :host github
  ;;                     :repo "minad/vertico"
  ;;                     :branch "main")
  :ensure
  :bind (:map vertico-map
         ;; ("C-j" . vertico-next)
         ;; ("C-k" . vertico-previous)
         ("C-f" . vertico-exit)
         :map minibuffer-local-map
         ("M-h" . dw/minibuffer-backward-kill))
  :custom
  (vertico-cycle t)
  :custom-face
  (vertico-current ((t (:background "#3a3f5a"))))
  :init
  (vertico-mode))

(use-package orderless
  :ensure
  :init
  (setq completion-styles '(orderless))
  (setq completion-category-defaults nil))

;; Enable richer annotations using the Marginalia package
(use-package marginalia
  :ensure
  ;; Either bind `marginalia-cycle` globally or only in the minibuffer
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  ;; The :init configuration is always executed (Not lazy!)
  :init

  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode))

(use-package consult
  :ensure)

(use-package embark
  :ensure t

  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; (use-package zenburn-theme
;;   :ensure
;;   :config (load-theme 'zenburn t))
(use-package dracula-theme
  :ensure
  :config (load-theme 'dracula t))

;; (use-package tree-sitter
;;   :ensure)

;; (use-package tree-sitter-langs
;;   :ensure)

(setq company-idle-delay 0)
(setq company-minimum-prefix-length 1)
(use-package company
  :ensure)

(use-package corfu
  :ensure
  ;; Optional customizations
  ;; :custom
  ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  ;; (corfu-auto t)                 ;; Enable auto completion
  ;; (corfu-commit-predicate nil)   ;; Do not commit selected candidates on next input
  ;; (corfu-quit-at-boundary t)     ;; Automatically quit at word boundary
  ;; (corfu-quit-no-match t)        ;; Automatically quit if there is no match
  ;; (corfu-echo-documentation nil) ;; Do not show documentation in the echo area

  ;; Optionally use TAB for cycling, default is `corfu-complete'.
  ;; :bind (:map corfu-map
  ;;        ("TAB" . corfu-next)
  ;;        ([tab] . corfu-next)
  ;;        ("S-TAB" . corfu-previous)
  ;;        ([backtab] . corfu-previous))

  ;; You may want to enable Corfu only for certain modes.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.
  ;; This is recommended since dabbrev can be used globally (M-/).
  :init
  (corfu-global-mode))

(use-package which-key
  :ensure)

(use-package lsp-mode
  :ensure
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")
  (setq lsp-file-watch-threshold 10000)
  :config
  (lsp-enable-which-key-integration t))

(use-package lsp-ui
  :ensure
  :init (setq lsp-ui-peek-list-width 80)
  :hook (lsp-mode . lsp-ui-mode))

(use-package lsp-treemacs
  :ensure
  :commands lsp-treemacs-errors-list)

(use-package lsp-python-ms
  :ensure
  :init (setq lsp-python-ms-executable "/home/rpoisel/.emacs.d/.cache/lsp/mspyls/Microsoft.Python.LanguageServer"))
(add-hook 'python-mode-hook 'lsp-deferred)

(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)
(use-package go-mode
  :ensure)
(add-hook 'go-mode-hook #'lsp-deferred)

;; https://emacs-lsp.github.io/lsp-mode/page/performance/
    ;; (setq lsp-log-io nil) ; if set to true can cause a performance hit
    ;; (setq lsp-print-performance t)
    ;; (setq lsp-auto-guess-root t) ; auto detect workspace and start lang server

;; lua
;; https://emacs-lsp.github.io/lsp-mode/page/lsp-lua-language-server/
(use-package f
  :ensure)

(setq lsp-clients-lua-language-server-install-dir (f-join (getenv "HOME") ".vscode/extensions/sumneko.lua-2.4.2/server"); Default: ~/.emacs.d/.cache/lsp/lua-language-server/
        lsp-clients-lua-language-server-bin (f-join lsp-clients-lua-language-server-install-dir "bin/Linux/lua-language-server")
        lsp-clients-lua-language-server-main-location (f-join lsp-clients-lua-language-server-install-dir "main.lua")
        lsp-lua-workspace-max-preload 2048 ; Default: 300, Max preloaded files
        lsp-lua-workspace-preload-file-size 1024; Default: 100, Skip files larger than this value (KB) when preloading.
)
(use-package lua-mode
  :ensure
  :init
  :config)

(use-package dap-mode
  :ensure)

(use-package docker
  :ensure t
  :bind ("C-c d" . docker))

(use-package docker-tramp
  :ensure)

(use-package kubernetes
  :ensure t
  :commands (kubernetes-overview))

(require 'kubernetes)
(evil-set-initial-state 'kubernetes-overview-mode 'emacs)

(use-package yaml-mode
  :ensure)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode))

(use-package rg
  :demand
  :ensure
  :after project
  :init
  (setq rg-show-header nil)
  :config
  (rg-enable-default-bindings)

  ;; Redefine searches from transient, hardcode :files to "everything".
  (rg-define-search rg-dwim-current-dir :query point :format literal :files "everything" :dir current)
  (rg-define-search rg-dwim-project-dir :query point :format literal :files "everything" :dir project)
  (rg-define-search rg :confirm prefix :files "everything")
  (rg-define-search rg-literal :format literal :confirm prefix :files "everything")
  (rg-define-search rg-project :files "everything" :dir project)

  :bind (:map rg-mode-map
              ;; Use the same key-binding that `wdired' uses by default.
              ("C-x C-q" . wgrep-change-to-wgrep-mode))
  :hook (rg-mode-hook . (lambda () (select-window (display-buffer (current-buffer))))))
(evil-set-initial-state 'rg-mode 'emacs)

(use-package magit
  :ensure
  :init
  :config)

(use-package forge
  :ensure
  :after magit)

(use-package git-timemachine
  :ensure
  :init
  :config)
(evil-set-initial-state 'git-timemachine-mode 'emacs)

(use-package dockerfile-mode
  :ensure
  :init
  :config)

(use-package avy
  :ensure
  :init
  :config)

(use-package treemacs
  :ensure
  :init
  :config)

(use-package treemacs-evil
  :ensure
  :init
  :config)

(use-package projectile
  :ensure
  :init
  :config)

(projectile-mode +1)
;; Recommended keymap prefix on macOS
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
;; Recommended keymap prefix on Windows/Linux
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(use-package php-mode
  :ensure
  :init
  :config)

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

(use-package windmove
  :bind (("S-M-<up>" . windmove-up)
         ("S-M-<right>" . windmove-right)
         ("S-M-<down>" . windmove-down)
         ("S-M-<left>" . windmove-left)))

(use-package vterm
  :ensure
  :commands vterm
  :config
  (setq vterm-max-scrollback 10000)
  ;; (setq vterm-kill-buffer-on-exit t)
  (defun turn-off-chrome ()
    (hl-line-mode -1)
    (display-line-numbers-mode -1))
  :hook (vterm-mode . turn-off-chrome))

(defun vmacs-auto-exit (buf event)
  ;; buf unused
  ;; event unused
  (kill-buffer-and-window))

;; (add-hook 'vterm-exit-functions (lambda (buf event)
;; 				  (message "Called.")
;; 				  (delete-window)))

(add-hook 'vterm-exit-functions #'vmacs-auto-exit)

(use-package vterm-toggle
  :ensure)

(use-package multi-vterm
  :ensure t
  :config
	(add-hook 'vterm-mode-hook
			(lambda ()
			(setq-local evil-insert-state-cursor 'box)
			(evil-insert-state)))
	(define-key vterm-mode-map [return]                      #'vterm-send-return)

	(setq vterm-keymap-exceptions nil)
	(evil-define-key 'insert vterm-mode-map (kbd "C-e")      #'vterm--self-insert)
	(evil-define-key 'insert vterm-mode-map (kbd "C-f")      #'vterm--self-insert)
	(evil-define-key 'insert vterm-mode-map (kbd "C-a")      #'vterm--self-insert)
	(evil-define-key 'insert vterm-mode-map (kbd "C-v")      #'vterm--self-insert)
	(evil-define-key 'insert vterm-mode-map (kbd "C-b")      #'vterm--self-insert)
	(evil-define-key 'insert vterm-mode-map (kbd "C-w")      #'vterm--self-insert)
	(evil-define-key 'insert vterm-mode-map (kbd "C-u")      #'vterm--self-insert)
	(evil-define-key 'insert vterm-mode-map (kbd "C-d")      #'vterm--self-insert)
	(evil-define-key 'insert vterm-mode-map (kbd "C-n")      #'vterm--self-insert)
	(evil-define-key 'insert vterm-mode-map (kbd "C-m")      #'vterm--self-insert)
	(evil-define-key 'insert vterm-mode-map (kbd "C-p")      #'vterm--self-insert)
	(evil-define-key 'insert vterm-mode-map (kbd "C-j")      #'vterm--self-insert)
	(evil-define-key 'insert vterm-mode-map (kbd "C-k")      #'vterm--self-insert)
	(evil-define-key 'insert vterm-mode-map (kbd "C-r")      #'vterm--self-insert)
	(evil-define-key 'insert vterm-mode-map (kbd "C-t")      #'vterm--self-insert)
	(evil-define-key 'insert vterm-mode-map (kbd "C-g")      #'vterm--self-insert)
	(evil-define-key 'insert vterm-mode-map (kbd "C-c")      #'vterm--self-insert)
	(evil-define-key 'insert vterm-mode-map (kbd "C-SPC")    #'vterm--self-insert)
	(evil-define-key 'normal vterm-mode-map (kbd "C-d")      #'vterm--self-insert)
	(evil-define-key 'normal vterm-mode-map (kbd ",c")       #'multi-vterm)
	(evil-define-key 'normal vterm-mode-map (kbd ",n")       #'multi-vterm-next)
	(evil-define-key 'normal vterm-mode-map (kbd ",p")       #'multi-vterm-prev)
	(evil-define-key 'normal vterm-mode-map (kbd "C-c 2")    #'rpoisel/multi-vterm-split-vertically)
	(evil-define-key 'normal vterm-mode-map (kbd "C-c 3")    #'rpoisel/multi-vterm-split-horizontally)
	(evil-define-key 'normal vterm-mode-map (kbd "i")        #'evil-insert-resume)
	(evil-define-key 'normal vterm-mode-map (kbd "o")        #'evil-insert-resume)
	(evil-define-key 'normal vterm-mode-map (kbd "<return>") #'evil-insert-resume))

(defun rpoisel/multi-vterm-split-vertically ()
  (interactive)
  (select-window (split-window-vertically))
  (multi-vterm))
(defun rpoisel/multi-vterm-split-horizontally ()
  (interactive)
  (select-window (split-window-horizontally))
  (multi-vterm))
(defun rpoisel/multi-vterm-jump-device ()
  (interactive)
  (let ((split (read-multiple-choice "Choose splitting"
				     '((?h "horizontal")
				       (?v "vertical")))))
    (let ((jumphost (read-multiple-choice "Choose jumphost"
					  '((?s "jumpstaging")
					    (?p "jump")))))
      (let ((mac (read-string "Enter MAC: ")))
	(cond
	 ((eq (nth 0 split) ?h) (rpoisel/multi-vterm-split-horizontally))
	 ((eq (nth 0 split) ?v) (rpoisel/multi-vterm-split-vertically)))
	(vterm-insert (format "ssh %s %s" (nth 1 jumphost) mac))
	(vterm-send-return)
	(sleep-for 1)
	(vterm-insert "alias ll=\"ls -la --color=always\"")
	(vterm-send-return)))))

(use-package drag-stuff
  :ensure t)
(drag-stuff-global-mode 1)
(drag-stuff-define-keys)

(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown"))

(use-package multiple-cursors
  :ensure t)

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

(use-package git-link
  :ensure
  :commands (git-link git-link-browse git-link-commit git-link-homepage)
  :config
  (defun git-link-browse (&rest args)
    "Foobar."
    (interactive)
    (let ((git-link-open-in-browser t))
      (if (interactive-p)
          (call-interactively #'git-link)
        (apply #'git-link args)))))

(use-package json-mode
  :ensure)

(use-package graphql-mode
  :ensure)

;; not used yet
(defun efs/org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Cantarell" :weight 'regular :height (cdr face)))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

(use-package org
  :hook (org-mode . efs/org-mode-setup)
  :config
  (setq org-ellipsis " ▾"))
  ;; (efs/org-font-setup))

(use-package org-bullets
  :ensure
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(defun efs/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :ensure
  :hook (org-mode . efs/org-mode-visual-fill))

(use-package capf-autosuggest
  :ensure)

(use-package rainbow-mode
  :ensure
  :defer t
  :hook (org-mode
         emacs-lisp-mode
         web-mode
         typescript-mode
         js2-mode))

;; global key map
(global-set-key (kbd "C-:") 'avy-goto-char)

(global-set-key (kbd "C-c t") 'multi-vterm)
(global-set-key (kbd "C-c 2") 'rpoisel/multi-vterm-split-vertically)
(global-set-key (kbd "C-c 3") 'rpoisel/multi-vterm-split-horizontally)
;; (global-set-key (kbd "C-c s 3") 'rpoisel/multi-vterm-jump-device-staging)
;; (global-set-key (kbd "C-c s 3") 'rpoisel/multi-vterm-jump-device-prod)

(global-set-key (kbd "C-c g b") 'magit-blame-addition)
(global-set-key (kbd "C-c g l") 'git-link)
(global-set-key (kbd "C-c g B") 'git-link-browse)

(global-set-key (kbd "C-c c b") 'consult-buffer)
(global-set-key (kbd "C-c c l") 'consult-line)
(global-set-key (kbd "C-c c y") 'consult-yank-from-kill-ring)

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :ensure
  :init
  (savehist-mode))

;; TODO only start if not yet started
(server-start)
