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

(use-package dracula-theme
  :ensure
  :config (load-theme 'dracula t))

;; Set default font
(set-face-attribute 'default nil
                    :family "Source Code Pro"
                    :height 100
                    :weight 'normal
                    :width 'normal)

(use-package exec-path-from-shell
  :ensure
  :config
  (when (display-graphic-p)
    (exec-path-from-shell-initialize)))

(require 'recentf)
(recentf-mode 1)

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
  :hook
  ;; (dired-mode-hook . dired-omit-mode)
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
  ;; (setq evil-want-C-u-scroll t)
  (setq evil-want-C-w-delete t)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join))
(evil-set-initial-state 'dired-mode 'emacs)

(use-package evil-surround
  :ensure
  :config
  (global-evil-surround-mode 1))

(use-package evil-visualstar
  :ensure
  :init
  (setq evil-visualstar/persistent t)
  :config
  (global-evil-visualstar-mode t))

(use-package undo-tree
  :ensure
  :config
  (with-eval-after-load 'evil
    (custom-set-variables '(evil-undo-system 'undo-tree))) ;; requires use of custom-set; setq doesn't work here
  (global-undo-tree-mode))

;;; general editting
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)

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
  "When minibuffer is completing a file name delete up to parent folder, otherwise delete a word."
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
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c b" . consult-bookmark)
         ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ("<help> a" . consult-apropos)            ;; orig. apropos-command
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings (search-map)
         ("M-s f" . consult-find)
         ("M-s F" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s m" . consult-multi-occur)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi))           ;; needed by consult-line to detect isearch

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI. You may want to also
  ;; enable `consult-preview-at-point-mode` in Embark Collect buffers.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Optionally replace `completing-read-multiple' with an enhanced version.
  (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key (kbd "M-."))
  ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-recent-file consult--source-project-recent-file consult--source-bookmark
   :preview-key (kbd "M-."))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; (kbd "C-+")

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; Optionally configure a function which returns the project root directory.
  ;; There are multiple reasonable alternatives to chose from.
  ;;;; 1. project.el (project-roots)
  (setq consult-project-root-function
        (lambda ()
          (when-let (project (project-current))
            (car (project-roots project)))))
  ;;;; 2. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-root-function #'projectile-project-root)
  ;;;; 3. vc.el (vc-root-dir)
  ;; (setq consult-project-root-function #'vc-root-dir)
  ;;;; 4. locate-dominating-file
  ;; (setq consult-project-root-function (lambda () (locate-dominating-file "." ".git")))
  )

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

(use-package f
  :ensure)

(use-package hydra
  :ensure)

(use-package crux
  :ensure)

;; see: https://git.riyyi.com/riyyi/dotfiles/src/branch/master/.config/emacs/config.org
(use-package lsp-mode
  :ensure
  :after which-key
  :commands lsp
  :init
  (setq lsp-keymap-prefix "C-c l"
        lsp-file-watch-threshold 10000)
  ;; :hook ((c-mode c++-mode lua-mode) . lsp)
  :config
  (setq lsp-clients-lua-language-server-install-dir "/usr/local/lua-language-server/"
        lsp-clients-lua-language-server-bin (f-join lsp-clients-lua-language-server-install-dir "bin/lua-language-server")
        lsp-clients-lua-language-server-main-location (f-join lsp-clients-lua-language-server-install-dir "main.lua")
        lsp-lua-workspace-max-preload 2048
        lsp-lua-workspace-preload-file-size 1024)
  (setq gc-cons-threshold (* 100 1024 1024)
        read-process-output-max (* 1024 1024)
        treemacs-space-between-root-nodes nil
        company-idle-delay 0.0
        company-minimum-prefix-length 1
        lsp-idle-delay 0.1)  ;; clangd is fast
  (setq lsp-auto-guess-root t)
  (setq lsp-prefer-flymake nil)
  (lsp-enable-which-key-integration t))

(use-package lsp-ui
  :ensure
  :commands lsp-ui-mode
  :after (flycheck lsp-mode)
  :init (setq lsp-ui-peek-list-width 80)
  :config
  (setq lsp-ui-flycheck-enable t)
  (setq lsp-ui-flycheck-list-position 'right)
  (setq lsp-ui-flycheck-live-reporting t)
  (setq lsp-ui-peek-enable nil)
  (setq lsp-ui-sideline-enable nil)
  (setq lsp-ui-sideline-show-symbol nil)
  :hook (lsp-mode . lsp-ui-mode))

(use-package lsp-treemacs
  :ensure
  :commands lsp-treemacs-errors-list)

;; Lua

;; https://emacs.stackexchange.com/a/5777/36387
;; https://emacs-lsp.github.io/lsp-mode/page/lsp-lua-language-server/
(use-package lua-mode
  :ensure
  :mode "\\.lua$"
  :interpreter "lua"
  :config
  (setq lua-indent-level 4
        lua-indent-string-contents t
        lua-prefix-key nil))
(defun lua-add-before-save-hook ()
  (add-hook 'before-save-hook #'er-indent-region-or-buffer nil 'local))
(add-hook 'lua-mode-hook #'lua-add-before-save-hook)
;; (remove-hook 'lua-mode-hook #'lua-add-before-save-hook)
(add-hook 'lua-mode-hook 'lsp)

;; C/C++

(setq clang-format-style "file")
;; https://emacs.stackexchange.com/a/48503/36387
(defun clang-format-save-hook-for-this-buffer ()
  "Create a buffer local save hook."
  (add-hook 'before-save-hook
            (lambda ()
              (when (locate-dominating-file "." ".clang-format")
                (clang-format-buffer))
              ;; Continue to save.
              nil)
            nil
            ;; Buffer local hook.
            t))
(use-package clang-format
  :ensure)
(add-hook 'c-mode-hook (lambda () (clang-format-save-hook-for-this-buffer)))
(add-hook 'c-mode-hook 'lsp)
(add-hook 'c++-mode-hook (lambda () (clang-format-save-hook-for-this-buffer)))
(add-hook 'c++-mode-hook 'lsp)
(add-hook 'glsl-mode-hook (lambda () (clang-format-save-hook-for-this-buffer)))

(use-package yasnippet
  :ensure)
(use-package dap-mode
  :ensure)

(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
  (require 'dap-cpptools)
  (yas-global-mode))

;; CMake
(use-package cmake-mode
  :ensure)

;; Python
(use-package lsp-python-ms
  :ensure
  :init (setq lsp-python-ms-executable "/home/rpoisel/.emacs.d/.cache/lsp/mspyls/Microsoft.Python.LanguageServer"))
(add-hook 'python-mode-hook 'lsp-deferred)

;; Golang
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

(use-package docker
  :ensure t
  :bind ("C-c d" . docker))

(use-package docker-tramp
  :ensure)

(use-package kubernetes
  :ensure t
  :commands (kubernetes-overview))

(evil-set-initial-state 'kubernetes-overview-mode 'emacs)

(use-package yaml-mode
  :ensure)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.gqlgen\\'" . yaml-mode))

(setq rg-custom-type-aliases
      '(("graphql" .    "*.graphql *.graphqls gqlgen.yml")))
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

(use-package prog-mode
  :config
  (with-eval-after-load 'evil
    (add-hook 'prog-mode-hook #'evil-normal-state))
  :hook ((prog-mode-hook . bug-reference-prog-mode)))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package magit
  :ensure
  :init
  :config)

(use-package with-editor
  :ensure)
(define-key (current-global-map)
            [remap async-shell-command] 'with-editor-async-shell-command)
(define-key (current-global-map)
            [remap shell-command] 'with-editor-shell-command)

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
  (setq avy-background t)
  :config
  (with-eval-after-load 'evil
    (dolist (state '(motion normal operator visual))
      :config
      (let ((map (intern (format "evil-%s-state-map" state))))
        (bind-key "SPC" #'avy-goto-char-timer map)))))

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
  :bind (("s-M-<up>" . windmove-up)
         ("s-M-<right>" . windmove-right)
         ("s-M-<down>" . windmove-down)
         ("s-M-<left>" . windmove-left)))

(use-package vterm
  :ensure
  :commands vterm
  :config
  (setq vterm-max-scrollback 10000)
  ;; (setq vterm-kill-buffer-on-exit t)
  (defun turn-off-chrome ()
    (hl-line-mode -1)
    (display-line-numbers-mode -1))
  :init
  (add-hook 'vterm-mode-hook 'turn-off-chrome)
  (add-hook 'vterm-mode-hook 'with-editor-export-editor))

(defun vmacs-auto-exit (buf event)
  ;; buf unused
  ;; event unused
  (kill-buffer-and-window))

;; (add-hook 'vterm-exit-functions (lambda (buf event)
;;                                (message "Called.")
;;                                (delete-window)))

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
  (evil-define-key 'normal vterm-mode-map (kbd "C-c 2")    #'rp/multi-vterm-split-vertically)
  (evil-define-key 'normal vterm-mode-map (kbd "C-c 3")    #'rp/multi-vterm-split-horizontally)
  (evil-define-key 'normal vterm-mode-map (kbd "i")        #'evil-insert-resume)
  (evil-define-key 'normal vterm-mode-map (kbd "o")        #'evil-insert-resume)
  (evil-define-key 'normal vterm-mode-map (kbd "<return>") #'evil-insert-resume))

(defun rp/multi-vterm-split-vertically ()
  (interactive)
  (select-window (split-window-vertically))
  (multi-vterm))
(defun rp/multi-vterm-split-horizontally ()
  (interactive)
  (select-window (split-window-horizontally))
  (multi-vterm))
(defun rp/multi-vterm-jump-device ()
  (interactive)
  (let ((split (read-multiple-choice "Choose splitting"
                                     '((?h "horizontal")
                                       (?v "vertical")))))
    (let ((jumphost (read-multiple-choice "Choose jumphost"
                                          '((?s "jumpstaging")
                                            (?p "jump")))))
      (let ((mac (read-string "Enter MAC: ")))
        (cond
         ((eq (nth 0 split) ?h) (rp/multi-vterm-split-horizontally))
         ((eq (nth 0 split) ?v) (rp/multi-vterm-split-vertically)))
        (vterm-insert (format "ssh %s %s" (nth 1 jumphost) mac))
        (vterm-send-return)
        (sleep-for 1)
        (vterm-insert "alias ll=\"ls -la --color=always\"")
        (vterm-send-return)))))
(defun rp/multi-vterm-yank-to-remote-file ()
  "Write remote file from yank buffer."
  (interactive)
  (let ((path (read-string "Enter path: ")))
  (vterm-insert (format "cat > %s" path))
  (vterm-send-return)
  (sit-for 0.3)
  (vterm-insert (current-kill 0 t))
  (sit-for 0.3)
  (vterm-send-C-d)
  (vterm-send-C-d)))

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

(add-to-list 'auto-mode-alist '("\\.graphqls\\'" . graphql-mode))
(use-package graphql-mode
  :ensure)

(setq revert-without-query '(".pdf"))

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

(setq org-todo-keywords
      '((sequence "TODO" "PREPARE" "ACTIVE" "|" "DONE" "DELEGATED" "CANCELLED")))
(setq org-default-notes-file "~/git/poisel.info/org/refile.org")
(setq org-capture-templates
      (quote (("t" "todo" entry (file "~/git/poisel.info/org/refile.org")
               "* TODO %?\n%U\n")
              ("r" "respond" entry (file "~/git/poisel.info/org/refile.org")
               "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n" :clock-in t :clock-resume t :immediate-finish t)
              ("n" "note" entry (file "~/git/poisel.info/org/refile.org")
               "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
              ("j" "Journal" entry (file+datetree "~/git/poisel.info/org/diary.org")
               "* %?\n%U\n" :clock-in t :clock-resume t)
              ("w" "org-protocol" entry (file "~/git/poisel.info/org/refile.org")
               "* TODO Review %c\n%U\n" :immediate-finish t)
              ("m" "Meeting" entry (file "~/git/poisel.info/org/refile.org")
               "* MEETING with %? :MEETING:\n%U" :clock-in t :clock-resume t)
              ("p" "Phone call" entry (file "~/git/poisel.info/org/refile.org")
               "* PHONE %? :PHONE:\n%U" :clock-in t :clock-resume t)
              ("h" "Habit" entry (file "~/git/poisel.info/org/refile.org")
               "* NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n"))))
(use-package org
  :config
  (setq org-ellipsis " ▾")
  (setq org-agenda-files (mapcar '(lambda (filename)
                                    (f-join "~/git/poisel.info/org" filename))
                                 (directory-files "~/git/poisel.info/org" nil "\\.org")))
  (setq org-refile-targets '((nil :maxlevel . 9)
                             (org-agenda-files :maxlevel . 9)))
  (setq org-outline-path-complete-in-steps nil)         ; Refile in a single go
  (setq org-refile-use-outline-path t)                  ; Show full paths for refiling
  ;; open directory links in emacs (see: https://emacs.stackexchange.com/a/10696/36387)
  (add-to-list 'org-file-apps '(directory . emacs)))


(setq org-latex-pdf-process
      '("lualatex -shell-escape -interaction nonstopmode %f"
        "lualatex -shell-escape -interaction nonstopmode %f"))

;; (efs/org-font-setup))
(add-hook 'org-mode-hook
          (lambda () (add-hook 'before-save-hook #'whitespace-cleanup nil 'local)))

(use-package org-superstar
  :ensure
  :config
  (setq org-superstar-special-todo-items t)
  (add-hook 'org-mode-hook (lambda ()
                             (org-superstar-mode 1))))

(use-package ox-reveal
  :ensure)

(use-package ox-hugo
  :ensure
  :after ox)

(defun efs/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :ensure
  :hook (org-mode . efs/org-mode-visual-fill))

(use-package capf-autosuggest
  :ensure)

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :ensure
  :init
  (savehist-mode))

(use-package guru-mode
  :ensure)

;; Functions

;; https://emacsredux.com/blog/2013/03/27/indent-region-or-buffer/
(defun er-indent-buffer ()
  "Indent the currently visited buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

(defun er-indent-region-or-buffer ()
  "Indent a region if selected, otherwise the whole buffer."
  (interactive)
  (save-excursion
    (if (region-active-p)
        (progn
          (indent-region (region-beginning) (region-end))
          (unless (member major-mode crux-untabify-sensitive-modes)
            (call-interactively #'untabify)))
      (progn
        (er-indent-buffer)))
    (whitespace-cleanup)))

(defun copy-current-line-position-to-clipboard ()
  "Copy current line in file to clipboard as '</path/to/file>:<line-number>'."
  (interactive)
  (let ((path-with-line-number
         (concat (buffer-file-name) "::" (number-to-string (line-number-at-pos)))))
    (kill-new path-with-line-number)
    (message (concat path-with-line-number " copied to clipboard"))))

(defun bh/switch-to-scratch ()
  "Switch to the temporary **scratch** buffer."
  (interactive)
  (switch-to-buffer "*scratch*"))

;; Variables

(setq kill-buffer-query-functions (delq 'process-kill-buffer-query-function kill-buffer-query-functions))

;; global key map
(global-set-key (kbd "C-:") 'avy-goto-char)

(global-set-key (kbd "C-c t") 'multi-vterm)
(global-set-key (kbd "C-c 2") 'rp/multi-vterm-split-vertically)
(global-set-key (kbd "C-c 3") 'rp/multi-vterm-split-horizontally)
;; (global-set-key (kbd "C-c s 3") 'rp/multi-vterm-jump-device-staging)
;; (global-set-key (kbd "C-c s 3") 'rp/multi-vterm-jump-device-prod)

(global-set-key (kbd "C-c g b") 'magit-blame-addition)
(global-set-key (kbd "C-c g f") 'magit-log-buffer-file)
(global-set-key (kbd "C-c g l") 'git-link)
(global-set-key (kbd "C-c g B") 'git-link-browse)

(global-set-key (kbd "C-c c b") 'bh/switch-to-scratch)
(global-set-key (kbd "C-c c c") 'org-capture)
(global-set-key (kbd "C-c c t") (lambda () (interactive) (org-capture nil "t")))
(global-set-key (kbd "C-c c d") 'display-line-numbers-mode)
(global-set-key (kbd "C-c c l") 'copy-current-line-position-to-clipboard)
(global-set-key (kbd "C-c c w") 'whitespace-mode)
(global-set-key (kbd "C-<prior>") 'tab-previous) ; page up key
(global-set-key (kbd "C-<next>") 'tab-next) ; page down key
(global-set-key (kbd "C-M-\\") #'er-indent-region-or-buffer)

;; ultra-fast keybindinds
;;; package.el ends here
