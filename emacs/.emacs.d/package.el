(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-and-compile
  (setq use-package-enable-imenu-support t)
  (setq use-package-hook-name-suffix nil))

(setq use-package-compute-statistics t)
(eval-when-compile
  (require 'use-package))

;; backups
(setq backup-directory-alist '(("." . "~/.emacs-backups/")))
(setq backup-by-copying t)
(setq version-control t)
(setq delete-old-versions t)
(setq kept-new-versions 6)
(setq kept-old-versions 2)
(setq create-lockfiles nil)

(use-package dracula-theme
  :ensure
  :config (load-theme 'dracula t))

;; Set default font
(set-face-attribute 'default nil
                    :family "Source Code Pro"
                    :height 100
                    :weight 'normal
                    :width 'normal)

(use-package svg-tag-mode
  :ensure)
(add-hook 'org-mode-hook 'svg-tag-mode)

(defconst date-re "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}")
(defconst time-re "[0-9]\\{2\\}:[0-9]\\{2\\}")
(defconst day-re "[A-Za-z]\\{3\\}")

(defun svg-progress-percent (value)
  (svg-image (svg-lib-concat
              (svg-lib-progress-bar (/ (string-to-number value) 100.0)
                                nil :margin 0 :stroke 2 :radius 3 :padding 2 :width 11)
              (svg-lib-tag (concat value "%")
                           nil :stroke 0 :margin 0)) :ascent 'center))

(defun svg-progress-count (value)
  (let* ((seq (mapcar #'string-to-number (split-string value "/")))
         (count (float (car seq)))
         (total (float (cadr seq))))
  (svg-image (svg-lib-concat
              (svg-lib-progress-bar (/ count total) nil
                                    :margin 0 :stroke 2 :radius 3 :padding 2 :width 11)
              (svg-lib-tag value nil
                           :stroke 0 :margin 0)) :ascent 'center)))

(setq svg-tag-tags
      `(
        ;; Org tags
        ;; (":\\([A-Za-z0-9]+\\)" . ((lambda (tag) (svg-tag-make tag))))
        ;; (":\\([A-Za-z0-9]+[ \-]\\)" . ((lambda (tag) tag)))

        ;; Task priority
        ("\\[#[A-Z]\\]" . ( (lambda (tag)
                              (svg-tag-make tag :face 'org-priority
                                            :beg 2 :end -1 :margin 0))))

        ;; Progress
        ("\\(\\[[0-9]\\{1,3\\}%\\]\\)" . ((lambda (tag)
                                            (svg-progress-percent (substring tag 1 -2)))))
        ("\\(\\[[0-9]+/[0-9]+\\]\\)" . ((lambda (tag)
                                          (svg-progress-count (substring tag 1 -1)))))

        ;; TODO / DONE
        ("TODO" . ((lambda (tag) (svg-tag-make "TODO" :face 'org-todo :inverse t :margin 0))))
        ("ACTIVE" . ((lambda (tag) (svg-tag-make "ACTIVE" :face 'org-todo :margin 0))))
        ("DONE" . ((lambda (tag) (svg-tag-make "DONE" :face 'org-done :margin 0))))


        ;; Citation of the form [cite:@Knuth:1984]
        ("\\(\\[cite:@[A-Za-z]+:\\)" . ((lambda (tag)
                                          (svg-tag-make tag
                                                        :inverse t
                                                        :beg 7 :end -1
                                                        :crop-right t))))
        ("\\[cite:@[A-Za-z]+:\\([0-9]+\\]\\)" . ((lambda (tag)
                                                (svg-tag-make tag
                                                              :end -1
                                                              :crop-left t))))


        ;; Active date (without day name, with or without time)
        (,(format "\\(<%s>\\)" date-re) .
         ((lambda (tag)
            (svg-tag-make tag :beg 1 :end -1 :margin 0))))
        (,(format "\\(<%s *\\)%s>" date-re time-re) .
         ((lambda (tag)
            (svg-tag-make tag :beg 1 :inverse nil :crop-right t :margin 0))))
        (,(format "<%s *\\(%s>\\)" date-re time-re) .
         ((lambda (tag)
            (svg-tag-make tag :end -1 :inverse t :crop-left t :margin 0))))

        ;; Inactive date  (without day name, with or without time)
         (,(format "\\(\\[%s\\]\\)" date-re) .
          ((lambda (tag)
             (svg-tag-make tag :beg 1 :end -1 :margin 0 :face 'org-date))))
         (,(format "\\(\\[%s *\\)%s\\]" date-re time-re) .
          ((lambda (tag)
             (svg-tag-make tag :beg 1 :inverse nil :crop-right t :margin 0 :face 'org-date))))
         (,(format "\\[%s *\\(%s\\]\\)" date-re time-re) .
          ((lambda (tag)
             (svg-tag-make tag :end -1 :inverse t :crop-left t :margin 0 :face 'org-date))))))

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
  ;; see: https://emacs.stackexchange.com/a/5604/36387
  (setq dired-dwim-target t)
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
  (dired-mode-hook . dired-hide-details-mode)
  :config
  (progn
    (setq dired-omit-verbose nil)
    ;; toggle `dired-omit-mode' with C-x M-o
    (add-hook 'dired-mode-hook #'dired-omit-mode)
    (setq dired-omit-files
          (concat dired-omit-files "\\|^.DS_STORE$\\|^.projectile$\\|\\.~undo-tree~$"))))

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

(setq evil-auto-indent nil)
(setq sentence-end-double-space nil)
(use-package evil
  :ensure
  :demand
  :init
  (setq evil-want-C-w-delete t)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join))
(evil-set-initial-state 'dired-mode 'emacs)
(evil-set-initial-state 'compilation-mode 'emacs)

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
  :ensure
  :bind (:map vertico-map
              ("M-n" . vertico-next)
              ("M-p" . vertico-previous)
              ("C-m" . vertico-exit)
              :map minibuffer-local-map
              ("M-h" . dw/minibuffer-backward-kill))
  :custom
  (vertico-cycle t)
  :init
  (vertico-mode))

(use-package vertico-multiform
  :after vertico
  :ensure nil
  :init
  (vertico-multiform-mode))
(setq vertico-multiform-commands
      '((consult-ripgrep buffer)
        (consult-line buffer)
        (consult-imenu buffer)
        (consult-flycheck buffer)
        (t reverse)))
(setq vertico-multiform-categories
      '((t reverse)))

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

(use-package consult-flycheck
  :ensure)

;;; add missing, deprecated function (can be removed as soon not referenced anymore)
(defun consult-completing-read-multiple (&rest args)
  "Deprecated function; call `completing-read-multiple' with ARGS."
  (advice-remove #'completing-read-multiple #'consult-completing-read-multiple)
  (run-at-time 0 nil #'message "`consult-completing-read-multiple' has been deprecated")
  (apply #'completing-read-multiple args))

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
         ("M-g f" . consult-flycheck)
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

(use-package consult-dir
  :ensure t
  :bind (("C-x C-d" . consult-dir)
         :map vertico-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))

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

;; https://company-mode.github.io/manual/Customization.html#Customization
;; configuration values might be overwritten by modes such as lsp-mode
(use-package company
  :ensure t
  :hook
  (after-init . global-company-mode)
  :config
  (setq company-backends (delete 'company-semantic company-backends)
        company-idle-delay 0
        company-minimum-prefix-length 4
        company-selection-wrap-around t
        company-tooltip-align-annotations t))

(use-package tempel
  :ensure
  :bind (("M-+" . tempel-complete) ;; Alternative tempel-expand
         ("M-*" . tempel-insert))

  :init
  (defvar-local tempel-buffer-local-templates nil
    "Buffer-local templates.")

  ;; Setup completion at point
  (defun tempel-setup-capf ()
    ;; Add the Tempel Capf to `completion-at-point-functions'. `tempel-expand'
    ;; only triggers on exact matches. Alternatively use `tempel-complete' if
    ;; you want to see all matches, but then Tempel will probably trigger too
    ;; often when you don't expect it.
    ;; NOTE: We add `tempel-expand' *before* the main programming mode Capf,
    ;; such that it will be tried first.
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions)))
  :config
  (add-to-list 'tempel-template-sources 'tempel-buffer-local-templates)

  :bind (:map tempel-map
              ("<tab>" . tempel-next)
              ("<backtab>" . tempel-previous))

  :hook ((prog-mode-hook . tempel-setup-capf)
         (text-mode-hook . tempel-setup-capf)
         (org-mode-hook . tempel-setup-capf))

  ;; Optionally make the Tempel templates available to Abbrev,
  ;; either locally or globally. `expand-abbrev' is bound to C-x '.
  ;; (add-hook 'prog-mode-hook #'tempel-abbrev-mode)
  ;; (tempel-global-abbrev-mode)
  )

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
  (global-corfu-mode))

(use-package which-key
  :ensure
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.5
        which-key-idle-secondary-delay 0.5)
  (which-key-setup-side-window-bottom))

;; see: https://git.riyyi.com/riyyi/dotfiles/src/branch/master/.config/emacs/config.org
(use-package lsp-mode
  :ensure
  :after which-key
  :commands (lsp lsp-deferred)
  ;; using :hook does not really work for me, so explictly invoking (add-hook ...) instead
  :init
  (setq lsp-auto-guess-root t
        lsp-keymap-prefix "C-c l"
        lsp-file-watch-threshold 10000)
  :config
  (add-hook 'c-mode-hook #'lsp-deferred)
  (add-hook 'c++-mode-hook #'lsp-deferred)
  (add-hook 'lua-mode-hook #'lsp-deferred)
  (add-hook 'go-mode-hook #'lsp-deferred)
  (add-hook 'python-mode-hook #'lsp-deferred)
  (setq lsp-clients-lua-language-server-install-dir "/usr/local/lua-language-server/"
        lsp-clients-lua-language-server-bin (f-join lsp-clients-lua-language-server-install-dir "bin/lua-language-server")
        lsp-clients-lua-language-server-main-location (f-join lsp-clients-lua-language-server-install-dir "main.lua")
        lsp-lua-workspace-max-preload 2048
        lsp-lua-workspace-preload-file-size 1024)
  (setq lsp-clangd-binary-path "/usr/bin/clangd"
        lsp-clients-clangd-args '("-j=4" "--header-insertion=iwyu" "--background-index" "--log=error" "--query-driver=/**/cc*,/**/c++*,/**/*g++*,/**/*gcc*,/**/clang*"))
  (setq gc-cons-threshold (* 100 1024 1024)
        read-process-output-max (* 1024 1024)
        treemacs-space-between-root-nodes nil
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

;; Rust

(use-package rustic
  :ensure t
  :config
  (setq
   rustic-format-on-save t
   rustic-lsp-format t))

;; Lua

(defun rpo-lua-format-buffer ()
  (let ((formatter-cfg-file ".lua-format")
        (formatter-cfg-dir  (locate-dominating-file "." ".lua-format")))
    (if formatter-cfg-dir
        (let* ((tmp-buffer-name (make-temp-name "tmp"))
               (tmp-buffer (get-buffer-create tmp-buffer-name)))
          (if (eq 0 (call-shell-region
                     (point-min) (point-max) (concat "lua-format --config=" (expand-file-name (concat formatter-cfg-dir formatter-cfg-file))) nil (list tmp-buffer nil)))
              (replace-buffer-contents tmp-buffer 3)
            (error "lua-format failed."))
          (kill-buffer tmp-buffer))
      #'er-indent-and-cleanup-region-or-buffer)))

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
(add-hook 'lua-mode-hook (lambda () (add-hook 'before-save-hook #'rpo-lua-format-buffer nil 'local)))
(add-hook 'lua-mode-hook 'rpo-turn-on-indent)

;; (remove-hook 'lua-mode-hook #'lua-add-before-save-hook)

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

(defun rpo-turn-on-indent ()
  (setq evil-auto-indent t)
  (make-local-variable 'evil-auto-indent)
  (electric-indent-local-mode))
(defun rpo-c-like-lang-mode-hook ()
  (clang-format-save-hook-for-this-buffer)
  (rpo-turn-on-indent))
(add-hook 'c-mode-hook 'rpo-c-like-lang-mode-hook)
(add-hook 'c++-mode-hook 'rpo-c-like-lang-mode-hook)
(add-hook 'glsl-mode-hook 'rpo-c-like-lang-mode-hook)

(use-package dap-mode
  :ensure)

(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
  (require 'dap-cpptools))

(add-hook 'find-file-hook #'fg/git-gutter-mode)

;; CMake
(use-package cmake-mode
  :ensure)
(add-hook 'cmake-mode-hook 'rpo-turn-on-indent)

;; Python
(use-package lsp-python-ms
  :ensure
  :init (setq lsp-python-ms-executable "~/.emacs.d/.cache/lsp/mspyls/Microsoft.Python.LanguageServer"))
(add-hook 'python-mode-hook 'rpo-turn-on-indent)

;; Golang
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)
(add-hook 'go-mode-hook 'rpo-turn-on-indent)
(use-package go-mode
  :ensure)

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
  :ensure
  :hook ((yaml-mode-hook . flycheck-mode)))
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.gqlgen\\'" . yaml-mode))

(use-package rg
  :demand
  :ensure
  :after project
  :init
  (setq rg-show-header nil)
  :config
  (setq rg-custom-type-aliases '(("graphql" . "*.graphql *.graphqls gqlgen.yml")))
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
  :hook ((prog-mode-hook . bug-reference-prog-mode)
         (prog-mode-hook . flycheck-mode)))

(use-package flycheck
  :ensure t)

(use-package magit
  :ensure
  :init
  :config)

(use-package git-gutter
  :ensure
  :config
  (setq git-gutter:update-interval 0.25)

  (defun fg/git-gutter-mode ()
    "Enable git-gutter mode if current buffer's file is under version control."
    (if (vc-backend (buffer-file-name))
        (git-gutter-mode 1))))

(use-package git-gutter-fringe
  :ensure
  :config
  (set-face-attribute 'git-gutter-fr:added nil :foreground "#99cc99" :inherit 'fringe)
  (set-face-attribute 'git-gutter-fr:modified nil :foreground "#9999cc" :inherit 'fringe)
  (set-face-attribute 'git-gutter-fr:deleted nil :foreground "#cc9999" :inherit 'fringe)
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom))

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
  :ensure)

(projectile-mode +1)
;; Recommended keymap prefix on macOS
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
;; Recommended keymap prefix on Windows/Linux
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(use-package php-mode
  :ensure
  :init
  :config)

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
  (evil-define-key 'insert vterm-mode-map (kbd "C-q")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-r")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-t")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-g")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-c")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-z")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-SPC")    #'vterm--self-insert)
  (evil-define-key 'normal vterm-mode-map (kbd "C-d")      #'vterm--self-insert)
  (evil-define-key 'normal vterm-mode-map (kbd "<escape>") #'vterm--self-insert)
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

(use-package shell-pop
  :ensure
  :custom
  ;; This binding toggles popping up a shell, or moving cursour to the shell pop-up.
  ;; (shell-pop-universal-key "C-t")

  ;; Percentage for shell-buffer window size.
  (shell-pop-window-size 30)

  (shell-pop-autocd-to-working-dir t)

  ;; Position of the popped buffer: top, bottom, left, right, full.
  (shell-pop-full-span t)
  (shell-pop-window-position "bottom")

  ;; Please use an awesome shell.
  (shell-pop-term-shell "/bin/bash"))

(use-package drag-stuff
  :ensure t)
(drag-stuff-global-mode 1)
(drag-stuff-define-keys)
(add-to-list 'drag-stuff-except-modes 'org-mode)
(global-auto-revert-mode t)

(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown"))

(use-package multiple-cursors
  :ensure t
  :bind (("C-S-c C-S-c" . 'mc/edit-lines)
         ("C->" . 'mc/mark-next-like-this)
         ("C-<" . 'mc/mark-previous-like-this)
         ("C-c C-<" . 'mc/mark-all-like-this)))

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

(use-package jenkinsfile-mode
  :ensure)

(setq revert-without-query '(".pdf"))

(setq org-todo-keywords
      '((sequence "TODO" "PREPARE" "ACTIVE" "WAITING" "|" "DONE" "DELEGATED" "CANCELLED")))
(setq org-default-notes-file "/tmp/refile.org")
(rpo-load-file-if-exists "~/Sync/org/capture_templates.el")
(use-package org
  :config
  (setq org-ellipsis " ▾")
  (custom-set-variables
   '(org-agenda-files
     (directory-files-recursively "~/Sync/org" "[a-zA-z]+\.org$"))
   '(org-directory "~/Sync/org"))
  (setq org-refile-targets '((nil :maxlevel . 9)
                             (org-agenda-files :maxlevel . 9)))
  (setq org-outline-path-complete-in-steps nil)         ; Refile in a single go
  (setq org-refile-use-outline-path t)                  ; Show full paths for refiling
  ;; open directory links in emacs (see: https://emacs.stackexchange.com/a/10696/36387)
  (add-to-list 'org-file-apps '(directory . emacs)))

(setq org-latex-pdf-process
      '("lualatex -shell-escape -interaction nonstopmode %f"
        "lualatex -shell-escape -interaction nonstopmode %f"))

(add-hook 'org-mode-hook
          (lambda () (add-hook 'before-save-hook #'whitespace-cleanup nil 'local)))

;; for GNU Makefiles
(setq org-src-preserve-indentation t)
(setq org-plantuml-jar-path
      (expand-file-name "/usr/local/plantuml.jar"))
(org-babel-do-load-languages
 'org-babel-load-languages '((awk . t)
                             (C . t)
                             (lua . t)
                             (makefile . t)
                             (plantuml . t)
                             (python . t)
                             (shell . t)))

(use-package org-appear
  :config
  (setq org-appear-autolinks t)
  :ensure)
(add-hook 'org-mode-hook 'org-appear-mode)

(use-package mixed-pitch
  :ensure
  :hook
  (org-mode . mixed-pitch-mode)
  :config
  (set-face-attribute 'default nil :font "Source Code Pro" :height 100)
  (set-face-attribute 'fixed-pitch nil :font "Source Code Pro")
  (set-face-attribute 'variable-pitch nil :font "DejaVu Sans"))

(use-package org-superstar
  :ensure
  :config
  (setq org-superstar-special-todo-items t)
  (add-hook 'org-mode-hook (lambda ()
                             (org-superstar-mode 1))))

(use-package org-re-reveal
  :ensure)

(use-package ox-hugo
  :ensure
  :after ox)

(use-package capf-autosuggest
  :ensure)

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :ensure
  :init
  (savehist-mode))

(use-package guru-mode
  :ensure)

(use-package bug-reference
  :ensure)
(add-hook 'org-mode-hook #'bug-reference-mode)

(use-package pdf-tools
  :ensure)
(pdf-tools-install)

(use-package org-noter
  :ensure
  :config
  (require 'org-noter-pdftools))

(use-package org-pdftools
  :ensure
  :hook (org-mode-hook . org-pdftools-setup-link))

(use-package org-noter-pdftools
  :ensure
  :after org-noter
  :config
  ;; Add a function to ensure precise note is inserted
  (defun org-noter-pdftools-insert-precise-note (&optional toggle-no-questions)
    (interactive "P")
    (org-noter--with-valid-session
     (let ((org-noter-insert-note-no-questions (if toggle-no-questions
                                                   (not org-noter-insert-note-no-questions)
                                                 org-noter-insert-note-no-questions))
           (org-pdftools-use-isearch-link t)
           (org-pdftools-use-freepointer-annot t))
       (org-noter-insert-note (org-noter--get-precise-info)))))

  ;; fix https://github.com/weirdNox/org-noter/pull/93/commits/f8349ae7575e599f375de1be6be2d0d5de4e6cbf
  (defun org-noter-set-start-location (&optional arg)
    "When opening a session with this document, go to the current location.
With a prefix ARG, remove start location."
    (interactive "P")
    (org-noter--with-valid-session
     (let ((inhibit-read-only t)
           (ast (org-noter--parse-root))
           (location (org-noter--doc-approx-location (when (called-interactively-p 'any) 'interactive))))
       (with-current-buffer (org-noter--session-notes-buffer session)
         (org-with-wide-buffer
          (goto-char (org-element-property :begin ast))
          (if arg
              (org-entry-delete nil org-noter-property-note-location)
            (org-entry-put nil org-noter-property-note-location
                           (org-noter--pretty-print-location location))))))))
  (with-eval-after-load 'pdf-annot
    (add-hook 'pdf-annot-activate-handler-functions #'org-noter-pdftools-jump-to-note)))

(use-package ob-dsq
  :ensure)

;; Functions

;; https://emacsredux.com/blog/2013/03/27/indent-region-or-buffer/
(defun er-indent-buffer ()
  "Indent the currently visited buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

(defun er-indent-and-cleanup-region-or-buffer ()
  "Indent a region if selected, otherwise the whole buffer."
  (interactive)
  (save-excursion
    (if (region-active-p)
        (progn
          (indent-region (region-beginning) (region-end)))
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

;; https://stackoverflow.com/a/2240286/203506
(defun rpo/format-xml ()
  (interactive)
  (shell-command-on-region 1 (point-max) "xmllint --format -" (current-buffer) t))

;; Variables

(setq kill-buffer-query-functions (delq 'process-kill-buffer-query-function kill-buffer-query-functions))
(setq compilation-scroll-output t)
(electric-indent-mode -1)

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

(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

(global-set-key (kbd "C-c w b") 'bh/switch-to-scratch)
(global-set-key (kbd "C-c w d") 'display-line-numbers-mode)
(global-set-key (kbd "C-c w l") 'copy-current-line-position-to-clipboard)
(global-set-key (kbd "C-c w w") 'whitespace-mode)
(global-set-key (kbd "C-c w v") 'visual-line-mode)

(global-set-key (kbd "C-<prior>") 'tab-previous) ; page up key
(global-set-key (kbd "C-<next>") 'tab-next) ; page down key
(global-set-key (kbd "C-M-\\") #'er-indent-and-cleanup-region-or-buffer)

(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

;; ultra-fast keybindinds
;;; package.el ends here
