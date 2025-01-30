(setq inhibit-startup-message t) ; no thank you
(scroll-bar-mode -1) ; no scrollbar
(tool-bar-mode -1) ; no toolbar
(tooltip-mode -1) ; no tooltip
(set-fringe-mode 10) ; I don't remember
(menu-bar-mode -1) ; no menu
(add-hook 'compilation-finish-functions 'switch-to-buffer-other-window 'compilation) ;; auto focus *compilation*

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
; (global-set-key (kbd "C-M-j") 'counsel-switch-buffer)

(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpha" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(require 'use-package)

(setq use-package-always-ensure t)

;; Example configuration for Consult
(use-package consult
  ;; Replace bindings. Lazily loaded by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)                  ;; Alternative: consult-fd
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Tweak the register preview for `consult-register-load',
  ;; `consult-register-store' and the built-in commands.  This improves the
  ;; register formatting, adds thin separator lines, register sorting and hides
  ;; the window mode line.
  (advice-add #'register-preview :override #'consult-register-window)
  (setq register-preview-delay 0.5)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep consult-man
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   ;; :preview-key '(:debounce 0.4 any)
   )

  ;; Optionally configure the narrowing key.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (keymap-set consult-narrow-map (concat consult-narrow-key " ?") #'consult-narrow-help)
)

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-split-window-below t)
  (setq evil-vsplit-window-right t)
  (setq evil-undo-system 'undo-redo)

  :config
  (evil-mode 1)

  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)

  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
  (evil-define-key '(normal visual) 'global "gc" #'evilnc-comment-operator)

  (evil-set-initial-state 'messages-buffer-mode 'normal))

(define-prefix-command 'leader-map)

(keymap-set evil-motion-state-map "SPC" 'leader-map)
(keymap-set evil-normal-state-map "SPC" 'leader-map)

(evil-define-key 'normal 'global
  (kbd "C-M-h") 'windmove-left
  (kbd "C-M-l") 'windmove-right
  (kbd "C-M-j") 'windmove-down
  (kbd "C-M-k") 'windmove-up

  "H" 'previous-buffer
  "L" 'next-buffer

  "[g" 'flycheck-previous-error
  "]g" 'flycheck-next-error)

(evil-define-key nil leader-map
  "e"  'treemacs
  ;; PROJECT (p)
  "pf" 'projectile-find-file
  "ps" 'projectile-shell-command
  "pp" 'projectile-switch-project

  ;; Search (s)
  "sb" 'swiper
  "sB" 'swiper-all
  "sd" 'counsel-rg
  "sf" 'locate)

(use-package evil-collection
  :after evil
  :ensure t
  :custom
  (evil-collection-want-find-usages-bindings t)
  (evil-collection-setup-minibuffer t)
  :config
  (evil-collection-init))

(use-package evil-nerd-commenter
  :after evil
  :bind
  ([remap comment-line] . #'evilnc-comment-or-uncomment-lines))

(set-face-attribute 'default nil :font "Fira Code" :height 90)
(set-face-attribute 'fixed-pitch nil :font "Fira Code" :height 90)
(set-face-attribute 'variable-pitch nil :font "ETBembo" :height 90 :weight 'thin)

(use-package doom-themes
  :init
  (load-theme 'doom-plain-dark t)
  :config
  ;; (set-face-attribute 'font-lock-comment-face nil :foreground (doom-color 'base6))
  )

(column-number-mode)
(global-display-line-numbers-mode t)

; disable line numbers on certain modes
(dolist (mode '(org-mode-hook
              term-mode-hook
              eshell-mode-hook
              treemacs-mode-hook
              shell-mode-hook
              vterm-mode-hook))
(add-hook mode (lambda () (display-line-numbers-mode 0))))

(use-package ivy
  :diminish
  :defer
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package counsel
  :bind (:map minibuffer-local-map
              ("C-r" . 'counsel-minibuffer-history))
  :config (counsel-mode 1)
  (setq ivy-initial-inputs-alist nil)) ;; no ^

(use-package all-the-icons)

(use-package nerd-icons)
(add-to-list 'nerd-icons-extension-icon-alist '("hx" nerd-icons-sucicon "nf-seti-haxe" :face nerd-icons-orange))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom (doom-modeline-height 15))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :init (which-key-mode)
  :diminish
  :config
  (setq which-key-idle-delay 0.3))

(use-package helpful
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package ligature
  :config
  (ligature-set-ligatures 't '("www"))
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  (ligature-set-ligatures 't
                        '(("=" (rx (+ (or ">" "<" "|" "/" "~" ":" "!" "="))))
                          (";" (rx (+ ";")))
                          ("&" (rx (+ "&")))
                          ("!" (rx (+ (or "=" "!" "\." ":" "~"))))
                          ("?" (rx (or ":" "=" "\." (+ "?"))))
                          ("%" (rx (+ "%")))
                          ("|" (rx (+ (or ">" "<" "|" "/" ":" "!" "}" "\]"
                                          "-" "=" ))))
                          ("\\" (rx (or "/" (+ "\\"))))
                          ("+" (rx (or ">" (+ "+"))))
                          (":" (rx (or ">" "<" "=" "//" ":=" (+ ":"))))
                          ("/" (rx (+ (or ">"  "<" "|" "/" "\\" "\*" ":" "!"
                                          "="))))
                          ("\." (rx (or "=" "-" "\?" "\.=" "\.<" (+ "\."))))
                          ("-" (rx (+ (or ">" "<" "|" "~" "-"))))
                          ("*" (rx (or ">" "/" ")" (+ "*"))))
                          ("w" (rx (+ "w")))
                          ("<" (rx (+ (or "\+" "\*" "\$" "<" ">" ":" "~"  "!"
                                          "-"  "/" "|" "="))))
                          (">" (rx (+ (or ">" "<" "|" "/" ":" "=" "-"))))
                          ("#" (rx (or ":" "=" "!" "(" "\?" "\[" "{" "_(" "_"
                                       (+ "#"))))
                          ("~" (rx (or ">" "=" "-" "@" "~>" (+ "~"))))
                          ("_" (rx (+ (or "_" "|"))))
                          ("0" (rx (and "x" (+ (in "A-F" "a-f" "0-9")))))
                          "Fl"  "Tl"  "fi"  "fj"  "fl"  "ft"
                          "{|"  "[|"  "]#"  "(*"  "}#"  "$>"  "^="))
  (global-ligature-mode t))

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/Documents/dev")
    (setq projectile-project-search-path '("~/Documents/dev")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :config (counsel-projectile-mode))

(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package treemacs
  :defer t
  :custom
  (treemacs-project-follow-mode t)
  (treemacs-hide-gitignored-files-mode t))

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

;; (use-package treemacs-evil
;;   :after (treemacs evil)
;;   :ensure t)

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

(defun bw/lsp-mode-setup ()
  ;; (setq lsp-headerline-breadcumbs-segments '(path-up-to-project file symbols))
  ;; (lsp-headerline-breadcrumb-mode)
  (lsp-headerline-breadcrumb-mode -1))


(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook (lsp-mode . bw/lsp-mode-setup)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (define-key evil-normal-state-map (kbd "g r") #'lsp-find-references)
  (lsp-enable-which-key-integration t)
  :custom
  (lsp-headerline-breadcrumb-enable nil))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom)
  (lsp-ui-sideline-show-diagnostics t))

(use-package lsp-treemacs
  :after lsp)

(use-package lsp-ivy)

(use-package typescript-mode
  :mode "\\.ts\\'"
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2))

(use-package lsp-mode
  :ensure t
  :hook ((haskell-mode . lsp-deferred))
  :commands (lsp lsp-deferred))

(use-package lsp-haskell
  :custom
  (lsp-haskell-server-path "~/.local/bin/haskell-language-server"))

(use-package envrc
  :hook (after-init . envrc-global-mode))

(use-package lsp-nix
  :ensure lsp-mode
  :after (lsp-mode)
  :demand t
  :custom
  (lsp-nix-nil-formatter ["alejandra"])
  (lsp-nix-nil-auto-eval-inputs nil))

(use-package nix-mode
  :hook (nix-mode . lsp-deferred)
  :ensure t)

(use-package haxe-mode
  :mode "\\.hx\\'"
  :hook (haxe-mode . (lambda ()
                       (lsp-deferred)
                       (haxe-format-on-save-mode))))

(use-package raku-mode
  :defer t)

(use-package flycheck
  :ensure t
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map
         ("<tab>" . company-complete-selection))
        (:map lsp-mode-map
         ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package company-box
  :hook (company-mode . company-box-mode))

(defun bw/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (auto-fill-mode 0))

(defun bw/org-font-setup ()
  ;; Replace list hyphen with dot
  (require 'org-indent)
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

(dolist (face '((org-level-1 . 1.5)
                (org-level-2 . 1.3)
                (org-level-3 . 1.25)
                (org-level-4 . 1.1)
                (org-level-5 . 1.0)
                (org-level-6 . 1.0)
                (org-level-7 . 1.0)
                (org-level-8 . 1.0)))
    (set-face-attribute (car face) nil :font "ETBembo" :weight 'thin :height (cdr face)))

(set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
(set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))
(set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)
(set-face-attribute 'org-block-end-line nil :background "transparent" :height 80)
(set-face-attribute 'org-block-begin-line nil :inherit 'variable-pitch :background "transparent" :height 80))

(use-package org
  :hook (org-mode . bw/org-mode-setup)
  :config
  (setq org-ellipsis " ▾"
        org-hide-emphasis-markers t)

  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)

  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")))

  (require 'org-habit)
  (add-to-list 'org-modules 'org-habit)
  (setq org-habit-graph-column 60)

  (setq org-agenda-files
        '("~/Documents/org/tasks.org"))
  (bw/org-font-setup))

(setq org-tag-alist
'((:startgroup)
    ; Put mutually exclusive tags here
    (:endgroup)
    ("@errand" . ?E)
    ("@home" . ?H)
    ("@work" . ?W)
    ("agenda" . ?a)
    ("planning" . ?p)
    ("publish" . ?P)
    ("batch" . ?b)
    ("note" . ?n)
    ("idea" . ?i)))

(setq org-refile-targets
'(("Archive.org" :maxlevel . 1)
    ("Tasks.org" :maxlevel . 1)))

(advice-add 'org-refile :after 'org-save-all-org-buffers)

(setq org-capture-templates
`(("t" "Tasks / Projects")
    ("tt" "Task" entry (file+olp "tasks.org" "Inbox")
        "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)

    ("j" "Journal Entries")
    ("jj" "Journal" entry
        (file+olp+datetree "journal.org")
        "\n* %<%I:%M %p> - Journal :journal:\n\n%?\n\n"
        :clock-in :clock-resume
        :empty-lines 1)
    ("jm" "Meeting" entry
        (file+olp+datetree "journal.org")
        "* %<%I:%M %p> - %a :meetings:\n\n%?\n\n"
        :clock-in :clock-resume
        :empty-lines 1)

    ("w" "Workflows")
    ("we" "Checking Email" entry (file+olp+datetree "journal.org")
        "* Checking Email :email:\n\n%?" :clock-in :clock-resume :empty-lines 1)

    ("m" "Metrics Capture")
    ("mw" "Weight" table-line (file+headline "metrics.org" "Weight")
    "| %U | %^{Weight} | %^{Notes} |" :kill-buffer t)))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(defun bw/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . bw/org-mode-visual-fill))

(org-babel-do-load-languages
  'org-babel-load-languages
  '((emacs-lisp . t)
    (python . t)))

(setq org-confirm-babel-evalute nil)

(require 'org-tempo)

(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))

(defun bw/org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
                      (expand-file-name "~/emacs-config/config.org"))
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'bw/org-babel-tangle-config)))

(use-package term
  :config
  (setq explicit-shell-file-name "fish"))
  ; (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")

(use-package eterm-256color
  :hook (term-mode . eterm-256color-mode))

(use-package vterm
  :commands vterm
  :config
  ;; (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *") 
  (setq vterm-shell "fish")              
  (setq vterm-max-scrollback 10000))

(defun efs/configure-eshell ()
  ;; Save command history when commands are entered
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)

  ;; Truncate buffer for performance
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)

  ;; Bind some useful keys for evil-mode
  (evil-define-key '(normal insert visual) eshell-mode-map (kbd "C-r") 'counsel-esh-history)
  (evil-define-key '(normal insert visual) eshell-mode-map (kbd "<home>") 'eshell-bol)
  (evil-normalize-keymaps)

  (setq eshell-history-size         10000
        eshell-buffer-maximum-lines 10000
        eshell-hist-ignoredups t
        eshell-scroll-to-bottom-on-input t))

(use-package eshell-git-prompt)

(use-package eshell
  :hook (eshell-first-time-mode . efs/configure-eshell)
  :config

  (with-eval-after-load 'esh-opt
    (setq eshell-destroy-buffer-when-process-dies t)
    (setq eshell-visual-commands '("htop" "zsh" "vim"))))

(use-package reformatter
  :config
  (reformatter-define haxe-format
    :program "haxelib"
    :args '("run" "formatter" "--source" "./" "--stdin")))
