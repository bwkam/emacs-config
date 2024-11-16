(setq inhibit-startup-message t) ; no thank you
(scroll-bar-mode -1) ; no scrollbar
(tool-bar-mode -1) ; no toolbar
(tooltip-mode -1) ; no tooltip
(set-fringe-mode 10) ; I don't remember
(menu-bar-mode -1) ; no menu

(set-face-attribute 'default nil :font "Fira Code" :height 110)
(set-face-attribute 'fixed-pitch nil :font "Fira Code" :height 110)
(set-face-attribute 'variable-pitch nil :font "Cantarell" :height 110 :weight 'regular)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "C-M-j") 'counsel-switch-buffer)

(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpha" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(require 'use-package)

(setq use-package-always-ensure t)

(column-number-mode)
(global-display-line-numbers-mode t)

; disable line numbers on certain modes
(dolist (mode '(org-mode-hook
              term-mode-hook
              eshell-mode-hook
              shell-mode-hook))
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

(use-package haxe-mode)

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

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-split-window-below t)
  (setq evil-vsplit-window-right t)

  :bind (:map evil-normal-state-map
              ("H" . 'previous-buffer)
              ("L" . 'next-buffer)
              ("C-M-h" . 'evil-window-left)
              ("C-M-l" . 'evil-window-right)
              ("C-M-k" . 'evil-window-up)
              ("C-M-j" . 'evil-window-down))

  :config
  (evil-mode 1)

  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)

  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

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

(defun bw/org-mode-setup ()
    (org-indent-mode)
    (variable-pitch-mode 1)
    (auto-fill-mode 0))

(defun bw/org-font-setup ()
    ;; Replace list hyphen with dot
    (font-lock-add-keywords 'org-mode
                            '(("^ *\\([-]\\) "
                            (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

(dolist (face '((org-level-1 . 1.2)
                (org-level-2 . 1.1)
                (org-level-3 . 1.05)
                (org-level-4 . 1.0)
                (org-level-5 . 1.1)
                (org-level-6 . 1.1)
                (org-level-7 . 1.1)
                (org-level-8 . 1.1)))
  (set-face-attribute (car face) nil :font "Cantarell" :weight 'regular :height (cdr face)))

(set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
(set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

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
        '("~/Documents/dev/org-testing/tasks.org"
          "~/Documents/dev/org-testing/birthdays.org"
          "~/Documents/dev/org-testing/habits.org"))
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
