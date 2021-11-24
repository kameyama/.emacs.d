;; init.el is created from emacs.org which emacs settings.
;; Do NOT modify this file.

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			     ("melpa-stable" . "https://stable.melpa.org/packages/")
			     ("org" . "https://orgmode.org/elpa/")
			     ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
      (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless  (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)

;; On non-Guix systems, "ensure" packages by default
(setq use-package-always-ensure t)
(use-package quelpa)

(use-package no-littering)

;; no-littering doesn't set this by default so we must place
;; auto save files in the same path as it uses for sessions
(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

(use-package auto-package-update
  :custom
  (auto-package-update-interval 7)
  (auto-package-update-prompt-before-update t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe)
  (auto-package-update-at-time "09:00"))

(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

;; Thanks, but no thanks
(setq inhibit-startup-message t)

;;(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)       ; Give some breathing room
(menu-bar-mode -1)            ; Disable the menu bar

;; Set up the visible bell
(setq visible-bell t)

;; show line numb
(column-number-mode)
(global-display-line-numbers-mode t)

;; Override some modes which derive from the above
(dolist (mode '(org-mode-hook
		shell-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;;ESC Cancels All
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(use-package general
  :config
  (general-create-definer rune/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")

  ;; (leader-keys
  ;; 	 "t" '(:ignore t :which-key "toggles")
  ;; 	 "tt" '(counsel-load-theme :which-key "choose theme")))


      (global-set-key (kbd "C-M-j") 'counsel-switch-buffer))

(use-package doom-themes
  :config
  (load-theme 'doom-dracula t))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package all-the-icons)

     (use-package doom-modeline
       :ensure t
       :init (doom-modeline-mode 1)
       :custom ((doom-modeline-hight 15))
       )

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
	 :map ivy-minibuffer-map
	 ("TAB" . ivy-alt-done)
	 ("C-f" . ivy-alt-done)
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
  (ivy-mode t))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1)
  )

(use-package counsel
:init
(setq-default dired-omit-files-p t)
(setq dired-omit-files "^\\.DS_Store")
(setq counsel-find-file-ignore-regexp (regexp-opt '(".DS_Store")))

:bind (
("M-x" . counsel-M-x)
("C-x b" . counsel-ibuffer)
("C-x C-f" . counsel-find-file)
;; ("C-M-j" . counsel-switch-buffer)
("C-M-l" . counsel-imenu)
:map minibuffer-local-map
("C-r" . 'counsel-minibuffer-history))
)

(use-package dired			
  :ensure nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :config
  ;; (evil-collection-define-key 'normal 'dired-mode-map
  ;;   "h" 'dired-up-directory
  ;;   "l" 'dired-find-file)
  )

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . helpful-function)
  ([remap describe-symbol] . helpful-symbol)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-command] . helpful-command)
  ([remap describe-key] . helpful-key))

(use-package hydra)

;; (defhydra hydra-text-scale (:timeout 4)
;;   ("j" text-scale-increase "in")
;;   ("k" text-scale-decrease "out")
;;   ("f" nil "finished" :exit t)
;;   )

;; (rune/leader-keys
;;   "ts" '(hydra-text-scale/body :which-key "scale text"))

;; font setting
;;(set-face-attribute 'default nil :font "Fira Mono" :height 280)
;;(set-face-attribute 'variable-pitch nil :font "Cantarell" :height 295 :wigth 'regular)
(set-face-attribute 'default nil :height 150)

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  ;; (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  ;; cursor colors
  (setq evil-normal-state-cursor '("cyan" box)) 
  (setq evil-emacs-state-cursor '("orange" box))

  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)

  ;; global  
  ;; Use visual line motions even outside of visual-line-mode buffers
  ;; (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  ;; (evil-global-set-key 'motion "k" 'evil-previous-visual-line)


  ;; emacs state
  (define-key evil-emacs-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)
  (define-key evil-emacs-state-map (kbd "<escape>") 'evil-normal-state)

  ;; normal state
  ;; C-f, C-b, C-n,C-p bindings in normal state
  (define-key evil-normal-state-map (kbd "C-f") 'evil-forward-char) ; C-f is evil-scroll-page-down by default
  (define-key evil-normal-state-map (kbd "C-b") 'evil-backward-char) ; C-b is evil-scroll-page-up by default
  (define-key evil-normal-state-map (kbd "C-n") 'evil-next-visual-line) ; C-n is evil-paste-pop-next by default
  (define-key evil-normal-state-map (kbd "C-p") 'evil-previous-visual-line) ; C-p is evil-paste-pop  by default
  (setq-default evil-cross-lines t) ; Make horizontal movement cross lines

  ;; visual state
  (define-key evil-visual-state-map (kbd "C-f") 'evil-forward-char) ; C-f is evil-scroll-page-down by default
  (define-key evil-visual-state-map (kbd "C-b") 'evil-backward-char) ; C-b is evil-scroll-page-up by default
  (define-key evil-visual-state-map (kbd "C-n") 'evil-next-visual-line) ; C-n is evil-paste-pop-next by default
  (define-key evil-visual-state-map (kbd "C-p") 'evil-previous-visual-line) ; C-p is evil-paste-pop  by default

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))


(defalias 'evil-insert-state 'evil-emacs-state)


(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package org
  ;;  hook (org-mode . dw/org-mode-setup)
  :config
  (org-indent-mode)
  (variable-pitch-mode 1)
  (auto-fill-mode 0)
  (visual-line-mode 1)

  (setq org-startup-truncated nil)
  (setq evil-auto-indent nil)
  (setq org-ellipsis " ▾"
	org-hide-emphasis-markers t)
  )

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

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
  ;;(set-face-attribute (car face) nil :font "Cantarell" :weight 'regular :height (cdr face))
  )

;; Make sure org-indent face is available
(require 'org-indent)

;; Ensure that anything that should be fixed-pitch in Org files appears that way
(set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
(set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))
(set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)

(with-eval-after-load 'org
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((emacs-lisp . t)
       (python . t)
       (shell . t)
       (lisp . t)
       )
     )
    )

(setq org-confirm-babel-evaluate nil)

;; This is needed as of Org 9.2
(require 'org-tempo)

(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))
(add-to-list 'org-structure-template-alist '("jl" . "src julia"))
(add-to-list 'org-structure-template-alist '("cl" . "src lisp"))

;; Automatically tangle our Emacs.org config file when we save it
(defun efs/org-babel-tangle-config ()
  (when (string-equal (file-name-directory (buffer-file-name))
		      (expand-file-name user-emacs-directory))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'efs/org-babel-tangle-config)))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")  ;; Or 'C-l', 's-l'
  :config
  (lsp-enable-which-key-integration t))

(defun lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode)

:hook (lsp-mode . lsp-mode-setup)
)

(use-package company
  :after lsp-mode
  :hook (prog-mode . company-mode)
  :bind (:map company-active-map
	 ("C-f" . company-complete-selection))
	(:map lsp-mode-map
	 ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode))

(setq lsp-ui-sideline-enable nil)
(setq lsp-ui-sideline-show-hover nil)

(use-package lsp-treemacs
  :after lsp)

(use-package lsp-ivy)

(use-package evil-nerd-commenter
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))

(use-package python-mode
	    :ensure t
	    :hook (python-mode . lsp-deferred)
	    :custom
	    (python-shell-interpreter "python3"))

(use-package lsp-docker
  :custom
  (defvar lsp-docker-client-packages '(lsp-clients lsp-bash lsp-pyls))

  (setq lsp-docker-client-configs
	'((:server-id bash-ls :docker-server-id bashls-docker :server-command "bash-language-server start")
	  (:server-id dockerfile-ls :docker-server-id dockerfilels-docker :server-command "docker-langserver --stdio")
	  (:server-id pyls :docker-server-id pyls-docker :server-command "pyls")
	  ))

  (lsp-docker-init-clients
   :path-mappings '(("path-to-projects-you-want-to-use" . "/projects"))
   :client-packages lsp-docker-client-packages
   :client-configs lsp-docker-client-configs)
  )

(set-language-environment "UTF-8")

(require 'eglot)
(add-hook 'julia-mode-hook 'eglot-ensure)

(require 'julia-mode)
(require 'julia-repl)
(add-hook 'julia-mode-hook 'julia-repl-mode)
(add-to-list 'eglot-server-programs
             '(julia-mode . ("julia" "-e using LanguageServer, LanguageServer.SymbolServer; runserver()")))

(use-package go-mode)

(use-package slime
  :config
  (setq inferior-lisp-program "clisp")
  (setq slime-net-coding-system 'utf-8-unix)
  )

(use-package scala-mode
  :interpreter
    ("scala" . scala-mode))

(use-package sbt-mode
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
   ;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
   (setq sbt:program-options '("-Dsbt.supershell=false"))
)

(use-package yaml-mode)

(use-package sqlformat)
(setq sqlformat-command 'pgformatter)
(setq sqlformat-args '("-s2" "-g"))

(use-package csv-mode)

(use-package docker
  :ensure t
  :bind ("C-c d" . docker))

(use-package dockerfile-mode)

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/projects/code")
    (setq projectile-project-search-path '("~/projects/code")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :config (counsel-projectile-mode))

(use-package term
  :config
  (setq explicit-shell-file-name "zsh")
  ;;(setq explicit-zsh-args '())
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *"))

(use-package eterm-256color
  :hook (term-mode . eterm-256color-mode))

(use-package magit
  :commands (magit-status magit-get-current-branch)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))
