;; init.el is created from emacs.org which emacs settings.
;; Do NOT modify this file.

;; Initialize package sources

(eval-and-compile
  (customize-set-variable
   'package-archives '(("org" . "https://orgmode.org/elpa/")
		       ("melpa" . "https://melpa.org/packages/")
		       ("melpa-stable" . "https://stable.melpa.org/packages/")
		       ("gnu" . "https://elpa.gnu.org/packages/")
		       ("elpa" . "https://elpa.gnu.org/packages/")
		       ))
  (package-initialize)
  (unless (package-installed-p 'leaf)
    (package-refresh-contents)
    (package-install 'leaf))

  (leaf leaf-keywords
    :ensure t
    :init
    ;; optional packages if you want to use :hydra, :el-get, :blackout,,,
    (leaf hydra :ensure t)
    (leaf el-get :ensure t)
    (leaf blackout :ensure t)
    (leaf diminish :ensure t)

    :config
    ;; initialize leaf-keywords.el
    (leaf-keywords-init)))

(leaf leaf-tree :ensure t)
(leaf leaf-convert :ensure t
  :config (leaf use-package :ensure t))
(leaf transient-dwim
  :ensure t
  :bind (("M-=" . transient-dwim-dispatch)))

(eval-when-compile
    (require 'use-package))

;; On non-Guix systems, "ensure" packages by default
(setq use-package-always-ensure t)
(leaf quelpa
  :ensure t
  :require t)


(leaf no-littering
  :ensure t
  :require t)

;; no-littering doesn't set this by default so we must place
;; auto save files in the same path as it uses for sessions
(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

;; (use-package auto-package-update
;;   :custom
;;   (auto-package-update-interval . 7)
;;   (auto-package-update-prompt-before-update . t)
;;   (auto-package-update-hide-results . t)
;;   :config
;;   (auto-package-update-maybe)
;;   (auto-package-update-at-time "09:00"))

(leaf exec-path-from-shell
:ensure t
:require t
:config
(exec-path-from-shell-initialize))

;; Thanks, but no thanks
    (setq inhibit-startup-message t)

    ;;(scroll-bar-mode -1)        ; Disable visible scrollbar
    (tool-bar-mode -1)          ; Disable the toolbar
    (tooltip-mode -1)           ; Disable tooltips
;;    (set-fringe-mode 10)       ; Give some breathing room
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

(leaf general
  :ensure t
  :bind (("C-M-j" . counsel-switch-buffer))
  :require t
  :config
  (general-create-definer rune/leader-keys :keymaps
    '(normal insert visual emacs)
    :prefix "SPC" :global-prefix "C-SPC"))

(leaf dashboard
   :ensure t
   :config (dashboard-setup-startup-hook)
     )

(leaf helpful
  :custom
  (counsel-describe-function-function . #'helpful-callable)
  (counsel-describe-variable-function . #'helpful-variable)
  :bind
  ([remap describe-function] . helpful-function)
  ([remap describe-symbol] . helpful-symbol)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-command] . helpful-command)
  ([remap describe-key] . helpful-key))

(leaf doom-themes
  :ensure t
  :require t
  :config
  (load-theme 'doom-dracula t))

(leaf rainbow-delimiters
  :ensure t
  :hook (prog-mode-hook))

(leaf all-the-icons
  :ensure t)

(leaf doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-hight . 15))
  )

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

;; (use-package ivy
;;   :diminish
;;   :bind (("C-s" . swiper)
;; 	 :map ivy-minibuffer-map
;; 	 ("TAB" . ivy-alt-done)
;; 	 ("C-f" . ivy-alt-done)
;; 	 ("C-l" . ivy-alt-done)
;; 	 ("C-j" . ivy-next-line)
;; 	 ("C-k" . ivy-previous-line)
;; 	 :map ivy-switch-buffer-map
;; 	 ("C-k" . ivy-previous-line)
;; 	 ("C-l" . ivy-done)
;; 	 ("C-d" . ivy-switch-buffer-kill)
;; 	 :map ivy-reverse-i-search-map
;; 	 ("C-k" . ivy-previous-line)
;; 	 ("C-d" . ivy-reverse-i-search-kill))
;;   :config
;;   (ivy-mode t))

;; (use-package ivy-rich
;;   :init
;;   (ivy-rich-mode 1)
;;   )

(leaf ivy
  :diminish
  :bind (("C-s" . swiper)

	 (:ivy-minibuffer-map
	  ("TAB" . ivy-alt-done)
	  ("C-f" . ivy-alt-done)
	  ("C-l" . ivy-alt-done)
	  ("C-j" . ivy-next-line)
	  ("C-k" . ivy-previous-line))

	 (:ivy-switch-buffer-map
	  ("C-k" . ivy-previous-line)
	  ("C-l" . ivy-done)
	  ("C-d" . ivy-switch-buffer-kill))

	 (:ivy-reverse-i-search-map
	  ("C-k" . ivy-previous-line)
	  ("C-d" . ivy-reverse-i-search-kill))
	 )
  :config
  (ivy-mode t)
  )

(leaf ivy-rich
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

(leaf evil
  :ensure t
  :require t
  :bind ((evil-emacs-state-map
	  ("C-h" . evil-delete-backward-char-and-join)
	  ("<escape>" . evil-normal-state))
	 (evil-normal-state-map
	  ("C-f" . evil-forward-char)
	  ("C-b" . evil-backward-char)
	  ("C-n" . evil-next-visual-line)
	  ("C-p" . evil-previous-visual-line))
	 (evil-visual-state-map
	  ("C-f" . evil-forward-char)
	  ("C-b" . evil-backward-char)
	  ("C-n" . evil-next-visual-line)
	  ("C-p" . evil-previous-visual-line))
	 (evil-insert-state-map
	  ("C-g" . evil-normal-state)))

  :pre-setq (evil-want-keybinding . nil)
  :setq (
	 (evil-want-integration . t)	    
	 (evil-want-C-i-jump . nil)
     (evil-normal-state-cursor . '("cyan" box))
     (evil-emacs-state-cursor . '("orange" box)))
  :setq-default ((evil-cross-lines . t))
  :config
  (evil-mode 1)
  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))
(defalias 'evil-insert-state 'evil-emacs-state)


(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(leaf lsp-mode
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

(use-package lsp-ivy
  :defer t)

(use-package evil-nerd-commenter
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))

(leaf python-mode
  :ensure t
  :hook (python-mode-hook . lsp-deferred)
  :custom (python-shell-interpreter . "python3")
  )

;; (use-package lsp-docker
;;   :defer t
;;   :custom
;;   (defvar lsp-docker-client-packages '(lsp-clients lsp-bash lsp-pyls))

;;   (setq lsp-docker-client-configs
;; 	'((:server-id bash-ls :docker-server-id bashls-docker :server-command "bash-language-server start")
;; 	  (:server-id dockerfile-ls :docker-server-id dockerfilels-docker :server-command "docker-langserver --stdio")
;; 	  (:server-id pyls :docker-server-id pyls-docker :server-command "pyls")
;; 	  ))

;;   (lsp-docker-init-clients
;;    :path-mappings '(("path-to-projects-you-want-to-use" . "/projects"))
;;    :client-packages lsp-docker-client-packages
;;    :client-configs lsp-docker-client-configs)
;;   )

;; (set-language-environment "UTF-8")

;; (require 'eglot)
;; (add-hook 'julia-mode-hook 'eglot-ensure)

;; (require 'julia-mode)
;; (require 'julia-repl)
;; (add-hook 'julia-mode-hook 'julia-repl-mode)
;; (add-to-list 'eglot-server-programs
;;              '(julia-mode . ("julia" "-e using LanguageServer, LanguageServer.SymbolServer; runserver()")))

;; (use-package eglot
;;   :defer t)
;; (add-hook 'julia-mode-hook 'eglot-ensure)
;; (use-package julia-mode
;;   :defer t)
;; (require 'julia-repl)
;; (add-hook 'julia-mode-hook 'julia-repl-mode)
;; (add-to-list 'eglot-server-programs
;; 	     '(julia-mode . ("julia" "-e using LanguageServer, LanguageServer.SymbolServer; runserver()")))

(use-package go-mode
  :defer t)

(use-package slime
:defer t
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

(use-package yaml-mode
  :defer t)

(use-package sqlformat
  :defer t)
(setq sqlformat-command 'pgformatter)
(setq sqlformat-args '("-s2" "-g"))

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
	 ("\\.md\\'" . markdown-mode)
	 ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package markdown-preview-mode)

(use-package jupyter
  :defer t)

(use-package csv-mode
  :defer t)

;; (use-package digdag-mode
;;   :defer t)

(autoload 'yatex-mode "yatex" "Yet Another LaTeX mode" t)
(setq auto-mode-alist
      (append '(("\\.tex$" . yatex-mode)
                ("\\.ltx$" . yatex-mode)
                ("\\.cls$" . yatex-mode)
                ("\\.sty$" . yatex-mode)
                ("\\.clo$" . yatex-mode)
                ("\\.bbl$" . yatex-mode)) auto-mode-alist))

(setq YaTeX-inhibit-prefix-letter t)
(setq YaTeX-kanji-code nil)
(setq YaTeX-latex-message-code 'utf-8)
(setq YaTeX-use-LaTeX2e t)
(setq YaTeX-use-AMS-LaTeX t)
(setq YaTeX-dvi2-command-ext-alist
      '(("Preview\\|TeXShop\\|TeXworks\\|Skim\\|mupdf\\|xpdf\\|Firefox\\|Adobe" . ".pdf")))

(setq tex-command "/Library/TeX/texbin/ptex2pdf -u -l -ot '-synctex=1'");uplatex

;(setq tex-command "/Library/TeX/texbin/ptex2pdf -l -ot '-synctex=1'");platex
;(setq tex-command "/Library/TeX/texbin/platex");platex

;(setq tex-command "xelatex -synctex=1");XeLatexでコンパイル
;(setq tex-command "/Library/TeX/texbin/latex");latex

;(setq bibtex-command "/Library/TeX/texbin/latexmk -e '$latex=q/uplatex %O -synctex=1 %S/' -e '$bibtex=q/upbibtex %O %B/' -e '$biber=q/biber %O --bblencoding=utf8 -u -U --output_safechars %B/' -e '$makeindex=q/upmendex %O -o %D %S/' -e '$dvipdf=q/dvipdfmx %O -o %D %S/' -norc -gg -pdfdvi")
(setq bibtex-command (cond ((string-match "uplatex\\|-u" tex-command) "/Library/TeX/texbin/upbibtex")((string-match "platex" tex-command) "/Library/TeX/texbin/pbibtex")((string-match "lualatex\\|luajitlatex\\|xelatex" tex-command) "/Library/TeX/texbin/bibtexu")((string-match "pdflatex\\|latex" tex-command) "/Library/TeX/texbin/bibtex")(t "/Library/TeX/texbin/pbibtex")))


(setq makeindex-command (cond ((string-match "uplatex\\|-u" tex-command) "/Library/TeX/texbin/mendex")
			      ((string-match "platex" tex-command) "/Library/TeX/texbin/mendex")
			      ((string-match "lualatex\\|luajitlatex\\|xelatex" tex-command) "/Library/TeX/texbin/texindy")
			      ((string-match "pdflatex\\|latex" tex-command) "/Library/TeX/texbin/makeindex")
			      (t "/Library/TeX/texbin/mendex")))
  ;; (setq dvi2-command "/usr/bin/open -a Preview")
(setq dvi2-command "/usr/bin/open -a Skim")
(setq tex-pdfview-command "/usr/bin/open -a Skim")
(setq dviprint-command-format "/usr/bin/open -a \"Adobe Acrobat Reader DC\" `echo %s | gsed -e \"s/\\.[^.]*$/\\.pdf/\"`")

  (auto-fill-mode -1)

(use-package terraform-mode
  :hook (terraform-mode-hook . #'terraform-format-on-save-mode)
)

(use-package docker
  :ensure t
  :bind ("C-c d" . docker))

(use-package dockerfile-mode
  :defer t)

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
:defer t
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
       (jupyter . t)
       )
     )
(add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)
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
