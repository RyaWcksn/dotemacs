(setq user-full-name "Pramudya Arya Wicaksana")

(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(global-hl-line-mode 1)
(setq make-backup-file nil
      auto-save-default t)
(setq split-width-threshold 1 )

;; Y/N
(defalias 'yes-or-no-p 'y-or-n-p)

(require 'org-tempo)

(add-to-list 'org-structure-template-alist '("elc" . "src emacs-lisp" ))

;; tabs off
(setq indent-tabs-mode nil)

(show-paren-mode t)

;; Pakcages

(require 'package)
(setq package-archives
	     '(("melpa" . "https://melpa.org/packages/")
              ("org" . "https://orgmode.org/elpa/")
              ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)

(package-install 'use-package)
(package-refresh-contents)

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
   (package-install 'use-package))

(require 'use-package)

(use-package general
  :ensure t)
  :config
  (general-create-definer rune/leader-keys
    :keymaps '(normal visual emacs)
    :prefix "SPC"
    :global-prefix "SPC")

  (rune/leader-keys
    "t"  '(:ignore t :which-key "Toggles")
    "tt" '(counsel-load-theme :which-key "Choose theme")
    ";" '(counsel-M-x :which-key "Meta")
    
    "w"  '(:ignore t :which-key "Window")
    "ws" '(evil-save :which-key "Save")
    "wj" '(evil-window-down :which-key "Go Bottom")
    "wk" '(evil-window-top :which-key "Go Top")
    "wh" '(evil-window-left :which-key "Go Left")
    "wl" '(evil-window-right :which-key "Go Right")
    "wb" '(evil-window-split :which-key "Split")
    "wv" '(evil-window-vsplit :which-key "Vsplit")
    "wq" '(delete-window :which-key "Quit")
    "wb" '(counsel-switch-buffer :which-key "Switch Buffer")

    "g" '(:ignore t :which-key "Git")
    "gs" '(magit-status :which-key "Magit")

    "o" '(:ignore t :which-key "Open")
    "ot" '(:ignore t :which-key "Treemacs")
    "otd" '(treemacs-select-directory :which-key "Select Directory")
    "ote" '(treemacs :which-key "Toggle Treemacs")

    "oo" '(:ignore t :which-key "Org")
    "ooa" '(org-agenda :which-key "Org Agenda")
    )

(use-package magit
  :ensure t
  :custom
  (magit-displey-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(general-define-key
 "M-x" 'counsel-M-x)

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :ensure t
  :after evil
  :config
  (evil-collection-init))

(use-package counsel
  :ensure t)




;; Paredit
(use-package paredit :ensure t)

;; Smex
(use-package smex
  :ensure t
  :config
  (smex-initialize))

;; LSP Mode


;; LSP UI
;; Company
;; DAP
;; Flycheck
(use-package flycheck :ensure t)
(use-package dap-mode :ensure t)
(use-package company
  :ensure t
  :config (progn
            ;; don't add any dely before trying to complete thing being typed
            ;; the call/response to gopls is asynchronous so this should have little
            ;; to no affect on edit latency
            (setq company-idle-delay 0)
            ;; start completing after a single character instead of 3
            (setq company-minimum-prefix-length 1)
            ;; align fields in completions
            (setq company-tooltip-align-annotations t)
            )
  )
  (company-mode 1)
  
(use-package which-key
  :ensure t
  :diminish (which-key-mode)
  :config
  (setq which-key-idle-delay 0.3)
  (which-key-mode 1))
(use-package yasnippet :ensure t
  :config
  (add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets/yasnippet-golang/go-mode/")
  (yas-global-mode 1)
  )

(use-package dash :ensure t)
(use-package ivy
  :ensure t
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

(use-package hydra :ensure t)
(use-package json-mode :ensure t)
(use-package key-chord :ensure t)

;; Dashboard
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook))

;; Set the title
(setq dashboard-banner-logo-title "Welcome to Emacs Dashboard")
;; Set the banner
(setq dashboard-startup-banner "~/.emacs.d/hikaru.png" )
;; Value can be
;; - nil to display no banner
;; - 'official which displays the official emacs logo
;; - 'logo which displays an alternative emacs logo
;; - 1, 2 or 3 which displays one of the text banners
;; - "path/to/your/image.gif", "path/to/your/image.png" or "path/to/your/text.txt" which displays whatever gif/image/text you would prefer
;; - a cons of '("path/to/your/image.png" . "path/to/your/text.txt")

;; Content is not centered by default. To center, set
(setq dashboard-center-content t)

;; To disable shortcut "jump" indicators for each section, set
(setq dashboard-show-shortcuts nil)

(load-theme 'doom-horizon)

;; Keybind

(use-package magit
  :bind ("C-x g" . magit-status)
  :ensure t
  :config
  (add-hook 'after-save-hook 'magit-after-save-refresh-status))

(unless (package-installed-p 'evil)
  (package-install 'evil))


(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024)
      company-idle-delay 0.0
      comany-minimum-prefix-length 1
      create-lockfiles nil)

(with-eval-after-load 'lsp-mode
  (require 'dap-chrome)
  (setq lsp-modeline-diagnostics-enable t)
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
  (yas-global-mode))

(use-package ido-vertical-mode
  :ensure t
  :init
  (require 'ido)
  (ido-mode t)
  (setq ido-enable-prefix nil
        ido-enable-flex-matching t
        ido-case-fold nil
        ido-auto-merge-work-directories-length -1
        ido-create-new-buffer 'always
        ido-use-filename-at-point nil
        ido-max-prospects 10)
  (require 'ido-vertical-mode)
  (ido-vertical-mode)

  (require 'dash)
  (defun my/ido-go-straight-home ()
    (interactive)
    (cond
     ((looking-back "~/") (insert "Developer/"))
     ((looking-back "~/") (insert "Developer/"))
     (:else (call-interactively 'self-insert-command))))
  (defun my/setup-ido ()
    (define-key ido-file-completion-map (kbd "~") 'my/ido-go-straight-home)
    (define-key ido-file-completion-map (kbd "C-~") 'my/ido-go-straight-home))

  (add-hook 'ido-setup-hook 'my/setup-ido)
  (add-to-list 'ido-ignore-directories "node_modules"))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

(column-number-mode)
(global-display-line-numbers-mode t)

(use-package ivy-rich
  :ensure t
  :init
 (ivy-rich-mode 1))

(use-package hydra
  :ensure t)

(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

(rune/leader-keys
  "ts" '(hydra-text-scale/body :which-key "scale text"))

(use-package all-the-icons
  :ensure t)

(defun efs/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

;; Org Mode Configuration ------------------------------------------------------

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
    (set-face-attribute (car face) nil :font "Fira Code Retina" :weight 'regular :height (cdr face)))

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
  (setq org-ellipsis " ▾")
  (efs/org-font-setup)
  (setq org-agenda-files
	'("~/Orgs/agenda.org")))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(defun efs/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . efs/org-mode-visual-fill))


(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  (setq lsp-keymap-prefix (SPC l)
  :config
  (setq lsp-headerline-breadcrumb-enable nil)
  (lsp-enable-which-key-integration t))

  (use-package lsp-ui
    :ensure t
    :commands lsp-ui-mode))

 (defun ime-go-before-save ()
  (interactive)
  (when lsp-mode
    (lsp-organize-imports)
    (lsp-format-buffer)))

(package-install 'go-mode)

(add-hook 'go-mode-hook #'lsp-deferred)
(add-hook 'go-mode-hook #'yas-minor-mode)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
 (python . t)))

(setq org-confirm-babel-evaluate nil)

(use-package go-mode
  :ensure t
  :bind (
         ;; If you want to switch existing go-mode bindings to use lsp-mode/gopls instead
         ;; uncomment the following lines
         ;; ("C-c C-j" . lsp-find-definition)
         ;; ("C-c C-d" . lsp-describe-thing-at-point)
         )
  :hook ((go-mode . lsp-deferred)
         (before-save . lsp-format-buffer)
         (before-save . lsp-organize-imports))
  :config
  (setq gofmt-command "goimports"))

(provide 'gopls-config)

;; Snippet

(use-package smudge
  :ensure t)
(setq smudge-oauth2-client-secret "463ea6db52404a62a9fd97b9428da25a")
(setq smudge-oauth2-client-id "d96cacf178594a9bab92506eea93b7bf")
(define-key smudge-mode-map (kbd "C-c .") 'smudge-command-map)
(setq smudge-transport 'connect)

(use-package doom-themes
  :ensure t)

(with-eval-after-load 'evil
    (evil-define-key '(normal visual) 'lsp-mode
      (kbd "SPC l") lsp-command-map)
    (evil-normalize-keymaps))
(load "~/.emacs.d/go-snippets/go-snippets.el" :after yasnippet)

(defhydra yt-hydra/help (:color blue :hint nil)
  "
_mp_ magit-push #_mc_ magit-commit #_md_ magit diff #_mla_ magit diff #_mla_ magit status
"
  ;;Magit part
  ("mp" magit-push)
  ("mc" magit-commit)
  ("md" magit-diff)
  ("mla" magit-log-all)
  ("ms" magit-status)
  )
(global-set-key (kbd "<f1>") 'yt-hydra/help/body)

(setq org-capture-templates
       '(("t" "todo" entry (file org-default-notes-file)
	  "* TODO %?\n%u\n%a\n" :clock-in t :clock-resume t)
	 ("m" "Meeting" entry (file org-default-notes-file)
	  "* MEETING with %? :MEETING:\n%t" :clock-in t :clock-resume t)
	 ("d" "Diary" entry (file+datetree "~/org/diary.org")
	  "* %?\n%U\n" :clock-in t :clock-resume t)
	 ("i" "Idea" entry (file org-default-notes-file)
	  "* %? :IDEA: \n%t" :clock-in t :clock-resume t)
	 ("n" "Next Task" entry (file+headline org-default-notes-file "Tasks")
	  "** NEXT %? \nDEADLINE: %t") ))
