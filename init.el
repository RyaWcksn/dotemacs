(setq user-full-name "Pramudya Arya Wicaksana")

(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(global-hl-line-mode 1)
(setq make-backup-file nil
      auto-save-default t)

;; Y/N
(defalias 'yes-or-no-p 'y-or-n-p)

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
(setq use-package-always-ensure t)

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
    "ws" '(evil-save :which-key "Save")
    ";" '(counsel-M-x :which-key "Meta")

    "g" '(:ignore t :which-key "Git")
    "gs" '(magit-status :which-key "Magit"))

(use-package magit
  :ensure t
  :custom
  (magit-displey-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package evil-magit
  :ensure t
  :after magit)

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
(use-package lsp-mode
  :ensure t
  :config
  (setq lsp-headerline-breadcrumb-enable nil))


;; LSP UI
;; Company
;; DAP
;; Flycheck
(use-package flycheck :ensure t)
(use-package lsp-ui :ensure t)
(use-package dap-mode :ensure t)
(use-package company
  :ensure t
  :config
  (company-mode 1)
  )
  
(use-package which-key
  :ensure t
  :diminish (which-key-mode)
  :config
  (setq which-key-idle-delay 0.3)
  (which-key-mode 1))
(use-package yasnippet :ensure t)
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
(setq dashboard-startup-banner "~/.emacs.d/mayu.png" )
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

(load-theme 'tango-dark)

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
  (setq org-ellipsis " ▾")
  (efs/org-font-setup))

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

