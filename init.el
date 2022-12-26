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

  ;; tabs off
  (setq indent-tabs-mode nil)

  (show-paren-mode t)
  
(require 'org-tempo)

(add-to-list 'org-structure-template-alist '("elc" . "src emacs-lisp"))

(setq make-backup-files nil)

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook))

;; Set the title
(setq dashboard-banner-logo-title "Personal Development Environment")
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

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

(use-package centaur-tabs
  :ensure t
  :demand
  :config
  (centaur-tabs-mode t)
  :bind
  ("C-<prior>" . centaur-tabs-backward)
  ("C-<next>" . centaur-tabs-forward))

(use-package ivy-rich
  :ensure t
  :init
 (ivy-rich-mode 1))

(use-package counsel
  :ensure t)

(general-define-key
 "M-x" 'counsel-M-x)

(use-package all-the-icons
  :ensure t)

(use-package doom-themes 
:ensure t
:init (load-theme 'doom-horizon t)
)

(setq explicit-shell-file-name "/usr/bin/zsh")
(setq shell-file-name "zsh")
(setq explicit-zsh-args '("--login" "--interactive"))
(defun zsh-shell-mode-setup ()
  (setq-local comint-process-echoes t))
(add-hook 'shell-mode-hook #'zsh-shell-mode-setup)

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
    "wc" '(evil-window-split :which-key "Split")
    "wv" '(evil-window-vsplit :which-key "Vsplit")
    "wq" '(delete-window :which-key "Quit")
    "wb" '(counsel-switch-buffer :which-key "Switch Buffer")

    "g" '(:ignore t :which-key "Git")
    "gs" '(magit-status :which-key "Magit")

    "o" '(:ignore t :which-key "Open")
    "ot" '(:ignore t :which-key "Treemacs")
    "otd" '(treemacs-select-directory :which-key "Select Directory")
    "ote" '(treemacs :which-key "Toggle Treemacs")

    "oa" '(org-agenda :which-key "Org Agenda")
    "oc" '(cfw:open-org-calendar :which-key "Calendar")
    "oe" '(neotree :which-key "Neotree")
    )

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

(use-package paredit :ensure t)

(use-package evil-collection
  :ensure t
  :after evil
  :config
  (evil-collection-init))

(use-package which-key
  :ensure t
  :diminish (which-key-mode)
  :config
  (setq which-key-idle-delay 0.3)
  (which-key-mode 1))

(use-package hydra
  :ensure t)

(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

(rune/leader-keys
  "ts" '(hydra-text-scale/body :which-key "scale text"))

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status)
  :custom
  (magit-displey-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package flycheck :ensure t)

(use-package dap-mode :ensure t)

;; Automatically tangle our Emacs.org config file when we save it
(defun efs/org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
                      (expand-file-name "~/Projects/Code/emacs-from-scratch/Emacs.org"))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'efs/org-babel-tangle-config)))

(require 'org-habit)
(add-to-list 'org-modules 'org-habit)
(setq org-habit-graph-column 60)


(setq org-todo-keywords
    '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
      (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)")))

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

(use-package org
  :hook (org-mode . efs/org-mode-setup)
  :config
  (setq org-ellipsis " ▾")
  (efs/org-font-setup)
  (setq org-agenda-files
	'("~/Orgs/")))

(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  (setq lsp-keymap-prefix ("C-c l")
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

(setq package-selected-packages 
  '(dart-mode lsp-mode lsp-dart lsp-treemacs flycheck company
    ;; Optional packages
    lsp-ui company hover))

(add-to-list 'lsp-dart-sdk-dir "~/Android/flutter/bin/cache/dart-sdk/")

(setq lsp-dart-sdk-dir "~/Android/flutter/bin/cache/dart-sdk/")

(when (cl-find-if-not #'package-installed-p package-selected-packages)
  (package-refresh-contents)
  (mapc #'package-install package-selected-packages))

(add-hook 'dart-mode-hook 'lsp)

(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024))

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

(add-hook 'go-mode-hook #'lsp-deferred)
(add-hook 'go-mode-hook #'yas-minor-mode)

(use-package go-fill-struct :ensure t)
(use-package go-add-tags :ensure t)
(use-package go-gen-test :ensure t)

;; Golang related setup
(rune/leader-keys
  "c"  '(:ignore t :which-key "Code")
  "cg"  '(:ignore t :which-key "Golang")
  "cgg" '(go-add-tags :which-key "Go add tags")
  "cgf" '(go-fill-struct :which-key "Go fill struct")
  "cgt" '(go-gen-test-all :which-key "Go gen tests"))
  
(custom-set-variables '(go-add-tags-style 'lower-camel-case))

(use-package smudge
  :ensure t)
(setq smudge-oauth2-client-secret "463ea6db52404a62a9fd97b9428da25a")
(setq smudge-oauth2-client-id "d96cacf178594a9bab92506eea93b7bf")
(define-key smudge-mode-map (kbd "C-c .") 'smudge-command-map)
(setq smudge-transport 'connect)

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
  
(company-tng-configure-default)
(add-to-list 'company-backends 'company-yasnippet)

(use-package yasnippet :ensure t
  :config
  (add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets/yasnippet-golang/go-mode/")
  (yas-global-mode 1)
  )

(use-package elcord :ensure t)
(elcord-mode)

(use-package emms :ensure t)
(require 'emms-setup)
(emms-all)
(setq emms-player-list '(emms-player-mpv))
(setq emms-source-file-default-directory "~/Music/")

(use-package calfw :ensure t)
(use-package calfw-org :ensure t)
(use-package calfw-ical :ensure t)
(use-package calfw-cal :ensure t)
(require 'calfw)
(require 'calfw-org)
(require 'calfw-cal)
(require 'calfw-ical)


(defun my-open-calendar ()
  (interactive)
  (cfw:open-calendar-buffer
   :contents-sources
   (list
    (cfw:org-create-source "Green")  ; orgmode source
    (cfw:howm-create-source "Blue")  ; howm source
    (cfw:cal-create-source "Orange") ; diary source
    (cfw:ical-create-source "Moon" "~/moon.ics" "Gray")  ; ICS source1
    (cfw:ical-create-source "gcal" "https://calendar.google.com/calendar/ical/pramudyaarya%40ayoconnect.id/public/basic.ics" "IndianRed") ; google calendar ICS
   )))

(use-package neotree :ensure t)
(setq centaur-tabs-set-icons t)

(setq centaur-tabs-set-bar 'under)
;; Note: If you're not using Spacmeacs, in order for the underline to display
;; correctly you must add the following line:
(setq x-underline-at-descent-line t)
