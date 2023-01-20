;; Pakcages

(require 'package)
(setq package-archives
             '(("melpa" . "https://melpa.org/packages/")
              ("org" . "https://orgmode.org/elpa/")
              ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

  ;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(setq user-full-name "Pramudya Arya Wicaksana")

(setq byte-compile-warnings '(cl-functions))

  ;; To set the garbage collection threshold to high (100 MB) since LSP client-server communication generates a lot of output/garbage
(setq gc-cons-threshold 100000000)
;; To increase the amount of data Emacs reads from a process
(setq read-process-output-max (* 1024 1024)) 

      (when (fboundp 'tool-bar-mode)
        (tool-bar-mode -1))
      (when (fboundp 'scroll-bar-mode)
        (scroll-bar-mode -1))
      (when (fboundp 'tool-tip-mode)
        (tool-tip-mode -1))
      (when (fboundp 'menu-bar-mode)
        (menu-bar-mode -1))
      (global-hl-line-mode 1)
      (setq make-backup-file nil
            auto-save-default t)

    (setq split-height-threshold nil)
  (setq split-width-threshold 160)

      ;; Y/N
      (defalias 'yes-or-no-p 'y-or-n-p)

      ;; tabs off
      (setq indent-tabs-mode nil)

      (show-paren-mode t)

      (require 'org-tempo)

      (add-to-list 'org-structure-template-alist '("elc" . "src emacs-lisp"))

      (setq make-backup-files nil)

      (set-face-attribute 'default nil :height 120)
      (display-battery-mode 1)
      (display-time-mode 1)
      (hl-line-mode)

      (global-display-line-numbers-mode 1)
      (setq display-line-numbers-type 'relative)

      (let ((path (shell-command-to-string ". ~/.zshrc; echo -n $PATH")))
        (setenv "PATH" path)
        (setq exec-path 
              (append
               (split-string-and-unquote path ":")
               exec-path)))

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook))

(setq dashboard-banner-logo-title "Personal Development Environment")
(setq dashboard-startup-banner "~/.emacs.d/hikaru.png" )
(setq dashboard-center-content t)
(setq dashboard-show-shortcuts nil)
(setq dashboard-items '((recents  . 5)
                        (bookmarks . 5)
                        (projects . 5)
                        (agenda . 5)
                        (registers . 5)))

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
  ("C-c <left>" . centaur-tabs-backward)
  ("C-c <right>" . centaur-tabs-forward))

(use-package ivy-rich
  :ensure t
  :init
 (ivy-rich-mode 1))

(use-package counsel
  :ensure t)

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
      "/" '(comment-region :which-key "Comment region")

      "w"  '(:ignore t :which-key "Window")
      "ws" '(evil-save :which-key "Save")
      "wj" '(evil-window-down :which-key "Go Bottom")
      "wk" '(evil-window-up :which-key "Go Top")
      "wh" '(evil-window-left :which-key "Go Left")
      "wl" '(evil-window-right :which-key "Go Right")
      "wc" '(evil-window-split :which-key "Split")
      "wv" '(evil-window-vsplit :which-key "Vsplit")
      "wq" '(delete-window :which-key "Quit")
      "wb" '(counsel-switch-buffer :which-key "Switch Buffer")

      "p"  '(:ignore t :which-key "Projectile")
      "pp" '(projectile-command-map :which-key "Command map")

      "f"  '(:ignore t :which-key "Find")
      "ff" '(projectile-find-file :which-key "Find File")


      "o" '(:ignore t :which-key "Open")

      "oa" '(org-agenda :which-key "Org Agenda")
      "oc" '(cfw:open-org-calendar :which-key "Calendar")
      "oe" '(neotree :which-key "Neotree")

      "C-c [" '(hs-hide-block :which-key "Fold")
      "C-c ]" '(hs-show-block :which-key "Unfold")

      "<left>" '(centaur-tabs-backward :which-key "Previous tab")
      "<right>" '(centaur-tabs-forward :which-key "Next tab")

      "b" '(:ignore :override t :which-key "Buffer")

      "bb" '(counsel-switch-buffer :which-key "Switch buffer")
      "bk" '(kill-buffer :which-key "Kill buffer")
      )
(general-auto-unbind-keys t)
(define-key minibuffer-local-completion-map (kbd "SPC") 'self-insert-command)

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
(use-package evil-escape
  :init
  (evil-escape-mode)
  :config
  (setq-default evil-escape-key-sequence "jk")
  )

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

(defun open-magit-in-vertical-split ()
  (interactive)
  (magit-status))

(rune/leader-keys
    "g" '(:ignore t :which-key "Git")
    "gs" '(open-magit-in-vertical-split :which-key "Magit"))

(use-package flycheck :ensure t)

(use-package dap-mode
	 :ensure t
  ;; Uncomment the config below if you want all UI panes to be hidden by default!
   ;; :custom
   ;; (lsp-enable-dap-auto-configure nil)
   ;; :config
   ;; (dap-ui-mode 1)
   :commands dap-debug
   :config
(dap-tooltip-mode 1)
;; use tooltips for mouse hover
;; if it is not enabled `dap-mode' will use the minibuffer.
(tooltip-mode 1)
;; displays floating panel with debug buttons
;; requies emacs 26+
(dap-ui-controls-mode 1)
   ;; Set up Node debugging
   (require 'dap-node)
   (dap-node-setup) ;; Automatically installs Node debug adapter if needed
   (require 'dap-dlv-go)
   (require 'dap-hydra)
   (require 'dap-gdb-lldb)
   (dap-gdb-lldb-setup)
   (general-define-key
    :keymaps 'lsp-mode-map
    :prefix lsp-keymap-prefix
    "d" '(dap-hydra t :wk "debugger"))
	 )

  (rune/leader-keys
    "d"  '(:ignore t :which-key "Debugging")
    "ds" '(dap-debug t :wk "Start debug")
    "db" '(dap-breakpoint-toggle t :wk "Toggle breakpoint")
    "dd" '(dap-hydra t :wk "Debugger"))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (shell . t)
   (js . t)
   ))

(setq org-agenda-custom-commands
      '(("h" "Daily habits" 
         ((agenda ""))
         ((org-agenda-show-log t)
          (org-agenda-ndays 7)
          (org-agenda-log-mode-items '(state))
          (org-agenda-skip-function '(org-agenda-skip-entry-if 'notregexp ":DAILY:"))))
        ;; other commands here
        ))

;; Automatically tangle our Emacs.org config file when we save it
(defun efs/org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
                      (expand-file-name "~/.emacs.d/config.org"))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'efs/org-babel-tangle-config)))

(require 'org-habit)
(add-to-list 'org-modules 'org-habit)

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
  (dolist (face '((org-level-1 . 1.4)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Fira Code Retina" :weight 'regular :height 180 ))

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

(use-package org
    :hook (org-mode . efs/org-mode-setup)
    :config
    (setq org-ellipsis " ▾")
    (efs/org-font-setup)
    (setq org-agenda-files
          '("~/Orgs/")))
(defun nolinum ()
  (global-display-line-numbers-mode 0)
)
(add-hook 'org-mode-hook 'nolinum)

(use-package org-journal
:defer t
 :init
  ;; Change default prefix key; needs to be set before loading org-journal
  (setq org-journal-prefix-key "C-c j ")
  :config
  (setq org-journal-dir "~/Orgs/Journal/"
  org-journal-date-format "%A, %d %B %Y")
  )
  
  (rune/leader-keys
  "j"  '(:ignore t :which-key "Journal")
  "jj" '(org-journal-new-entry :which-key "Write journal")
  )

(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory (file-truename "~/Orgs/roam"))
  (org-roam-capture-templates
   '(("d" "default" plain
      "%?"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)

     ("l" "programming language" plain
      "* Characteristics\n\n- Family: %?\n- Inspired by: \n\n* Reference:\n\n"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)
     ("p" "project" plain "* Goals\n\n%?\n\n* Tasks\n\n** TODO Add initial tasks\n\n* Dates\n\n"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+filetags: Project")
      :unnarrowed t)
     ("m" "morning routine" plain
      "* Morning Routine\n\n** TODO 20 minutes workout\n\n** TODO 20 minutes read book\n\n* ** TODO 15 minutes meditation\n\n"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+filetags: Daily")
      :unnarrowed t)
     ("b" "book" plain "\n* Source\n\nAuthor: %^{Author}\nTitle: ${title}\nYear: %^{Year}\n\n* Summary\n\n%?"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+filetags: Books")
      :unnarrowed t)
   ))
  (org-roam-dailies-capture-templates
   '(("d" "default" entry "* %<%I:%M %p>: %?"
     :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n"))

     ("m" "morning routine" plain
      "* Morning Routine\n\n** TODO 20 minutes workout\n\n** TODO 20 minutes read book\n\n** TODO 15 minutes meditation\n\n** Today Agenda\n\n\n"
      :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>"))

     ("e" "evening reflection" plain
      "* Evening Reflection\n\n** What went well?\n\n** What did i learn today?\n\n** What could gone better?\n\n\n"
      :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>"))

     ("p" "project" plain "* Goals\n\n%?\n\n* Tasks\n\n** TODO Add initial tasks\n\n* Dates\n\n"
      :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>"))
   ))
  :config
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  (require 'org-roam-protocol)


  )

(rune/leader-keys
  "r"  '(:ignore t :which-key "Roam")
  "rj" '(org-roam-dailies-capture-today :which-key "Capture today")
  "rd" '(org-roam-dailies-find-directory :which-key "Journal directory")
  "ri" '(org-roam-node-insert :which-key "Node insert")
  "rf" '(org-roam-node-find :which-key "Node find")
  "rc" '(org-roam-capture :which-key "Capture")
  "rl" '(org-roam-buffer-toggle :which-key "Buffer toggle")
  "rg" '(org-roam-ui-open :which-key "Graph"))

(use-package org-roam-ui
  :after org-roam
  ;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
  ;;         a hookable mode anymore, you're advised to pick something yourself
  ;;         if you don't care about startup time, use
  ;;  :hook (after-init . org-roam-ui-mode)
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp
  )
  (defun ime-go-before-save ()
    (interactive)
    (when lsp-mode
      (lsp-organize-imports)
      (lsp-format-buffer)))

(setq lsp-completion-provider :none)
(setq lsp-ui-doc-show-with-cursor t)

 (use-package lsp-ui
  :ensure t
  :config
  (setq lsp-ui-sideline-ignore-duplicate t)
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

(rune/leader-keys
  "l"  '(:ignore t :which-key "LSP")
  "lg" '(lsp-goto-type-definition :which-key "Go to definition")
  "li" '(lsp-goto-implementation :which-key "Go to implementation")
  "lc" '(lsp-execute-code-action :which-key "Code action")
  "ll" '(lsp-avy-lens :which-key "Code lens")
  "lr" '(lsp-rename :which-key "Code lens")
  "ld" '(lsp-ui-peek-find-definitions :which-key "Goto declaration")
  "la" '(lsp-ui-peek-find-implementation :which-key "Code implement"))

(setq package-selected-packages 
    '(dart-mode lsp-mode lsp-dart lsp-treemacs flycheck company
      ;; Optional packages
      lsp-ui company hover))

  (use-package dart-mode)

;; export ANDROID_HOME=$HOME/Android
;; export PATH=$ANDROID_HOME/cmdline-tools/tools/bin/:$PATH
;; export PATH=$ANDROID_HOME/platform-tools/:$PATH

;; export PATH="$PATH:$HOME/Android/flutter/bin/"


  (setq lsp-dart-sdk-dir "~/Android/flutter/bin/cache/dart-sdk/")


  (add-hook 'dart-mode-hook 'lsp)

(use-package go-mode
  :ensure t
  :hook
  (
   (go-mode . lsp-deffered)
   (go-mode . company-mode)
   )
  :config
  (setq gofmt-command "gofmt")
  (require 'lsp-go)
  (setq lsp-go-analyses
        '((fieldalignment . t)
          (nilness . t)
          (httpresponse . t)
          (unusedwrite . t)
          (unusedparams . t)
          ))
  )

(provide 'gopls-config)

(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)
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

(use-package flycheck-golangci-lint
  :hook (go-mode . flycheck-golangci-lint-setup)
  :config
  (setq flycheck-golangci-lint-test t)
  (setq flycheck-golangci-lint-enable-all t)
  (setq flycheck-golangci-lint-disable-linters '("unused" "staticcheck" "misspell"))
  )

(use-package smudge
  :ensure t)
(setq smudge-oauth2-client-secret "463ea6db52404a62a9fd97b9428da25a")
(setq smudge-oauth2-client-id "d96cacf178594a9bab92506eea93b7bf")
(define-key smudge-mode-map (kbd "C-c .") 'smudge-command-map)
(setq smudge-transport 'connect)

(use-package company
   :after lsp-mode
   :hook (lsp-mode . company-mode)
   :bind (
          :map company-active-map
          ("<tab>" . company-complete-common-or-cycle)
          ("C-j" . company-select-next-or-abort)
          ("C-k" . company-select-previous-or-abort)
          ("C-l" . company-other-backend)
          ("C-h" . nil)
          )
          (:map lsp-mode-map
          ("<tab>" . company-indent-or-complete-common))
   :custom
   (company-minimum-prefix-length 1)
   (company-idle-delay 0.0))

 (setq company-backends '((company-capf company-yasnippet)))

(use-package yasnippet :ensure t
  :config
  (add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets/yasnippet-golang/")
  (yas-global-mode 1)
  )

(use-package elcord :ensure t)

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
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))
(setq centaur-tabs-set-icons t)

(setq centaur-tabs-set-bar 'under)
;; Note: If you're not using Spacmeacs, in order for the underline to display
;; correctly you must add the following line:
(setq x-underline-at-descent-line t)

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy)))
  ;; NOounsTE: Set this to the folder where you keep your Git repos!

;;  (use-package wakatime-mode)
  ;; (global-wakatime-mode)
  ;;'(wakatime-api-key "waka_13d2f057-5212-4cc1-8cfa-172eca1f84c2")

(defun open-terminal-in-vertical-split ()
  (interactive)
  (split-window-below)
  (other-window 1)
  (let ((shell (if (eq system-type 'darwin) "/bin/zsh" "/usr/bin/zsh")))
    (ansi-term shell))
  )

(rune/leader-keys
  "ot" '(open-terminal-in-vertical-split :which-key "Open Terminal"))

(use-package tree-sitter
  :ensure t
  :defer t
  :hook (
         (go-mode        . tree-sitter-hl-mode)
         )

)

(use-package tree-sitter-langs
  :ensure t
  :after tree-sitter-mode
)

(use-package multiple-cursors
  :ensure t
  :defer t
  :bind (
  ("C-c C-<right>" . 'mc/mark-next-like-this-word) ; choose same word next
  ("C-c C-<left>" . 'mc/mark-previous-word-like-this) ; choose same word previous
  ("M-n" . 'mc/mark-next-like-this) ; choose char from next line same position
  ("M-m" . 'mc/mark-previous-like-this); choose char from previous line same position
  ("C-c C-_" . 'mc/mark-all-like-this)
  ("C-x M-m" . 'back-to-indentation)
  )
  :hook
  (multiple-cursors-mode . (lambda()
                             (unbind-key "<return>" mc/keymap)
                             (key-chord-define mc/keymap "ew" 'mc/keyboard-quit)
                             ))
)

(use-package markdown-mode
  :ensure t
  :defer t
  :mode "\\.\\(md\\|mdown\\|markdown\\)\\'"
  :custom
  (markdown-header-scaling t)
  :bind(:map markdown-mode-map
             ("M-n" . mc/mark-next-like-this)
             ("M-m" . mc/mark-previous-like-this))
)

(use-package helm
  :config
  (global-set-key (kbd "M-x") #'helm-M-x)
  (global-set-key (kbd "C-x C-f") #'helm-find-files)
  )
(rune/leader-keys
  "h"  '(helm-command-prefix :which-key "Helm"))
