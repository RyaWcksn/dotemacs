(require 'package)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("org" . "https://orgmode.org/elpa/")
        ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(package-refresh-contents)

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(setq user-full-name "Pramudya Arya Wicaksana")

(setq byte-compile-warnings '(cl-functions))
(setq ring-bell-function 'ignore)
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


(setq make-backup-files nil)

(set-face-attribute 'default nil :height 120)
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

(setq-default mode-line-format
                '("%e"
                  mode-line-front-space
                  mode-line-mule-info
                  mode-line-client
                  mode-line-modified
                  mode-line-remote
                  mode-line-frame-identification
                  mode-line-buffer-identification
                  " "
                  mode-line-position
                  (:eval
                   (if vc-mode
                       (let* ((noback (replace-regexp-in-string (format "^ %s" (vc-backend buffer-file-name)) " " vc-mode))
                              (face (cond ((string-match "^ -" noback) 'mode-line-vc)
                                          ((string-match "^ [:@]" noback) 'mode-line-vc-edit)
                                          ((string-match "^ [!\\?]" noback) 'mode-line-vc-modified))))
                         (format " %s" (substring noback 2)))))
                  "  "
                  mode-line-misc-info
                  mode-line-end-spaces
))

(use-package which-key
  :diminish (which-key-mode)
  :config
  (setq which-key-idle-delay 0.3)
  (which-key-mode 1))

(use-package general
  :ensure t)
  :config
  (general-create-definer neko/leader-keys
    :keymaps '(normal visual emacs)
    :prefix "SPC"
    :global-prefix "SPC")
(general-auto-unbind-keys t)
(define-key minibuffer-local-completion-map (kbd "SPC") 'self-insert-command)


(neko/leader-keys
  ";" '(helm-M-x :which-key "Meta")
  "/" '(comment-region :which-key "Comment region")
  "s" '(evil-save :which-key "Save")
  "b" '(:ignore :override t :which-key "Buffer")
  "bb" '(counsel-switch-buffer :which-key "Switch buffer")
  "bk" '(kill-buffer :which-key "Kill buffer")
  "qq" '(kill-buffer-and-window :which-key "Kill buffer")
  "ba" '(kill-other-buffers :which-key "Kill other buffer except this"))

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

(use-package evil-collection
  :ensure t
  :after evil
  :config
  (evil-collection-init))

(use-package hydra
  :ensure t)

(setq tab-bar-show 1)
(defun neko/current-tab-name ()
      (alist-get 'name (tab-bar--current-tab)))

(use-package ivy-rich
  :init
 (ivy-rich-mode 1))

(use-package all-the-icons
  :ensure t)

(use-package catppuccin-theme)
(setq catppuccin-flavor 'mocha) ;; or 'latte, 'macchiato, or 'mocha
(catppuccin-reload)
(load-theme 'catppuccin t)


(rune/leader-keys
     "t"  '(:ignore t :which-key "Theme")
     "tt" '(counsel-load-theme :which-key "Choose theme"))

(setq explicit-shell-file-name "/usr/bin/zsh")
(setq shell-file-name "zsh")
(setq explicit-zsh-args '("--login" "--interactive"))
(defun zsh-shell-mode-setup ()
  (setq-local comint-process-echoes t))
(add-hook 'shell-mode-hook #'zsh-shell-mode-setup)

(when (window-system)
  (set-frame-font "Fira Code"))
(let ((alist '((33 . ".\\(?:\\(?:==\\|!!\\)\\|[!=]\\)")
               (35 . ".\\(?:###\\|##\\|_(\\|[#(?[_{]\\)")
               (36 . ".\\(?:>\\)")
               (37 . ".\\(?:\\(?:%%\\)\\|%\\)")
               (38 . ".\\(?:\\(?:&&\\)\\|&\\)")
               (42 . ".\\(?:\\(?:\\*\\*/\\)\\|\\(?:\\*[*/]\\)\\|[*/>]\\)")
               (43 . ".\\(?:\\(?:\\+\\+\\)\\|[+>]\\)")
               (45 . ".\\(?:\\(?:-[>-]\\|<<\\|>>\\)\\|[<>}~-]\\)")
               (46 . ".\\(?:\\(?:\\.[.<]\\)\\|[.=-]\\)")
               (47 . ".\\(?:\\(?:\\*\\*\\|//\\|==\\)\\|[*/=>]\\)")
               (48 . ".\\(?:x[a-zA-Z]\\)")
               (58 . ".\\(?:::\\|[:=]\\)")
               (59 . ".\\(?:;;\\|;\\)")
               (60 . ".\\(?:\\(?:!--\\)\\|\\(?:~~\\|->\\|\\$>\\|\\*>\\|\\+>\\|--\\|<[<=-]\\|=[<=>]\\||>\\)\\|[*$+~/<=>|-]\\)")
               (61 . ".\\(?:\\(?:/=\\|:=\\|<<\\|=[=>]\\|>>\\)\\|[<=>~]\\)")
               (62 . ".\\(?:\\(?:=>\\|>[=>-]\\)\\|[=>-]\\)")
               (63 . ".\\(?:\\(\\?\\?\\)\\|[:=?]\\)")
               (91 . ".\\(?:]\\)")
               (92 . ".\\(?:\\(?:\\\\\\\\\\)\\|\\\\\\)")
               (94 . ".\\(?:=\\)")
               (119 . ".\\(?:ww\\)")
               (123 . ".\\(?:-\\)")
               (124 . ".\\(?:\\(?:|[=|]\\)\\|[=>|]\\)")
               (126 . ".\\(?:~>\\|~~\\|[>=@~-]\\)")
               )
             ))
  (dolist (char-regexp alist)
    (set-char-table-range composition-function-table (car char-regexp)
                          `([,(cdr char-regexp) 0 font-shape-gstring]))))

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy)))
  ;; NOounsTE: Set this to the folder where you keep your Git repos!

  (neko/leader-keys
     "p"  '(:ignore t :which-key "Projectile")
     "pp" '(projectile-command-map :which-key "Command map"))
     "pf" '(projectile-find-file :which-key "Find File")

(neko/leader-keys
     "w"  '(:ignore t :which-key "Window")
     "ws" '(evil-window-split :which-key "Split")
     "wj" '(evil-window-down :which-key "Go Bottom")
     "wk" '(evil-window-up :which-key "Go Top")
     "wh" '(evil-window-left :which-key "Go Left")
     "wl" '(evil-window-right :which-key "Go Right")
     "wv" '(evil-window-vsplit :which-key "Vsplit")
     "wq" '(delete-window :which-key "Quit")
     "wb" '(counsel-switch-buffer :which-key "Switch Buffer"))



(use-package helm
  :config
  (global-set-key (kbd "M-x") #'helm-M-x)
  (global-set-key (kbd "C-x C-f") #'helm-find-files)
  )

(neko/leader-keys
  "h"  '(helm-command-prefix :which-key "Helm"))

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status)
  :custom
  (magit-displey-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(defun open-magit-in-vertical-split ()
  (interactive)
  (magit-status))

(neko/leader-keys
    "g" '(:ignore t :which-key "Git")
    "gs" '(open-magit-in-vertical-split :which-key "Magit"))

(use-package blamer
  :ensure t
  :defer 20
  :custom
  (blamer-idle-time 0.3)
  (blamer-min-offset 70)
  :custom-face
  (blamer-face ((t :foreground "#7a88cf"
                    :background nil
                    :height 140
                    :italic t)))
  )
  (neko/leader-keys
      "gb" '(blamer-mode :which-key "Blame"))

(use-package hydra)
(use-package smerge-mode
  :config
  (defhydra hydra-smerge (:color red :hint nil)
    "
Navigate       Keep               other
----------------------------------------
_j_: previous  _RET_: current       _e_: ediff
_k_: next      _m_: mine  <<      _u_: undo
_j_: up        _o_: other >>      _r_: refine
_k_: down      _a_: combine       _q_: quit
               _b_: base
"
    ("k" smerge-next)
    ("j" smerge-prev)
    ("RET" smerge-keep-current)
    ("m" smerge-keep-mine)
    ("o" smerge-keep-other)
    ("b" smerge-keep-base)
    ("a" smerge-keep-all)
    ("e" smerge-ediff)
    ("J" previous-line)
    ("K" forward-line)
    ("r" smerge-refine)
    ("u" undo)
    ("q" nil :exit t))

  (defun enable-smerge-maybe ()
    (when (and buffer-file-name (vc-backend buffer-file-name))
      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward "^<<<<<<< " nil t)
          (smerge-mode +1)
          (hydra-smerge/body))))))


(neko/leader-keys
     "gm"  '(scimax-smerge/body :which-key "Toggle smerge")
     )

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

  (neko/leader-keys
    "d"  '(:ignore t :which-key "Debugging")
    "ds" '(dap-debug t :wk "Start debug")
    "db" '(dap-breakpoint-toggle t :wk "Toggle breakpoint")
    "dd" '(dap-hydra t :wk "Debugger"))

(use-package docker
  :ensure t
  :bind ("C-c d" . docker))

(use-package dockerfile-mode)

(setq org-babel-python-command "python3")
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (shell . t)
   (js . t)
   (latex . t)
   ))

(use-package org
  :hook (org-mode . efs/org-mode-setup)
  :config
  (setq org-ellipsis " ...")
  (efs/org-font-setup)
  (setq org-agenda-files
        '("~/Orgs/")))
(defun nolinum ()
  (global-display-line-numbers-mode 0)
  )
(add-hook 'org-mode-hook 'nolinum)

(global-set-key (kbd "C-c c") #'org-capture)


(setq org-capture-templates '(("t" "Todo [inbox]" entry
                               (file+headline "~/Orgs/inbox.org" "Tasks")
                               "* TODO %i%?")
                              ("T" "Tickler" entry
                               (file+headline "~/Orgs/tickler.org" "Tickler")
                               "* %i%? \n %U")
                              ("b" "Braindump" entry
                               (file+headline "~/Orgs/braindump.org" "Braindump")
                               "* %? \n")))

(setq org-refile-targets '(("~/Orgs/agenda.org" :maxlevel . 3)
                           ("~/Orgs/someday.org" :level . 1)
                           ("~/Orgs/tickler.org" :maxlevel . 2)))

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

     ("c" "Campus" plain
      "* Overview\n\n- Subject: %?\n- Lecturer: \n\n** Reference:\n\n** Notes:\n\n"
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
      "* Evening Reflection\n\n** What went well?\n\n** What did i learn today?\n\n** What could gone better?\n\n** Summary\n\n\n"
      :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>"))

     ("p" "project" plain "* Goals\n\n%?\n\n* Tasks\n\n** TODO Add initial tasks\n\n* Dates\n\n"
      :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>"))
     ))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today))
  :config
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  (require 'org-roam-protocol))

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

(require 'org-habit)
(add-to-list 'org-modules 'org-habit)

(setq org-todo-keywords
      '((sequence "TODO(t)" "WAITING(n)" "|" "DONE(d)" "CANCEL(c)")))

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

(defun efs/org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
                      (expand-file-name "~/.emacs.d/config.org"))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'efs/org-babel-tangle-config)))

(neko/leader-keys
     "o" '(:ignore t :which-key "Open")
     "oa" '(org-agenda :which-key "Org Agenda")
     "oc" '(cfw:open-org-calendar :which-key "Calendar")
     "oe" '(neotree :which-key "Neotree")
     "od" '(dired :which-key "Dired"))

(use-package posframe)


    (use-package lsp-mode
      :init
      ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
      (setq lsp-keymap-prefix "C-c l")
      :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
             ;; if you want which-key integration
             (lsp-mode . lsp-enable-which-key-integration))
      :commands lsp
      :config
          (setq lsp-intelephense-multi-root nil) ; don't scan unnecessary projects
          (with-eval-after-load 'lsp-intelephense
            (setf (lsp--client-multi-root (gethash 'iph lsp-clients)) nil))
          (define-key lsp-mode-map (kbd "C-c l") lsp-command-map)
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
      (add-hook 'lsp-mode-hook 'lsp-ui-mode)
      )

(setq lsp-ui-doc-position 'at-point)

    (use-package helm-lsp
  :ensure t
  :after (lsp-mode)
  :commands (helm-lsp-workspace-symbol)
  :init (define-key lsp-mode-map [remap xref-find-apropos] #'helm-lsp-workspace-symbol))

(neko/leader-keys
  "l"  '(:ignore t :which-key "LSP")
  "lg" '(lsp-goto-type-definition :which-key "Go to definition")
  "li" '(lsp-goto-implementation :which-key "Go to implementation")
  "lc" '(helm-lsp-code-actions :which-key "Code action")
  "ll" '(lsp-avy-lens :which-key "Code lens")
  "lr" '(lsp-rename :which-key "Code lens")
  "lf" '(lsp-format-buffer :which-key "Format buffer")
  "ld" '(lsp-ui-peek-find-definitions :which-key "Goto declaration")
  "le" '(helm-lsp-diagnostics :which-key "Error diagnostics")
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
   (go-mode . lsp-deferred)
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
(use-package godoctor)
(use-package gotest)
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

(use-package rjsx-mode
  :config
  (add-to-list 'auto-mode-alist '("components\\/.*\\.js\\'" . rjsx-mode))
  (add-to-list 'auto-mode-alist '("pages\\/.*\\.js\\'" . rjsx-mode))
  )
(add-hook 'js-mode-hook #'lsp)

(use-package react-snippets)



(use-package typescript-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.tsx.*$" . typescript-mode)))
(add-hook 'typescript-mode-hook #'lsp)

(use-package lsp-java
    :if (executable-find "mvn")
    :init
    :config (add-hook 'java-mode-hook 'lsp)
    (use-package request :defer t)
    :custom
    (lsp-java-server-install-dir (expand-file-name "~/.emacs.d/eclipse.jdt.ls/server/"))
    (lsp-java-workspace-dir (expand-file-name "~/.emacs.d/eclipse.jdt.ls/workspace/")))
  (require 'lsp-java-boot)
  (add-hook 'lsp-mode-hook #'lsp-lens-mode)
  (add-hook 'java-mode-hook #'lsp-java-boot-lens-mode)
(if (eq system-type 'darwin)
    ;; Configuration for macOS
    (progn
  (setq lsp-java-java-path
        "/Library/Java/JavaVirtualMachines/jdk-19.jdk/Contents/Home/bin/java"))
  ;; Configuration for Linux
  (progn
  (setq lsp-java-java-path
        "/opt/jdk-13.0.1/bin/java")))

(add-hook 'robot-mode-hook #'lsp)

(setq lsp-python-ms-auto-install-server t)
(use-package lsp-python-ms
:ensure t
:init (setq lsp-python-ms-auto-install-server t)
:hook (python-mode . (lambda ()
                        (require 'lsp-python-ms)
                        (lsp-deferred))))  ; or lsp-deferred
(use-package lsp-pyright
:ensure t
:hook (python-mode . (lambda ()
                        (require 'lsp-pyright)
                        (lsp))))  ; or lsp-deferred


(use-package pyvenv
  :demand t
  :config
  (setq pyvenv-workon "emacs")  ; Default venv
  (pyvenv-tracking-mode 1))  ; Automatically use pyvenv-workon via dir-locals

;; Enable sbt mode for executing sbt commands
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
   (setq sbt:program-options '("-Dsbt.supershell=false")))

  (use-package scala-mode
  :interpreter ("scala" . scala-mode))

  (use-package lsp-metals
    :ensure t
    :custom
    ;; You might set metals server options via -J arguments. This might not always work, for instance when
    ;; metals is installed using nix. In this case you can use JAVA_TOOL_OPTIONS environment variable.
    (lsp-metals-server-args '(;; Metals claims to support range formatting by default but it supports range
                              ;; formatting of multiline strings only. You might want to disable it so that
                              ;; emacs can use indentation provided by scala-mode.
                              "-J-Dmetals.allow-multiline-string-formatting=off"
                              ;; Enable unicode icons. But be warned that emacs might not render unicode
                              ;; correctly in all cases.
                              "-J-Dmetals.icons=unicode"))
    ;; In case you want semantic highlighting. This also has to be enabled in lsp-mode using
    ;; `lsp-semantic-tokens-enable' variable. Also you might want to disable highlighting of modifiers
    ;; setting `lsp-semantic-tokens-apply-modifiers' to `nil' because metals sends `abstract' modifier
    ;; which is mapped to `keyword' face.
    (lsp-metals-enable-semantic-highlighting t)
    :hook (scala-mode . lsp))

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
   (company-idle-delay 0.0)
   :config
   (add-hook 'after-init-hook 'global-company-mode)
   )

 (setq company-backends '((company-capf company-yasnippet)))

(use-package yasnippet :ensure t
  :config
  (add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets/yasnippet-golang/")
  (yas-global-mode 1)
  )

(use-package elcord :ensure t)

(use-package calfw :ensure t)
(use-package calfw-org :ensure t)
(require 'calfw-org)

(use-package neotree :ensure t)
(setq neo-smart-open t)
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))
;; Note: If you're not using Spacmeacs, in order for the underline to display
;; correctly you must add the following line:
(setq x-underline-at-descent-line t)

(defun open-terminal-in-vertical-split ()
  (interactive)
  (split-window-below)
  (other-window 1)
  (let ((shell (if (eq system-type 'darwin) "/bin/zsh" "/usr/bin/zsh")))
    (ansi-term shell))
  )

(neko/leader-keys
  "ot" '(open-terminal-in-vertical-split :which-key "Open Terminal"))

(require 'ox-latex)

(defun nd-email-filter (contents backend info)
(let ((email (plist-get info :email)))
  (replace-regexp-in-string "@EMAIL@" email contents t)))

(add-to-list 'org-export-filter-final-output-functions (function nd-email-filter))
(setq org-latex-listings 't)
(setq org-latex-listings 'minted
    org-latex-packages-alist '(("" "minted"))
    org-latex-pdf-process
    '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
      "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
      "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

(add-to-list 'org-latex-minted-langs '(ipython "python"))
(setq org-src-fontify-natively t)

(add-to-list 'org-latex-classes
             '("assignment"
               "\\documentclass[a4paper,12pt]{report}
  \\renewcommand{\\chaptername}{Lab}
  \\makeatletter
  \\renewcommand{\\maketitle}{
    \\begin{titlepage}
      \\begin{center}
        \\vspace*{2em}
        \\Huge \\textbf{ASSIGNMENT} \\\\
        \\vspace{4em}
        \\Huge \\textbf{\\@title} \\\\
        \\vspace{4em}
        \\Large \\textbf{\\@date} \\\\
        \\bigskip
        \\Large \\textbf{\\@author} \\\\
        \\Large \\textbf{\\@email} \\\\
        \\medskip
        \\large 2242805 \\\\
        \\bigskip
        \\includegraphics[width=16em]{../../manit-logo.png} \\\\
        \\bigskip
        \\large Teknik Informatika \\\\
        \\large STMIK AMIK Bandung \\\\
      \\end{center}
    \\end{titlepage}
  }
  \\makeatother
  \\usepackage[margin=0.7in]{geometry}"
               ("\\chapter{%s}" . "\\chapter*{%s}")
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))


(add-to-list 'org-latex-classes
             '("research"
               "\\documentclass[a4paper,12pt]{report}
  \\usepackage{listings}
  \\makeatletter
  \\renewcommand{\\maketitle}{
    \\begin{titlepage}
      \\begin{center}
        \\vspace*{2em}
        \\Huge \\textbf{REPORT} \\\\
        \\vspace{4em}
        \\Huge \\textbf{\\@title} \\\\
        \\vspace{4em}
        \\Large \\textbf{\\@date} \\\\
        \\bigskip
        \\Large \\textbf{\\@author} \\\\
        \\medskip
      \\end{center}
    \\end{titlepage}
  }
  \\makeatother
  \\usepackage[margin=0.7in]{geometry}"
               ("\\chapter{%s}" . "\\chapter*{%s}")
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(use-package perspective
  :bind
  ("C-x k" . persp-kill-buffer*)         ; or use a nicer switcher, see below
  :custom
  (persp-mode-prefix-key (kbd "C-c M-p"))  ; pick your own prefix key here
  :init
  (persp-mode))


     (neko/leader-keys
       "p"  '(:ignore t :which-key "Perspective")
       "ps" '(persp-switch :which-key "Switch perspective")
       "pm" '(persp-merge :which-key "Merge perspective")
       "pb" '(persp-list-buffers :which-key "Buffers perspective")
       )

     (rune/leader-keys
       "]" '(persp-next :which-key "Next perspective")
       "[" '(persp-prev :which-key "Prev perspective")
       )
