;; Emacs from Scratch v1

(defvar danny/default-font-size 100)

;; Disable splash screen
(setq inhibit-startup-message t)

;; Enable visible bell
;; (setq visible-bell t) ; replaced by `doom-themes-visual-bell-config`

;; Turn off some unneeded UI elements
(menu-bar-mode -1)   ; disable menubar
(tool-bar-mode -1)   ; disable toolbar
(scroll-bar-mode -1) ; disable visible scrollbar
(tooltip-mode -1)    ; disable tooltips
(set-fringe-mode 10) ; give some breathing space

(set-face-attribute 'default nil :font "MesloLGS NF" :height danny/default-font-size)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
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

; Display column number in mode-line
(column-number-mode)

;; Display line numbers in every buffer
(global-display-line-numbers-mode 1)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
		term-mode-hook
		shell-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; ivy
;; https://github.com/abo-abo/swiper
(use-package ivy
  :diminish
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

;; swiper
(use-package swiper)

;; counsel
(use-package counsel
  :bind (("M-x" . counsel-M-x)
	 ;; I prefer `counsel-switch-buffer` to `counsel-ibuffer` for the preview functionality
	 ("C-x b" . counsel-switch-buffer)
	 ("C-x C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history)))

;; NOTE: the first time you load your configuration on a new machine, you'll
;; need to run the following command interactively so that mode-line icons
;; display correctly:
;;
;; M-x all-the-icons-install-fonts
(use-package all-the-icons)

;; doom-modeline
;; https://github.com/seagle0128/doom-modeline
(use-package doom-modeline
  :init (doom-modeline-mode 1))

;; doom-themes
;; https://github.com/doomemacs/themes
(use-package doom-themes
  :init (load-theme 'doom-palenight t))

;; Flash mode-line on error
(doom-themes-visual-bell-config)

;; rainbow-delimiters 
;; https://github.com/Fanael/rainbow-delimiters
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; which-key
;; https://github.com/justbur/emacs-which-key
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

;; ivy-rich
;; https://github.com/Yevgnen/ivy-rich
(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

;; helpful
;; https://github.com/Wilfred/helpful
(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

;; general
;; https://github.com/noctuid/general.el
(use-package general
  :config

  (general-create-definer danny/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")

  (danny/leader-keys
    "t" '(:ignore t :which-key "toggles")
    "tt" '(counsel-load-theme :which-key "choose theme")))

;; evil
;; https://github.com/emacs-evil/evil
(use-package evil
  :init
  ;; TODO: check out other `evil-want-*` variables
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-u-delete t)
  (setq evil-want-C-i-jump nil)
  (setq evil-want-C-w-delete t) ;; TODO: remote, it's set to `t` by default
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

;; evil-collection
;; https://github.com/emacs-evil/evil-collection
(use-package evil-collection
  :after evil
  :custom (evil-collection-setup-minibuffer t)
  :config
  (evil-collection-init))

;; hydra
;; https://github.com/abo-abo/hydra
(use-package hydra)

(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

(danny/leader-keys
  "ts" '(hydra-text-scale/body :which-key "scale text"))

;; projectile
;; https://github.com/bbatsov/projectile
(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/personal/repos")
    (setq projectile-project-search-path '("~/personal/repos")))
  (setq projectile-switch-project-action #'projectile-dired))

;; counsel-projectile
;; https://github.com/ericdanan/counsel-projectile
(use-package counsel-projectile
  :config (counsel-projectile-mode))

;; magic
;; https://magit.vc/
(use-package magit
  :commands (magit-status magit-git-current-branch)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; forge
;; https://github.com/magit/forge
(use-package forge)

;; Emacs from Scratch v2

;; M-x recentf-open-files
(recentf-mode 1)

;; Save what you enter into minibuffer prompts
;; M-n, M-p
(setq history-length 25)
(savehist-mode 1)

;; Remember and restore the last cursor location of opened files
(save-place-mode 1)

;; Move customization variables to a separate file and load it
(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)

;; Don't pop up UI dialogs when prompting
(setq use-dialog-box nil)

;; Revert buffers when the underlying file has changed
(global-auto-revert-mode 1)

;; Revert Dired and other buffers
(setq global-auto-revert-non-file-buffers t)
