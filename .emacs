(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  (add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
)

(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))


(global-unset-key (kbd "C-z"))

(global-set-key (kbd "C-z C-z") 'my-suspend-frame)

(defun my-suspend-frame ()
  "In a GUI environment, do nothing; otherwise `suspend-frame'."
  (interactive)
  (if (display-graphic-p)
      (message "suspend-frame disabled for graphical displays.")
    (suspend-frame)))



;; EMACS SERVER START
; use emacs-server, all subsequent edits on same host become clients
;     working in own frame
; set up server to start each new client on its own frame
(add-hook 'server-visit-hook 'make-frame)
; switch the parent frame back to its own buffer
(add-hook 'server-switch-hook
          '(lambda ()
             (server-switch-buffer (other-buffer))))

(require 'server)
(or (server-running-p)
        (server-start))

;;
;;; GNU emacs customization file
;;;

;; The delay caused by set-default-font can be worked around by adding the following
;; at the beginning of your .emacs file:
(modify-frame-parameters nil '((wait-for-wm . nil)))

(tool-bar-mode -1)
(menu-bar-mode -1)  ;; shows full-screen button for mac port
(setq-default
 blink-cursor-delay 0.5
 blink-cursor-interval 1
 use-file-dialog nil
 use-dialog-box nil
 inhibit-startup-screen t
 inhibit-startup-echo-area-message t
 truncate-lines t
 truncate-partial-width-windows nil
 visible-bell 1
 transient-mark-mode t   ;; highlight the active region when mark is active
 show-trailing-whitespace t ;; don't show trailing whitespace globally
 blink-matching-paren t
 initial-frame-alist '((left-fringe . 1) (right-fringe . 1) (scroll-bar-width . 0) (vertical-scroll-bars . nil))
 default-frame-alist '((left-fringe . 1) (right-fringe . 1) (scroll-bar-width . 0) (vertical-scroll-bars . nil))
 scroll-bar-width 0
 default-frame-scroll-bars nil)

(defun mode-line-bell ()
  (let ((orig (face-attribute 'mode-line :background)))
    (set-face-attribute 'mode-line nil :background "red")
    (sit-for 0 70)
    (set-face-attribute 'mode-line nil :background orig)))

(setq
 visible-bell nil
 ring-bell-function 'mode-line-bell)



(setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))
(setq exec-path
      '(
    "/usr/local/bin"
    "/usr/bin"
    "/bin"
    "/sbin"
    ))

(load "cl")
(setq-default compilation-read-command t)
(setq transient-mark-mode t)

;; FLYCHECK syntax highlighting
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))
(add-hook 'after-init-hook #'global-flycheck-mode)

(defconst inhibit-startup-message t)
;; delete current map of ESC-[
(global-unset-key "\e[")

(global-set-key "\e[222z" 'scroll-down)
(global-set-key "\e[216z" 'scroll-up)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (sanityinc-solarized-dark)))
 '(custom-safe-themes
   (quote
    ("4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" default)))
 '(neo-auto-indent-point nil)
 '(neo-autorefresh nil)
 '(neo-confirm-change-root (quote off-p))
 '(neo-confirm-create-directory (quote off-p))
 '(neo-confirm-create-file (quote off-p))
 '(neo-show-updir-line nil)
 '(neo-smart-open t)
 '(neo-theme (quote classic))
 '(neo-vc-integration (quote (face)))
 '(neo-window-fixed-size nil)
 '(neo-window-width 35)
 '(package-selected-packages
   (quote
    (markdown-mode groovy-mode dockerfile-mode docker prettier-js helm-flx helm-fuzzier all-the-icons floobits magit helm-ag neotree jsx-mode js2-mode monokai-theme color-theme-sanityinc-solarized multiple-cursors git-gutter-fringe tide flycheck use-package)))
 '(typescript-indent-level 2))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#272822" :foreground "#F8F8F2" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 113 :width normal :family "Fira Code")))))

(autoload 'javascript-mode "javascript-mode" "javascript mode" t nil)
(autoload 'php-mode "php-mode" "php mode" t)


(use-package js2-mode :ensure t)
(use-package web-mode :ensure t)

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . jsx-mode))
(autoload 'jsx-mode "jsx-mode" "JSX mode" t)
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))

(use-package prettier-js :ensure t)
(add-hook 'js2-mode-hook 'prettier-js-mode)

(add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "tsx" (file-name-extension buffer-file-name))
              (setup-tide-mode))))

(use-package vue-mode :ensure t)
(add-to-list 'auto-mode-alist '("\\.vue\\'" . vue-mode))


;; enable typescript-tslint checker
(flycheck-add-mode 'typescript-tslint 'web-mode)

;; Better imenu
(add-hook 'js2-mode-hook #'js2-imenu-extras-mode)

(setq-default indent-tabs-mode nil)

;; EMACS SERVER START
; use emacs-server, all subsequent edits on same host become clients
;     working in own frame
; set up server to start each new client on its own frame
(add-hook 'server-visit-hook 'make-frame)
; switch the parent frame back to its own buffer
(add-hook 'server-switch-hook
          '(lambda ()
             (server-switch-buffer (other-buffer))))

(require 'server)
(or (server-running-p)
        (server-start))


; frames
(setq frame-title-format "%b")    ; use buffer name for title
(setq display-buffer-reuse-frames t)    ; no new frame if already open

(use-package company :ensure t)
(use-package company-tern :ensure t)
(require 'company)
(require 'company-tern)
(add-to-list 'company-backends 'company-tern)
(add-hook 'js2-mode-hook (lambda ()
                           (tern-mode)
                           (company-mode)))

;; Disable completion keybindings, as we use xref-js2 instead
(define-key tern-mode-keymap (kbd "M-.") nil)
(define-key tern-mode-keymap (kbd "M-,") nil)

(provide '.emacs)
;;; .emacs ends here

;; Helm
(use-package helm
  :ensure t
  :init
  (setq helm-M-x-fuzzy-match t
  helm-mode-fuzzy-match t
  helm-buffers-fuzzy-matching t
  helm-recentf-fuzzy-match t
  helm-locate-fuzzy-match t
  helm-semantic-fuzzy-match t
  helm-imenu-fuzzy-match t
  helm-completion-in-region-fuzzy-match t
  helm-candidate-number-list 150
  helm-split-window-in-side-p t
  helm-move-to-line-cycle-in-source t
  helm-echo-input-in-header-line t
  helm-autoresize-max-height 0
  helm-autoresize-min-height 20)
  :config
  (helm-mode 1))

(require 'helm-config)
(helm-mode 1)
(define-key global-map [remap find-file] 'helm-find-files)
(define-key global-map [remap occur] 'helm-occur)
(define-key global-map [remap list-buffers] 'helm-buffers-list)
(define-key global-map [remap dabbrev-expand] 'helm-dabbrev)
(define-key global-map [remap execute-extended-command] 'helm-M-x)
(unless (boundp 'completion-in-region-function)
  (define-key lisp-interaction-mode-map [remap completion-at-point] 'helm-lisp-completion-at-point)
  (define-key emacs-lisp-mode-map       [remap completion-at-point] 'helm-lisp-completion-at-point))

(defun cljs-node-repl ()
  (interactive)
  (inf-clojure "java -cp cljs.jar clojure.main ~/elisp/repl.clj"))

(use-package "helm-ag" :ensure t)
(use-package "projectile"
  :ensure t
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c C-p") 'projectile-command-map)
)


(use-package "helm-projectile"
  :ensure t
  :config (helm-projectile-on)
)


(global-set-key (kbd "C-x g") 'magit-status)
;;;(global-display-line-number-mode)

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

(defun move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(defun move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))

(global-set-key [(control shift up)]  'move-line-up)
(global-set-key [(control shift down)]  'move-line-down)

(use-package monokai-theme
  :ensure t
  :config
  (load-theme 'monokai t))

(if (functionp 'display-line-numbers-mode)
    (and (add-hook 'display-line-numbers-mode-hook
                   (lambda () (setq display-line-numbers-type t)))
         (add-hook 'prog-mode-hook #'display-line-numbers-mode))
  )

(defun air--delete-trailing-whitespace-in-proc-and-org-files ()
  "Delete trailing whitespace if the buffer is in `prog-' or `org-mode'."
  (if (or (derived-mode-p 'prog-mode)
          (derived-mode-p 'org-mode))
      (delete-trailing-whitespace)))
(add-to-list 'write-file-functions 'air--delete-trailing-whitespace-in-proc-and-org-files)

(defun duplicate-line()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank)
)
(global-set-key (kbd "C-S-d") 'duplicate-line)
(global-set-key (kbd "C-S-f") 'projectile-ag)

(use-package tide
  :ensure t
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save)))

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (setcar (memq 'source-inplace (flycheck-checker-get 'typescript-tslint 'command))
        'source-original)
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; formats the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)

(add-hook 'typescript-mode-hook #'setup-tide-mode)

(defun newline-without-break-of-line ()
"1. move to end of the line.
2. insert newline with index"

  (interactive)
  (let ((oldpos (point)))
    (end-of-line)
    (newline-and-indent)))

(global-set-key (kbd "<C-return>") 'newline-without-break-of-line)

(use-package git-gutter-fringe :ensure t
  :config
  (global-git-gutter-mode)
  )

(setq dotfiles-dir "~/.dotfiles/")
;; Don't clutter up directories with files~
(setq backup-directory-alist `(("." . ,(expand-file-name
                                    (concat dotfiles-dir "backups")))))

;; Don't clutter with #files either
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name (concat dotfiles-dir "backups")))))

(use-package multiple-cursors :ensure t)
(global-set-key (kbd "C-d") 'mc/mark-next-like-this)
(global-set-key (kbd "C-M-d") 'mc/mark-next-like-this)

(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(global-set-key (kbd "C-c m c") 'mc/edit-lines)
(global-set-key (kbd "C-M-<left>") 'windmove-left)
(global-set-key (kbd "C-M-<right>") 'windmove-right)
(global-set-key (kbd "C-M-<up>") 'windmove-up)
(global-set-key (kbd "C-M-<down>") 'windmove-down)


(setq scroll-error-top-bottom t)
(setq scroll-preserve-screen-position nil)

;; All The Icons
(use-package all-the-icons :ensure t)

;; NeoTree
(use-package neotree
  :ensure t
  :init

  (global-set-key [f8] 'neotree-toggle)
  (setq projectile-switch-project-action `open-projectile-and-term)
  (setq neo-theme (if (display-graphic-p) 'icons))
  )

(use-package "magit" :ensure t)

(use-package "floobits" :ensure t)

;; Disable backup files
(setq make-backup-files nil) ; stop creating backup~ files
(setq auto-save-default nil) ; stop creating #autosave# files

;; Show matching parens
(setq show-paren-delay 0)
(show-paren-mode 1)
(setq inhibit-compacting-font-caches t)

(use-package helm-fuzzier
  :ensure t
  :init
    (helm-fuzzier-mode 1)

  )

(use-package helm-flx
  :ensure t
  :init
  (helm-flx-mode +1)
  (setq helm-flx-for-helm-find-files t ;; t by default
      helm-flx-for-helm-locate t) ;; nil by default
  )

 (global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
    (global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
    (global-set-key (kbd "S-C-<down>") 'shrink-window)
    (global-set-key (kbd "S-C-<up>") 'enlarge-window)

(defun toggle-window-dedicated ()
  "Control whether or not Emacs is allowed to display another
buffer in current window."
  (interactive)
  (message
   (if (let (window (get-buffer-window (current-buffer)))
         (set-window-dedicated-p window (not (window-dedicated-p window))))
       "%s: Can't touch this!"
     "%s is up for grabs.")
   (current-buffer)))

(defun open-projectile-and-term ()
  (interactive)
  (delete-other-windows)
  (neotree-projectile-action)
  (text-scale-set -2)
  (other-window 1 nil)
  (text-scale-set 0)
  (split-window-right)
  (other-window 1 nil)
  (term "/usr/bin/fish")
  (text-scale-set -2)
  (toggle-window-dedicated)
  (message "test")
 )

(global-set-key (kbd "C-c t") 'toggle-window-dedicated)

;(global-visual-line-mode t)
    (add-hook 'text-mode-hook 'turn-on-visual-line-mode)
(use-package markdown-mode :ensure t)

(add-hook
 'term-mode-hook
 (lambda() (setq show-trailing-whitespace nil)))

(setq smerge-command-prefix "\C-cv")
