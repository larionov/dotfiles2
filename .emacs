(require 'package)
(add-to-list 'load-path "~/elisp/")
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

;;
;;; GNU emacs customization file
;;;

;; The delay caused by set-default-font can be worked around by adding the following
;; at the beginning of your .emacs file:
(modify-frame-parameters nil '((wait-for-wm . nil)))
(add-hook 'before-save-hook 'delete-trailing-whitespace)
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



(defconst inhibit-startup-message t)
;; delete current map of ESC-[
(global-unset-key "\e[")

(use-package "magit" :ensure t)

(use-package avy
  :ensure t
  :bind
  ("M-S" . avy-goto-char))

(setq-default cursor-type 'bar)
(set-cursor-color "#ffcccc")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(helm-ag-ignore-patterns (quote ("node_modules")))
 '(monokai-background "#151515")
 '(monokai-highlight "#19483E")
 '(org-support-shift-select t)
 '(package-selected-packages
   (quote
    (all-the-icons-ibuffer ibuffer-projectile hydra ssh-deploy counsel swiper expand-region multiple-cursors parinfer ag flx-ido persp-projectile perspective js-mode clojure-mode company clojure-emacs clojure-emacx paredit cider window-numbering yaml-mode haskell-mode haskell-emacs groovy-mode editorconfig ranger avy magit use-package)))
 '(safe-local-variable-values
   (quote
    ((ssh-deploy-script lambda nil
                        (let
                            ((default-directory ssh-deploy-root-remote))
                          (shell-command "bash compile.sh")))
     (ssh-deploy-on-explicit-save . 1)
     (ssh-deploy-async . 1)
     (ssh-deploy-root-remote . "/ssh:sandbox5:ui-qa-page/vendor/odesk/ui-components-bundle/")
     (ssh-deploy-root-local . "/home/larionov/project/ui-components-bundle/")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-block ((t (:background "gray8" :foreground "#F8F8F0"))))
 '(window-numbering-face ((t (:foreground "DeepPink" :underline "DeepPink" :weight bold))) t))


(setq create-lockfiles nil)
(use-package "monokai-theme" :ensure t)

;; Helm
(use-package helm
  :ensure t
  :defer t
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
(use-package flx :ensure t)

;; Better matching for HELM
(with-eval-after-load 'helm
  ;; make sure you have flx installed
  (require 'flx)
  ;; this is a bit hackish, ATM, redefining functions I don't own
  (defvar helm-flx-cache (flx-make-string-cache #'flx-get-heatmap-str))

  (defun helm-score-candidate-for-pattern (candidate pattern)
    (or (car (flx-score candidate pattern helm-flx-cache)) 0))

  (defun helm-fuzzy-default-highlight-match (candidate)
    (let* ((pair (and (consp candidate) candidate))
            (display (if pair (car pair) candidate))
            (real (cdr pair)))
      (with-temp-buffer
        (insert display)
        (goto-char (point-min))
        (if (string-match-p " " helm-pattern)
          (cl-loop with pattern = (split-string helm-pattern)
            for p in pattern
            do (when (search-forward p nil t)
                 (add-text-properties
                   (match-beginning 0) (match-end 0) '(face helm-match))))
          (cl-loop with pattern = (cdr (flx-score display
                                         helm-pattern helm-flx-cache))
            for index in pattern
            do (add-text-properties
                 (1+ index) (+ 2 index) '(face helm-match))))
        (setq display (buffer-string)))
      (if real (cons display real) display))))


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

;(use-package "helm-projectile"
;  :ensure t
;  :config (helm-projectile-on)
;)
(use-package "perspective" :ensure t :defer t
  :config
  (persp-mode)
  )
(use-package "persp-projectile" :ensure t)
(use-package "projectile"
  :ensure t
  :defer t
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c C-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "s-s") 'projectile-persp-switch-project)
  (define-key projectile-mode-map (kbd "C-c C-s") 'projectile-persp-switch-project)
;;  (define-key projectile-mode-map (kbd "C-x <left>") 'projectile-previous-project-buffer)
;;  (define-key projectile-mode-map (kbd "C-x <right>") 'projectile-next-project-buffer)
  (projectile-register-project-type 'npm '("package.json")
                  :compile "npm install"
                  :test "npm test"
                  :run "npm start"
                  :test-suffix ".spec")

  )

;(global-set-key (kbd "C-S-f") 'projectile-ag)

(use-package helm-ag
  :ensure helm-ag
  :bind ("C-S-p" . helm-projectile-ag)
  :commands (helm-ag helm-projectile-ag)
  :init (setq helm-ag-insert-at-point 'symbol
	      helm-ag-command-option "--path-to-ignore ~/.agignore"))

(use-package flx-ido :ensure t
  :init
;  (ido-mode 1)
;  (ido-everywhere 1)
  (flx-ido-mode 1)
  ;; disable ido faces to see flx highlights.
  (setq ido-enable-flex-matching t)
  (setq ido-use-faces nil)
  )

(setq gc-cons-threshold 20000000)

(use-package helm-fuzzier
  :ensure t
  :defer t
  :init
    (helm-fuzzier-mode 1)

    )
(use-package helm-flx
  :ensure t
  :defer t
  :init
  (helm-flx-mode +1)
  (setq helm-flx-for-helm-find-files t ;; t by default
      helm-flx-for-helm-locate t) ;; nil by default
  )

 (defun helm-buffer-face-mode ()
   "Helm buffer face"
   (interactive)
   (with-helm-buffer
     (setq line-spacing 2)
     (buffer-face-set '(:family "Fira Mono" :height 80))))

(use-package ranger
  :ensure t
  :defer t
  :config
   (setq ranger-preview-file t)
   (setq ranger-dont-show-binary t)

  :bind
  ("M-s" . ranger))

(ranger-override-dired-mode t)

(load-theme 'monokai t)

(use-package web-mode :ensure t)
(use-package flycheck :ensure t)
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

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))
(add-hook 'web-mode-hook
	  (lambda ()
	    (when (string-equal "ts" (file-name-extension buffer-file-name))
	      (setup-tide-mode))
	    (when (string-equal "tsx" (file-name-extension buffer-file-name))
	      (setup-tide-mode))))
;; enable typescript-tslint checker
(flycheck-add-mode 'typescript-tslint 'web-mode)

(use-package editorconfig
  :ensure t
  :defer t
  :config
  (editorconfig-mode 1))

(use-package haskell-emacs :ensure t :defer t)
(use-package haskell-mode :ensure t :defer t)

(use-package vue-mode
  :ensure t
  :defer t
  :config
  ;; 0, 1, or 2, representing (respectively) none, low, and high coloring
  (setq mmm-submode-decoration-level 0))

(add-hook 'js-mode-hook (lambda () (setq syntax-ppss-table nil)))
(setq mmm-js-mode-enter-hook (lambda () (setq syntax-ppss-table nil)))
(setq mmm-typescript-mode-enter-hook (lambda () (setq syntax-ppss-table nil)))

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

(setq scroll-error-top-bottom t)
(setq scroll-preserve-screen-position nil)



 (defun helm-buffer-face-mode ()
   "Helm buffer face"
   (interactive)
   (with-helm-buffer
     (setq line-spacing 2)
     (buffer-face-set '(:family "Fira Mono" :height 80))))

(use-package window-numbering :ensure t)
(require 'window-numbering)
; highlight the window number in pink color


(setq tab-always-indent 'complete)
(add-to-list 'completion-styles 'initials t)

(window-numbering-mode 1)
(global-display-line-numbers-mode 1)



(add-hook 'prog-mode-hook #'editorconfig-mode)

(with-eval-after-load 'editorconfig
  (add-to-list 'editorconfig-indentation-alist
               '(vue-mode css-indent-offset
                          js-indent-level
                          sgml-basic-offset
                          ssass-tab-width
                          )))


(setq make-backup-files nil) ; stop creating backup~ files
(setq auto-save-default nil) ; stop creating #autosave# files

(setq backup-directory-alist '(("" . "~/.emacs.d/backup")))

;; make backup to a designated dir, mirroring the full path

(defun my-backup-file-name (fpath)
  "Return a new file path of a given file path.
If the new path's directories does not exist, create them."
  (let* (
        (backupRootDir "~/.emacs.d/backup/")
        (filePath (replace-regexp-in-string "[A-Za-z]:" "" fpath )) ; remove Windows driver letter in path, for example, “C:”
        (backupFilePath (replace-regexp-in-string "//" "/" (concat backupRootDir filePath "~") ))
        )
    (make-directory (file-name-directory backupFilePath) (file-name-directory backupFilePath))
    backupFilePath
  )
)

(setq make-backup-file-name-function 'my-backup-file-name)

(require 'nice-jumper)

(global-nice-jumper-mode t)
(global-set-key (kbd "C-o") 'nice-jumper/backward)
(define-key input-decode-map [?\C-i] [control-i])
(global-set-key [control-i] 'nice-jumper/forward)
(global-set-key (kbd "M-.") 'xref-find-definitions)


;;(use-package js-mode :ensure t)
(use-package company :ensure t)
(use-package flycheck :ensure t)
(use-package mmm-mode :ensure t)

(add-to-list 'auto-mode-alist '("\\.vue\\'" . vue-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . tide-mode))
(add-to-list 'auto-mode-alist '("\\.svelte\\'" . mhtml-mode))

;;; runs eslint --fix on the current file after save
;;; alpha quality -- use at your own risk

(defun eslint-fix-file ()
  (interactive)
  (message "eslint --fixing the file" (buffer-file-name))
  (shell-command (concat "eslint --fix " (buffer-file-name))))
(defun prettier-fix-file ()
  (interactive)
  (message "prettier the file" (buffer-file-name))
  (shell-command (concat "prettier --write " (buffer-file-name))))

(defun eslint-fix-file-and-revert ()
  (interactive)
  (prettier-fix-file)
  (eslint-fix-file)
  (revert-buffer t t))

(global-set-key (kbd "C-c f") 'eslint-fix-file-and-revert)

(use-package clojure-mode :ensure t :defer t)
(use-package paredit :ensure t :defer t)
(use-package cider
  :defer t
  :ensure t
  :pin melpa-stable)

(defadvice next-buffer (after avoid-messages-buffer-in-next-buffer)
  "Advice around `next-buffer' to avoid going into the *Messages* buffer."
  (when (string-match "^\\*.*\\*$" (buffer-name))
    (next-buffer)))

(defadvice previous-buffer (after avoid-messages-buffer-in-next-buffer)
  "Advice around `next-buffer' to avoid going into the *Messages* buffer."
  (when (string-match "^\\*.*\\*$" (buffer-name))
    (previous-buffer)))

;; activate the advice
(ad-activate 'next-buffer)
(ad-activate 'previous-buffer)

(setf helm-boring-buffer-regexp-list '("\\` " "\\*helm" "\\*helm-mode"
                                       "\\*Echo Area" "\\*Minibuf" "\\*monky-cmd-process\\*"
                                       "\\*epc con" "\\*Compile-Log\\*" "\\*monky-process\\*"
                                       "\\*CEDET CScope\\*" "\\*Messages\\*" "\\*Flycheck error"
                                       "\\*.+(.+)" "elpa/.+" "tramp/.+"
				       "\\*lsp-log\\*"
                                       "\\*Gofmt Errors\\*" "\\*autopep8"
                                       "\\*magit-process:" "\\*magit-diff:" "\\*anaconda-mode\\*"))

(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR." t)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(require 'saveplace)
(setq-default save-place t)

(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-'") 'ibuffer)
(global-set-key (kbd "M-z") 'zap-up-to-char)

(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

(show-paren-mode 1)
(setq-default indent-tabs-mode nil)
(setq save-interprogram-paste-before-kill t
      apropos-do-all t
      mouse-yank-at-point t
      require-final-newline t
      visible-bell t
      load-prefer-newer t
      ediff-window-setup-function 'ediff-setup-windows-plain
      save-place-file (concat user-emacs-directory "places")
      backup-directory-alist `(("." . ,(concat user-emacs-directory
					       "backups"))))


(use-package multiple-cursors
  :ensure t
  :defer t
  :config
  )
  (global-set-key (kbd "C-{") 'mc/edit-lines)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-S-d") 'mc/mark-all-like-this)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ediff                                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'ediff)
;; don't start another frame
;; this is done by default in preluse
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
;; put windows side by side
(setq ediff-split-window-function (quote split-window-horizontally))
;;revert windows on exit - needs winner mode
(winner-mode)
(add-hook 'ediff-after-quit-hook-internal 'winner-undo)

(use-package expand-region
  :ensure t
  :defer t
  :config
  ;; Expand region (increases selected region by semantic units)
  (global-set-key (kbd "C-'") 'er/expand-region)
  )

(defalias 'yes-or-no-p 'y-or-n-p)

(use-package swiper
  :ensure t
  :config (progn
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    (setq enable-recursive-minibuffers t)
    ;; enable this if you want `swiper' to use it
    ;; (setq search-default-mode #'char-fold-to-regexp)
    (global-set-key "\C-r" 'swiper)
    (global-set-key "\C-s" 'swiper)
    (global-set-key (kbd "C-c C-r") 'ivy-resume)
    (global-set-key (kbd "<f6>") 'ivy-resume)
))
(use-package hydra :ensure t)

(use-package ssh-deploy
  :ensure t
  :demand
  :after hydra
  :hook ((after-save . ssh-deploy-after-save)
         (find-file . ssh-deploy-find-file))
  :config
  (ssh-deploy-line-mode) ;; If you want mode-line feature
  (ssh-deploy-add-menu) ;; If you want menu-bar feature
  (ssh-deploy-hydra "C-c C-z") ;; If you want the hydra feature
)

(setq projectile-mode-line "Projectile")
(setq remote-file-name-inhibit-cache nil)
(setq vc-ignore-dir-regexp
      (format "%s\\|%s"
                    vc-ignore-dir-regexp
                    tramp-file-name-regexp))
(setq tramp-verbose 1)

(defvar org-babel-js-function-wrapper
  "process.stdout.write(function(){\n%s\n}())"
  "Javascript code to print value of body.")

(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (python . t)
   (js . t)
   (calc . t)
   )
 )


(defun evaluate-and-display-images ()
  (interactive)
  (org-ctrl-c-ctrl-c)
  ;;
  (org-redisplay-inline-images)
  )
(org-display-inline-images t t)
(global-set-key (kbd "C-c C-u")
                `evaluate-and-display-images)

(use-package ibuffer-projectile
  :ensure t
  :init
  (add-hook 'ibuffer-hook
            (lambda ()
              (ibuffer-projectile-set-filter-groups)
              (unless (eq ibuffer-sorting-mode 'alphabetic)
                (ibuffer-do-sort-by-project-name)
                (ibuffer-do-sort-by-alphabetic)
                )))
  )

(defun duplicate-line()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank)
)
(global-set-key (kbd "C-c C-d") 'duplicate-line)
(setq projectile-enable-caching t)

;; M-x all-the-icons-install-fonts
(use-package all-the-icons-ibuffer
  :ensure t
  :init (all-the-icons-ibuffer-mode 1))
