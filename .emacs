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
 '(helm-ls-git-status-command (quote magit-status-internal))
 '(monokai-background "#151515")
 '(monokai-highlight "#19483E")
 '(org-support-shift-select t)
  '(package-selected-packages
     (quote
       (wgrep-ag helm-ls-git interleave org-ref quelpa rust-mode zig-mode flycheck-joker all-the-icons-ibuffer ibuffer-projectile hydra ssh-deploy counsel swiper expand-region multiple-cursors parinfer ag flx-ido persp-projectile perspective js-mode clojure-mode company clojure-emacs clojure-emacx paredit cider window-numbering yaml-mode haskell-mode haskell-emacs groovy-mode editorconfig avy magit use-package)))
 '(projectile-completion-system (quote ivy))
  '(projectile-globally-ignored-directories
     (quote
       (".idea" ".ensime_cache" ".eunit" ".git" ".hg" ".fslckout" "_FOSSIL_" ".bzr" "_darcs" ".tox" ".svn" ".stack-work" "*node_modules")))
  '(safe-local-variable-values
     (quote
       ((ssh-deploy-script lambda nil
          (let
            ((default-directory ssh-deploy-root-remote))
            (shell-command "bash compile.sh")))
         (ssh-deploy-on-explicit-save . 1)
         (ssh-deploy-async . 1)
         (ssh-deploy-root-remote . "/ssh:sandbox5:ui-qa-page/vendor/odesk/ui-components-bundle/")
         (ssh-deploy-root-local . "/home/larionov/project/ui-components-bundle/"))))
  '(so-long-minor-modes
     (quote
       (font-lock-mode display-line-numbers-mode flymake-mode flyspell-mode goto-address-mode goto-address-prog-mode hi-lock-mode highlight-changes-mode hl-line-mode linum-mode nlinum-mode prettify-symbols-mode visual-line-mode whitespace-mode diff-hl-amend-mode diff-hl-flydiff-mode diff-hl-mode dtrt-indent-mode flycheck-mode hl-sexp-mode idle-highlight-mode rainbow-delimiters-mode editorconfig-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-block ((t (:background "gray8" :foreground "#F8F8F0"))))
 '(window-numbering-face ((t (:foreground "DeepPink" :underline "DeepPink" :weight bold))) t))


(setq create-lockfiles nil)
(use-package "monokai-theme" :ensure t)


;; (use-package "projectile"
;;   :ensure t
;;   :defer t
;;   :config
;;   (projectile-mode +1)
;; ;  (define-key projectile-mode-map (kbd "C-c C-p") 'projectile-command-map)
;; ;  (define-key projectile-mode-map (kbd "C-c C-s") 'projectile-switch-project)
;;   )

;; (global-set-key (kbd "C-S-f") 'projectile-ag)

(use-package flx :ensure t)
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

;; (use-package ranger
;;   :ensure t
;;   :defer t
;;   :config
;;   (setq ranger-preview-file nil)
;;   (setq ranger-show-literal nil)
;;   (setq ranger-dont-show-binary t)

;;   :bind
;;   ("M-s" . ranger))

;; (ranger-override-dired-mode t)

(load-theme 'monokai t)

(use-package web-mode :ensure t)
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))
(add-hook 'web-mode-hook
  (lambda ()
    (when (string-equal "ts" (file-name-extension buffer-file-name))
      (setup-tide-mode))
    (when (string-equal "tsx" (file-name-extension buffer-file-name))
      (setup-tide-mode))))

(add-hook 'dired-mode-hook
  (lambda ()
    (local-set-key (kbd "C-s") 'isearch-forward)
    ))

(use-package editorconfig
  :ensure t
  :defer t
  :config
  (editorconfig-mode 1))

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

(use-package window-numbering :ensure t)
(require 'window-numbering)
; highlight the window number in pink color


(setq tab-always-indent 'complete)

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


(use-package mmm-mode :ensure t)

(add-to-list 'auto-mode-alist '("\\.vue\\'" . vue-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js-mode))
(add-to-list 'auto-mode-alist '("\\.svelte\\'" . mhtml-mode))

(use-package clojure-mode :ensure t :defer t)
(use-package cider
  :defer t
  :ensure t
  :pin melpa-stable)

(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR." t)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(require 'saveplace)
(require 'dired+)

(setq-default save-place t)

(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-x C-b") 'ibuffer)
;(global-set-key (kbd "C-'") 'ibuffer)
(global-set-key (kbd "M-z") 'zap-up-to-char)

;(global-set-key (kbd "C-s") 'isearch-forward-regexp)
;(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

(require 'dired-arrow-keys)
(dired-arrow-keys-install)
(global-set-key (kbd "M-s") 'dired-jump)
(toggle-diredp-find-file-reuse-dir 1)

(defun mydired-sort ()
  "Sort dired listings with directories first."
  (save-excursion
    (let (buffer-read-only)
      (forward-line 2) ;; beyond dir. header
      (sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max)))
    (set-buffer-modified-p nil)))

(defadvice dired-readin
  (after dired-after-updating-hook first () activate)
  "Sort dired listings with directories first before adding marks."
  (mydired-sort))

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
  (global-set-key (kbd "C-;") 'er/expand-region)
  )

(defalias 'yes-or-no-p 'y-or-n-p)

(use-package ivy
  :ensure t
  :config (progn
    (ivy-mode 0)
    (setq ivy-use-virtual-buffers t)
    (setq enable-recursive-minibuffers t)
    ;; enable this if you want `swiper' to use it
    ;; (setq search-default-mode #'char-fold-to-regexp)
    (global-set-key (kbd "C-c C-r") 'ivy-resume)
    (global-set-key (kbd "<f6>") 'ivy-resume)
    (global-set-key (kbd "M-x") 'counsel-M-x)
    (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)
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

;; (use-package ibuffer-projectile
;;   :ensure t
;;   :init
;;   (add-hook 'ibuffer-hook
;;             (lambda ()
;;               (ibuffer-projectile-set-filter-groups)
;;               (unless (eq ibuffer-sorting-mode 'alphabetic)
;;                 (ibuffer-do-sort-by-project-name)
;;                 (ibuffer-do-sort-by-alphabetic)
;;                 )))
;;   )

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

(use-package helm
  :ensure t)

;; (use-package ag
;;   :ensure t
;;   :bind ("C-S-p" . ag-project)
;;   )

;; (use-package wgrep-ag
;;   :ensure t
;;   )
(use-package helm-ag
  :ensure t
  :bind ("C-S-p" . helm-projectile-ag)
  :commands (helm-ag helm-projectile-ag)
  :init (
         setq helm-ag-insert-at-point 'symbol
              helm-ag-command-option "--path-to-ignore ~/.agignore"))
(setq-default bidi-display-reordering nil)
(setq bidi-paragraph-direction 'left-to-right)

(put 'dired-find-alternate-file 'disabled nil)
(use-package zig-mode  :ensure t)
(use-package rust-mode  :ensure t)

 (when (require 'so-long nil :noerror)
   (global-so-long-mode 1))
(put 'upcase-region 'disabled nil)

(use-package org-ref  :ensure t)
(use-package interleave :ensure t)

(global-set-key (kbd "C-c c") 'org-capture)
(setq org-default-notes-file "~/sync/org/Inbox.org")
(setq org-agenda-files (list "~/sync/org/Inbox.org"))
;;(setq org-ref-notes-directory "~/sync/org/references/notes"
      ;; org-ref-bibliography-notes "~/sync/org/references/articles.org"
      ;; org-ref-default-bibliography '("~/sync/org/references/articles.bib")
      ;; org-ref-pdf-directory "~/sync/org/references/pdfs/")


  (defun my/org-contacts-template-email (&optional return-value)
    "Try to return the contact email for a template.
  If not found return RETURN-VALUE or something that would ask the user."
    (or (cadr (if (gnus-alive-p)
                  (gnus-with-article-headers
                    (mail-extract-address-components
                     (or (mail-fetch-field "Reply-To") (mail-fetch-field "From") "")))))
        return-value
        (concat "%^{" org-contacts-email-property "}p")))


(defvar my/org-basic-task-template "* TODO %^{Task}
:PROPERTIES:
:Effort: %^{effort|1:00|0:05|0:15|0:30|2:00|4:00}
:END:
Captured %<%Y-%m-%d %H:%M>
%?

%i
" "Basic task data")
(defvar my/org-inbox-file "~/sync/org/Inbox.org")
(setq org-capture-templates
  `(("t" "Quick task" entry
      (file ,my/org-inbox-file)
      "* TODO %^{Task}\n"
      :immediate-finish t)
     ("T" "Task" entry
       (file ,my/org-inbox-file)
       "* TODO %^{Task}\n")
     ("." "Today" entry
       (file ,my/org-inbox-file)
       "* TODO %^{Task}\nSCHEDULED: %t\n"
       :immediate-finish t)
     ("v" "Video" entry
       (file ,my/org-inbox-file)
       "* TODO %^{Task}  :video:\nSCHEDULED: %t\n"
       :immediate-finish t)
     ("e" "Errand" entry
       (file ,my/org-inbox-file)
       "* TODO %^{Task}  :errands:\n"
       :immediate-finish t)
     ("w" "Something to write about" entry
       (file ,my/org-inbox-file)
       "* %^{Task}  :writing:\n"
       :immediate-finish t)
     ("n" "Note" entry
       (file ,my/org-inbox-file)
       "* %^{Note}\n"
       :immediate-finish t)
     ("N" "Note" entry
       (file ,my/org-inbox-file)
       "* %^{Note}\n")
     ("i" "Interrupting task" entry
       (file ,my/org-inbox-file)
       "* STARTED %^{Task}"
       :clock-in :clock-resume)
     ("b" "Business task" entry
       (file+headline "~/sync/org/business.org" "Tasks")
       ,my/org-basic-task-template)
     ("p" "People task" entry
       (file "~/sync/org/people.org")
       ,my/org-basic-task-template)
     ("c" "Protocol Link" entry (file+headline ,org-default-notes-file "Inbox")
       "* [[%:link][%:description]] \n\n#+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n%?\n\nCaptured: %U")
     ("db" "Done - Business" entry
       (file+headline "~/sync/org/business.org" "Tasks")
       "* DONE %^{Task}\nSCHEDULED: %^t\n%?")
     ("dp" "Done - People" entry
       (file+headline "~/sync/org/people.org" "Tasks")
       "* DONE %^{Task}\nSCHEDULED: %^t\n%?")
     ("dt" "Done - Task" entry
       (file+headline "~/sync/org/organizer.org" "Inbox")
       "* DONE %^{Task}\nSCHEDULED: %^t\n%?")
     ("q" "Quick note" item
       (file+headline "~/sync/org/organizer.org" "Quick notes"))
     ("C" "Contact" entry (file "~/sync/org/contacts.org")
       "* %(org-contacts-template-name)
  :PROPERTIES:
  :EMAIL: %(my/org-contacts-template-email)
  :END:")))
(bind-key "C-M-r" 'org-capture)
(bind-key (kbd "<f5>") 'org-capture)

(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region compilation-filter-start (point))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

(setq projectile-mode-line
         '(:eval (format " Projectile[%s]"
                        (projectile-project-name))))
(use-package helm-ls-git
  :ensure t
  :init

  (global-set-key (kbd "C-p") 'helm-browse-project)
  (global-set-key (kbd "C-\"") 'helm-projects-history)
  )
