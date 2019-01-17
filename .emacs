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
 '(package-selected-packages (quote (avy magit use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
