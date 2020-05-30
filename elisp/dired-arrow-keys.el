;;; dired-arrow-keys.el --- Navigate Dired buffers with the arrow keys
;;
;;; Copyright (C) 2018  Free Software Foundation, Inc.
;;
;; Author: Eric Crosson <eric.s.crosson@utexas.com>
;; Version: 1.0.0
;; Keywords: convenience
;; URL: https://github.com/EricCrosson/dired-arrow-keys
;; Package-Requires: ((emacs "24"))
;;
;; This file is not a part of GNU Emacs.
;;
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;
;;
;;; Commentary:
;;
;;
;; Install `dired-arrow-keys' by modifying `dired-mode-map'.
;;
;; Map
;;
;;     <right> to `dired-find-file'
;;     <left> to `dired-find-parent-directory'
;;
;; and for `evil' users, map
;;
;;     \\[evil-forward-char] to `dired-find-file'
;;     \\[evil-backward-char] to `dired-find-parent-directory'
;;
;; Modeled after dired-details, with the `install' function.

;;; Code:

(eval-when-compile (require 'dired))


;;;###autoload
(defun dired-find-parent-directory ()
  "Open a `dired'-buffer of the parent directory."
  (interactive)
  (find-alternate-file ".."))

;;;###autoload
(defun dired-arrow-keys-install ()
  "Install `dired-arrow-keys' by modifying `dired-mode-map'.

Map

    <right> to `dired-find-file'
    <left> to `dired-find-parent-directory'

and for `evil' users, map

    \\[evil-forward-char] to `dired-find-file'
    \\[evil-backward-char] to `dired-find-parent-directory'"
  (interactive)
  (define-key dired-mode-map (kbd "<right>") 'dired-find-alternate-file)
  (define-key dired-mode-map (kbd "<left>") 'dired-up-directory)
  (eval-after-load 'evil
    '(progn
       (define-key dired-mode-map (vector 'remap 'evil-forward-char) 'dired-find-file)
       (define-key dired-mode-map (vector 'remap 'evil-backward-char) 'dired-up-directory))))

 (eval-after-load "dired"
       ;; don't remove `other-window', the caller expects it to be there
       '(defun dired-up-directory (&optional other-window)
          "Run Dired on parent directory of current directory."
          (interactive "P")
          (let* ((dir (dired-current-directory))
     	    (orig (current-buffer))
     	    (up (file-name-directory (directory-file-name dir))))
            (or (dired-goto-file (directory-file-name dir))
     	   ;; Only try dired-goto-subdir if buffer has more than one dir.
     	   (and (cdr dired-subdir-alist)
     		(dired-goto-subdir up))
     	   (progn
     	     (kill-buffer orig)
     	     (dired up)
     	     (dired-goto-file dir))))))
(provide 'dired-arrow-keys)

;;; dired-arrow-keys.el ends here
