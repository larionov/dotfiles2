;;; parinfer-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "parinfer" "parinfer.el" (0 0 0 0))
;;; Generated autoloads from parinfer.el

(autoload 'parinfer-mode "parinfer" "\
Parinfer mode.

If called interactively, enable Parinfer mode if ARG is positive, and
disable it if ARG is zero or negative.  If called from Lisp,
also enable the mode if ARG is omitted or nil, and toggle it
if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(autoload 'parinfer-region-mode "parinfer" "\
Available when region is active.

If called interactively, enable Parinfer-Region mode if ARG is positive, and
disable it if ARG is zero or negative.  If called from Lisp,
also enable the mode if ARG is omitted or nil, and toggle it
if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "parinfer" '("parinfer-")))

;;;***

;;;### (autoloads nil "parinfer-ext" "parinfer-ext.el" (0 0 0 0))
;;; Generated autoloads from parinfer-ext.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "parinfer-ext" '("parinfer-")))

;;;***

;;;### (autoloads nil "parinferlib" "parinferlib.el" (0 0 0 0))
;;; Generated autoloads from parinferlib.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "parinferlib" '("parinferlib-")))

;;;***

;;;### (autoloads nil nil ("parinfer-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; parinfer-autoloads.el ends here
