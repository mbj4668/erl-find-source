;;; This module implements find source for Erlang programs.

;;; Author: Martin Bjorklund <mbj4668@gmail.com>

;;; Most of the code is copied from distel by Luke Gorrie,
;;; but this version doesn't use an erlang node to find the source files;
;;; instead it searches the local filesystem.
;;;
;;; The code can find function, type, record and macro definitions,
;;; and callers of a function.

;;; Usage example:
;;;
;;; (defun my-erlang-mode-hook ()
;;;  "Configuration for Erlang Mode. Add this to `erlang-mode-hook'."
;;;   (setq indent-tabs-mode nil)
;;;  ;; I use M-. to push to stack (default), and M-* to pop from stack
;;;  (local-set-key "\e." 'erlfs-find-source-under-point)
;;;  (local-set-key "\e*" 'erlfs-find-source-unwind))
;;;  ;; Some prefer M-, for pop
;;;  (local-set-key "\e," 'erlfs-find-source-unwind))
;;;  ;; I use M-? to find callers of the current function.
;;;  (local-set-key "\e?" 'erlfs-find-callers))
;;;
;;; (add-hook 'erlang-mode-hook 'my-erlang-mode-hook)

(require 'erlang)

;;; Base framework

(defgroup erlfs '()
  "Erlang find source"
  :group 'tools)

(defcustom erlfs-tags-compliant nil
  "Tags compliant, i.e. let M-. ask for confirmation."
  :type 'boolean
  :group 'erlfs)

(defcustom erlfs-search-patterns '("../../*/src/")
  "List of directory patterns to search for erlang modules."
  :type 'list
  :group 'erlfs)

(defcustom erlfs-hrl-search-patterns '("../../*/include/")
  "List of directory patterns to search for erlang include files."
  :type 'list
  :group 'erlfs)

;;;; Identifier reference lookup

(defun erlfs-read-identifier-ref ()
  "Read identifier reference at point or from user.
Returns the result in a list."
  (interactive) ; for testing
  (let* ((id-at-point (erlfs-get-identifier-at-point))
         (id (if (or (null id-at-point)
                     current-prefix-arg
                     erlfs-tags-compliant)
                 (erlfs-parse-mfa-as-id
                  (read-string
                   "Function reference: "
                   (if current-prefix-arg nil
                     (erlfs-format-mfa id-at-point))))
               id-at-point)))
    (list id)))

;; Like erlang-get-identifier-at-point, but also handle arity in
;; [mod:]func/arity
(defun erlfs-get-identifier-at-point ()
    (let ((id (erlang-get-identifier-at-point)))
      (if (null (erlang-id-arity id))
          (save-excursion
            (erlfs-goto-end-of-call-name)
            (let ((k (erlang-id-kind id))
                  (m (erlang-id-module id))
                  (n (erlang-id-name id))
                  (a (erlfs-arity-at-point)))
              (if a
                  (list k m n a)
                id)))
        id)))

(defun erlfs-format-mfa (id)
  "Format as MOD:FUN/ARITY or return nil."
  (if (and id (erlang-id-arity id))
      (let ((m (or erlang-id-module) (erlang-get-module))
            (f (erlang-id-name))
            (a (erlang-id-arity)))
        (format "%s:%s/%S" m f a))))

(defun erlfs-parse-mfa-as-id (string)
  (with-temp-buffer
    (with-syntax-table erlang-mode-syntax-table
      (insert string)
      (goto-char (point-min))
      (erlfs-get-identifier-at-point))))

(defun erlfs-arity-at-point ()
  "Get the number of arguments in a function reference.
Should be called with point directly before the opening ( or /."
  (save-excursion
    (cond ((looking-at "/")
           ;; form is /<n>, like the /2 in foo:bar/2
           (forward-char)
           (let ((start (point)))
             (if (re-search-forward "[0-9]+" nil t)
                 (ignore-errors (car (read-from-string (match-string 0)))))))
          (t
           (forward-char)
           (erlang-get-arity)))))

;;;; Definition finding

(defvar erlfs-find-history-ring (make-ring 20)
  "History ring tracing for following functions to their definitions.")

(defun erlfs-find-source-under-point ()
  "Goto the source code that defines the identifier being referenced at point.

For remote function calls, it uses the following algorithm:

  If a buffer exists with the name <module>.erl, goto that buffer.

  Otherwise, if a file <module>.erl exists in the current directory,
  visit that file.

  Otherwise, for each directory pattern in `erlfs-search-patterns`,
  if <module>.erl exists in a directory matching that pattern, visit that
  file.

  Otherwise, report that the file can't be found.

When `erlfs-tags-compliant' is non-nil, or a numeric prefix argument
is given, the user is prompted for the function to lookup (with a
default.)"
  (interactive)
  (apply #'erlfs-find-source
         (or (erlfs-read-identifier-ref) (error "No call at point."))))

(defun erlfs-find-source-unwind ()
  "Unwind back from uses of `erlfs-find-source-under-point'."
  (interactive)
  (unless (ring-empty-p erlfs-find-history-ring)
    (let* ((marker (ring-remove erlfs-find-history-ring))
           (buffer (marker-buffer marker)))
      (if (buffer-live-p buffer)
          (progn (switch-to-buffer buffer)
                 (goto-char (marker-position marker)))
        ;; If this buffer was deleted, recurse to try the next one
        (erlfs-find-source-unwind)))))

(defun erlfs-goto-end-of-call-name ()
  "Go to the end of the function or module:function at point."
  ;; We basically just want to do forward-sexp iff we're not already
  ;; in the right place
  (unless (or (member (char-before) '(?  ?\t ?\n))
              (and (not (eobp))
                   (member (char-syntax (char-after (point))) '(?w ?_))))
    (backward-sexp))
  (forward-sexp)
  ;; Special case handling: On some emacs installations (Tobbe's
  ;; machine), the (forward-sexp) won't skip over the : in a remote
  ;; function call. This is a workaround for that. The issue seems to
  ;; be that the emacs considers : to be punctuation (syntax class
  ;; '.'), whereas my emacs calls it a symbol separator (syntax class
  ;; '_'). FIXME.
  (when (eq (char-after) ?:)
    (forward-sexp)))

(defun erlfs-find-source (id)
  "Find the source code for ID in a buffer, loading it if necessary."
  ;; Add us to the history list
  (ring-insert-at-beginning erlfs-find-history-ring
                            (copy-marker (point-marker)))
  (let ((module (erlang-id-module id)))
    (cond ((or (null module) (equal module (erlang-get-module)))
           (erlfs-search-identifier id))
          (t (let ((buf (concat module ".erl")))
               (if (erlfs-switch-to-module-buffer module)
                   (erlfs-search-identifier id)))))))

(defun erlfs-switch-to-module-buffer (module)
  (let ((fname (concat module ".erl")))
    (cond ((member fname (mapcar (function buffer-name) (buffer-list)))
           (switch-to-buffer fname))
          ((file-exists-p fname)
           (find-file fname))
          (t
           (let ((patterns erlfs-search-patterns)
                 (found nil))
             (while (and patterns (not found))
               (let* ((pattern (car patterns))
                      (matches (file-expand-wildcards (concat pattern fname))))
                 (cond ((eq 1 (length matches))
                        (setq found t)
                        (find-file (car matches)))
                       (t
                        (setq patterns (cdr patterns))))))
             found)))))

(defun erlfs-search-identifier (id)
  "Goto the definition of the identifier in the current buffer.
Value is non-nil if search is successful."
  (let ((kind (erlang-id-kind id))
        (name (erlang-id-name id))
        (arity (erlang-id-arity id)))
    (cond
     ((eq kind 'record)
      (erlfs-search-record name))
     ((eq kind 'macro)
      (erlfs-search-macro name))
     (t
      (erlfs-search-function name arity)))))

(defun erlfs-search-function (name arity &optional type)
  "Goto the definition of NAME/ARITY in the current buffer.
Value is non-nil if search is successful."
  (let ((re (concat "^" (and type "-type\\s-*") (regexp-quote name) "\\s-*("))
        found)
    (save-excursion
      (goto-char (point-min))
      (while (and (not found)
                  (let ((case-fold-search nil)) (re-search-forward re nil t)))
        (backward-char)
        (when (or (null arity) (eq (erlfs-arity-at-point) arity))
          (setq found (line-beginning-position)))))
    (cond
     (found (goto-char found))
     ((and arity (not type))
      (message "Function %s/%s not found; ignoring arity..."
               name arity)
      (erlfs-search-function name nil nil))
     ((not type)
      (erlfs-search-function name 0 t))
     (t (message "Couldn't find definition %S" name)
        nil))))

(defun erlfs-search-record (name)
  (erlfs-search-directive "record" 'record name))

(defun erlfs-search-macro (name)
  (erlfs-search-directive "define" 'macro name))

(defun erlfs-search-directive (directive kind name)
  (cond ((erlfs-search-local-directive directive name))
        ((erlfs-search-remote-directive directive name))
        (t (message "Couldn't find %s %S" kind name)
           nil)))

(defun erlfs-search-local-directive (directive name)
  (let ((re (concat "^-" directive "\\s-*(" (regexp-quote name) "\\s-*[,(]"))
        found)
    (save-excursion
      (goto-char (point-min))
      (cond
       ((re-search-forward re nil t)
        (setq found (line-beginning-position)))))
    (cond
     (found (goto-char found)))))

;; this function traverses all include and include_lib header files
;; note: works only when the include/include_lib directive has a
;; plain string argument, e.g., '-include("foo.hrl").'.  doesn't work
;; if is uses macros, e.g., '-include(?MY_HRL)'
(defun erlfs-search-remote-directive (directive name)
  (let* ((hrl-files nil)
         (file (erlfs-find-remote-directive-file directive name hrl-files)))
    (if file
        (progn
          (find-file file)
          (erlfs-search-local-directive directive name)))))

(defun erlfs-find-remote-directive-file (directive name hrl-files)
  ;; search all includes and include_libs in the local file
  (let ((includes (erlfs-get-includes))
        (found nil))
    (while (and includes (not found))
      (let* ((include (car includes))
             (kind (car include))
             (fname (cdr include)))
        (if (and (not (file-exists-p fname))
                 (eq kind 'include-lib))
            ;; if fname exists, include_lib works like include
            (progn
              (if (null hrl-files) (setq hrl-files (erlfs-find-hrl-files)))
              (setq fname (erlfs-get-hrl-file fname hrl-files))))
        (setq includes (cdr includes))
        (if (and fname (file-exists-p fname))
            (with-temp-buffer
              (insert-file-contents fname)
              (cond ((erlfs-search-local-directive directive name)
                     (setq found fname))
                    (t (setq includes
                             (append (erlfs-get-includes) includes))))))))
    found))

(defun erlfs-get-includes ()
  (let (res)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^-include" nil t)
        (cond ((looking-at "(\"\\(.*\\)\")")
               (setq res (cons (cons 'include (match-string-no-properties 1))
                               res)))
              ((looking-at "_lib(\"\\(.*\\)\")")
               (setq res
                     (cons (cons 'include-lib (match-string-no-properties 1))
                           res))))))
    res))

(defun erlfs-find-hrl-files ()
  (let (hrl-files)
    (dolist (pattern
             (append erlfs-search-patterns erlfs-hrl-search-patterns)
             hrl-files)
      (setq hrl-files
            (append hrl-files
                    (file-expand-wildcards (concat pattern "*.hrl")))))
    (mapcar (lambda (fname)
              (cons (file-name-nondirectory fname) fname))
            hrl-files)))

(defun erlfs-get-hrl-file (fname hrl-files)
  (let* ((key (file-name-nondirectory fname))
         (r (assoc key hrl-files)))
    (if r (progn
            ;; remove fname from hrl-files so that we don't test it again
            (setq hrl-files (assoc-delete-all key hrl-files))
            (cdr r)))))

(defun erlfs-find-callers ()
  "Uses `grep` to find callers of the function at point."
  (interactive)
  (grep-apply-setting 'grep-use-null-device nil)
  (grep
   (format
    ;; regexp below: do not start line with "-" (directive like -spec),
    ;; no line comment, match "Fun(" in local file; "Mod:Fun(" in other files,
    ;; then highlight just Fun
    (concat "(grep -nHE '^[^-][^%%]*[^\\w:]%s\\(' ./%s "
            "; grep --include=\"*.erl\" -nrE '^[^-][^%%]*%s:%s\\(' %s )"
            "| grep --color -w %s")
    (symbol-at-point)
    (file-name-nondirectory (buffer-file-name))
    (erlang-get-module)
    (symbol-at-point)
    (string-join erlfs-search-patterns " ")
    (symbol-at-point))))

(provide 'erl-find-source)
