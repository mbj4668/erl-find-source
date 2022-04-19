;;; This module implements find source for Erlang programs.
;;; Most of the code is copied from distel by Luke Gorrie,
;;; but this version doesn't use an erlang node.

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

;;;; Base framework

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

;;;;; Call MFA lookup

(defun erlfs-read-call-mfa ()
  "Read module, function, arity at point or from user.
Returns the result in a list: module and function as strings, arity as
integer."
  (interactive) ; for testing
  (let* ((mfa-at-point (erlfs-mfa-at-point))
         (mfa (if (or (null mfa-at-point)
                      current-prefix-arg
                      erlfs-tags-compliant)
                  (erlfs-parse-mfa
                   (read-string
                    "Function reference: "
                    (if current-prefix-arg nil
                      (erlfs-format-mfa mfa-at-point))))
                mfa-at-point)))
    mfa))

(defun erlfs-format-mfa (mfa)
  "Format (MOD FUN ARITY) as MOD:FUN/ARITY.
If MFA is nil then return nil.
If only MOD is nil then return FUN/ARITY."
  (if mfa
      (let ((m (car mfa))
            (f (cadr mfa))
            (a (caadr mfa)))
        (if m (format "%s:%s/%S" m f a) (format "%s/%S" f a)))))

(defun erlfs-parse-mfa (string &optional default-module)
  "Parse MFA from a string using `erlfs-mfa-at-point'."
  (when (null default-module) (setq default-module (erlfs-buffer-module-name)))
  (with-temp-buffer
    (with-syntax-table erlang-mode-syntax-table
      (insert string)
      (goto-char (point-min))
      (erlfs-mfa-at-point default-module))))

(defun erlfs-buffer-module-name ()
  "Return the current buffer's module name, or nil."
  (erlang-get-module))

(defun erlfs-mfa-at-point (&optional default-module)
  "Return the module, function, arity of the function reference at point.
If not module-qualified then use DEFAULT-MODULE."
  (when (null default-module) (setq default-module (erlfs-buffer-module-name)))
  (save-excursion
    (erlfs-goto-end-of-call-name)
    (let ((arity (erlfs-arity-at-point))
          (mf (erlang-get-function-under-point)))
      (if (null mf)
          nil
        (let ((m (car mf))
              (f (cadr mf)))
          (list (or m default-module) f arity))))))

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
  "Goto the source code that defines the function being called at point.
For remote calls, it uses the following algorithm:

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
         (or (erlfs-read-call-mfa) (error "No call at point."))))

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

(defun erlfs-find-source (module &optional function arity)
  "Find the source code for MODULE in a buffer, loading it if necessary.
When FUNCTION is specified, the point is moved to its start."
  ;; Add us to the history list
  (ring-insert-at-beginning erlfs-find-history-ring
                            (copy-marker (point-marker)))
  (if (equal module (erlang-get-module))
      (when function
        (erlfs-search-function function arity))
    (let ((buf (concat module ".erl")))
      (if (erlfs-switch-to-module-buffer module)
          (when function
            (erlfs-search-function function arity))))))

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
      (message "Searching type definition...")
      (erlfs-search-function name 0 t))
     (t (message "Couldn't find function or type %S" name)
        nil))))

(defun erlfs-read-symbol-or-nil (prompt)
  "Read a symbol, or NIL on empty input."
  (let ((s (read-string prompt)))
    (if (string= s "")
        nil
      (intern s))))

(defun erlfs-find-callers ()
  "Uses `grep` to find callers of the function at point."
  (interactive)
  (grep
   (format
    ;; regexp below:do not start line with "-" (directive like -spec),
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
