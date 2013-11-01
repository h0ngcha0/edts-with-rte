;; rte related commands start
(require 'edts-rte-mode)

(defun edts-rte-run ()
  (interactive)
  (let* ((module         (car   (find-mfa-under-point)))
         (fun            (cadr  (find-mfa-under-point)))
         (arity          (caddr (find-mfa-under-point))))
    (if (get-buffer (param-buffer))
          (let* ((mfa            (parse-mfa))
                 (module-buffer  (car mfa))
                 (node           (edts-buffer-node-name))
                 (fun-buffer     (cadr mfa))
                 (args-buffer    (caddr mfa)))
            (if (and (string-equal module module-buffer)
                     (string-equal (fun-arity-str fun arity) fun-buffer))
                (edts-rte-run-with-args args-buffer))))))

(defun edts-rte-run-with-args (arguments &optional node module)
  "Run on function using rte_run"
  (interactive "sInput Arguments:")
  (if (server-running-p)
      (let* ((module     (or module (ferl-get-module)))
             (node       (or node (edts-node-name)))
             (function   (cadr (find-mfa-under-point)))
             (arity      (caddr (find-mfa-under-point)))
             (args       (list (cons "module"   module)
                               (cons "function" function)
                               (cons "args"     arguments)))
             (result     (edts-plugin-call node 'edts_rte 'rte_run args)))
        (ensure-args-saved arguments)
        (edts-rte-log-info "Running %s:%s/%s" module function arity))
    (edts-rte-log-error "Emacs server is not running")))

(defun edts-rte-interpret-module (&optional node module)
  "Interpret the module"
  (interactive)
  (let* ((module (or module (ferl-get-module)))
         (node   (or node (edts-node-name)))
         (args   (list (cons "module" module))))
    (edts-rte-log-info "Interpreting module: %s" module)
    (edts-plugin-call node 'edts_rte 'interpret_module args)))

(defun edts-rte-uninterpret-module (&optional node module)
  "Un-interpret the module"
  (interactive)
  (let* ((module (or module (ferl-get-module)))
         (node   (or node (edts-node-name)))
         (args   (list (cons "module" module))))
    (edts-rte-log-info "Uninterpreting module: %s" module)
    (edts-plugin-call node 'edts_rte 'uninterpret_module args)))

(defun edts-rte-update-record-defs (&optional node module)
  "Update the record definitions"
  (interactive)
  (let* ((module (or module (ferl-get-module)))
         (node   (or node (edts-node-name)))
         (args   (list (cons "module" module)))
         (result (edts-plugin-call node 'edts_rte 'update_record_defs args)))
    (edts-rte-log-info "Update the record definitions: %s" module)
    (null (edts-rte-log-info
           "%s" (cdr (assoc 'message (cdr (assoc 'body result))))))))

(defun edts-rte-forget-record-defs (record-name &optional node module)
  "Make RTE forget a particular record definition, if not specified
then forget all"
  (interactive "sInput Arguments:")
  (let* ((module (or module (ferl-get-module)))
         (node   (or node (edts-node-name)))
         (args   (list (cons "record" record-name)))
         (result (edts-plugin-call node 'edts_rte 'forget_record_defs args)))
    (if (eq "" record-name)
        (edts-rte-log-info "Forget the record definitions of %s" record-name)
      (edts-rte-log-info "Forget all the record definitions"))
    (null (edts-rte-log-info
           "%s" (cdr (assoc 'message (cdr (assoc 'body result))))))))

(defun edts-rte-list-stored-record-names (&optional node)
  "List the name of all the record that are stored by RTE"
  (interactive)
  (let* ((node   (or node (edts-node-name)))
         (result (edts-plugin-call node 'edts_rte 'list_record_names nil))
         )
    (edts-rte-log-info "List all the record definitions...")
    (null (edts-rte-log-info
           "%s" (cdr (assoc 'message (cdr (assoc 'body result))))))))

(defun param-buffer ()
  "Return the name of the parameter buffer for the current node"
  (let* ((node (edts-buffer-node-name)))
    (concat "*" node "-" "params" "*")
    ))

(defun ensure-args-saved (args)
  "Ensure that the module function and arguments are saved in
   the parameter buffer"
  (let* ((module         (car   (find-mfa-under-point)))
         (fun            (cadr  (find-mfa-under-point)))
         (arity          (caddr (find-mfa-under-point))))
    (save-excursion
      (set-buffer (get-buffer-create (param-buffer)))
      (erase-buffer)
      (insert (concat "module: " module                    "\n"
                      "fun: "    (fun-arity-str fun arity) "\n"
                      "args: "   args                      "\n"
                      )))))

(defun fun-arity-str (fun arity)
  "Return the func/arity string"
  (concat fun "/" (number-to-string arity)))

(defun parse-mfa ()
  "Parse the module function args"
  (save-excursion
    (set-buffer (param-buffer))
    (let* ((mfa    (split-string (trim-string (buffer-string)) "\n"))
           (module (trim-string (car mfa)))
           (fun    (trim-string (cadr mfa)))
           (args   (trim-string (caddr mfa))))
      (mapcar (lambda (str)
                (print str)
                (trim-string
                 (cadr
                  (split-string (trim-string str) ":"))))
              (list module fun args)))))

(defun trim-string (string)
  "Remove white spaces in beginning and ending of STRING.
White space here is any of: space, tab, emacs newline (line feed, ASCII 10)."
(replace-regexp-in-string "\\`[ \t\n]*" "" (replace-regexp-in-string "[ \t\n]*\\'" "" string)))


;; find the mfa of the point
(defun find-mfa-under-point ()
  "find the mfa in which the current cursor is located."
  (interactive)
  (save-excursion
    (ferl-beginning-of-function)
    (edts-mfa-at)))

(defun edts-display-erl-fun-in-emacs (string buffer)
  "display a piece of erlang code in a buffer"
  (window-normalize-buffer-to-switch-to buffer)
  (display-buffer buffer)
  (with-current-buffer buffer
    (save-excursion
      (erase-buffer)
      (goto-char (point-max))
      (insert string)
      (erlang-mode)
      (edts-rte-mode))))

(defun edts-rte-log-error (msg &rest args)
  "Log MSG at error-level."
  (apply #'edts-log-error (concat "RTE " msg) args))

(defun edts-rte-log-info (msg &rest args)
  "Log MSG at info-level."
  (apply #'edts-log-info (concat "RTE " msg) args))

(provide 'edts-rte)
