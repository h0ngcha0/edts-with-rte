;;;###autoload
(define-minor-mode edts-rte-mode
  "Display the replaced value returned by edts-rte.
When edts-rte replaces a variable with a value, a tuple in the format
of {\"__edts-rte__\", VarName, Value} is returned. Value should be displayed
and VarName should be displayed in the pop up window when the cursor is on
top of it"
  :init-value nil
  :lighter "-EDTS-RTE"
  (cond (edts-rte-mode
         (highlight-rte-vars)
         (replace-rte-vars)
         (activate-advice)
         (font-lock-fontify-buffer))
        (t
         (font-lock-remove-keywords
          nil (rte-keywords))
         (save-excursion
           (goto-char (point-min))
           (while (re-search-forward (rte-regex) nil t)
             (funcall (switch-invisible) nil)))
         (deactivate-advice))))

(defun highlight-rte-vars ()
  (interactive)
  "Highlight the tuple {\"__edts-rte__\", VarName, Value} returned by edts rte"
  (font-lock-add-keywords
   nil (rte-keywords)))

(defun replace-rte-vars ()
  "Replace the tuple {\"__edts-rte__\", VarName, Value} returned by edts rte
with Value"
  (interactive)
  (save-excursion
      (goto-line (point-min))
      (while (re-search-forward (rte-regex) nil t)
        (funcall (switch-invisible) t))))

(defun display-rte-var ()
  "Display the variable name in the tuple {\"__edts-rte__\", VarName, Value}
returned by edts rte"
  (interactive)
  (let* ((cur-point   (point))
         (displayed-p nil))
    (save-excursion
      (goto-line (point-min))
      (while (and (not displayed-p)
                  (re-search-forward (rte-regex) nil t))
        (if (and (>= cur-point (match-beginning 0))
                 (<= cur-point (match-end 0)))
            (progn (put-text-property (match-beginning 1) (match-end 1) 'invisible nil)
                   (message (concat "Variable Name: "
                                    (buffer-substring (match-beginning 1) (match-end 1))))
                   (put-text-property (match-beginning 1) (match-end 1) 'invisible t)
                   (setq displayed-p t))
          (message ""))))))

;; question mark means non-greedy
(defun rte-regex ()
  "Regex to match the return replaced vars from the edts-rte"
  "\{b,\\(.+?\\),s,\\(.+?\\)\,e}")

(defadvice forward-char (after forward-display-rte-var)
  "Advice for forward-char for displaying the rte variable name"
  (display-rte-var))

(defadvice backward-char (after backward-display-rte-var)
  "Advice for backward-char for displaying the rte variable name"
  (display-rte-var))

(defadvice next-line (after next-line-display-rte-var)
  "Advice for next-line for displaying the rte variable name"
  (display-rte-var))

(defadvice previous-line (after previous-line-display-rte-var)
  "Advice for previous for displaying the rte variable name"
  (display-rte-var))

(defun activate-advice ()
  (interactive)
  "Activate all the advices"
  (mapcar (lambda (advice) (ad-activate advice)) (rte-advices)))

(defun deactivate-advice ()
  (interactive)
  "Deactivate all the advices"
  (mapcar (lambda (advice) (ad-deactivate advice)) (rte-advices)))

(defun rte-advices ()
  "All the advices defined in edts rte mode"
  '(forward-char backward-char next-line previous-line))

(defun switch-invisible ()
  "Return a function to switch the invisible property for the part of the
value returned by rte."
  (lambda (flag)
    (put-text-property (match-beginning 0) (match-beginning 2) 'invisible flag)
    (put-text-property (match-end 2) (match-end 0) 'invisible flag)))

(defun rte-keywords ()
  "Keywords in edts-rte mode that needs to be added to font lock"
  `((,(rte-regex) 0 'font-lock-warning-face prepend)
    ("^\\(\\.+\\)" 0 'highlight prepend)))

(add-hook 'edts-code-after-compile-hook 'edts-rte)

(defun edts-rte (result)
  "Execute the edts-rte-run function when there is no error in
the file"
  (when (not (eq result 'error))
    (edts-rte-run)))

(provide 'edts-rte-mode)
