;;; eat.el --- Combine avy and transient for faster editing -*- lexical-binding: t -*-

(require 'avy)
(require 'transient)

;;;; Infixes

(defvar eat-current-region-action nil)

(defclass eat-region-action (transient-variable)
  ((symbol :initarg :symbol)
   (default :initarg :default :initform nil)
   (region-operation :initarg :region-operation)
   (post-action :initarg :post-action)))

(cl-defmethod transient-init-value ((obj eat-region-action))
  (when (oref obj default)
    (setq eat-current-region-action obj)
    (oref obj symbol)))

(cl-defmethod transient-infix-read ((_ eat-region-action))
  (oref obj symbol))

(cl-defmethod transient-infix-set ((obj eat-region-action) value)
  (oset obj value value)
  (setq eat-current-region-action obj))

(cl-defmethod transient-format-value ((obj eat-region-action))
  (let ((value (and eat-current-region-action
                    (eq (oref eat-current-region-action symbol)
                        (oref obj symbol)))))
    (propertize (if value
                    "to cursor"
                  "N/A")
                'face (if value
                          'transient-value
                        'transient-inactive-value))))

(defun eat--current-region-action ()
  (let ((x eat-current-region-action))
    (list (oref x region-operation)
          (oref x post-action))))

;;;;; Mirror to cursor

(transient-define-infix eat-duplicate-action ()
  :class 'eat-region-action
  :description "Duplicate"
  :symbol 'duplicate
  :default t
  :region-operation (lambda (beg end)
                      (kill-new (buffer-substring beg end)))
  :post-action #'yank)

(transient-define-infix eat-transport-action ()
  :class 'eat-region-action
  :description "Transport"
  :symbol 'transport
  :region-operation #'kill-region
  :post-action #'yank)

;;;; Suffixes

(cl-defmacro eat-def-thing (name &key description region)
  (declare (indent 1))
  `(transient-define-suffix ,name (operation post-action)
     :description ,description
     (interactive (eat--current-region-action))
     (let ((avy-all-windows t)
           (avy-pre-action (eat--region-pre-action
                            operation
                            ,region)))
       (save-excursion
         (save-window-excursion
           (call-interactively #'avy-goto-char-timer)))
       (funcall post-action))))

(defmacro eat--region-pre-action (operation operand)
  `(lambda (res)
     (let ((start (caar res))
           (window (cdr res)))
       (with-current-buffer (window-buffer window)
         (save-excursion
           (goto-char start)
           (apply ,operation ,operand))))))

(eat-def-thing eat-symbol
  :description "symbol"
  :region
  (list (if (looking-at (rx symbol-start))
            (point)
          (re-search-backward (rx symbol-start) nil t))
        (save-excursion
          (re-search-forward
           (rx (group (+? anything)) symbol-end)
           nil t))))

(eat-def-thing eat-word
  :description "word"
  :region
  (list (if (looking-at (rx word-start))
            (point)
          (re-search-backward (rx word-start) nil t))
        (save-excursion
          (re-search-forward
           (rx (group (+? anything)) word-end)
           nil t))))

;;;; Commands

(transient-define-prefix eat-edit ()
  "FIXME"
  ["Action"
   ("d" eat-duplicate-action)
   ("t" eat-transport-action)]
  ["Thing"
   ("s" eat-symbol)
   ("w" eat-word)])

(provide 'eat)
;;; eat.el ends here
