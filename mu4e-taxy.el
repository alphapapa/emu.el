;;; mu4e-taxy.el --- Grouped headers view for mu4e   -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Adam Porter

;; Author: Adam Porter <adam@alphapapa.net>
;; Keywords: comm
;; URL: https://github.com/alphapapa/mu4e-taxy.el
;; Package-Requires: ((taxy-magit-section "0.12.1"))
;; Version: 0.1-pre

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This library implements a grouped message list for mu4e based on
;; `taxy' and `taxy-magit-section'.  To use, activate `mu4e-taxy-mode'
;; and run an mu4e search; the results will be shown in the
;; "*mu4e-taxy*" buffer.

;;; Code:

(require 'mu4e)

(require 'taxy-magit-section)

;;;; Variables

(defvar-local mu4e-taxy-visibility-cache nil)

(defvar mu4e-taxy-old-headers-append-func nil)

;;;; Keys

(eval-and-compile
  (taxy-define-key-definer mu4e-taxy-define-key
    mu4e-taxy-keys "mu4e-taxy-key" "FIXME: Docstring."))

(mu4e-taxy-define-key date (&key (format "%F (%A)"))
  (let ((time (mu4e-message-field item :date)))
    (format-time-string format time)))

(mu4e-taxy-define-key year (&key (format "%Y"))
  (let ((time (mu4e-message-field item :date)))
    (format-time-string format time)))

(mu4e-taxy-define-key month (&key (format "%m (%B %Y)"))
  (let ((time (mu4e-message-field item :date)))
    (format-time-string format time)))

(mu4e-taxy-define-key week (&key (format "W%V (%Y)"))
  (let ((time (mu4e-message-field item :date)))
    (format-time-string format time)))

(mu4e-taxy-define-key from (&key name from)
  (cl-labels ((format-contact (contact)
                (pcase-let* (((map :email :name) contact)
                             (address (format "<%s>" email))
                             (name (when name
                                     (format "%s " name))))
                  (concat name address))))
    (let ((message-from (mu4e-message-field item :from)))
      (pcase from
        ((or `nil (guard (cl-loop for contact on message-from
                                  thereis (or (string-match-p from (plist-get contact :email))
                                              (string-match-p from (plist-get contact :name))))))
         (or name (string-join (mapcar #'format-contact message-from) ",")))))))

(mu4e-taxy-define-key list (&key name list)
  (let ((message-list (mu4e-message-field item :list)))
    (pcase list
      ((or `nil (guard (equal message-list list)))
       (or name message-list)))))

(mu4e-taxy-define-key thread ()
  (pcase-let* ((meta (mu4e-message-field item :meta))
               ((map :thread-subject) meta)
               (subject (mu4e-message-field item :subject)))
    ;; (if thread-subject
    ;;     (concat (mu4e~headers-thread-prefix meta) subject)
    ;;   subject)
    ;; HACK:
    (truncate-string-to-width (string-trim-left subject (rx "Re:" (0+ blank))) 80 nil nil t t)))

(mu4e-taxy-define-key subject (subject &key name match-group)
  (let ((message-subject (mu4e-message-field item :subject)))
    (when (string-match subject message-subject)
      (or (when match-group
            (match-string match-group message-subject))
          name message-subject))))

(defvar mu4e-taxy-default-keys
  `(((subject ,(rx (group "bug#" (1+ digit))) :name "Bugs")
     (subject ,(rx (group "bug#" (1+ digit))) :match-group 1))
    ((not :name "Non-list" :keys (list))
     from thread)
    ((list :name "Mailing lists") list thread))
  "Default keys.")

;;;; Columns

(eval-and-compile
  (taxy-magit-section-define-column-definer "mu4e-taxy"))

(mu4e-taxy-define-column "From" (:max-width 40 :face mu4e-contact-face)
  (cl-labels ((format-contact (contact)
                (pcase-let* (((map :email :name) contact)
                             (address (format "<%s>" email))
                             (name (when name
                                     (format "%s " name))))
                  (concat name address))))
    (pcase-let* (((and meta (map :thread-subject)) (mu4e-message-field item :meta))
                 (prefix (when thread-subject
                           (concat (mu4e~headers-thread-prefix meta) " "))))
      (concat prefix
              (string-join (mapcar #'format-contact (mu4e-message-field item :from)) ",")))))

(mu4e-taxy-define-column "Subject" (:face message-header-subject :max-width 100)
  (mu4e-message-field item :subject))

(mu4e-taxy-define-column "Thread" (:face message-header-subject :max-width 100)
  (let* ((meta (mu4e-message-field item :meta))
         (subject (mu4e-message-field item :subject)))
    (mu4e~headers-thread-prefix meta)
    (if (plist-get meta :thread-subject)
        subject "")))

(mu4e-taxy-define-column "Date" (:face org-time-grid)
  (format-time-string "%c" (mu4e-message-field item :date)))

(mu4e-taxy-define-column "List" ()
  (mu4e-message-field item :list))

(unless mu4e-taxy-columns
  (setq-default mu4e-taxy-columns
                (get 'mu4e-taxy-columns 'standard-value)))

(setq mu4e-taxy-columns '("Date" "From" "Subject"))

;;;; Commands

;; (cl-defun mu4e-taxy (query)
;;   "FIXME: "
;;   (interactive (list mu4e--search-last-query))
;;   (let* ((mu4e-headers-append-func #'mu4e-taxy--headers-append-func)
;;          (rewritten-expr (funcall mu4e-query-rewrite-function query))
;;          (maxnum (unless mu4e-search-full mu4e-search-results-limit)))
;;     (mu4e--server-find
;;      rewritten-expr
;;      mu4e-search-threads
;;      mu4e-search-sort-field
;;      mu4e-search-sort-direction
;;      maxnum
;;      mu4e-search-skip-duplicates
;;      mu4e-search-include-related)))

(cl-defun mu4e-taxy--headers-append-func (messages)
  "FIXME: "
  (let ((buffer (get-buffer-create "*mu4e-taxy*")))
    (with-current-buffer buffer
      (with-silent-modifications
        (erase-buffer)
        (delete-all-overlays)
        (mu4e-taxy-view-mode)
        (setf messages (nreverse (cl-sort messages #'time-less-p
                                          :key (lambda (message)
                                                 (mu4e-message-field message :date)))))
        (mu4e-taxy--insert-taxy-for messages :query mu4e--search-last-query
                                    :prefix-item (lambda (message)
                                                   (mu4e~headers-docid-cookie (mu4e-message-field message :docid)))
                                    :item-properties (lambda (message)
                                                       (list 'docid (plist-get message :docid)
                                                             'msg message)))
        (pop-to-buffer (current-buffer))))))

;;;; Headers commands

;; What a mess, all because mu4e uses `defsubsts' in many places it
;; shouldn't.

(defmacro mu4e-taxy-defcommand (command)
  "FIXME: COMMAND."
  (declare (debug (&define symbolp)))
  (let ((new-name (intern (concat "mu4e-taxy-" (symbol-name command)))))
    `(defun ,new-name (&rest args)
       (interactive)
       ;; HACK: The hackiest of hacks, but it seems to work...
       (let ((major-mode 'mu4e-headers-mode))
         (save-excursion
           (cl-typecase (oref (magit-current-section) value)
             (taxy (dolist (child (oref (magit-current-section) children))
                     (magit-section-goto child)
                     (funcall ',new-name)))
             (otherwise (call-interactively ',command))))))))

;;;; Mode

(defvar-keymap mu4e-taxy-view-mode-map
  :parent magit-section-mode-map
  :doc "Local keymap for `mu4e-taxy-view-mode' buffers."
  "g" #'revert-buffer
  "s" #'mu4e-search
  "RET" (mu4e-taxy-defcommand mu4e-headers-view-message)
  "d" (mu4e-taxy-defcommand mu4e-headers-mark-for-trash)
  "r" (mu4e-taxy-defcommand mu4e-headers-mark-for-refile)
  "u" (mu4e-taxy-defcommand mu4e-headers-mark-for-unmark)
  "x" (mu4e-taxy-defcommand mu4e-mark-execute-all))

(define-derived-mode mu4e-taxy-view-mode magit-section-mode
  "mu4e-taxy"
  "FIXME:"
  :group 'mu4e
  :interactive nil
  ;; HACK:
  (mu4e--mark-initialize)
  (setq revert-buffer-function #'mu4e-taxy-revert-buffer))

(define-minor-mode mu4e-taxy-mode
  "FIXME:"
  :global t
  :group 'mu4e
  (if mu4e-taxy-mode
      (setf mu4e-taxy-old-headers-append-func mu4e-headers-append-func
            mu4e-headers-append-func #'mu4e-taxy--headers-append-func)
    (setf mu4e-headers-append-func mu4e-taxy-old-headers-append-func
          mu4e-taxy-old-headers-append-func nil))
  (message "mu4e-taxy-mode %s." (if mu4e-taxy-mode "enabled" "disabled")))

;;;; Functions

(defun mu4e-taxy-revert-buffer (&optional _ignore-auto _noconfirm)
  "Revert `mu4e-taxy-mode' buffer.
Runs `mu4e-taxy' again with the same query."
  (mu4e-search mu4e--search-last-query))

(cl-defun mu4e-taxy--insert-taxy-for
    (messages &key (keys mu4e-taxy-default-keys) (query mu4e--search-last-query)
              (prefix-item #'ignore) (item-properties #'ignore))
  "Insert and return a `taxy' for `mu4e-taxy', optionally having ITEMS.
KEYS should be a list of grouping keys, as in
`mu4e-taxy-default-keys'."
  (let (format-table column-sizes)
    (cl-labels ((format-item (item)
                  (let ((string (concat (funcall prefix-item item)
                                        (gethash item format-table))))
                    (add-text-properties 0 (length string)
                                         (funcall item-properties item) string)
                    string))
                (make-fn (&rest args)
                  (apply #'make-taxy-magit-section
                         :make #'make-fn
                         :format-fn #'format-item
                         ;; FIXME: Make indent an option again.
                         :level-indent 2
                         ;; :visibility-fn #'visible-p
                         ;; :heading-indent 2
                         :item-indent 0
                         ;; :heading-face-fn #'heading-face
                         args)))
      (let* ((taxy-magit-section-insert-indent-items nil)
             ;; (taxy-magit-section-item-indent 0)
             ;; (taxy-magit-section-level-indent 0)
             (taxy
              (thread-last
                (make-fn :name (format "mu4e: %s" query)
                         :take (taxy-make-take-function keys mu4e-taxy-keys))
                (taxy-fill messages)))
             (format-cons
              (taxy-magit-section-format-items
               mu4e-taxy-columns mu4e-taxy-column-formatters
               taxy))
             (inhibit-read-only t))
        (setf format-table (car format-cons)
              column-sizes (cdr format-cons)
              header-line-format (taxy-magit-section-format-header
                                  column-sizes mu4e-taxy-column-formatters)
              ;; Sort taxys by the most recent message in each.
              taxy (thread-last taxy
                                (taxy-sort-taxys (lambda (a b)
                                                   (not (time-less-p a b)))
                                  (lambda (taxy)
                                    (when (taxy-items taxy)
                                      (mu4e-message-field (car (seq-sort (lambda (a b)
                                                                           (not (time-less-p (mu4e-message-field a :date)
                                                                                             (mu4e-message-field b :date))))
                                                                         (taxy-items taxy)))
                                                          :date))))
                                (taxy-sort-taxys (lambda (a _b)
                                                   (not (equal a "Mailing lists")))
                                  #'taxy-name)))
        ;; Before this point, no changes have been made to the buffer's contents.
        (save-excursion
          (taxy-magit-section-insert taxy :items 'first :initial-depth 0))
        taxy))))

(provide 'mu4e-taxy)
;;; mu4e-taxy.el ends here
