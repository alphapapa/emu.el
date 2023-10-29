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

(mu4e-taxy-define-key date-format (&key (format "%B (%Y-%m)"))
  (let ((time (mu4e-message-field item :date)))
    (format-time-string format time)))

(defvar mu4e-taxy-default-keys
  '(date-format)
  "Default keys.")

;;;; Columns

(eval-and-compile
  (taxy-magit-section-define-column-definer "mu4e-taxy"))

(mu4e-taxy-define-column "From" (:max-width 35 :face mu4e-contact-face)
  (cl-labels ((format-contact (contact)
                (pcase-let* (((map :email :name) contact)
                             (address (format "<%s>" email))
                             (name (when name
                                     (format "%s " name))))
                  (concat name address))))
    (string-join (mapcar #'format-contact (mu4e-message-field item :from)) ",")))

(mu4e-taxy-define-column "Subject" (:face message-header-subject)
  (mu4e-message-field item :subject))

(mu4e-taxy-define-column "Date" (:face org-time-grid)
  (format-time-string "%c" (mu4e-message-field item :date)))

(unless mu4e-taxy-columns
  (setq-default mu4e-taxy-columns
                (get 'mu4e-taxy-columns 'standard-value)))

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
        (mu4e-taxy--insert-taxy-for (nreverse messages) :query mu4e--search-last-query)
        (pop-to-buffer (current-buffer))))))

;;;; Mode

(defvar-keymap mu4e-taxy-view-mode-map
  :parent magit-section-mode-map
  :doc "Local keymap for `mu4e-taxy-view-mode' buffers."
  "g" #'revert-buffer
  "s" #'mu4e-search)

(define-derived-mode mu4e-taxy-view-mode magit-section-mode
  "mu4e-taxy"
  "FIXME:"
  :group 'mu4e
  :interactive nil
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
    (messages &key (keys mu4e-taxy-default-keys) (query mu4e--search-last-query))
  "Insert and return a `taxy' for `mu4e-taxy', optionally having ITEMS.
KEYS should be a list of grouping keys, as in
`mu4e-taxy-default-keys'."
  (let (format-table column-sizes)
    (cl-labels ((format-item (item) (gethash item format-table))
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
                                  column-sizes mu4e-taxy-column-formatters))
        ;; Before this point, no changes have been made to the buffer's contents.
        (save-excursion
          (taxy-magit-section-insert taxy :items 'first :initial-depth 0))
        taxy))))

(provide 'mu4e-taxy)
;;; mu4e-taxy.el ends here
