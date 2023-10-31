;;; emu.el --- Alternative UI for for mu4e   -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Adam Porter

;; Author: Adam Porter <adam@alphapapa.net>
;; Keywords: comm
;; URL: https://github.com/alphapapa/emu.el
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

;; This library implements an alternative UI for mu4e based on `taxy'
;; and `taxy-magit-section'.  To use, activate `emu-mode' and run a
;; mu4e search; the results will be shown in the "*emu*" buffer.

;;; Code:

(require 'mu4e)

(require 'taxy-magit-section)

;;;; Variables

(defvar-local emu-visibility-cache nil)

(defvar emu-old-headers-append-func nil)

(defvar emu-new-headers nil
  "Accumulates headers from a new search.")

(defvar emu-progress-reporter nil)

;;;; Faces

(defface emu-contact '((t (:inherit font-lock-variable-name-face)))
  "Contact names.")

(defface emu-subject '((t (:inherit font-lock-function-name-face)))
  "Subjects.")

(defface emu-unread '((t (:inherit bold)))
  "Unread messages.")

(defface emu-new '((t (:underline t)))
  "New messages.")

(defface emu-flagged '((t ()))
  "Flagged messages.")

;;;; Keys

(eval-and-compile
  (taxy-define-key-definer emu-define-key
    emu-keys "emu-key" "FIXME: Docstring."))

(emu-define-key date (&key (format "%F (%A)"))
  (let ((time (mu4e-message-field item :date)))
    (format-time-string format time)))

(emu-define-key year (&key (format "%Y"))
  (let ((time (mu4e-message-field item :date)))
    (format-time-string format time)))

(emu-define-key month (&key (format "%m (%B %Y)"))
  (let ((time (mu4e-message-field item :date)))
    (format-time-string format time)))

(emu-define-key week (&key (format "W%V (%Y)"))
  (let ((time (mu4e-message-field item :date)))
    (format-time-string format time)))

(emu-define-key from (&key title name address match-group)
  (cl-labels ((format-contact (contact)
                (pcase-let* (((map :email :name) contact)
                             (address (format "<%s>" email))
                             (name (when name
                                     (format "%s " name))))
                  (concat name address))))
    (let ((message-from (mu4e-message-field item :from))
          matched)
      (if (or name address)
          (when (cl-loop for contact in message-from
                         thereis (pcase-let (((map :email (:name contact-name)) contact))
                                   (or (when email
                                         (when (string-match address email)
                                           (if match-group
                                               (setf matched (match-string match-group email))
                                             t)))
                                       (when name
                                         (when (string-match name contact-name)
                                           (if match-group
                                               (setf matched (match-string match-group contact-name))
                                             t))))))
            (or matched title (string-join (mapcar #'format-contact message-from) ",")))
        (string-join (mapcar #'format-contact message-from) ",")))))

(emu-define-key list (&key name regexp)
  (when-let ((message-list (mu4e-message-field item :list)))
    (pcase regexp
      ((or `nil (guard (string-match-p regexp message-list)))
       (or name message-list)))))

(emu-define-key thread ()
  (pcase-let* ((meta (mu4e-message-field item :meta))
               ((map :thread-subject) meta)
               (subject (mu4e-message-field item :subject)))
    ;; (if thread-subject
    ;;     (concat (mu4e~headers-thread-prefix meta) subject)
    ;;   subject)
    ;; HACK:
    (truncate-string-to-width (string-trim-left subject (rx "Re:" (0+ blank))) 80 nil nil t t)))

(emu-define-key subject (subject &key name match-group)
  (let ((message-subject (mu4e-message-field item :subject)))
    (when (string-match subject message-subject)
      (or (when match-group
            (match-string match-group message-subject))
          name message-subject))))

(emu-define-key maildir (&key name regexp)
  (let ((maildir (mu4e-message-field item :maildir)))
    (when (string-match regexp maildir)
      (or name maildir))))

(emu-define-key num-to/cc> (&key name num)
  (let ((contacts (append (mu4e-message-field item :to)
                          (mu4e-message-field item :cc))))
    (when (> (length contacts) num)
      (or name num))))

(emu-define-key sent (&key name)
  (when-let ((address (cl-loop for contact in (mu4e-message-field item :from)
                               for address = (plist-get contact :email)
                               when (mu4e-personal-address-p address)
                               return address)))
    (or name (concat "Sent from:" address))))

(defvar emu-default-keys
  `((sent thread)
    ((maildir :name "Spam" :regexp ,(rx "/" (or "Junk" "Spam") eos))
     from)
    ((maildir :name "Trash" :regexp ,(rx "/Trash"))
     ((num-to/cc> :name "Group conversations" :num 1)
      thread)
     from)
    ((maildir :name "Archives" :regexp ,(rx "/Archives/"))
     ((num-to/cc> :name "Group conversations" :num 1)
      thread)
     from)
    ((maildir :name "Inbox" :regexp ,(rx "/Inbox"))
     ((num-to/cc> :name "Group conversations" :num 1)
      thread)
     ((list :name "Mailing lists")
      (list :name "GitHub" :regexp "github.com")
      list thread)
     from)
    ((subject ,(rx (group "bug#" (1+ digit))) :name "Bugs")
     (subject ,(rx (group "bug#" (1+ digit))) :match-group 1))
    ((not :name "Non-list" :keys (list))
     from thread)
    ((list :name "Mailing lists")
     (list :name "GitHub" :regexp "github.com")
     list thread))
  "Default keys.")

(defvar emu-mailing-list-keys `(thread))

(defvar emu-spam-keys
  `((sent thread)
    ((maildir :name "Trash" :regexp ,(rx "/Trash"))
     ((num-to/cc> :name "Group conversations" :num 1)
      thread)
     from)
    ((maildir :name "Archives" :regexp ,(rx "/Archives/"))
     ((num-to/cc> :name "Group conversations" :num 1)
      thread)
     from)
    ((from :title "Domain" :address ,(rx (group "." (1+ (not (any ".")))) eos) :match-group 1))
    ((maildir :name "Inbox" :regexp ,(rx "/Inbox"))
     ((num-to/cc> :name "Group conversations" :num 1)
      thread)
     from)
    ((not :name "Non-list" :keys (list))
     from thread)
    ((list :name "Mailing lists") list thread))
  "Keys helpful for collecting and deleting old spam.")

;; (setq emu-default-keys emu-mailing-list-keys)

;; (setq emu-default-keys emu-spam-keys)

;;;; Columns

(eval-and-compile
  (taxy-magit-section-define-column-definer "emu"))

(emu-define-column "From" (:max-width 40 :face emu-contact)
  (cl-labels ((format-contact (contact)
                (pcase-let* (((map :email :name) contact)
                             (address (format "<%s>" email))
                             (name (when name
                                     (format "%s " name))))
                  (concat name address))))
    (pcase-let* (((and meta (map :thread-subject)) (mu4e-message-field item :meta))
                 (prefix (when thread-subject
                           (concat (mu4e~headers-thread-prefix meta) " "))))
      ;; (concat prefix
      ;;         (string-join (mapcar #'format-contact (mu4e-message-field item :from)) ","))
      (string-join (mapcar #'format-contact (mu4e-message-field item :from)) ","))))

(emu-define-column "Subject" (:face emu-subject :max-width 100)
  (mu4e-message-field item :subject))

(emu-define-column "Thread" (:face emu-subject :max-width 100)
  (let* ((meta (mu4e-message-field item :meta))
         (subject (mu4e-message-field item :subject)))
    (if (plist-get meta :thread-subject)
        (concat (mu4e~headers-thread-prefix meta)
                " " subject)
      subject)))

(emu-define-column "Date" (:face org-time-grid)
  (format-time-string "%c" (mu4e-message-field item :date)))

(emu-define-column "List" ()
  (mu4e-message-field item :list))

(emu-define-column "Maildir" ()
  (mu4e-message-field item :maildir))

(emu-define-column "Flags" (:face font-lock-warning-face)
  (mu4e~headers-flags-str (mu4e-message-field item :flags)))

(emu-define-column #("🚩" 0 1 (help-echo "Flagged")) ()
  (if (member 'flagged (mu4e-message-field item :flags))
      "🚩" " "))

(emu-define-column #("🧍" 0 1 (help-echo "Personal")) ()
  (if (member 'personal (mu4e-message-field item :flags))
      "🧍" " "))

(emu-define-column #("👓" 0 1 (help-echo "Unread")) ()
  (if (member 'unread (mu4e-message-field item :flags))
      ;; The emoji's visual width isn't calculated correctly.
      "U" " "))

(emu-define-column #("📎" 0 1 (help-echo "Attachment")) ()
  (if (member 'attach (mu4e-message-field item :flags))
      "📎" " "))

(emu-define-column #("🗑" 0 1 (help-echo "Trash")) ()
  (if (member 'trashed (mu4e-message-field item :flags))
      ;; The emoji's visual width isn't calculated correctly.
      "T" " "))

(unless emu-columns
  (setq-default emu-columns
                (get 'emu-columns 'standard-value)))

(setq emu-columns '("🗑" "📎" "👓" "🚩" "🧍" "Flags" "Date" "From" "Thread" "Maildir"))

;;;; Commands

;; (cl-defun emu (query)
;;   "FIXME: "
;;   (interactive (list mu4e--search-last-query))
;;   (let* ((mu4e-headers-append-func #'emu--headers-append-func)
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

(cl-defun emu--headers-append-func (messages)
  "FIXME: "
  (cl-callf append emu-new-headers messages)
  (progress-reporter-update emu-progress-reporter nil (format "%s" (length emu-new-headers))))

(cl-defun emu--headers-found-hook ()
  "FIXME: "
  (let ((buffer (get-buffer-create "*emu*"))
        (messages emu-new-headers))
    (setf emu-new-headers nil)
    (when messages
      ;; HACK: Why would messages be nil?
      (with-current-buffer buffer
        (with-silent-modifications
          (erase-buffer)
          (delete-all-overlays)
          (emu-view-mode)
          (setf messages (nreverse (cl-sort messages #'time-less-p
                                            :key (lambda (message)
                                                   (mu4e-message-field message :date)))))
          (save-excursion
            (emu--insert-taxy-for messages :query mu4e--search-last-query
                                  :prefix-item (lambda (message)
                                                 (mu4e~headers-docid-cookie (mu4e-message-field message :docid)))
                                  :item-properties (lambda (message)
                                                     (list 'docid (plist-get message :docid)
                                                           'msg message))
                                  :add-faces (lambda (message)
                                               (remq nil
                                                     (list (when (member 'unread (mu4e-message-field message :flags))
                                                             'emu-unread)
                                                           (when (member 'flagged (mu4e-message-field message :flags))
                                                             'emu-flagged)
                                                           ;; (when (member 'new (mu4e-message-field message :flags))
                                                           ;;   'emu-new)
                                                           )))))
          (when magit-section-visibility-cache
            (save-excursion
              ;; Somehow `magit-section-forward' isn't working from the root section.
              (forward-line 1)
              (cl-loop with last-section = (magit-current-section)
                       do (oset (magit-current-section) hidden
                                (magit-section-cached-visibility (magit-current-section)))
                       while (progn
                               (forward-line 1)
                               (and (magit-current-section)
                                    (not (eobp))
                                    (not (equal last-section (magit-current-section))))))))
          (pop-to-buffer (current-buffer) '(display-buffer-same-window)))))))

;;;; Headers commands

;; What a mess, all because mu4e uses `defsubsts' in many places it
;; shouldn't.

(defmacro emu-defcommand (command)
  "FIXME: COMMAND."
  (declare (debug (&define symbolp)))
  (let ((new-name (intern (concat "emu-" (symbol-name command)))))
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

(defmacro emu-define-mark-command (command)
  "FIXME: COMMAND."
  (declare (debug (&define symbolp)))
  (let ((new-name (intern (concat "emu-" (symbol-name command)))))
    `(defun ,new-name (&rest args)
       (interactive)
       ;; HACK: The hackiest of hacks, but it seems to work...
       (let ((major-mode 'mu4e-headers-mode))
         (save-excursion
           (cl-typecase (oref (magit-current-section) value)
             (taxy (dolist (child (oref (magit-current-section) children))
                     (magit-section-goto child)
                     (funcall ',new-name)))
             (otherwise (call-interactively ',command)))))
       (magit-section-forward))))

(defun emu-peek-message ()
  (interactive)
  (letrec ((temp-fn
            (lambda ()
              (remove-hook 'mu4e-view-rendered-hook temp-fn)
              (when-let ((buffer (get-buffer "*emu*")))
                (run-at-time nil nil (lambda ()
                                       (pop-to-buffer buffer '(display-buffer-reuse-window))))))))
    (add-hook 'mu4e-view-rendered-hook temp-fn)
    (call-interactively #'emu-mu4e-headers-view-message)))

(defun emu-message-at-point ()
  (let ((major-mode 'mu4e-headers-mode))
    (mu4e-message-at-point)))

;;;; Mode

(defvar-keymap emu-view-mode-map
  :parent magit-section-mode-map
  :doc "Local keymap for `emu-view-mode' buffers."
  "g" #'revert-buffer
  "s" #'mu4e-search
  "RET" (emu-defcommand mu4e-headers-view-message)
  "SPC" #'emu-peek-message
  "a" (emu-define-mark-command mu4e-headers-mark-for-refile)
  "k" (emu-define-mark-command mu4e-headers-mark-for-trash)
  "f" (emu-define-mark-command mu4e-headers-mark-for-flag)
  "F" (emu-define-mark-command mu4e-headers-mark-for-unflag)
  "m" (emu-define-mark-command mu4e-headers-mark-for-move)
  "r" (emu-define-mark-command mu4e-headers-mark-for-read)
  "R" (emu-define-mark-command mu4e-headers-mark-for-unread)
  "u" (emu-define-mark-command mu4e-headers-mark-for-unmark)
  "x" (emu-defcommand mu4e-mark-execute-all))

(define-derived-mode emu-view-mode magit-section-mode
  "emu"
  "FIXME:"
  :group 'mu4e
  :interactive nil
  ;; HACK:
  (mu4e--mark-initialize)
  (setq revert-buffer-function #'emu-revert-buffer))

(define-minor-mode emu-mode
  "FIXME:"
  :global t
  :group 'mu4e
  (letrec ((search-fn
            (lambda (&rest _)
              (setf emu-progress-reporter (make-progress-reporter "Emu searching...")))))
    (if emu-mode
        (progn
          (setf emu-old-headers-append-func mu4e-headers-append-func
                mu4e-headers-append-func #'emu--headers-append-func)
          (add-hook 'mu4e-search-hook search-fn)
          (add-hook 'mu4e-headers-found-hook #'emu--headers-found-hook))
      (setf mu4e-headers-append-func emu-old-headers-append-func
            emu-old-headers-append-func nil)
      (remove-hook 'mu4e-headers-found-hook #'emu--headers-found-hook)
      (remove-hook 'mu4e-search-hook search-fn)))
  (message "emu-mode %s." (if emu-mode "enabled" "disabled")))

;;;; Functions

(defun emu-revert-buffer (&optional _ignore-auto _noconfirm)
  "Revert `emu-mode' buffer.
Runs `emu' again with the same query."
  (cl-assert (derived-mode-p 'emu-view-mode))
  ;; HACK:
  (cl-letf (((symbol-function 'mu4e--get-current-buffer-type)
             (lambda (&rest _)
               'headers)))
    (unless (zerop (mu4e-mark-marks-num))
      (emu-mu4e-mark-execute-all)))
  (mu4e-search mu4e--search-last-query))

(cl-defun emu--insert-taxy-for
    (messages &key (keys emu-default-keys) (query mu4e--search-last-query)
              (prefix-item #'ignore) (item-properties #'ignore) (add-faces #'ignore))
  "Insert and return a `taxy' for `emu', optionally having ITEMS.
KEYS should be a list of grouping keys, as in
`emu-default-keys'."
  (setf emu-progress-reporter (make-progress-reporter "Emu: Sorting messages..." 0 (length messages)))
  (let (format-table column-sizes)
    (cl-labels ((format-item (item)
                  (let ((string (concat (funcall prefix-item item)
                                        (gethash item format-table))))
                    (add-text-properties 0 (length string)
                                         (funcall item-properties item) string)
                    (dolist (face (funcall add-faces item))
                      (add-face-text-property 0 (length string) face nil string))
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
             (taxy (make-fn :name (format "mu4e: %s" query)
                            :take (taxy-make-take-function keys emu-keys)))
             (_ (cl-loop with target-chunks = 20
                         with total = (length messages)
                         with chunk-size = (if (< total target-chunks)
                                               total
                                             (/ total target-chunks))
                         with num-chunks = (/ total chunk-size)
                         for i from 0 to num-chunks
                         do (progn
                              (taxy-fill (cl-subseq messages (* i chunk-size)
                                                    (min (+ (* i chunk-size) chunk-size) total))
                                         taxy)
                              (progress-reporter-update emu-progress-reporter (* i chunk-size)))))
             (format-cons
              (taxy-magit-section-format-items
               emu-columns emu-column-formatters
               taxy))
             (inhibit-read-only t))
        (setf format-table (car format-cons)
              column-sizes (cdr format-cons)
              header-line-format (taxy-magit-section-format-header
                                  column-sizes emu-column-formatters)
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
        (setf emu-progress-reporter (make-progress-reporter "Emu: Inserting messages..."))
        (let (magit-section-visibility-cache)
          (save-excursion
            (taxy-magit-section-insert taxy :items 'first :initial-depth 0)))
        (progress-reporter-done emu-progress-reporter)
        taxy))))

(provide 'emu)
;;; emu.el ends here
