;;; org-popnote.el --- Pop to new Org headings for quick notetaking  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Adam Porter

;; Author: Adam Porter <adam@alphapapa.net>
;; Url: https://github.com/alphapapa/org-popnote
;; Version: 0.1-pre
;; Package-Requires: ((emacs "26.1") (org "9.0"))
;; Keywords: convenience, outlines

;;; License:

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

;; This package makes it easy to quickly take a note in a new heading
;; displayed in a new indirect buffer.  It's similar to Org's built-in
;; capture functionality, but it's not limited to one capture
;; "session", so you can pop up a new note at any time, without
;; interfering with other activities in Emacs.

;; This is especially useful with `yequake' because, since
;; `org-popnote' returns a buffer, it can be used in the `yequake'
;; `buffer-fns'.  So with one, globally bound keystroke, a new Emacs
;; frame appears showing a new, empty Org heading, timestamped with
;; the current time, ready for taking notes.  See
;; <https://github.com/alphapapa/yequake>.

;;; Code:

;;;; Requirements

(require 'cl-lib)
(require 'org)

;;;; Variables

;;;; Customization

(defgroup org-popnote nil
  "Options for `org-popnote'."
  :group 'org
  :link '(url-link "https://github.com/alphapapa/org-popnote"))

(defcustom org-popnote-file "~/org/temp.org"
  "File to put new notes in."
  :type '(file :must-match t))

(defcustom org-popnote-outline-path '("Notes")
  "Outline path where new notes are created."
  :type '(repeat string))

(defcustom org-popnote-timestamp-format "[%Y-%m-%d %a %H:%M:%S]"
  "Timestamp format string used for new headings.
Org does not include the second in timestamps, but it's important
that it be included here, because it's used to disambiguate notes
created within a short period of time.  (One-second resolution is
generally good enough.)"
  :type 'string)

;;;; Commands

;;;###autoload
(defun org-popnote ()
  "Make new entry in `org-popnote-file' and show it in an indirect buffer.
Return indirect buffer.  In the indirect buffer, \"RET\" is bound
to a function which renames the buffer to the first heading when
point is on a heading."
  ;; FIXME: If two notes are made in the same clock minute, they're both shown in the indirect
  ;; buffer, because the name of the buffer is computed to be the same, because the first
  ;; time, the buffer name is computed to be just the timestamp and filename, and the second
  ;; time it's computed the same, even if the heading was modified before.  Or something like
  ;; that.  This is way harder than it should be.  But this works well enough for now.
  (interactive)
  ;; NOTE: We do not use `with-current-buffer' around the whole function.  Trust me.
  (switch-to-buffer (or (get-file-buffer org-popnote-file)
			(find-file-noselect org-popnote-file)))
  (pcase-let* ((parent-marker (org-find-olp org-popnote-outline-path 'this-buffer))
               (timestamp (org-popnote-timestamp))
               (`(,_ ,_file ,_ ,pos)
                (org-refile-new-child (list nil org-popnote-file nil parent-marker) timestamp))
	       (indirect-buffer (org-with-point-at pos
				  (org-popnote-tree-indirect-buffer))))
    (switch-to-buffer indirect-buffer)
    (end-of-line)
    (insert " ")
    (org-popnote-rebind-ret)
    (current-buffer)))

;;;; Functions

(defun org-popnote-tree-indirect-buffer ()
  "Return an indirect buffer narrowed to current subtree.
Like `org-tree-to-indirect-buffer', but does what we need."
  (let* ((pos (point))
	 (buffer-name (concat (nth 4 (org-heading-components)) "::"
                              (file-name-nondirectory (buffer-file-name (current-buffer)))))
         (buffer (make-indirect-buffer (current-buffer) buffer-name 'clone)))
    (with-current-buffer buffer
      ;; NOTE: Point must be set again in the indirect buffer.  I don't know why.
      (goto-char pos)
      (org-narrow-to-subtree)
      (current-buffer))))

(defun org-popnote-timestamp ()
  "Return current timestamp."
  ;; TODO: If there's an existing Org function that does this, use it; otherwise, propose upstream.
  (with-temp-buffer
    (insert (format-time-string org-popnote-timestamp-format))
    (buffer-string)))

(defun org-popnote-rebind-ret ()
  "Bind RET in current buffer to a special function in a copied keymap.
The function renames the buffer to the first heading's name when
point is on a heading, then call RET's previous definition."
  (let* ((map (copy-keymap (current-local-map)))
         (orig-def (lookup-key map (kbd "RET") t)))
    ;; Bind RET, only in this buffer, to rename the buffer to the heading.
    (define-key map (kbd "RET")
      `(lambda ()
         ,(format "With point on a heading, rename buffer accordingly.  Then call %s." orig-def)
         (interactive)
         (when (save-excursion
                 (beginning-of-line)
                 (org-at-heading-p))
           (org-popnote-rename-buffer))
         (funcall #',orig-def)))
    (use-local-map map)))

(defun org-popnote-rename-buffer ()
  "Rename current buffer based on first heading in buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (unless (org-at-heading-p)
      (outline-next-heading))
    (let* ((buffer (or (buffer-base-buffer (current-buffer))
                       (current-buffer)))
           (file-name (file-name-nondirectory (buffer-file-name buffer))))
      (rename-buffer (concat (nth 4 (org-heading-components)) "::" file-name)))))

;;;; Footer

(provide 'org-popnote)

;;; org-popnote.el ends here
