;;; org-notepad.el --- Pop to new Org headings for quick notetaking  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Adam Porter

;; Author: Adam Porter <adam@alphapapa.net>
;; Url: https://github.com/alphapapa/org-notepad
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
;; `org-notepad' returns a buffer, it can be used in the `yequake'
;; `buffer-fns'.  So with one, globally bound keystroke, a new Emacs
;; frame appears showing a new, empty Org heading, timestamped with
;; the current time, ready for taking notes.  See
;; <https://github.com/alphapapa/yequake>.

;; [2020-01-29 Wed 18:18] Renaming to org-notepad because I want to
;; expand its scope slightly beyond just popping up a new note.  I
;; want it to pop up a "notepad"-like window with a new, empty
;; heading, but also showing existing notes for easy access.  I'm just
;; experimenting for now.

;;; Code:

;;;; Requirements

(require 'cl-lib)
(require 'org)

;;;; Variables

;;;; Customization

(defgroup org-notepad nil
  "Options for `org-notepad'."
  :group 'org
  :link '(url-link "https://github.com/alphapapa/org-notepad"))

(defcustom org-notepad-file "~/org/temp.org"
  "File to put new notes in."
  :type '(file :must-match t))

(defcustom org-notepad-outline-path '("Notes")
  "Outline path where new notes are created."
  :type '(repeat string))

(defcustom org-notepad-new-note-hook
  '(org-notepad-rebind-ret org-notepad-new-note-timestamp)
  "Hook called when new note is created.
Called with point on the new heading.  Each hook function should
return with point in the same place, unless its purpose is to
move point."
  :type 'hook)

;;;; Commands

;;;###autoload
(defun org-notepad ()
  "Return buffer showing the `org-notepad' heading with a new, empty note.
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
  (switch-to-buffer (or (get-file-buffer org-notepad-file)
			(find-file-noselect org-notepad-file)))
  (pcase-let* ((parent-marker (org-find-olp org-notepad-outline-path 'this-buffer)))
    (goto-char parent-marker)
    (if (save-excursion
          (goto-char (org-end-of-subtree))
          (outline-back-to-heading)
          (nth 4 (org-heading-components)))
        (progn
          ;; Last heading is non-empty: insert a new heading.
          (org-insert-heading-respect-content)
          ;; TODO: Ensure the region is deactivated before demotion.
          (org-do-demote))
      ;; Last heading is empty: go to it.
      (goto-char (org-end-of-subtree))
      (outline-back-to-heading))
    (switch-to-buffer (org-notepad-tree-indirect-buffer))
    (run-hooks 'org-notepad-new-note-hook)
    (current-buffer)))

;;;; Functions

(defun org-notepad-new-note-timestamp ()
  "Insert timestamp and leave point in heading.
For `org-notepad-new-note-hook'."
  (end-of-line)
  (save-excursion
    (insert "\n\n")
    (org-insert-time-stamp (current-time) 'with-hm 'inactive)
    (insert "  ")))

(defun org-notepad-tree-indirect-buffer ()
  "Return an indirect buffer narrowed to current subtree.
Like `org-tree-to-indirect-buffer', but does what we need."
  (let* ((pos (point))
	 (buffer-name (generate-new-buffer-name
                       (concat (or (nth 4 (org-heading-components)) "") "::"
                               (file-name-nondirectory (buffer-file-name (current-buffer))))))
         (buffer (make-indirect-buffer (current-buffer) buffer-name 'clone)))
    (with-current-buffer buffer
      ;; NOTE: Point must be set again in the indirect buffer.  I don't know why.
      (goto-char pos)
      (org-narrow-to-subtree)
      (current-buffer))))

(defun org-notepad-rebind-ret ()
  "Bind RET in current buffer to a special function in a copied keymap.
The function renames the buffer to the first heading's name when
point is on a heading, then calls `org-notepad-goto-entry-end'."
  (let* ((map (copy-keymap (current-local-map)))
         (orig-def (lookup-key map (kbd "RET") t))
         (docstring (format "With point on a heading, rename buffer accordingly, then call `org-notepad-goto-entry-end'.
Otherwise, call %s."
                            orig-def))
         (fn `(lambda ()
                ,docstring
                (interactive)
                (if (save-excursion
                      (beginning-of-line)
                      (org-at-heading-p))
                    (progn
                      (org-notepad-rename-buffer)
                      (org-notepad-goto-entry-end))
                  (funcall #',orig-def)))))
    (define-key map (kbd "RET") fn)
    (use-local-map map)))

(defun org-notepad-goto-entry-end ()
  "Move point to end of entry content."
  (goto-char (org-entry-end-position))
  (when (re-search-backward (rx (not space)) (org-entry-beginning-position) t)
    (end-of-line)))

(defun org-notepad-rename-buffer ()
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

(provide 'org-notepad)

;;; org-notepad.el ends here
