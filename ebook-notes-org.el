;;; ebook-notes-org.el --- Generate Org entries from the clippings -*- lexical-binding: t -*-
;;
;; Copyright (C) 2025 Stewart V. Wright

;; Author: Stewart V. Wright <stewart@vifortech.com>

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

;; Helper functions to generate and insert Org mode entries from the clippings.

(require 'ebook-notes-parse)
(require 'ebook-notes-bibtex)


(defcustom generate-entry-string 'ebook-notes/generate-org-entry-string
  "Function for generating an org entry from the clippings data.

Arguments for this function must be:
 HIGHLIGHT  NOTE-TEXT  &optional NOTE-AS-HIGHLIGHT

See ebook-notes/generate-org-entry-string for what those arguments should contain."
  :type 'function
  :group 'kindle-notes)


(defun ebook-notes/generate-org-entry-string (highlight note-text &optional note-as-highlight)
  "Generates an Org-mode entry string from a Kindle highlight and/or note.

This function creates a new Org-mode heading with a unique ID and a reference
to the source book. It can handle several scenarios based on the input:

- If both HIGHLIGHT and NOTE-TEXT are present, it combines them into a
  single entry, with the highlight formatted as a quote and the note text
  preceding it.
- If only HIGHLIGHT is present (and NOTE-TEXT is an empty string), it
  creates an entry with the highlight as a quote and adds a
  `:interpretation_needed:` property to flag it for later review.
- If only HIGHLIGHT is present and NOTE-AS-HIGHLIGHT is non-nil,
  then the note is masquerading as a highlight, so it generates
  an entry with the note text directly, without a quote block.
- It automatically finds a BibTeX reference key for the clipping if one
  exists, otherwise it uses the book's title and author as a reference.

Arguments:
HIGHLIGHT -- A `kindle-entry' struct representing the highlight.
NOTE-TEXT -- A string containing the text of the associated note.
NOTE-AS-HIGHLIGHT -- An optional boolean flag. If non-nil, the function
  treats the note as the primary content for the entry, formatting it
  without a quote block. This is useful for standalone notes."
  (with-slots (title author type page location date text) highlight
    (let* ((no-note (string= note-text ""))
           (bibtex-key (ebook-notes/find-bibtex-entry-for-clipping highlight))
           ;; REFERENCE is eiher:
           ;; - [[cite:&myref]]
           ;; - TITLE-AUTHOR   (if there's no BibTeX reference
           (reference (if bibtex-key (ebook-notes/org-ref-citation-link bibtex-key)
                        (format "%s (%s)" title author))))
      ;; Construct the Org-mode entry string
      ;; (message ">> Is there a note? %s" no-note)
      ;; (message ">> Is this ONLY a note? %s" note-as-highlight)

      (cl-flet ((ref-field (value &optional prefix)
                  (when value (format "- %s%s\n" (if prefix prefix "") value))))
        (format
         ;; "* %s%s\n:PROPERTIES:\n:ID: %s\n:ROAM_REFS: %s\n:END:\n\n%s%s\n%s"
         "* %s%s\n:PROPERTIES:\n:ROAM_REFS: %s\n:END:\n\n%s%s\n%s"
         ;; Title
         (format "Capture: %s %.25s%s"
                 (cl-subseq date 1 11) title
                 (if (> (length title) 25) "(ctd.)" ""))
         ;; If there's a highlight and no note, we want to interpret it
         (if (and (not note-as-highlight) no-note) "   :interpretation_needed:" "")
         ;; ;; ID
         ;; (org-id-new)
         ;; Reference
         reference
         ;; Notes, if any
         (if no-note "" (concat note-text "\n\n"))
         ;; If this is a note-as-highlight, just display the note text, if it
         ;; is a highlight, put it as a quote
         (format (if note-as-highlight "%s\n"
                   "#+begin_quote\n%s\n#+end_quote")
                 text)
         ;; Now add the text reference
         (concat (ref-field title)
                 (ref-field author)
                 (ref-field page "Page: ")
                 (ref-field location "Location: ")
                 (ref-field date "Capture date: ")
                 ;; (ref-field id "Hash: ")
                 ))))))



(defun ebook-notes/generate-org-entries-from-new-clippings (new-clippings all-clippings)
  "Generates a list of Org-mode entries from a list of new clippings.
  It handles new highlights and new notes, including those associated with
  previously processed highlights.

Uses the function defined in GENERATE-ENTRY-STRING to generate the Org entry"
  (let (org-entries
        new-notes
        seen-notes)
    ;; Deal with highlights first
    (cl-loop for new-clipping in new-clippings do
             (pcase (kindle-entry-type new-clipping)
               (:highlight
                ;; Create a new entry for the new highlight
                (let ((associated-notes-text ""))
                  ;; Find all notes associated with this highlight, both new and old
                  (cl-loop for note in all-clippings
                           when (ebook-notes/note-about-highlight-p note new-clipping)
                           do (setq associated-notes-text
                                    (concat associated-notes-text
                                            (if (string= associated-notes-text "") "" "\n\n")
                                            (kindle-entry-text note) "\n"))
                           and do (push (kindle-entry-id note) seen-notes))
                  (push (funcall generate-entry-string new-clipping associated-notes-text)
                        org-entries)))
               (:note
                ;; Initially just push new notes onto a list and then review later.
                (push new-clipping new-notes))))

    ;; Now see if any of the notes is an orphan
    (cl-loop for note in new-notes
             with matched-highlight
             ;; If we've already parsed it, skip it
             unless (member (kindle-entry-id note) seen-notes)
             ;; See if any previously seen highlights match
             do (setq matched-highlight
                      (cl-loop for highlight in ebn-clippings
                               when (and (eq (kindle-entry-type highlight) :highlight)
                                         (ebook-notes/note-about-highlight-p note highlight))
                               do (cl-return highlight)))
             and if matched-highlight
             do (push (funcall generate-entry-string
                               matched-highlight
                               (kindle-entry-text note))
                      org-entries)
             else
             ;; No matching highlights, this is an orphan note
             ;; Set the note-as-highlight flag to T
             do (push (funcall generate-entry-string note "" t) org-entries))

    (nreverse org-entries)))



(defun ebook-notes/append-org-entries-to-file (org-entries file-path &optional headings)
  "Appends a list of `org-entries' to a file, optionally under a heading.

  Arguments:
  ORG-ENTRIES -- A list of Org-mode entry strings to append.
  FILE-PATH -- The path to the Org file. The file will be created if it does not exist.
  HEADINGS -- An optional string or list of strings representing the heading
              hierarchy. E.g., \"My Header\" or '(\"Book Notes\" \"Chapter 1\")."
  (when (not (null org-entries))
    (with-current-buffer (find-file-noselect file-path)
      ;; Use a variable to track the insertion point
      (let ((insertion-point (point-max))
            (heading-level 1))

        (when headings
          (let ((heading-list (if (stringp headings) (list headings) headings)))
            (goto-char (point-min))
            ;; Find or create the heading hierarchy
            (cl-loop for heading-text in heading-list do
                     (let ((heading-string (format "^%s %s$"
                                                   (apply 'concat (make-list heading-level "\\*"))
                                                   (regexp-quote heading-text))))
                       (if (re-search-forward heading-string nil t)
                           ;; Heading found
                           (setq
                            ;; Update the insertion point to the end of its subtree
                            insertion-point (point)
                            ;; Move to the next heading level
                            heading-level (1+ heading-level))
                         ;; Heading not found, create it at the current insertion point
                         (goto-char insertion-point)
                         (org-end-of-subtree t)
                         (insert (format "\n%s %s\n" (make-string heading-level ?*) heading-text))
                         (setq insertion-point (point))
                         ;; Move to the next heading level
                         (setq heading-level (1+ heading-level)))))

            (setq insertion-point (org-end-of-subtree t))))

        ;; Go to the final insertion point to append the entries
        (goto-char insertion-point)
        (insert "\n")                   ; Add a newline for separation

        ;; Now append the entries with the correct level
        (cl-loop for entry in org-entries do
                 (insert (make-string (1- heading-level) ?*) entry "\n")))
      (save-buffer))))



(provide 'ebook-notes-org)
