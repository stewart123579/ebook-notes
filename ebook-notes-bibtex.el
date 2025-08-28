;;; ebook-notes-bibtex.el --- Extract BibTeX references for clippings -*- lexical-binding: t -*-
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

;; ebook-notes-bibtex are helper functions to identify if a highlight/note is in
;; your BibTeX list. It's pretty rough-and-ready and probably needs more edge
;; cases managed.
;;
;; Features:
;; - Finds if there is an associated BibTeX entry for a clipping
;; - Normalises strings to collapse accented characters to their ASCII
;;   equivalients and strips BibTeX/LaTeX equivalent accenting features
;; - Normalises BibTeX-style LastName, Firstname to Firstname Lastname

(require 'bibtex)
(require 'org-ref)
(require 'cl-lib) ; For cl-loop and other utilities
(require 'ucs-normalize)
(require 'ebook-notes-parse)


(defvar bibtex-entries nil
  "A variable to store the list of BibTeX entries received via
BIBTEX-COMPLETION-CANDIDATES.

Note that this is dynamically bound, so if you want to force a
reload, just set it to NIL.")


(defun ebook-notes/ascii-normalize-filter (string)
  ;; https://lists.gnu.org/archive/html/help-gnu-emacs/2018-05/msg00232.html
  ;; by Teemu Likonen
  ;; via https://emacs.stackexchange.com/a/78241
  ;; Licenced under Creative Commons Attribution-ShareAlike 4.0 International (CC BY-SA 4.0)
  (cl-remove-if (lambda (char)
                  (> char 127))
                (ucs-normalize-NFKD-string string)))



(defun ebook-notes/convert-specific-latex-escapes (text)
  "Converts common LaTeX-style character escapes in TEXT to their basic ASCII equivalents.

For example, '{\\\"o}' becomes 'o', '{\\~n}' becomes 'n', '{\\c c}' becomes 'c'.
This function also correctly handles nested braces like '{{\\'e}}'."
  (let ((result text))
    ;; Handle specific edge cases that do not fit the general pattern, like {\ss}
    (setq result (replace-regexp-in-string "{\\\\ss}" "ss" result))
    ;; Handle patterns with nested braces and a single accent command (e.g., {{\\'e}})
    (setq result (replace-regexp-in-string "{{\\\\?'\\([a-zA-Z]\\)}}" "\\1" result))
    ;; Handle patterns with an accent command followed by a character (e.g., {\"o})
    (setq result (replace-regexp-in-string "{\\\\?[\"\\'\\`\\~\\^c]? *\\([a-zA-Z]\\)}" "\\1" result))
    result))


(defun ebook-notes/normalize-person-name (name-string)
  "Normalizes a person's name string for robust comparison.
  Steps include:
  1. Convert LaTeX-style escapes (e.g., '{\\\"o}') to their basic ASCII equivalents.
  2. Handle 'Last, First' format by reordering to 'First Last'.
  3. Convert to lowercase.
  4. Remove any remaining diacritics (accents) from Unicode characters.
  5. Trim leading/trailing whitespace and collapse multiple internal spaces.

  Arguments:
  NAME-STRING -- The input name string (e.g., 'Sönke Ahrens', 'Ahrens, S{\\\"o}nke').

  Returns:
  A normalized string suitable for comparison."
  (let* ((normalized-name name-string)
         (parts nil))

    ;; 1. Convert LaTeX-style escapes to plain ASCII (e.g., {\"o} -> o)
    (setq normalized-name (ebook-notes/convert-specific-latex-escapes normalized-name))

    ;; 2. Handle 'Last, First' format by reordering to 'First Last'
    ;; Split by comma and optional space, then reverse if two parts
    (setq parts (split-string normalized-name ", *"))
    (when (= (length parts) 2)
      (setq normalized-name (string-join (nreverse parts) " ")))

    ;; 3. Convert to lowercase
    (setq normalized-name (downcase normalized-name))

    ;; 4. Remove any remaining diacritics (accents)
    ;; This function needs unaccent.el to be loaded.
    (setq normalized-name (ebook-notes/ascii-normalize-filter normalized-name))

    ;; 5. Trim leading/trailing whitespace and collapse multiple internal spaces
    (setq normalized-name (replace-regexp-in-string " +" " " (string-trim normalized-name)))

    normalized-name))

(defun ebook-notes/are-names-equal (name1 name2)
  "Compares two person name strings for equality after normalization.
  This function considers variations in formatting, LaTeX escapes,
  and diacritics.

  Arguments:
  NAME1 -- The first name string.
  NAME2 -- The second name string.

  Returns:
  t if the normalized names are equal, nil otherwise."
  (string= (ebook-notes/normalize-person-name name1)
           (ebook-notes/normalize-person-name name2)))



(defun ebook-notes/normalize-title (title-string)
  "Normalizes a title string for robust comparison.
  Removes LaTeX escapes, converts to lowercase, removes diacritics,
  and strips common subtitle separators like colons and semicolons.

  Arguments:
  TITLE-STRING -- The input title string.

  Returns:
  A normalized string suitable for comparison."
  (let ((normalized-title title-string))
    ;; 1. Convert LaTeX-style escapes to plain ASCII
    (setq normalized-title (ebook-notes/convert-specific-latex-escapes normalized-title))
    ;; 2. Remove curly braces used for capitalization protection in BibTeX
    (setq normalized-title (replace-regexp-in-string "{{\\|}}" "" normalized-title))
    ;; 3. Remove leading and trailing single curly braces
    (setq normalized-title (replace-regexp-in-string "^{" "" normalized-title))
    (setq normalized-title (replace-regexp-in-string "}$" "" normalized-title))
    ;; 4. Convert to lowercase
    (setq normalized-title (downcase normalized-title))
    ;; 5. Remove diacritics using the new function
    (setq normalized-title (ebook-notes/ascii-normalize-filter normalized-title))
    ;; 6. Trim leading/trailing whitespace and collapse multiple internal spaces
    (setq normalized-title (replace-regexp-in-string " +" " " (string-trim normalized-title)))
    normalized-title))


(defun ebook-notes/bibtex-entry-key (author title)
  "The string to be used as the hash for BibTeX entry lookups.

Note that normalisation occurs in this function, so no need to do
it beforehand."
  (format "%s:-:%s"
          (ebook-notes/normalize-person-name author)
          (ebook-notes/normalize-title title)))

(defun ebook-notes/make-bibtex-entries-dict ()
  "Convert the output of BIBTEX-COMPLETION-CANDIDATES to a dict to
speed up lookups."
  (setq bibtex-entries (make-hash-table :test 'equal))
  (cl-loop for entry in (bibtex-completion-candidates)
           do (let ((bib-author (cdr (assoc "author" entry)))
                    (bib-title (cdr (assoc "title" entry))))
                ;; Check if both author and title match
                (when (and bib-author bib-title)
                  (puthash (ebook-notes/bibtex-entry-key bib-author bib-title)
                           (cdr (assoc "=key=" entry))
                           bibtex-entries))))
  bibtex-entries)


(defun ebook-notes/find-bibtex-entry-for-clipping (clipping)
  "Finds a matching BibTeX entry for a kindle clipping.
This function requires `bibtex` package to be loaded and assumes there is BibTeX data.

Arguments:
CLIPPING -- A `kindle-entry` struct (represented as an alist) with `:author` and `:title` keys.

Returns:
The `org-ref` citation key if a match is found, nil otherwise."
  (unless bibtex-entries
    (ebook-notes/make-bibtex-entries-dict))
  (with-slots (author title) clipping
    (when (and bibtex-entries author title)
      (gethash (ebook-notes/bibtex-entry-key author title) bibtex-entries))))


(defun ebook-notes/org-ref-citation-link (key)
  "Create a citation link as org-ref would do for KEY

The citation code was taken from ORG-REF-INSERT-CITE-KEY"
  (format "[[%s:%s%s]]"
          org-ref-default-citation-link
          (pcase org-ref-cite-insert-version
            (2 "")
            (3 "&"))
          key))

(provide 'ebook-notes-bibtex)
