;;; ebook-notes.el --- Parse Kindle clippings  -*- lexical-binding: t -*-
;;
;; Copyright (C) 2025 Stewart V. Wright

;; Author: Stewart V. Wright <stewart@vifortech.com>
;; Version: 0.3
;; Keywords: notes-management, tools
;; URL: https://github.com/stewart123579/ebook-notes

;; SPDX-License-Identifier: GPL-3.0-or-later

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

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; ebook-notes is a simple package for extracting, deduplicating and organising
;; notes and highlights from Kindle clipping files.
;;
;; It comes with functions to optionally save the clippings as structured
;; Org-mode entries, with references to BibTeX records.
;;
;; The deduplication functionality is standalone, meaning it can be used in
;; other places and is designed for the cache variarble to be set dynamically.
;;
;; Features:
;;
;; Core Functionality
;; - *Clipping Processing:* Parses Kindle text files to extract highlights and
;;   notes.
;; - *Org-mode Integration:* Generates new entries in a standard Org-mode
;;   format, appending them to a specified file and optionally placing them
;;   under a user-defined heading hierarchy.
;; - *BibTeX Support:* Links clippings to BibTeX entries for contextual
;;   citations and generates ~org-ref~ references where a source document
;;   matches a BibTeX entry.
;;
;; Duplicate Prevention & Note Handling
;; - *Deduplication:* Prevents highlights or notes from being processed more
;;   than once by keeping a log of processed clippings. This logic is
;;   standalone, allowing it to be used in other projects.
;; - *Note Association:** Automatically groups notes with their associated
;;   highlights from the same location in a book. This also includes creating
;;   separate entries for new notes attached to old highlights.
;;
;; Extensibility
;; - *Modular Design:* The underlying functions are self-contained and modular,
;;   allowing for easy customisation and integration into other workflows.
;;
;;
;; Usage:
;;
;; The flow of logic is to
;;
;; 1. Process the clippings file into a list of ~kindle-entry~ structs
;;
;; 2. Get the list of new/unseen clippings with ~ebook-notes/get-new-clippings~
;;    - this uses the ~dupe-prevention~ package in the background
;;
;; 3. /(Optional)/ Generate Org entries from the new clippings
;;    (~ebook-notes/generate-org-entries-from-new-clippings~)
;;
;; 4. /(Optional)/ Write the Org entries to file
;;    (~ebook-notes/append-org-entries-to-file org-entries~)
;;
;;
;; TL;DR - just make it work
;;
;; The quick and dirty way to do all the processing is:
;;
;; (require 'ebook-notes)
;; (ebook-notes-all
;;  "~/Downloads/My Clippings.txt"         ; Source of clippings
;;  "~/clippings.org"                      ; Where they will be written
;;  ;; === Optional settings ===
;;  ;; The Org headings
;;  '("My Clippings" "New")
;;  ;; Logging?
;;  t
;;  ;; Where to save the list of already seen clippings
;;  "~/.emacs.d/processed-kindle-clippings.log")


(require 'ebook-notes-parse)
(require 'ebook-notes-bibtex)
(require 'ebook-notes-org)


(defun ebook-notes-all (clipping-file output-org-file &optional headings logging dupe-log-file)
  "Function that imports Kindle clippings and writes unseen notes/highlights to file."

  (when logging (message "--------\nStarting run...   %s"
                         (format-time-string "[%Y-%m-%d %a %H:%M:%S]" (current-time))))

  (message "Clippinng file: %s" clipping-file)
  (message "output: %s" output-org-file)
  (message "headings: %s" headings)
  (message "logging: %s" logging)
  (message "dupe-prev-log-file: %s" dupe-prevention-log-file)

  (let ((ebn-clippings (ebook-notes/process-kindle-clippings clipping-file))
        (dupe-prevention-log-file (if dupe-log-file dupe-log-file
                                    "~/.emacs.d/processed-kindle-clippings.log"))
        dupe-prevention-cache
        bibtex-entries)
    (when logging (message "There are  %s  clippings" (length ebn-clippings)))
    (let* ((new-clippings (ebook-notes/get-new-clippings ebn-clippings))
           ;; Now, generate the Org entries from the new clippings.
           (org-entries (ebook-notes/generate-org-entries-from-new-clippings new-clippings ebn-clippings)))
      ;; Append the new entries to your Org file.
      (ebook-notes/append-org-entries-to-file org-entries output-org-file headings)))
  (when logging (message "--------\nFinished run...   %s"
                         (format-time-string "[%Y-%m-%d %a %H:%M:%S]" (current-time)))))



(provide 'ebook-notes)
