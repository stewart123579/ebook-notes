;;; ebook-notes-parse.el --- Parse the Kindle clippings -*- lexical-binding: t -*-
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

;; Parse the Kindle clippings into a list of kindle-entry structs, identify the
;; new/unseen ones.

(require 'cl-lib) ; For cl-defstruct, cl-loop, and other utilities
(require 'parse-time) ; For parsing date strings
(require 'md5) ; For the md5 hash
(require 'dupe-prevention)

;; Define a structure to hold the extracted information for each entry
(cl-defstruct kindle-entry
  title                                 ; String: "The serpent on the crown"
  author                                ; String: "Elizabeth Peters"
  type                                  ; Symbol: :highlight, :note, or :unknown
  page                                  ; Integer or nil (if not found)
  location                              ; String: "2642-2644"
  date                                  ; String: "[YYYY-MM-DD Day HH:MM:SS]"
  text                                  ; String: The highlight or note content
  id)                                   ; String: ID for dedupification


(defcustom generate-clipping-id 'ebook-notes/create-clipping-identifier
  "Function for generating unique identifiers based on a clipping."
  :type 'function
  :group 'kindle-notes)


(defun ebook-notes/parse-kindle-entry-string (entry-string)
  "Parses a single Kindle entry string and returns a `kindle-entry' struct.
  Returns nil if the entry-string cannot be parsed into a valid format."
  (let (title author type page location date text)
    (with-temp-buffer
      (insert entry-string)
      (goto-char (point-min))

      ;; 1. Parse Title and Author from the first line
      (when (re-search-forward "^\\(.+\\) (\\(.+\\))\r?$" nil t 1)
        (setq title (match-string 1)
              author (match-string 2)))

      ;; Move to the second line to parse metadata
      (forward-line 1)

      ;; 2. Parse Type, Page, Location, Date
      ;; Example: - Your Highlight on page 173 | location 2642-2644 | Added on Friday, 1 November 2019 22:29:27
      (when (re-search-forward "^- Your \\(Highlight\\|Note\\) on page \\([0-9]+\\) | location \\([0-9-]+\\) | Added on \\(.+\\)\r?$" nil t)
        (setq type (pcase (match-string 1)
                     ("Note" :note)
                     ("Highlight" :highlight)
                     (_ :unknown))
              page (string-to-number (match-string 2))
              location (match-string 3))

        ;; Parse the date string and format it
        (let* ((raw-date-str (match-string 4))
               (parsed-time (ignore-errors (parse-time-string raw-date-str))))
          (when parsed-time
            ;; Format date as "[%Y-%m-%d Day HH:MM:SS]"
            (setq date (format-time-string "[%Y-%m-%d %a %H:%M:%S]" (encode-time parsed-time))))))

      ;; Move past the metadata line and the blank line
      (forward-line 2)

      ;; 3. Capture the text content until "=========="
      (let ((start-of-text (point)))
        (if (re-search-forward "^==========" nil t)
            (setq text (buffer-substring start-of-text (match-beginning 0)))
          ;; If "==========" not found, take rest of buffer
          (setq text (buffer-substring start-of-text (point-max)))))

      ;; Clean up whitespace from text
      (when text
        (setq text (string-trim text)))

      ;; Create and return the kindle-entry struct if essential fields are present
      (when (and title
                 author
                 type
                 page
                 location
                 date)
        (make-kindle-entry
         :title title
         :author author
         :type type
         :page page
         :location location
         :date date
         :text text)))))


(defun ebook-notes/process-kindle-clippings (filename)
  "Reads a Kindle clippings file, splits it into individual entries,
  parses each entry, and returns a list of `kindle-entry' structs.

  Arguments:
  FILENAME -- The path to the Kindle clippings text file."
  (interactive "fEnter Kindle clippings file: ")
  (unless (file-readable-p filename)
    (user-error "File not found or not readable: %s" filename))

  (let ((entries-list nil))
    (with-temp-buffer
      (insert-file-contents filename)
      (goto-char (point-min))

      ;; Iterate through the buffer, finding each entry
      (while (re-search-forward "^\\(.+\\) (\\(.+\\))" nil t)
        (let* ((entry-start (match-beginning 0))
               (entry-end (save-excursion
                            (re-search-forward "^==========" nil t)
                            (match-end 0)))
               (entry-string (buffer-substring entry-start entry-end))
               (parsed-entry (ebook-notes/parse-kindle-entry-string entry-string)))
          (when parsed-entry
            (push parsed-entry entries-list))
          ;; Move past the current entry to find the next one
          (goto-char entry-end))))

    ;; Return the list of entries in the original order
    (nreverse entries-list)))



(defun ebook-notes/parse-location-string (location-str)
  "Parses a location string (e.g., '2642-2644' or '2643') into a number
  or a cons cell (START . END). Returns nil if parsing fails.

  Arguments:
  LOCATION-STR -- The location string from a Kindle entry."
  (cond
   ;; Case 1: Range (e.g., "2642-2644")
   ((string-match "\\([0-9]+\\)-\\([0-9]+\\)" location-str)
    (let ((start (string-to-number (match-string 1 location-str)))
          (end (string-to-number (match-string 2 location-str))))
      (when (and start end)
        (cons start end))))
   ;; Case 2: Single number (e.g., "2643")
   ((string-match "^[0-9]+\r?$" location-str)
    (string-to-number location-str))
   ;; Case 3: Invalid format
   (t nil)))



(defun ebook-notes/note-about-highlight-p (note-entry highlight-entry)
  "Determines if a NOTE-ENTRY is semantically 'about' a HIGHLIGHT-ENTRY.
  This is true if:
  1. Their titles and authors match.
  2. The NOTE-ENTRY is of type 'Note' and HIGHLIGHT-ENTRY is of type 'Highlight'.
  3. The note's location falls within the highlight's location range
     (inclusive of start and end).

  Arguments:
  NOTE-ENTRY      -- A `kindle-entry` struct representing a note.
  HIGHLIGHT-ENTRY -- A `kindle-entry` struct representing a highlight."
  (let ((note-title (kindle-entry-title note-entry))
        (note-author (kindle-entry-author note-entry))
        (note-type (kindle-entry-type note-entry))
        (note-location-str (kindle-entry-location note-entry))

        (highlight-title (kindle-entry-title highlight-entry))
        (highlight-author (kindle-entry-author highlight-entry))
        (highlight-type (kindle-entry-type highlight-entry))
        (highlight-location-str (kindle-entry-location highlight-entry)))

    (and
     ;; 1. Check entry types
     (eq note-type :note)
     (eq highlight-type :highlight)

     ;; 2. Check Title and Author match
     (string= note-title highlight-title)
     (string= note-author highlight-author)

     ;; 3. Check location range
     (let ((note-loc (ebook-notes/parse-location-string note-location-str))
           (highlight-loc-range (ebook-notes/parse-location-string highlight-location-str)))
       (and
        (integerp note-loc)             ; Note location must be a single number
        (consp highlight-loc-range)     ; Highlight location must be a range (cons cell)
        (let ((highlight-start (car highlight-loc-range))
              (highlight-end (cdr highlight-loc-range)))
          (and
           (>= note-loc highlight-start)
           (<= note-loc highlight-end))))))))


(defun ebook-notes/get-new-clippings (all-clippings)
  "Filters a list of all clippings to return only those that have not
  been previously processed. This function uses the dupe-prevention module
  to check the unique identifier for each clipping."
  (dupe-prevention-load-cache)
  (let (new-clippings-list
        ids-to-record)
    (cl-loop for clipping in all-clippings
             do (let ((id (funcall generate-clipping-id clipping)))
                  (setf (kindle-entry-id clipping) id)
                  (unless (dupe-prevention-identifier-exists-p id)
                    (push clipping new-clippings-list)
                    (push id ids-to-record))))
    ;; Record the new clippings' IDs before returning them.
    ;; This prevents them from being processed again in the same session.
    (when ids-to-record
      (dupe-prevention-record-identifier ids-to-record))
    (nreverse new-clippings-list)))


(defun ebook-notes/format-dynamic-string (&rest args)
  "Creates a string from a variable number of arguments, separated
by `:-:`.

For example, (ebook-notes/format-dynamic-string 1 2 3) would return
\"1:-:2:-:3\"."
  ;; First, check if there are any arguments. If not, return an empty string.
  (if (null args)
      ""
    ;; Otherwise, join the arguments into a single string.
    ;; `mapcar` applies `prin1-to-string` to each argument,
    ;; converting it to a string. `string-join` then combines
    ;; them with the specified separator.
    (string-join (mapcar 'prin1-to-string args) ":-:")))


(defun ebook-notes/create-clipping-identifier (clipping)
  "Creates a unique MD5 hash for a `kindle-entry`.
The hash is based on the author, title, and the clipping's text.

Arguments:
CLIPPING -- A `kindle-entry` struct (an alist).

Returns:
A string representing the MD5 hash."
  (with-slots (author title location text) clipping
    (let ((combined-string (ebook-notes/format-dynamic-string author title location text)))
      (md5 combined-string))))



(provide 'ebook-notes-parse)
