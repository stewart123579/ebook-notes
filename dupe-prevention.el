;;; dupe-prevention.el --- Support funtions to dedupe data -*- lexical-binding: t -*-
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

;; This file provides generic functions to prevent duplication of items
;; based on a unique string identifier.

(require 'cl-lib)

(defvar dupe-prevention-log-file nil
  "Path to a file that stores a list of unique identifiers for processed items.

Perhaps something like:  ~/.emacs.d/dupe-prevention.log

NOTE: This variable is dynamically bound, so you can use this
duplication protection in multiple places within your code. In
fact, I recommend USING dynamical binding.")

(defvar dupe-prevention-cache nil
  "An in-memory hash table to cache processed item identifiers.

NOTE: This variable is dynamically bound, so you can use this
duplication protection in multiple places within your code. In
fact, I recommend USING dynamical binding.")


(defun dupe-prevention-load-cache ()
  "Loads all identifiers from the log file into the cache."
  (unless dupe-prevention-log-file
    (error "'dupe-prevention-log-file' is undefined"))
  (unless (hash-table-p dupe-prevention-cache)
    (setq dupe-prevention-cache (make-hash-table :test 'equal)))
  (when (file-exists-p dupe-prevention-log-file)
    (with-temp-buffer
      (insert-file-contents dupe-prevention-log-file)
      (goto-char (point-min))
      (while (not (eobp))
        (let* ((line (buffer-substring (point) (line-end-position)))
               (stripped-line (string-trim line)))
          (unless (string= stripped-line "")
            (puthash stripped-line t dupe-prevention-cache)))
        (forward-line)))))

(defun dupe-prevention-identifier-exists-p (identifier)
  "Checks if a given identifier exists in the in-memory cache."
  (unless dupe-prevention-cache
    (dupe-prevention-load-cache))
  (gethash identifier dupe-prevention-cache))


(defun dupe-prevention-record-identifier (identifiers)
  "Adds an identifier to the cache and the log file.

Arguments:
IDENTIFIERS -- Either a string, or a list of strings that are used as unique identifiers."
  (unless dupe-prevention-cache
    (dupe-prevention-load-cache))
  ;; For a single identifier string, make it a list
  (unless (listp identifiers)
    (setf identifiers (list identifiers)))
  (with-current-buffer (find-file-noselect dupe-prevention-log-file)
    (goto-char (point-max))
    (cl-loop for id in identifiers
             ;; Add the ID to the in-memory cache
             do (puthash id t dupe-prevention-cache)
             ;; and the file
             and do (insert id "\n"))
    (save-buffer)))

(provide 'dupe-prevention)
