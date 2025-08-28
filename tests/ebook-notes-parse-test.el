;;; -*- lexical-binding: t -*-

;; Load Buttercup, which you would need to install from MELPA
(require 'buttercup)
(require 'cl-lib) ; Explicitly require cl-lib for &key syntax support
;; Load the main file of your package
(require 'ebook-notes-parse)
(require 'ebook-notes-test-common)


;; --- Test Suite for the core parsing and logic ---
(describe "ebook-notes-parse"
          (describe "ebook-notes/parse-kindle-entry-string"
                    (it "correctly parses a simple highlight entry"
                        (let ((entry-string "The serpent on the crown (Elizabeth Peters)\n- Your Highlight on page 173 | location 2642-2644 | Added on Friday, 1 November 2019 22:29:27\n\nSome highlight text.\n=========="))
                          (let ((entry (ebook-notes/parse-kindle-entry-string entry-string)))
                            (expect (kindle-entry-title entry) :to-equal "The serpent on the crown")
                            (expect (kindle-entry-author entry) :to-equal "Elizabeth Peters")
                            (expect (kindle-entry-type entry) :to-equal :highlight)
                            (expect (kindle-entry-page entry) :to-equal 173)
                            (expect (kindle-entry-location entry) :to-equal "2642-2644")
                            (expect (kindle-entry-date entry) :to-equal "[2019-11-01 Fri 22:29:27]")
                            (expect (kindle-entry-text entry) :to-equal "Some highlight text."))))

                    (it "correctly parses a simple note entry"
                        (let ((entry-string "How to Take Smart Notes (Sönke Ahrens)\n- Your Note on page 97 | location 998 | Added on Tuesday, 9 June 2020 18:20:59\n\nNote without highlight\n=========="))
                          (let ((entry (ebook-notes/parse-kindle-entry-string entry-string)))
                            (expect (kindle-entry-title entry) :to-equal "How to Take Smart Notes")
                            (expect (kindle-entry-author entry) :to-equal "Sönke Ahrens")
                            (expect (kindle-entry-type entry) :to-equal :note)
                            (expect (kindle-entry-page entry) :to-equal 97)
                            (expect (kindle-entry-location entry) :to-equal "998")
                            (expect (kindle-entry-date entry) :to-equal "[2020-06-09 Tue 18:20:59]")
                            (expect (kindle-entry-text entry) :to-equal "Note without highlight")))))

          (describe "ebook-notes/note-about-highlight-p"
                    (it "returns t when a note's location is within a highlight's range"
                        (let ((highlight (test/make-kindle-entry "Title" "Author" "Highlight text" :location "100-110"))
                              (note (test/make-kindle-entry "Title" "Author" "Note text" :type :note :location "105")))
                          (expect (ebook-notes/note-about-highlight-p note highlight) :to-be t)))

                    (it "returns nil when a note's location is outside a highlight's range"
                        (let ((highlight (test/make-kindle-entry "Title" "Author" "Highlight text" :location "100-110"))
                              (note (test/make-kindle-entry "Title" "Author" "Note text" :type :note :location "120")))
                          (expect (ebook-notes/note-about-highlight-p note highlight) :to-be nil)))

                    (it "returns nil if titles or authors don't match"
                        (let ((highlight (test/make-kindle-entry "Correct Title" "Correct Author" "Highlight text" :location "100-110"))
                              (note (test/make-kindle-entry "Wrong Title" "Correct Author" "Note text" :type :note :location "105")))
                          (expect (ebook-notes/note-about-highlight-p note highlight) :to-be nil))))

          (describe "ebook-notes/create-clipping-identifier"
                    (it "creates the same ID for identical clippings"
                        (let ((clipping1 (test/make-kindle-entry "The Title" "The Author" "Some text here"))
                              (clipping2 (test/make-kindle-entry "The Title" "The Author" "Some text here")))
                          (expect (ebook-notes/create-clipping-identifier clipping1)
                                  :to-equal (ebook-notes/create-clipping-identifier clipping2))))
                    (it "creates a different ID for different clipping content"
                        (let ((clipping1 (test/make-kindle-entry "The Title" "The Author" "Some text here"))
                              (clipping2 (test/make-kindle-entry "The Title" "The Author" "Different text here")))
                          (expect (ebook-notes/create-clipping-identifier clipping1)
                                  :not :to-equal (ebook-notes/create-clipping-identifier clipping2)))))

          (describe "ebook-notes/process-kindle-clippings"
                    (it "parses a file with multiple entries correctly"
                        ;; Create a temporary file with mock content
                        (let ((temp-file-name (make-temp-file "test-clippings-"))
                              (file-content "Book One (Author One)\n- Your Highlight on page 1 | location 1-5 | Added on Friday, 1 November 2019 22:29:27\n\nHighlight 1.\n==========\nBook Two (Author Two)\n- Your Note on page 2 | location 6 | Added on Saturday, 2 November 2019 22:29:27\n\nNote 2.\n=========="))
                          (with-temp-buffer
                            (insert file-content)
                            (write-file temp-file-name))

                          (unwind-protect
                              (let ((clippings (ebook-notes/process-kindle-clippings temp-file-name)))
                                (expect (length clippings) :to-equal 2)
                                (expect (kindle-entry-title (car clippings)) :to-equal "Book One")
                                (expect (kindle-entry-title (cadr clippings)) :to-equal "Book Two"))
                            ;; Clean up the temporary file
                            (delete-file temp-file-name))))))
