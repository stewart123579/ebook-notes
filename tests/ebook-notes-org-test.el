;;; -*- lexical-binding: t -*-

(require 'buttercup)
(require 'cl-lib) ; For cl-loop
(require 'org-id) ; For org-id-new in the test helpers
(require 'ebook-notes-org)
(require 'ebook-notes-test-common)


(describe "ebook-notes-org"
          ;; Test for the ebook-notes/generate-org-entry-string function
          (describe "ebook-notes/generate-org-entry-string"
                    (it "generates a correct Org entry for a highlight without BibTeX link"
                        (let ((clipping (test/make-kindle-entry "Book Title" "Author Name" "Some highlight text.")))
                          (cl-letf (((symbol-function 'bibtex-completion-candidates) (lambda () nil)))
                            (let ((entry-string (ebook-notes/generate-org-entry-string clipping "")))
                              (expect entry-string :to-match "\\* Capture: [0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} Book Title[[:space:]]*:interpretation_needed:[[:space:]]*\n:PROPERTIES:\n:ROAM_REFS: Book Title (Author Name)\n:END:\n\n#\\+begin_quote\nSome highlight text\\.\n#\\+end_quote\n- Book Title\n- Author Name\n- Page: 10\n- Location: 10-20\n- Capture date: \\[2020-06-09 Tue 18:20:59\\]")))))

                    (it "generates a correct Org entry for a highlight with a BibTeX link"
                        (let ((clipping (test/make-kindle-entry "Book Title" "Author Name" "Some highlight text.")))
                          (cl-letf (((symbol-function 'ebook-notes/find-bibtex-entry-for-clipping) (lambda (c) "bibtex-key")))
                            (let ((entry-string (ebook-notes/generate-org-entry-string clipping "")))
                              (expect entry-string :to-match "\\* Capture: [0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} Book Title[[:space:]]*:interpretation_needed:[[:space:]]*\n:PROPERTIES:\n:ROAM_REFS: \\[\\[cite:&bibtex-key\\]\\]\n:END:\n\n#\\+begin_quote\nSome highlight text\\.\n#\\+end_quote\n- Book Title\n- Author Name\n- Page: 10\n- Location: 10-20\n- Capture date: \\[2020-06-09 Tue 18:20:59\\]")))))

                    (it "generates a correct Org entry for a note with content and a BibTeX link"
                        (let ((clipping (test/make-kindle-entry "Book Title" "Author Name" "Some highlight text.")))
                          (cl-letf (((symbol-function 'ebook-notes/find-bibtex-entry-for-clipping) (lambda (c) "bibtex-key")))
                            (let ((entry-string (ebook-notes/generate-org-entry-string clipping "Some note content.")))
                              (expect entry-string :to-match "\\* Capture: [0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} Book Title\n:PROPERTIES:\n:ROAM_REFS: \\[\\[cite:&bibtex-key\\]\\]\n:END:\n\nSome note content\\.\n\n#\\+begin_quote\nSome highlight text\\.\n#\\+end_quote\n- Book Title\n- Author Name\n- Page: 10\n- Location: 10-20\n- Capture date: \\[2020-06-09 Tue 18:20:59\\]")))))

                    ;; Test for the ebook-notes/generate-org-entries-from-clippings function
                    (describe "ebook-notes/generate-org-entries-from-clippings"
                              (it "generates a list of Org entries from a list of clippings"
                                  (let ((clippings (list (test/make-kindle-entry "Book A" "Author A" "Text A")
                                                         (test/make-kindle-entry "Book A" "Author A" "Text B")
                                                         (test/make-kindle-entry "Book B" "Author B" "Text C"))))
                                    (cl-letf (((symbol-function 'ebook-notes/find-bibtex-entry-for-clipping) (lambda (c) nil)))
                                      (let ((org-entries (ebook-notes/generate-org-entries-from-new-clippings clippings clippings)))
                                        (expect (length org-entries) :to-equal 3)
                                        (expect (car org-entries) :to-match "\\* Capture: [0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} Book A[[:space:]]*:interpretation_needed:[[:space:]]*\n")
                                        (expect (cadr org-entries) :to-match "\\* Capture: [0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} Book A[[:space:]]*:interpretation_needed:[[:space:]]*\n")
                                        (expect (caddr org-entries) :to-match "\\* Capture: [0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} Book B[[:space:]]*:interpretation_needed:[[:space:]]*\n")))))

                              (it "generates a list of Org entries from a list of new clippings"
                                  (let ((clippings (list (test/make-kindle-entry "Book A" "Author A" "Text A")
                                                         (test/make-kindle-entry "Book A" "Author A" "Text B")
                                                         (test/make-kindle-entry "Book B" "Author B" "Text C"))))
                                    (cl-letf (((symbol-function 'ebook-notes/find-bibtex-entry-for-clipping) (lambda (c) nil)))
                                      (let ((org-entries (ebook-notes/generate-org-entries-from-new-clippings (cl-subseq clippings 2) clippings)))
                                        (expect (length org-entries) :to-equal 1)
                                        (expect (car org-entries) :to-match "\\* Capture: [0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} Book B[[:space:]]*:interpretation_needed:[[:space:]]*\n"))))))

                    ;; Test for the ebook-notes/append-org-entries-to-file function
                    ;; (describe "ebook-notes/append-org-entries-to-file"
                    (it "correctly appends entries to a new file"
                        (let ((temp-file-name (make-temp-file "test-org-file-" nil ".org")))
                          (unwind-protect
                              (let ((org-entries (list "* Entry 1\nContent 1." "* Entry 2\nContent 2.")))
                                (ebook-notes/append-org-entries-to-file org-entries temp-file-name '("Level 1" "Level 2")))
                            (let ((file-content (with-temp-buffer (insert-file-contents temp-file-name) (buffer-string))))
                              (expect file-content :to-match "^\\* Level 1\n\\*\\* Level 2\n\\*\\*\\* Entry 1\nContent 1.\n\\*\\*\\* Entry 2\nContent 2.\n")
                              (delete-file temp-file-name)))))

                    (it "correctly appends entries to an existing file with existing headings"
                        (let ((temp-file-name (make-temp-file "test-org-file-" nil ".org"))
                              (initial-content "* Existing Heading\nContent.\n\n* Level 1\n* Some Other Heading\n"))
                          (with-temp-buffer
                            (insert initial-content)
                            (write-file temp-file-name))
                          (unwind-protect
                              (let ((org-entries (list "* Entry 1\nContent 1." "* Entry 2\nContent 2.")))
                                (ebook-notes/append-org-entries-to-file org-entries temp-file-name '("Level 1" "Level 2")))
                            (let ((file-content (with-temp-buffer (insert-file-contents temp-file-name) (buffer-string))))
                              (expect file-content :to-match "^\\* Existing Heading\nContent.\n\n\\* Level 1\n\\*\\* Level 2\n\\*\\*\\* Entry 1\nContent 1.\n\\*\\*\\* Entry 2\nContent 2.\n\n\n\\* Some Other Heading\n")
                              (delete-file temp-file-name)))))))
