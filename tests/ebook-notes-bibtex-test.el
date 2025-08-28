;;; -*- lexical-binding: t -*-

(require 'buttercup)
(require 'cl-lib) ; Explicitly require cl-lib for &key syntax support
(require 'bibtex)
;; Load the package to be tested
(require 'ebook-notes-bibtex) ; Needed for the bibtex-completion-candidates mock
(require 'ebook-notes-test-common)


;; --- Test Suite for the bibtex sub-package functions ---
(describe "ebook-notes-bibtex"
          ;; Test for the ebook-notes/ascii-normalize-filter function
          (describe "ebook-notes/ascii-normalize-filter"
                    (it "removes non-ASCII characters while preserving ASCII"
                        (expect (ebook-notes/ascii-normalize-filter "Héllo, Wörld! ©") :to-equal "Hello, World! ")))

          ;; Test for the ebook-notes/convert-specific-latex-escapes function
          (describe "ebook-notes/convert-specific-latex-escapes"
                    (it "converts common LaTeX escapes to ASCII"
                        (expect (ebook-notes/convert-specific-latex-escapes "S{\\\"o}nke Ahrens") :to-equal "Sonke Ahrens"))
                    (it "handles multiple escapes in a single string"
                        (expect (ebook-notes/convert-specific-latex-escapes "N{\\~n}o{{\\'e}}lle") :to-equal "Nnoelle"))
                    (it "handles the Eszett character"
                        (expect (ebook-notes/convert-specific-latex-escapes "{\\ss}tudios") :to-equal "sstudios"))
                    (it "returns the string unchanged if no escapes are found"
                        (expect (ebook-notes/convert-specific-latex-escapes "A plain string") :to-equal "A plain string"))
                    (it "handles the accent command followed by space and character"
                        (expect (ebook-notes/convert-specific-latex-escapes "{\\c c}ontext") :to-equal "context")))

          ;; Test for the ebook-notes/normalize-person-name function
          (describe "ebook-notes/normalize-person-name"
                    (it "correctly handles 'Last, First' format"
                        (expect (ebook-notes/normalize-person-name "Ahrens, Sönke") :to-equal "sonke ahrens"))
                    (it "correctly handles 'First Last' format"
                        (expect (ebook-notes/normalize-person-name "Sönke Ahrens") :to-equal "sonke ahrens"))
                    (it "converts to lowercase and removes diacritics"
                        (expect (ebook-notes/normalize-person-name "Frédéric Chopin") :to-equal "frederic chopin"))
                    (it "handles LaTeX escapes and formatting"
                        (expect (ebook-notes/normalize-person-name "Ahrens, S{\\\"o}nke") :to-equal "sonke ahrens"))
                    (it "handles extra whitespace"
                        (expect (ebook-notes/normalize-person-name "  john  doe ") :to-equal "john doe")))

          ;; Test for the ebook-notes/are-names-equal function
          (describe "ebook-notes/are-names-equal"
                    (it "correctly compares names with different capitalization and latex"
                        (expect (ebook-notes/are-names-equal "Sönke Ahrens" "Ahrens, S{\\\"o}nke") :to-be t))
                    (it "correctly handles names with diacritics and without"
                        (expect (ebook-notes/are-names-equal "Frédéric" "Frederic") :to-be t))
                    (it "correctly returns false for different names"
                        (expect (ebook-notes/are-names-equal "John Doe" "Jane Smith") :to-be nil)))

          ;; Test for the ebook-notes/normalize-title function
          (describe "ebook-notes/normalize-title"
                    (it "removes LaTeX escapes and curly braces from a title"
                        (let ((raw-title "{The {\\~N}ote-taking G{\\\"u}ide"))
                          (expect (ebook-notes/normalize-title raw-title) :to-equal "the note-taking guide")))
                    (it "converts to lowercase and handles unicode"
                        (expect (ebook-notes/normalize-title "Böök Title") :to-equal "book title"))
                    (it "strips leading and trailing braces"
                        (expect (ebook-notes/normalize-title "{A Title}") :to-equal "a title"))
                    (it "handles subtitles separated by colons"
                        (expect (ebook-notes/normalize-title "A Title: Subtitle Here") :to-equal "a title: subtitle here")))

          ;; Test for the ebook-notes/find-bibtex-entry-for-clipping function
          (describe "ebook-notes/find-bibtex-entry-for-clipping"
                    (it "returns the citation key for a matching entry"
                        ;; Mock the bibtex-completion-candidates function to simulate BibTeX data
                        (setq bibtex-entries nil)
                        (cl-letf (((symbol-function 'bibtex-completion-candidates)
                                   (lambda () '((("title" . "{The {\\~N}ote-taking G{\\\"u}ide}")
                                            ("author" . "Ahrens, S{\\\"o}nke")
                                            ("=key=" . "ahrens2020"))))))
                          (let ((clipping (test/make-kindle-entry "The Note-taking Guide" "Sönke Ahrens" "Some text")))
                            (expect (ebook-notes/find-bibtex-entry-for-clipping clipping) :to-equal "ahrens2020"))))
                    (it "returns nil for a non-matching entry"
                        (setq bibtex-entries nil)
                        (cl-letf (((symbol-function 'bibtex-completion-candidates)
                                   (lambda () '((("title" . "Wrong Title")
                                            ("author" . "Wrong Author")
                                            ("=key=" . "wrong2020"))))))
                          (let (bibtex-entries
                                (clipping (test/make-kindle-entry "The Note-taking Guide" "Sönke Ahrens" "Some text")))
                            (expect (ebook-notes/find-bibtex-entry-for-clipping clipping) :to-be nil)))))
          ;; --- Test Suite for the citation link function ---
          (describe "ebook-notes/org-ref-citation-link"
                    ;; Test case for org-ref version 2 style citations
                    (it "correctly formats a citation link for org-ref version 2"
                        (setq bibtex-entries nil)
                        (cl-letf (((symbol-value 'org-ref-cite-insert-version) 2)
                                  ((symbol-value 'org-ref-default-citation-link) "cite"))
                          (expect (ebook-notes/org-ref-citation-link "ahrens-smarter-2017")
                                  :to-equal "[[cite:ahrens-smarter-2017]]")))

                    ;; Test case for org-ref version 3 style citations
                    (setq bibtex-entries nil)
                    (it "correctly formats a citation link for org-ref version 3"
                        (setq bibtex-entries nil)
                        (cl-letf (((symbol-value 'org-ref-cite-insert-version) 3)
                                  ((symbol-value 'org-ref-default-citation-link) "cite"))
                          (expect (ebook-notes/org-ref-citation-link "luhmann-system-1984")
                                  :to-equal "[[cite:&luhmann-system-1984]]")))

                    ;; Test case for a different link type
                    (it "correctly formats a link with a different default link type"
                        (setq bibtex-entries nil)
                        (cl-letf (((symbol-value 'org-ref-cite-insert-version) 2)
                                  ((symbol-value 'org-ref-default-citation-link) "ref"))
                          (expect (ebook-notes/org-ref-citation-link "some-key")
                                  :to-equal "[[ref:some-key]]")))

                    ;; Test case for version 3 with a different link type
                    (it "correctly formats a link with version 3 and a different link type"
                        (setq bibtex-entries nil)
                        (cl-letf (((symbol-value 'org-ref-cite-insert-version) 3)
                                  ((symbol-value 'org-ref-default-citation-link) "ref"))
                          (expect (ebook-notes/org-ref-citation-link "some-other-key")
                                  :to-equal "[[ref:&some-other-key]]")))))
