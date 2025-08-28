
(cl-defun test/make-kindle-entry (title author text &key (type :highlight) (page 10) (location "10-20") (date "[2020-06-09 Tue 18:20:59]"))
  "Helper to create a `kindle-entry' struct for testing purposes."
  (make-kindle-entry :title title
                     :author author
                     :type type
                     :page page
                     :location location
                     :date date
                     :text text))

(provide 'ebook-notes-test-common)
