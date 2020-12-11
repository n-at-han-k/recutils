(define-module (recutils tests records)
  #:use-module (oop goops)
  #:use-module (srfi srfi-64)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (recutils))

(test-begin "Records")

(test-equal "constructor makes an empty record"
  0
  (length (rec->alist (make <rec>))))

(test-equal "rec->alist works"
  '(("Foo" . "Bar"))
  (rec->alist (append (make <rec>) "Foo" "Bar")))

(test-equal "rec->alist on empty cell"
  '()
  (rec->alist (make <rec>)))

(let ((empty (make <rec>)))
  (test-equal "Order is preserved"
    '(("Baz" . "Bap") ("Foo" . "Bar"))
    (rec->alist (append (append empty "Baz" "Bap") "Foo" "Bar"))))

(let ((alist '(("Foo" . "Bar") ("Bip" . "1234") ("Foo" . "Bott"))))
  (test-equal "alist->rec works"
    alist (rec->alist (alist->rec alist))))

(test-equal "rec->string" "Foo: Bar\nBar: 1" (rec->string (alist->rec '(("Foo" . "Bar") ("Bar" . 1)))))
(test-equal "string->rec"
  '(("Quux" . "1") (comment . " hehe") ("Test" . "lol"))
  (rec->alist (string->rec "Quux: 1\n# hehe\nTest: lol")))

(define-record-type my-test-rec
  (make-my-test-rec foo bar)
  my-test-rec?
  (foo my-test-rec-foo)
  (bar my-test-rec-bar))

(let ((my-rec (make-my-test-rec "asdf" "fghj")))
  (test-equal '(("foo" . "asdf")
                ("bar" . "fghj"))
    (rec->alist (record->rec my-rec))))

(let* ((rec (alist->rec '(("test" . "heh")
                          ("hahaha" . "quux"))))
       (ptrs (rec-fields rec)))
  (test-assert (every (lambda (f) (is-a? f <field>)) ptrs))
  (test-equal "rec-fields (1/4)" "test" (field-name (car ptrs)))
  (test-equal "rec-fields (2/4)" "heh" (field-value (car ptrs)))
  (test-equal "rec-fields (3/4)" "hahaha" (field-name (cadr ptrs)))
  (test-equal "rec-fields (4/4)" "quux" (field-value (cadr ptrs))))


(test-equal "append rec1 rec2"
  '(("Foo" . "Bar") ("Beep" . "Boop")
    ("Lorem" . "Ipsum") ("Dolor" . "Sit amet"))
  (let ((rec1 (alist->rec '(("Foo" . "Bar") ("Beep" . "Boop"))))
        (rec2 (alist->rec '(("Lorem" . "Ipsum") ("Dolor" . "Sit amet")))))
    (rec->alist (append rec1 rec2))))

(test-equal "append! rec1 rec2"
  '(("Foo" . "Bar") ("Beep" . "Boop")
    ("Lorem" . "Ipsum") ("Dolor" . "Sit amet"))
  (let ((rec1 (alist->rec '(("Foo" . "Bar") ("Beep" . "Boop"))))
        (rec2 (alist->rec '(("Lorem" . "Ipsum") ("Dolor" . "Sit amet")))))
    (append! rec1 rec2)
    (rec->alist rec1)))

(test-end "Records")
