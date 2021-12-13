(define-module (recutils tests records)
  #:use-module (oop goops)
  #:use-module (srfi srfi-64)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 hash-table)
  #:use-module (recutils))

(test-begin "Records")

(test-equal "constructor makes an empty record"
  0
  (length (rec->alist (make <rec>))))

(test-equal "rec->alist works"
  '(("Foo" . "Bar"))
  (rec->alist (append-to-rec (make <rec>) "Foo" "Bar")))

(test-equal "rec->alist on empty cell"
  '()
  (rec->alist (make <rec>)))

(let ((empty (make <rec>)))
  (test-equal "Order is preserved"
    '(("Baz" . "Bap") ("Foo" . "Bar"))
    (rec->alist (append-to-rec (append-to-rec empty "Baz" "Bap") "Foo" "Bar"))))

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
       (ptrs (elems rec)))
  (test-assert (every (lambda (f) (is-a? f <field>)) ptrs))
  (test-equal "rec-fields (1/4)" "test" (field-name (car ptrs)))
  (test-equal "rec-fields (2/4)" "heh" (field-value (car ptrs)))
  (test-equal "rec-fields (3/4)" "hahaha" (field-name (cadr ptrs)))
  (test-equal "rec-fields (4/4)" "quux" (field-value (cadr ptrs))))


(test-equal "append-to-rec rec1 rec2"
  '(("Foo" . "Bar") ("Beep" . "Boop")
    ("Lorem" . "Ipsum") ("Dolor" . "Sit amet"))
  (let ((rec1 (alist->rec '(("Foo" . "Bar") ("Beep" . "Boop"))))
        (rec2 (alist->rec '(("Lorem" . "Ipsum") ("Dolor" . "Sit amet")))))
    (rec->alist (append-to-rec rec1 rec2))))

(test-equal "append-to-rec! rec1 rec2"
  '(("Foo" . "Bar") ("Beep" . "Boop")
    ("Lorem" . "Ipsum") ("Dolor" . "Sit amet"))
  (let ((rec1 (alist->rec '(("Foo" . "Bar") ("Beep" . "Boop"))))
        (rec2 (alist->rec '(("Lorem" . "Ipsum") ("Dolor" . "Sit amet")))))
    (append-to-rec! rec1 rec2)
    (rec->alist rec1)))

(test-equal "rec->hash-table"
  '(("Lol" "Bob") ("Foobar" "Baz" . "Bazbaz"))
  (hash-map->list cons
   (rec->hash-table
    (let ((rec (make <rec>)))
      (append-to-rec! rec "Lol" "Bob")
      (append-to-rec! rec "Foobar" "Baz")
      (append-to-rec! rec "Foobar" "Bazbaz")
      rec))))

(let* ((alist '(("Test" . "Testy test")
                ("Zest" . "Is best")))
       (hash (alist->hash-table alist)))
  (test-equal "hash-table->rec" alist (rec->alist (hash-table->rec hash))))


(test-assert "comments become instances of <comment>"
  (every (lambda (elem) (is-a? elem <comment>))
         (elems
          (alist->rec '((comment . "Foo") (comment . "Bar"))))))

(test-end "Records")
