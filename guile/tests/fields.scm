(define-module (recutils tests fields)
  #:use-module (oop goops)
  #:use-module (srfi srfi-64)
  #:use-module (srfi srfi-9)
  #:use-module (recutils))

(test-begin "Fields")

(test-equal "field?" #f (field? 1))

(test-assert "field constructor" (is-a? (make <field> #:name "foo" #:value "bar") <field>))

(let ((field (make <field> #:name "asdf" #:value "Foobar")))
  (test-equal "field name (get)" "asdf" (field-name field))
  (test-equal "field value (get)" "Foobar" (field-value field)))

(let ((field (make <field> #:name "asdf" #:value "Foobar")))
  (set! (field-name field) "best")
  (set! (field-value field) "test")
  (test-equal "field name (set)" "best" (field-name field))
  (test-equal "field value (set)" "test" (field-value field)))

(let ((parsed (string->field "Hello: \n+ World")))
  (test-equal "Hello" (field-name parsed))
  (test-equal "\nWorld" (field-value parsed)))

(test-equal "field->string"
  "Whatever: Hello there!"
  (field->string (make <field> #:name "Whatever" #:value "Hello there!")))

(test-equal "conversion idempotency"
  "Hehehe: Hahaha"
  (field->string (string->field "Hehehe: Hahaha")))

(test-end "Fields")
