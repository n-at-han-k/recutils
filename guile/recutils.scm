;;; recutils.scm -- Guile bindings for recutils -*- mode: scheme -*-

;; Copyright 2010-2020 Jose E. Marchesi

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.


(define-module (recutils)
  #:use-module (system foreign)
  #:use-module (ice-9 format)
  #:use-module (ice-9 exceptions)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (oop goops)
  #:export (<field>
            field-name field-value
            string->field field->string
            
            <rec>
            elems
            rec->alist alist->rec string->rec rec->string
            record->rec rec->hash-table hash-table->rec

            append-to-rec append-to-rec!

            <comment>
            comment-text))


;; Load the recutils dynamic library.
;; When developing, we have a script (pre-inst-env)
;; which modifies the environment variable below
;; to be the working directory, that way we can develop
;; the library without affecting the system installed file.
;; Otherwise we just let Guile find the library from its
;; system directory (usually $GUILE_SITE_DIR).
(load-extension
 (let ((extdir (getenv "LIBREC_GUILE_EXTENSION_DIR")))
   (if extdir
       (string-append extdir "/libguile-recutils")
       "libguile-recutils"))
 "scm_init_recutils")

;;;; Fields

;; The wrapper class wrapping a recutils field.
(define-class <field> ()
  ;; Internal pointer type.
  (ptr #:init-form (new-field)
       #:init-keyword #:ptr)

  ;; Virtual slots for name and value. Calls C code
  ;; underneath.
  (name #:init-keyword #:name #:allocation #:virtual
        #:accessor field-name
        #:slot-ref (lambda (field)
                     (%field-name (slot-ref field 'ptr)))
        #:slot-set! (lambda (field name)
                      (set-field-name! (slot-ref field 'ptr) name)))
  
  (value #:init-keyword #:value #:allocation #:virtual
         #:accessor field-value
         #:slot-ref (lambda (field)
                      (%field-value (slot-ref field 'ptr)))
         #:slot-set! (lambda (field value)
                       (set-field-value! (slot-ref field 'ptr) value))))

;;;; Comments
;;; Comments are just strings.

(define-class <comment> ()
  (text #:init-keyword #:text #:accessor comment-text))

;;;;; Conversion methods

(define (string->field str)
  "Convert STR into a field. If parsing fails, return #f."
  (make <field> #:ptr (%parse-field str)))

(define-method (field->string (field <field>))
  "Convert FIELD into a string."
  (%field-to-string (slot-ref field 'ptr)))

;;;; Records

(define-class <rec> ()
  (ptr #:init-form (%make-empty-rec) #:init-keyword #:ptr)
  (elems #:init-keyword #:elems #:init-value '()
         #:allocation #:virtual
         #:accessor elems
         #:slot-ref (lambda (r)
                      (map (match-lambda
                             (('field . field)
                              (make <field> #:ptr field))
                             (('comment . comment)
                              (make <comment> #:text comment)))
                           (%rec-elem-ptrs (slot-ref r 'ptr))))
         #:slot-set! (lambda (r fields)
                       #f)))

;;;; Methods on records

(define-generic append-to-rec)

(define-method (append-to-rec (rec <rec>) (name <string>) (value <string>))
  "Append a new field consisting of NAME and VALUE to REC, returning
a new record. "
  (let ((new (make <rec>)))
    (append-to-rec! new rec)
    (append-to-rec! new name value)
    new))

(define-method (append-to-rec (rec <rec>) (name <string>) (value <top>))
  "Append a new field consisting of NAME and VALUE to REC, converting VALUE
first to string using `object-to-string'. Returns a new record."
  (append-to-rec rec name (object->string value)))

(define-method (append-to-rec (rec <rec>) (second <rec>))
  "Append two records together, returning a new record."
  (make <rec> #:ptr (%rec-append-rec (slot-ref rec 'ptr)
                                     (slot-ref second 'ptr))))

(define-method (append-to-rec (rec <rec>) (comment <comment>))
  "Append COMMENT to REC."
  (let ((new (make <rec>)))
    (append-to-rec! new rec)
    (append-to-rec! new comment)
    new))

(define-generic append-to-rec!)

(define-method (append-to-rec! (rec <rec>) (second <rec>))
  "Append two records together, returning a new record."
  (%rec-append-rec! (slot-ref rec 'ptr)
                    (slot-ref second 'ptr)))

(define-method (append-to-rec! (rec <rec>) (name <string>) (value <string>))
  "Like (append-to-rec rec name value), but modifies rec destructively."
  (%rec-add-field-value! (slot-ref rec 'ptr) name value))

(define-method (append-to-rec! (rec <rec>) (name <string>) (value <top>))
  "Like (append-to-rec rec name value), but modifies rec destructively."
  (append-to-rec! rec name (object->string value)))

(define-method (append-to-rec! (rec <rec>) (comment <comment>))
  (%rec-add-comment (slot-ref rec 'ptr) (slot-ref comment 'text)))

;;;; Conversion functions
;;; NOTE: Use destructive variants of `append-to-rec!' for maximum efficiency,
;;; since while the API makes it easy to operate on records directly, there will
;;; be lots of conversions going on. We don't want to allocate too much memory
;;; in that case.

(define (rec->string rec)
  "Print REC as a string."
  (%rec->string (slot-ref rec 'ptr)))

(define (string->rec string)
  "Parse STRING into a recutils rec."
  (let ((parsed (%string->rec string)))
    (if parsed
        (make <rec> #:ptr (%string->rec string))
        #f)))

(define (rec->alist rec)
  "Convert REC to an alist. Comments will be of the form (comment . 'comment')."
  (%rec->alist (slot-ref rec 'ptr)))

(define (alist->rec alist)
  "Convert ALIST to a recutils rec."
  (fold
   (lambda (cell rec)
     (begin
       (if (equal? 'comment (car cell))
           (append-to-rec! rec (make <comment> #:text (cdr cell)))
           (append-to-rec! rec (car cell) (cdr cell)))
       rec))
   (make <rec>)
   alist))

(define (rec->hash-table rec)
  "Convert REC into a hash table. If any given field has multiple values, the
hash table value for that field will be converted into a list. Comments are
skipped. If you convert this hash table into a record using `hash-table->rec',
consider that it will most likely not have the same order as the original
record, as such, hash tables should only be used for efficient lookup and the
original record should be kept available elsewhere."
  (let ((hash (make-hash-table)))
    (for-each
     (lambda (field)
       (let* ((name (field-name field))
              (value (field-value field))
              (hash-value (hash-ref hash name)))
         (hash-set! hash name
                    (if (list? hash-value)
                        (append hash-value value)
                        (list value)))))
     (slot-ref rec 'elems))
    hash))

(define (hash-table->rec hash)
  "Convert hash-table HASH into a record. Do note that if the hash table was
created using `rec->hash-table', due to the hashing function you are not likely
going to get the original record."
  (let ((rec (make <rec>)))
    (hash-for-each
     (lambda (k v)
       (append-to-rec! rec k v))
     hash)
    rec))

(define (record->rec record)
  "Convert RECORD, a Scheme record, to a recutils record."
  (let loop ((fields (record-type-fields (record-type-descriptor record)))
             (idx 0)
             (rec (make <rec>)))
    (if (not (null? fields))
        (begin
          (let ((name (symbol->string (car fields)))
                (raw-value (struct-ref record idx)))
            (loop (cdr fields)
                  (+ 1 idx)
                  (begin
                    (append-to-rec! rec name
                                    (struct-ref record idx))
                    rec))))
        rec)))


;;; Record sets

;;; Databases

;;; recutils.in ends here

;; Local Variables:
;; outline-regexp: ";;;"
;; End:
