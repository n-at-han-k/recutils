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
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (oop goops)
  #:export (<field>
            field-name field-value
            string->field field->string
            
            <rec>
            rec-fields
            rec->alist alist->rec string->rec rec->string
            record->rec

            append append!))


;;; Forward declaration to keep the byte compiler happy.
(define %field-to-string #f)
(define %parse-field #f)
(define %field-name #f)
(define %field-value #f)
(define new-field #f)
(define %rec-field-ptrs #f)
(define %rec->string #f)
(define %string->rec #f)
;;; Extension loading.

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
  (fields #:init-keyword #:fields #:init-value '()
          #:allocation #:virtual
          #:accessor rec-fields
          #:slot-ref (lambda (r)
                       (map (lambda (field)
                              (make <field> #:ptr field))
                            (%rec-field-ptrs (slot-ref r 'ptr))))
          #:slot-set! (lambda (r fields)
                        #f)))

;;;; Methods on records

(define-generic append)

(define-method (append (rec <rec>) (name <string>) (value <string>))
  "Append a new field consisting of NAME and VALUE to REC, returning
a new record. "
  (make <rec> #:ptr (%rec-add-field-value (slot-ref rec 'ptr) name value)))

(define-method (append (rec <rec>) (name <string>) (value <top>))
  "Append a new field consisting of NAME and VALUE to REC, converting VALUE
first to string using `object-to-string'. Returns a new record."
  (append rec name (object->string value)))

(define-method (append (rec <rec>) (second <rec>))
  "Append two records together, returning a new record."
  (make <rec> #:ptr (%rec-append-rec (slot-ref rec 'ptr)
                                     (slot-ref second 'ptr))))

(define-generic append!)

(define-method (append! (rec <rec>) (second <rec>))
  "Append two records together, returning a new record."
  (%rec-append-rec! (slot-ref rec 'ptr)
                    (slot-ref second 'ptr)))

(define-method (append! (rec <rec>) (name <string>) (value <string>))
  "Like (append rec name value), but modifies rec destructively."
  (%rec-add-field-value! (slot-ref rec 'ptr) name value))

(define-method (append! (rec <rec>) (name <string>) (value <top>))
  "Like (append rec name value), but modifies rec destructively."
  (append! rec name (object->string value)))

;;;; Record modification

(define (rec-add-field-value rec name value)
  "Add a field with NAME and VALUE to REC."
  (%rec-add-field-value (slot-ref rec 'ptr) name value))

;;;; Conversion functions

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
       (append! rec (car cell) (cdr cell))
       rec))
   (make <rec>)
   alist))

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
                    (append! rec name
                             (struct-ref record idx))
                    rec))))
        rec)))


;;; Record sets

;;; Databases

(define-class <db>)

;;; recutils.in ends here

;; Local Variables:
;; outline-regexp: ";;;"
;; End:
