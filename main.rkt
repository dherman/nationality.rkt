#lang racket/base

(require (for-syntax racket/base)
         racket/contract
         racket/list
         racket/string
         racket/runtime-path
         (for-syntax mzlib/etc))

;; NOTE: no collective plurals

;; =============================================================================
;; DATA DEFINITIONS
;; =============================================================================

(define-struct (exn:fail:uninhabited exn:fail) ())
(define-struct (exn:fail:unknown exn:fail) ())

(define-syntax raise-uninhabited
  (syntax-rules ()
    [(_ nation) (raise (make-exn:fail:uninhabited
                        (string->immutable-string
                         (format "~a: no known inhabitants" nation))
                        (current-continuation-marks)))]))

(define-syntax raise-unknown
  (syntax-rules ()
    [(_ nation) (raise (make-exn:fail:unknown
                        (string->immutable-string
                         (format "unknown geographical location: ~a" nation))
                        (current-continuation-marks)))]))

;; type = (union 'singular 'feminine/singular 'plural)
;; pos-map = (alistof type string)
;; row = pos-map * pos-map

(define-struct row (noun adjective))

;; =============================================================================
;; DATABASE
;; =============================================================================

;; parse-row : (alistof (union 'noun 'adjective) pos-map) -> row
(define (parse-row row-sexp)
  (and row-sexp
       (make-row (cdr (assq 'noun row-sexp))
                 (cdr (assq 'adjective row-sexp)))))

(define-syntax (load-database stx)
  (syntax-case stx ()
    [(_ filename)
     (string? (syntax->datum #'filename))
     (let* ([fn (build-path (this-expression-source-directory) (syntax->datum #'filename))]
            [data (with-input-from-file fn read)])
       #`(let ([sexp (quote #,data)]
               [table (make-hasheq)])
           (for-each (lambda (entry)
                       (hash-set! table
                                  (car entry)
                                  (parse-row (cdr entry))))
                     sexp)
           table))]))

(define database (load-database "database.en.txt"))

;; location? : string -> boolean
(define (location? name)
  (let/ec break
    (hash-ref database name (lambda () (break #f)))
    #t))

;; location-inhabited? : location -> boolean
(define (location-inhabited? locn)
  (and (lookup-location locn) #t))

;; lookup-location : location -> row
(define (lookup-location location)
  (hash-ref database location (lambda ()
                                (raise-unknown location))))

(define locations
  (sort (hash-map database (lambda (key val) key)) string<?))
(define inhabited-locations (filter location-inhabited? locations))
(define uninhabited-locations (filter (compose not location-inhabited?)
                                      locations))

;; =============================================================================
;; NATIONALITY LOOKUP
;; =============================================================================

;; try : (cons symbol string) * (listof symbol) -> (optional string)
(define (try pair keys)
  (let ([key (car pair)])
    (if (eq? key '*)
        (cdr pair)
        (let loop ([keys keys])
          (cond
            [(null? keys) #f]
            [(eq? key (car keys)) (cdr pair)]
            [else (loop (cdr keys))])))))

;; lookup : (alistof symbol string) * (listof symbol) -> (optional string)
(define (lookup entry keys)
  (let loop ([pairs entry])
    (and (pair? pairs)
         (or (try (car pairs) keys)
             (loop (cdr pairs))))))

;; type-keys : type -> (listof symbol)
;; produces the list of keys to search for for a given type, in search order
(define (type-keys type)
  (case type
    [(singular)          '(masculine singular)]
    [(feminine/singular) '(feminine singular)]
    [(plural)            '(plural)]))

;; nationality-lookup-function : (row -> pos-map)
;;                            -> (location * [type] -> string)
(define (nationality-lookup-function selector)
  (lambda (location [type 'singular])
    (let ([row (lookup-location location)])
      (if (not row)
          (raise-uninhabited location)
          (lookup (selector row) (type-keys type))))))

;; nationality-adjective : location * [type] -> string
(define nationality-adjective (nationality-lookup-function row-adjective))

;; nationality-noun : location * [type] -> string
(define nationality-noun (nationality-lookup-function row-noun))

(define nationality-lookup-function/c
  ((string?)
   ((symbols 'singular 'feminine/singular 'plural))
   . ->* .
   string?))

;; =============================================================================
;; LOCATION NAMES
;; =============================================================================

;; location->phrase : string [boolean] -> string
(define (location->phrase location [capitalized? #t])
  (let* ([split (regexp-split #rx", *" location)]
         [len (length split)])
    (printf "~v~n" split)
    (if (= len 1)
        location
        (let* ([rev (reverse split)]
               [tail (car rev)]
               [head (reverse (cdr rev))])
          (let ([phrase (apply string-append (cons tail (cons " " head)))])
            (if capitalized?
                phrase
                (regexp-replace #rx"^The " phrase "the ")))))))

(provide/contract [location? (string? . -> . boolean?)]
                  [location-inhabited? (location? . -> . boolean?)]
                  [locations (listof string?)]
                  [location->phrase ((string?) (boolean?) . ->* . string?)]
                  [inhabited-locations (listof string?)]
                  [uninhabited-locations (listof string?)]
                  [nationality-adjective nationality-lookup-function/c]
                  [nationality-noun nationality-lookup-function/c]
                  [exn:fail:uninhabited? (any/c . -> . boolean?)]
                  [exn:fail:unknown? (any/c . -> . boolean?)])
