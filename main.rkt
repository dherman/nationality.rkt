#lang racket/base

(require (for-syntax racket/base)
         racket/contract
         racket/list
         racket/string racket/function racket/match
         (for-syntax racket/file syntax/parse racket/runtime-path))

;; NOTE: no collective plurals

;; =============================================================================
;; DATA DEFINITIONS
;; =============================================================================

(struct exn:fail:uninhabited exn:fail ())
(struct exn:fail:unknown exn:fail ())

(define-syntax-rule (raise-uninhabited nation)
  (raise (exn:fail:uninhabited
          (string->immutable-string
           (format "~a: no known inhabitants" nation))
          (current-continuation-marks))))

(define-syntax-rule (raise-unknown nation)
  (raise (exn:fail:unknown
          (string->immutable-string
           (format "unknown geographical location: ~a" nation))
          (current-continuation-marks))))

;; type = (union 'singular 'feminine/singular 'plural)
;; pos-map = (alistof type string)
;; row = pos-map * pos-map

(struct row (noun adjective))

;; =============================================================================
;; DATABASE
;; =============================================================================

;; parse-row : (alistof (union 'noun 'adjective) pos-map) -> row
(define (parse-row row-sexp)
  (and row-sexp
       (row (cdr (assq 'noun row-sexp))
                 (cdr (assq 'adjective row-sexp)))))

(begin-for-syntax (define-runtime-path here "."))

(define-syntax (load-database stx)
  (syntax-parse stx 
    [(_ filename:str)
     (let* ([fn (build-path here (syntax->datum #'filename))]
            [data (file->value fn)])
       #`(for/hash ([entry (quote #,data)])
           (values (car entry)
                   (parse-row (cdr entry)))))]))

(define database (load-database "database.en.rktd"))

;; location? : string -> boolean
(define (location? name)
  (and
   (hash-ref database name #f)
   #t))

;; location-inhabited? : location -> boolean
(define (location-inhabited? locn)
  (and (lookup-location locn) #t))

;; lookup-location : location -> row
(define (lookup-location location)
  (hash-ref database location (λ () (raise-unknown location))))

(define locations
  (sort (hash-keys database) string<?))
(define inhabited-locations (filter location-inhabited? locations))
(define uninhabited-locations (filter-not location-inhabited? locations))

;; =============================================================================
;; NATIONALITY LOOKUP
;; =============================================================================

;; try : (cons symbol string) * (listof symbol) -> (optional string)
(define (try pair keys)
  (define key (car pair))
  (if (eq? key '*)
      (cdr pair)
      (let loop ([keys keys])
        (cond
          [(null? keys) #f]
          [(eq? key (car keys)) (cdr pair)]
          [else (loop (cdr keys))]))))

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
(define ((nationality-lookup-function selector) location [type 'singular])
  (define row (lookup-location location))
  (unless row
    (raise-uninhabited location))
  (lookup (selector row) (type-keys type)))

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
  (define split (regexp-split #rx", *" location))
  (define len (length split))
  (printf "~v~n" split)
  (cond [(= len 1) location]
        [else
         (define rev (reverse split))
         (define tail (car rev))
         (define head (reverse (cdr rev)))
         (define phrase (apply string-append (cons tail (cons " " head))))
         (if capitalized?
             phrase
             (regexp-replace #rx"^The " phrase "the "))]))

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
