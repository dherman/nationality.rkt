#lang racket

(require rackunit
         rackunit/gui
         "../main.rkt")
(provide all-tests)

(define philippines-tests
  (test-suite
   "Philippines tests"
   (test-case "singular adjective"
              (check-equal? (nationality-adjective "Philippines, The" 'singular)
                            "Philippine"))
   (test-case "feminine singular adjective"
              (check-equal? (nationality-adjective "Philippines, The" 'feminine/singular)
                            "Philippine"))
   (test-case "plural adjective"
              (check-equal? (nationality-adjective "Philippines, The" 'plural)
                            "Philippine"))
   (test-case "singular noun"
              (check-equal? (nationality-noun "Philippines, The" 'singular)
                            "Filipino"))
   (test-case "feminine singular noun"
              (check-equal? (nationality-noun "Philippines, The" 'feminine/singular)
                            "Filipina"))
   (test-case "plural noun"
              (check-equal? (nationality-noun "Philippines, The" 'plural)
                            "Filipinos"))
   ))

;; TODO: Antigua, Barbuda, Bosnia, Herzegovina, Saint Kitts, Nevis,
;;       Trinidad, Tobago, Serbia, Montenegro

(define two-part-country-tests
  (test-suite
   "two-part country tests"
   ))

(define (has-adjective-forall-types? location)
  (and (nationality-adjective location 'singular)
       (nationality-adjective location 'feminine/singular)
       (nationality-adjective location 'plural)
       #t))

(define general-tests
  (test-suite
   "general tests"
   (test-case "every inhabited country has an adjective for all adjective types"
              (check-true (andmap has-adjective-forall-types? inhabited-locations)))
   (test-case "uninhabited location is a location"
              (check-true (location? "Antarctica")))
   (test-case "uninhabited location is uninhabited"
              (check-false (location-inhabited? "Antarctica")))
   (test-case "inhabited location is a location"
              (check-true (location? "France")))
   (test-case "inhabited location is not uninhabited"
              (check-true (location-inhabited? "France")))
   (test-case "non-location cannot be tested for being inhabited"
              (check-exn exn? (lambda ()
                                (location-inhabited? "Boobooland"))))
   ))

(define location->phrase-tests
  (test-suite "location->phrase tests"
              (test-case "US default"
                         (check-equal? "The United States" (location->phrase "United States, The")))
              (test-case "US capitalized"
                         (check-equal? "The United States" (location->phrase "United States, The" #t)))
              (test-case "US uncapitalized"
                         (check-equal? "the United States" (location->phrase "United States, The" #f)))
              (test-case "Belgium default"
                         (check-equal? "Belgium" (location->phrase "Belgium")))
              (test-case "Belgium capitalized"
                         (check-equal? "Belgium" (location->phrase "Belgium" #t)))
              (test-case "Belgium uncapitalized"
                         (check-equal? "Belgium" (location->phrase "Belgium" #f)))
              (test-case "DR of Congo default"
                         (check-equal? "The Democratic Republic of the Congo" (location->phrase "Congo, The Democratic Republic of the")))
              (test-case "DR of Congo capitalized"
                         (check-equal? "The Democratic Republic of the Congo" (location->phrase "Congo, The Democratic Republic of the" #t)))
              (test-case "DR of Congo uncapitalized"
                         (check-equal? "the Democratic Republic of the Congo" (location->phrase "Congo, The Democratic Republic of the" #f)))))

(define all-tests
  (test-suite
   "all nationality.plt tests"
   philippines-tests
   two-part-country-tests
   general-tests
   location->phrase-tests
   ))

(test/gui all-tests)
