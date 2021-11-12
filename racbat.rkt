#lang racket/base

; battler
; now in a decent environment

(require racket/port
         racket/file
         racket/gui/easy
         racket/gui/easy/operator
         json)

;; Read the data
(define dmi-info
  (with-input-from-string
    (file->string "./dmi_data.json")
    (lambda () (read-json))))

;; Define army structures
(struct unit (type count))
(struct commander (type xp items magic))
(struct army (commander units))

;; Use data to make armies via interface
(define @armies (@ '()))

(define (add-army armies)
  (cons (army #f '()) armies))

(define (unit->string unit)
  (string-append (number->string (unit-count unit))
                 " "
                 (unit-type unit)))

(define (commander->string commander)
  (commander-type commander))

(define (army->string army)
  (string-append "Commander: " (commander->string
                                (army-commander army))
                 "\n"
                 "Units: " (map unit->string (army-units army))))

;; Interface
(render
 (window
  (hpanel
   (button "Make Army" (λ () (@armies . <~ . add-army)))
   (text (@armies . ~> . (lambda (armies)
                           (map army->string armies)))))))

;; Template the data and write the output file

(define template-map
  (file->string "./battler/template.map"))
