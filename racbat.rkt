#lang racket/base

; battler
; now in a decent environment

(require racket/port
         racket/file
         racket/string
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
(struct commander (type xp units items magic))
(struct army (nation commanders))

;; Use data to make armies via interface
(define (unit->string unit)
  (string-append (number->string (unit-count unit))
                 " "
                 (unit-type unit)))

(define (commander->string commander)
  (commander-type commander))

(define (army->string army)
  (string-append*
   "Nation: " (army-nation army) "\n"
   (map (lambda (commander)
          (string-append "Commander: "
                         (commander->string commander)
                         "\n"
                         "Units: "
                         (map unit->string (commander-units commander))))
        (army-commanders army))))



(define @current-nation (@ "Shinuyama"))
(define @armies (@ '()))

(define (add-army armies)
  (cons (army (obs-peek @current-nation)
              '())
        armies))

;; Interface
(render
 (window
  (hpanel
   (vpanel
    (choice '("Shinuyama" "Arcocephale")
            (lambda (selection) (:= @current-nation selection)))
    (button "Make Army" (λ () (<~ @armies add-army))))
   (text (~> @armies
             (lambda (armies)
               (string-append* ""
                               (map army->string armies))))))))

;; Template the data and write the output file

(define template-map
  (file->string "./battler/template.map"))
