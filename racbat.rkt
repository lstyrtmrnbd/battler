#lang racket/base

; battler
; now in a decent environment

(require racket/class
         racket/string
         racket/snip
         racket/gui/easy
         racket/gui/easy/operator
         "data.rkt")

;; Define army structures
(struct unit (type count))
(struct commander (type xp units items magic))
(struct army (nation commanders))

(define (set-nation input-army nation)
  (army nation (army-commanders input-army)))

(define (add-commander input-army cmdr)
  (army (army-nation) (cons cmdr (army-commanders input-army))))

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

(define (army-view @army header)
  (vpanel
   (text header)
   (choice (hash-keys nations)
           (lambda (selection)
             (<~ @army
                 (lambda (army)
                   (set-nation army selection)))))
   (text (~> @army army->string))
   (snip "" (lambda (in h v)
              (new snip%))
         #:style '(combo))))

(define @green-army (@ (army "Shinuyama" '())))
(define @blue-army (@ (army "Asphodel" '())))

;; Interface
(define (show)
  (render
   (window #:title "battler"
    (menu-bar
     (menu "File"
           (menu-item "Save As...")
           (menu-item "Exit")))
    (hpanel
     (army-view @green-army "Green Team")
     (army-view @blue-army "Blue Team")))))

(show)

;; Template the data and write the output file

