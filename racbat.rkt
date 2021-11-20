#lang racket/base

; battler
; now in a decent environment,
; electron deez

(require racket/string
         racket/snip
         racket/gui/easy
         racket/gui/easy/operator
         "data.rkt")

; an army is a nation and its commanders
(struct army (nation commanders))
(struct commander (type xp units items magic))
(struct unit (type count))

(define (set-nation input-army nation)
  (army nation (army-commanders input-army)))

(define (add-commander input-army cmdr)
  (army (army-nation) (cons cmdr (army-commanders input-army))))

; output armies for display
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

; the GUI
(define (find-content regx content-list)
  "Finds a regx match in a list"
  (findf (lambda (elt)
           (regexp-match regx elt))
         content-list))

(define (input-selection-box content-list @selector)
  "Lets you select something by searching within the content list"
  (vpanel
   (text @selector)
   (input ""
          (lambda (action contents)
            (<~ @selector
                (lambda (val)
                  (or (find-content (regexp contents)
                                    content-list)
                      "not found")))))))

(define (unit-selection-contrivance unit-list add-unit)
  (let [(@selected (@ "Search units"))
        (count 0)]
    (hpanel
     (input-selection-box unit-list @selected)
     (input ""
            (lambda (action contents)
              (set! count (string->number contents))))
     (button "Add"
             (lambda ()
               (add-unit (obs-peek @selected) count))))))

(define (army-view @army header)
  (vpanel
   (text header)
   (choice (hash-keys nations)
           (lambda (selection)
             (<~ @army
                 (lambda (army)
                   (set-nation army selection)))))
   (text (~> @army army->string))
   (vpanel
    (input-selection-box (hash-keys commanders)
                         "Search commanders")
    (unit-selection-contrivance (hash-keys units)
                                (lambda (unit count) (display "hi"))))))

(define @green-army (@ (army "Shinuyama" '())))
(define @blue-army (@ (army "Asphodel" '())))

; render interface
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

