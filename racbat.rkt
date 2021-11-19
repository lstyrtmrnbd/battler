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
  (findf (lambda (elt)
           (regexp-match regx elt))
         content-list))

(define (input-box content-list)
  (let [(@selected (@ "Your unit sir"))]
    (vpanel
     (text @selected)
     (input ""
            (lambda (action str) ;str is the contents
              (<~ @selected
                  (lambda (val)
                    (or (find-content (regexp str)
                                      content-list)
                        ""))))
            #:label "yeh"
            ;#:value=? eq?
            ))))

(define (army-view @army header)
  (vpanel
   (text header)
   (choice (hash-keys nations)
           (lambda (selection)
             (<~ @army
                 (lambda (army)
                   (set-nation army selection)))))
   (text (~> @army army->string))
   (input-box (hash-keys units))))

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

