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

(define nations
  (make-immutable-hash
   '(("EA Arcoscephale" 5)
     ("Ermor" 6)
     ("Ulm" 7)
     ("Marverni" 8)
     ("Sauromatia" 9)
     ("T'ien Ch'i" 10)
     ("Machaka" 11)
     ("Mictlan" 12)
     ("Abysia" 13)
     ("Caelum" 14)
     ("C'tis" 15)
     ("Pangaea" 16)
     ("Agartha" 17)
     ("Tir na n'Og" 18)
     ("Fomoria" 19)
     ("Vanheim" 20)
     ("Helheim" 21)
     ("Niefelheim" 22)
     ("Kailasa" 25)
     ("Lanka" 26)
     ("Yomi" 27)
     ("Hinnom" 28)
     ("Ur" 29)
     ("Berytos" 30)
     ("Xibalba" 31)
     ("Atlantis" 36)
     ("R'lyeh" 37)
     ("Pelagia" 38)
     ("Oceania" 39)
     ("Therodos" 40)
     ("MA Arcoscephale" 43)
     ("Ermor" 44)
     ("Sceleria" 45)
     ("Pythium" 46)
     ("Man" 47)
     ("Eriu" 48)
     ("Ulm" 49)
     ("Marignon" 50)
     ("Mictlan" 51)
     ("T'ien Ch'i" 52)
     ("Machaka" 53)
     ("Agartha" 54)
     ("Abysia" 55)
     ("Caelum" 56)
     ("C'tis" 57)
     ("Pangaea" 58)
     ("Asphodel" 59)
     ("Vanheim" 60)
     ("Jotunheim" 61)
     ("Vanarus" 62)
     ("Bandar Log" 63)
     ("Shinuyama" 64)
     ("Ashdod" 65)
     ("Uruk" 66)
     ("Nazca" 67)
     ("Xibalba" 68)
     ("Atlantis" 73)
     ("R'lyeh" 74)
     ("Pelagia" 75)
     ("Oceania" 76)
     ("Ys" 77)
     ("LA Arcoscephale" 80)
     ("Pythium" 81)
     ("Lemur" 82)
     ("Man" 83)
     ("Ulm" 84)
     ("Marignon" 85)
     ("Mictlan" 86)
     ("T'ien Ch'i" 87)
     ("Jomon" 89)
     ("Agartha" 90)
     ("Abysia" 91)
     ("Caelum" 92)
     ("C'tis" 93)
     ("Pangaea" 94)
     ("Midgård" 95)
     ("Utgård" 96)
     ("Bogarus" 97)
     ("Patala" 98)
     ("Gath" 99)
     ("Ragha" 100)
     ("Xibalba" 101)
     ("Atlantis" 106)
     ("R'lyeh" 107)
     ("Erytheia" 108))))

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
    (choice (hash-keys nations)
            (lambda (selection) (:= @current-nation selection)))
    (button "Make Army" (λ () (<~ @armies add-army))))
   (text (~> @armies
             (lambda (armies)
               (string-append* ""
                               (map army->string armies))))))))

;; Template the data and write the output file

(define template-map
  (file->string "./battler/template.map"))
