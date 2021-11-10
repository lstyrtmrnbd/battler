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

(define template-map
  (file->string "./battler/template.map"))

;; Define army structures
(struct unit (type count))
(struct commander (type xp items magic))
(struct army (commander units))

;; Use data to fill army structures via interface
()


;; Template the data and write the output file

;; Interface
(define @count (@ 0))
(render
 (window
  (hpanel
   (button "-" (λ () (@count . <~ . sub1)))
   (text (@count . ~> . number->string))
   (button "+" (λ () (@count . <~ . add1))))))
