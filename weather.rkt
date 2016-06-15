#lang racket

(require 2htdp/batch-io)

; The first of the remaining tuple is the day with the smallest temp diff.
(first
  ; Find the tuple with minimum second argument
  (foldl
    (lambda (a result)
      (if (< (second a) (second result)) a result))
      '(100 100)
      ; Filter out the empty items
      (filter-not null?
        ; Build a list of tuples (Day TempDiff)
        (for/list ([i (in-naturals 1)]
                   [line (read-lines "data/weather.dat")])
          ; Skip the first two lines of garbage
          (if (> i 2)
            (let ([split (string-split line)]) ; split the line on spaces
              (let ([stuff ; Convert the list of strings to numbers after
                           ; removing asterisk
                (map string->number
                  (map
                    (lambda (text)
                      (list->string (remove*
                        (string->list "*")
                        (string->list text))))
                    (list (first split) (second split) (third split))))])
            ; Create the tuple of (Day TempDiff)
            (list (first stuff) (- (second stuff) (third stuff)))))
            ; Else return empty list
            '())))))
