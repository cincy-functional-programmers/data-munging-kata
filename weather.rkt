#lang racket

(require 2htdp/batch-io)

(first
  (foldl
    (lambda (a result)
      (if (< (second a) (second result)) a result))
      '(100 100)
      (filter-not null?
        (for/list ([i (in-naturals 1)]
                   [line (read-lines "data/weather.dat")])
          (if (> i 2)
            (let ([split (string-split line)])
              (let ([stuff
                (map string->number
                  (map
                    (lambda (text)
                      (list->string (remove*
                        (string->list "*")
                        (string->list text))))
                    (list (first split) (second split) (third split))))])
            (list (first stuff) (- (second stuff) (third stuff)))))
            '())))))
