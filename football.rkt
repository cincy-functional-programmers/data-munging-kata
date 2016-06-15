#lang racket

(require 2htdp/batch-io)

(define (transpose xss)
  (apply map list xss))

  (filter-not null?
    (for/list ([i (in-naturals 1)]
               [line (read-lines "data/football.dat")])
      (let ([split (filter-not (lambda (a) (= a "-"))(string-split line))])
        (if (< i 2)
          split
          (rest split)))))
