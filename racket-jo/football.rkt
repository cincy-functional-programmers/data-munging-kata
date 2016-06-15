#lang racket

(require 2htdp/batch-io)

(first
  (foldl (lambda (a result) (if (< (second a) (second result)) a result)) '("" 100)
    (map (lambda (a) (list (first a) (abs (- (string->number (second a)) (string->number (third a))))))
      (apply map list
        (map rest
          (filter
            (lambda (a)
              (let ([fst (first a)])
                (or (string=? fst "Team") (string=? fst "F") (string=? fst "A"))))
            (apply map list
              (filter-not null?
                (for/list ([i (in-naturals 1)]
                           [line (read-lines "../data/football.dat")])
                  (let ([split (filter-not (lambda (a) (string=? a "-")) (string-split line))])
                    (if (< i 2)
                      split
                      (rest split))))))))))))
