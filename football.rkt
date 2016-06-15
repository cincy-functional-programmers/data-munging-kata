#lang racket

(require 2htdp/batch-io)

(for ([line (read-lines "data/football.dat")])
  (println line))
