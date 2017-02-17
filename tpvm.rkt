#lang racket

;; threaded parsing machine after Thompson & Pike

;; pat =
;; | (e)
;; | (char c)
;; | (any)
;; | (opt pat)
;; | (star pat)
;; | (plus pat)
;; | (seq pat pat)
;; | (ord pat pat)
;; ---------------
;; | (look pat)
;; | (neg pat)

;; instruction =
