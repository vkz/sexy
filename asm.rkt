#lang racket
(require (combine-in
          (rename-in rackunit [fail test/fail])
          (except-in rackunit fail))
         rackunit/text-ui)

(provide (except-out (all-defined-out)
                     state
                     stack))

;; instruction =
;; | (char c)
;; | (any)
;; | (choice offset)
;; | (jump offset)
;; | (call offset)
;; | (return)
;; | (commit offset)
;; | (fail)

(define-struct state (pc i e)
  #:mutable)

(define-struct stack (s)
  #:mutable
  #:property prop:procedure (λ (self) (stack-s self))
  #:methods gen:custom-write
  [(define (write-proc self port mode)
     (fprintf port "[~a : ...]" (first (stack-s self))))])

(define (new-stack)
  (make-stack '()))

(define (push stack el)
  (set-stack-s! stack (list* el (stack))))
(define (pop stack)
  (set-stack-s! stack (rest (stack))))

(define (new-state)
  (make-state 0 0 (new-stack)))

(define-syntax-rule (ch c)         `(ch ,c))
;; (define-syntax-rule (any)          `(any))
;; (define-syntax-rule (choice label) `(choice ,label))
;; (define-syntax-rule (jump label)   `(jump ,label))
;; (define-syntax-rule (call label)   `(call ,label))
;; (define-syntax-rule (return)       `(return))
;; (define-syntax-rule (commit label) `(commit ,label))
;; (define-syntax-rule (fail)         `(fail))
;; (define-syntax-rule (label id)     `(label ,id))

(module+ test
  (test-begin
    (let ([s (new-stack)])
      (check-true (empty? (s)))
      (push s "a")
      (push s "b")
      (check-equal? (s) (list "b" "a"))
      (pop s)
      (check-equal? (s) (list "a")))))
