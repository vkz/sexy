#lang racket

;; LPeg parsing machine with backtracking via stack

;; pat =
;; | (e)
;; | (char c)
;; | (any)
;; | (opt pat)
;; | (star pat)
;; | (plus pat)
;; | (seq pat pat)
;; | (ord pat pat)
;; | (look pat)
;; | (neg pat)

;; instruction =
;; | (char c)
;; | (any)
;; | (choice offset)
;; | (jump offset)
;; | (call offset)
;; | (return)
;; | (commit offset)
;; | (fail)

(require (combine-in
          (rename-in rackunit [fail test/fail])
          (except-in rackunit fail))
         rackunit/text-ui)

(module asm racket
  ;; semantics
  (provide (except-out (all-defined-out)
                       state
                       stack))

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
  (define-syntax-rule (any)          `(any))
  (define-syntax-rule (choice label) `(choice ,label))
  (define-syntax-rule (jump label)   `(jump ,label))
  (define-syntax-rule (call label)   `(call ,label))
  (define-syntax-rule (return)       `(return))
  (define-syntax-rule (commit label) `(commit ,label))
  (define-syntax-rule (fail)         `(fail))
  (define-syntax-rule (label id)     `(label ,id)))

(module+ test
  (require (submod ".." asm))
  (test-begin
    (let ([s (new-stack)])
      (check-true (empty? (s)))
      (push s "a")
      (push s "b")
      (check-equal? (s) (list "b" "a"))
      (pop s)
      (check-equal? (s) (list "a")))))

(module lang racket
  ;; parser
  (require (submod ".." asm))
  (require racket/base)
  (provide char
           (except-out (all-from-out racket/base) #%module-begin)
           (rename-out [my-module-begin #%module-begin]))

  (define state (make-parameter (new-state)))

  (define-syntax-rule (my-module-begin body ...)
    (#%plain-module-begin
     (parameterize ([state (new-state)])
       body ...)))

  (define-syntax-rule (char c)
    (ch c)))

(module main (submod ".." lang)
  (define res (char "c"))
  (println res))


