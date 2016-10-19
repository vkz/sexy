#lang racket

;; LPeg parsing machine with backtracking via stack

;; pat =
;; | (e)
;; | (char c)
;; | (any)
;; | (opt pat)
;; | (star pat)
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

  (define-struct asm (ops)
    #:property prop:procedure (λ (self) (asm-ops self))
    #:transparent)
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
  (require racket/pretty)
  (provide char
           any-char
           seq
           ord
           neg
           look
           star
           (except-out (all-from-out racket/base) #%module-begin)
           (all-from-out racket/pretty)
           (rename-out [my-module-begin #%module-begin]))

  (define state (make-parameter (new-state)))

  (define-syntax-rule (my-module-begin body ...)
    (#%plain-module-begin
     (parameterize ([state (new-state)])
       body ...)))

  (begin-for-syntax
    (define-syntax-rule (with-asm-labels [lbl ...] body ...)
      (with-syntax ([(lbl ...) (generate-temporaries '(lbl ...))])
        body ...)))

  ;; compile patters to asm
  (define-syntax-rule (char c)
    (list (ch c)))

  (define-syntax-rule (any-char)
    (list (any)))

  (define-syntax-rule (seq l r)
    (append l r))

  (define-syntax (ord stx)
    (syntax-case stx ()
      [(_ l r)
       (with-asm-labels [lbl-1 lbl-2]
         #'(append (list (choice 'lbl-1))
                   l
                   (list (commit 'lbl-2))
                   (list (label 'lbl-1))
                   r
                   (list (label 'lbl-2))))]))

  (define-syntax (neg stx)
    (syntax-case stx ()
      [(_ p)
       (with-asm-labels [lbl lbl-fail]
         #'(append (list (choice 'lbl))
                   p
                   (list (commit 'lbl-fail))
                   (list (label 'lbl-fail))
                   (list (fail))
                   (list (label 'lbl))))]))

  (define-syntax-rule (look p)
    (neg (neg p)))

  (define-syntax (star stx)
    (syntax-case stx ()
      [(_ p)
       (with-asm-labels [lbl-more lbl-skip]
         #'(append (list (choice 'lbl-skip))
                   (list (label 'lbl-more))
                   p
                   (list (commit 'lbl-more))
                   (list (label 'lbl-skip))))])))

(module main (submod ".." lang)
  (pretty-print (vector
                 (char "c")
                 (any-char)
                 (seq (char "c") (any-char))
                 (ord (char "c") (char "d"))
                 (neg (char "c"))
                 (look (char "c"))
                 (star (char "c")))))
