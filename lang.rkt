#lang racket
(require "asm.rkt")
(provide char
         (rename-out [my-module-begin #%module-begin]))

(define state (make-parameter (new-state)))

(define-syntax-rule (my-module-begin body ...)
  (#%plain-module-begin
   (parameterize ([state (new-state)])
     body ...)))

(define-syntax-rule (char c)
  (ch c))
