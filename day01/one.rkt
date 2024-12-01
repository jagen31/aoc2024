#lang racket

(require art art/sequence/ravel "input.rkt")

(define-art-rewriter solve
   (λ (stx)
     #'(run-apl (reduce apl:+ 
      (magnitude 
       (reduce apl:- 
         (transpose 
           (mix ((each (monad-dfn (apl:index ω (grade-up ω)))) 
                 (split (transpose *ctxt*)))))))))))
