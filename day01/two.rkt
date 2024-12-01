#lang racket

(require art art/sequence/ravel (for-syntax syntax/parse) "input.rkt")

;; ravel (apl) code to get a column
(define-art-rewriter do-it
   (λ (stx) (syntax-parse stx [(_ n) #'(run-apl (mix (apl:index (split (transpose *ctxt*)) (lit n))))])))

(define-art-rewriter solve
  (λ (stx)
    (define source (rewrite-in (current-ctxt) #`(do-it 0)))
    (define target (rewrite-in (current-ctxt) #`(do-it 1)))
    (define result 
      (for/sum ([n source])
        (* (number-value n) 
           (number-value 
             (car (rewrite-in target 
                    ;; metaprogram the apl
                    #`(run-apl (reduce apl:+ (apl:= (lit #,(number-value n)) *ctxt*)))))))))
    #`(replace-full-context (number #,result))))
