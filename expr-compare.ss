#lang racket


(provide expr-compare test-expr-compare)

; boolean comparison and direct value comparison into one function
(define (compare-values x y)
  (cond
    [(and (boolean? x) (boolean? y))
     (if x (if y #t '%) (if y '(not %) #f))] ; Simplified boolean logic
    [(equal? x y) x]
    [else `(if % ,x ,y)]))

;  list and lambda comparison logic 
(define (analyze-structure x y)
  (match (list (list? x) (list? y))
    [(list #t #t)
     (cond
       ; Handle quoted expressions more precisely, especially for numeric constants
       [(and (equal? (car x) 'quote) (equal? (car y) 'quote))
        (let ((qx (cadr x))
              (qy (cadr y)))
          (cond
            [(and (number? qx) (number? qy)) (compare-values qx qy)]
            [else (if (equal? x y) x `(if % ,x ,y))]))]
       ; Check if one is an if expression and the other is not
       [(xor (equal? (car x) 'if) (equal? (car y) 'if))
        `(if % ,x ,y)]
       ; Lambda expressions comparison
       [(or (equal? (car x) 'lambda) (equal? (car x) 'λ))
        (compare-lambda x y)]
       ; General list processing
       [else (process-lists x y)])]
    ; Direct value comparison for non-list expressions
    [else (compare-values x y)]))



; Helper function to recursively replace parameter names in the lambda body


; Updated lambda comparison to handle body parameter name replacement
(define (compare-lambda lambda1 lambda2)
  (let* ((params1 (cadr lambda1))
         (params2 (cadr lambda2))
         (params-count-equal? (= (length params1) (length params2))))
    (if params-count-equal?
        (let* ((merged-params (map (lambda (p1 p2)
                                     (if (equal? p1 p2)
                                         p1
                                         (string->symbol (format "~a!~a" p1 p2))))
                                   params1 params2))
               (body1 (caddr lambda1))
               (body2 (caddr lambda2))
               (body1-replaced (replace-params body1 params1 merged-params))
               (body2-replaced (replace-params body2 params2 merged-params))
               (body-comparison (analyze-structure body1-replaced body2-replaced)))
          `(,(if (or (equal? (car lambda1) 'λ) (equal? (car lambda2) 'λ)) 'λ 'lambda)
            ,merged-params
            ,body-comparison))
        ;; Handling lambda expressions with different numbers of parameters
        `(if %
             ,(cons (if (equal? (car lambda1) 'λ) 'λ 'lambda) 
                    (cons params1 (list (caddr lambda1))))
             ,(cons (if (equal? (car lambda2) 'λ) 'λ 'lambda) 
                    (cons params2 (list (caddr lambda2))))))))




(define (replace-param-in-body body replacements)
  (cond
    [(list? body)
     (if (or (equal? (car body) 'lambda) (equal? (car body) 'λ))
         (let* ((params (cadr body))
                (body-part (caddr body))
                ; Exclude parameters of this lambda from replacements in its body
                (scoped-replacements (filter (lambda (pair) (not (member (car pair) params))) replacements)))
           (list (car body) params (replace-param-in-body body-part scoped-replacements)))
         (map (lambda (sub-exp) (replace-param-in-body sub-exp replacements)) body))]
    [(symbol? body)
     (let ((replacement (assoc body replacements)))
       (if replacement (cdr replacement) body))]
    [else body]))



(define (replace-params body original-params merged-params)
  (let ((param-map (map cons original-params merged-params)))
    (replace-param-in-body body param-map)))


; Processing lists, checking for special forms or proceeding with element-wise comparison
(define (process-lists l1 l2)
  (if (= (length l1) (length l2))
      (map analyze-structure l1 l2)
      `(if % ,l1 ,l2)))

; Entry function for expression comparison, simplified by focusing on type checks
(define (expr-compare x y)
  (cond
    [(or (number? x) (boolean? x) (symbol? x)) (compare-values x y)]
    [(or (list? x) (list? y)) (analyze-structure x y)]
    [else `(if % ,x ,y)]))

; This procedure tests the expr-compare function by evaluating both expressions
; in the context of % being true or false and checks if they yield the same value.
(define (test-expr-compare x y)
  (let* ((comparison-result (expr-compare x y))
         (evaluated-true (eval `(let ([% #t]) ,comparison-result) (make-base-namespace)))
         (evaluated-false (eval `(let ([% #f]) ,comparison-result) (make-base-namespace)))
         (x-eval (eval x (make-base-namespace)))
         (y-eval (eval y (make-base-namespace))))
    (and (equal? x-eval evaluated-true)
         (equal? y-eval evaluated-false))))
