;debug function from https://github.com/crowding/sicp/blob/master/chap2.5.3.rkt
;https://github.com/ivanjovanovic/sicp/blob/master/4.1/4.1.scm
#lang planet neil/sicp
(define apply-in-underlying-scheme apply)
(define (show . args)
  ;; write a debugging message.
  (cond ((null? args)
         (display "\n"))
        ((string? (car args))
         (display (car args))
         (display " ")
         (apply show (cdr args)))
        (else
         (write (car args))
         (display " ")
         (apply-in-underlying-scheme show (cdr args)))))

(define (eval exp env)
  (show "EVAL-ENTER" exp)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((let? exp) (eval (let->combination exp) env))
        ((letrec? exp) (eval (letrec->let exp) env))
        ((let*? exp) (eval (let*->nested-lets exp) env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((application? exp)
         (begin (show "APPLICATION" exp)
         (metacirculator-apply (eval (operator exp) env)
                (list-of-values (operands exp) env))))
        (else
          (error "Unknown expression type -- EVAL" exp))))

; metacirculator-apply takes procedure and its arguments and does the application
(define (metacirculator-apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (begin (show 'APPLY-PRIMITIVE procedure)
                (apply-primitive-procedure procedure arguments)))
        ((compound-procedure? procedure)
         (eval-sequence
           (procedure-body procedure)
           (extend-environment
             (procedure-parameters procedure)
             arguments
             (procedure-environment procedure))))
        (else
          (error
            "Unknow procedure type -- metacirculator-apply" procedure))))

(define (and? exp)
  (tagged-list? exp 'and))
(define (and-clauses exp) (cdr exp))
(define (expand-and-clauses clauses)
  (if (null? clauses)
      'true
      (make-if (car clauses)
               (expand-and-clauses (cdr clauses))
               'false)))
(define (and->if exp)
  (expand-and-clauses (and-clauses exp)))

(define (or? exp)
  (tagged-list? exp 'or))
(define (or-clauses exp) (cdr exp))
(define (expand-or-clauses clauses)
  (if (null? clauses)
      'false
      (make-if (car clauses)
               'true
               (expand-or-clauses (cdr clauses)))))

(define (or->if exp)
  (expand-or-clauses (or-clauses exp)))

 (define (let? exp) (tagged-list? exp 'let))
 (define let-associations cadr)
 (define (let-vars exp) (map car (let-associations exp)))
 (define (let-value exp) (map cadr (let-associations exp)))
 (define (let-body exp) (cddr exp))
 (define (let->combination exp)
   (cons (make-lambda (let-vars exp) (let-body exp))
         (let-value exp)))

(define (letrec? exp) (tagged-list? exp 'letrec))
(define (letrec->let exp)
  (let* ((assi (let-associations exp))
         (rest-of-lambda (let-body exp))
         (symbols (map car assi))
         (let-body (map (lambda (s) (list s ''*unassigned*))
                              symbols))
         (set-body (map (lambda (s) (list 'set! (car s) (cadr s)))
                        assi)))
    (if (null? let-body)
      exp
      (append (list 'let let-body) set-body rest-of-lambda))))

(define (let*? expr) (tagged-list? expr 'let*))
(define (let*-body expr) (caddr expr))
(define (let*-inits expr) (cadr expr))
(define (let*->nested-lets expr)
  (let ((inits (let*-inits expr))
        (body (let*-body expr)))
    (define (make-lets exprs)
      (if (null? exprs)
        body
        (list 'let (list (car exprs)) (make-lets (cdr exprs)))))
    (make-lets inits)))

; list of arguments to which procedure is applied
(define (list-of-values exps env)
  (if (no-operands? exps)
    '()
    (cons (eval (first-operand exps) env)
                (list-of-values (rest-operands exps) env))))

; evaluating conditional
(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
    (eval (if-consequent exp) env)
    (eval (if-alternative exp) env)))

; sequence of expressions
(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

; assignments
(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)

; definitions
(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (eval (definition-value exp) env)
                    env)
  'ok)

; Now we have to implement recognition of the syntactic forms. We
; abstract the actual form of the syntax by implementation of predicates
; and selectors of the syntax parts

; self-evaluating predicate
(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

; variables
(define (variable? exp) (symbol? exp))

; quotation (quote text-of-quotation)
(define (quoted? exp) (tagged-list? exp 'quote))
(define (text-of-quotation exp) (cadr exp))
(define (tagged-list? exp tag)
  (if (pair? exp)
    (eq? (car exp) tag)
    false))

; assignments (set! x val)
(define (assignment? exp) (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

; definitions
(define (definition? exp) (tagged-list? exp 'define))
(define (definition-variable exp)
  (if (symbol? (cadr exp))
    (cadr exp)
    (caadr exp)))
(define (definition-value exp)
  (if (symbol? (cadr exp))
    (caddr exp)
    (make-lambda (cdadr exp)
                 (cddr exp))))

; lambda expressions
(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))
(define (make-lambda parameters body) (cons 'lambda (cons parameters body)))

; conditionals
(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
    (cadddr exp)
    'false))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

; begin - sequencing
(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))
(define (make-begin seq) (cons 'begin seq))

; application related selectors and predicate
(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))


; cond, can be represented as nested ifs
(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
    'false                          ; no else clause
    (let ((first (car clauses))
          (rest (cdr clauses)))
      (if (cond-else-clause? first)
        (if (null? rest)
          (sequence->exp (cond-actions first))
          (error "ELSE clause isn't last -- COND->IF" clauses))
        (let ((test (cond-predicate first))
              (recepient (if (eq? (car (cond-actions first)) '=>)
                           (cadr (cond-actions first))
                           false)))
          (make-if test
                  (if recepient
                    (list recepient test) ;test-recepient cond
                    (sequence->exp (cond-actions first))) ;normal cond
                  (expand-clauses rest)))))))

(define (make-application operator operands)
  (cons operator operands))

(define (make-define name parameters body)
  (list 'define name (make-lambda parameters body)))

; evaluator data structures
;
; Evaluator internally deals with different data. This data has to
; be orgnized in compact, un-ambiguous and efficient simple
; data definitions and their combinations (data structures)

; testing of predicates

; thurth is represented as anything that is not explicitly false.
; Data abstraction around the truthiness is defined by the following predicates
(define (true? x)
  (not (eq? x false)))
(define (false? x)
  (eq? x false))

; representing procedures
;
; Procedure consists of params, body and the environment in which is defined by the followig
; abstraction.

(define (make-procedure parameters body env)
  (list 'procedure parameters (rearrange body) env))
(define (compound-procedure? p)
  (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

; another entity we have to define is environment.
; It is represented as the list of frames.

(define the-empty-environment '())
(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))

; A frame is implemented as a pair of lists.
; First element of the pair is list of variables of the frame.
; Second list of elements is the list of values of these variables.

(define (make-frame variables values)
  (cons variables values))
(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))
(define (add-binding-to-the-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
    (cons (make-frame vars vals) base-env)
    (if (< (length vars) (length vals))
      (error "Too many arguments supplied" vars vals)
      (error "Too few arguments supplied" vars vals))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (if (equal? (car vals) '*unassigned*)
               (error "Unassigned variable" var)
               (car vals)))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
      (error "Unbound variable" var)
      (let ((frame (first-frame env)))
        (scan (frame-variables frame)
              (frame-values frame)))))
  (env-loop env))

(define (filter predicate list)
  (cond ((null? list) '())
        ((predicate (car list))
         (cons (car list) (filter predicate (cdr list))))
        (else
          (filter predicate (cdr list)))))


(define (rearrange body)
  (let* ((definitions (filter definition? body))
         (rest-of-lambda (filter (lambda (x)
                                   (not (definition? x)))
                                 body)))
    (append definitions rest-of-lambda)))

(define (scan-out-defines body)
  (let* ((definitions (filter definition? body))
         (rest-of-lambda (filter (lambda (x)
                                   (not (definition? x)))
                                 body))
         (symbols (map cadr definitions))
         (let-body (map (lambda (s) (list s ''*unassigned*))
                              symbols))
         (set-body (map (lambda (s) (list 'set! (cadr s) (caddr s)))
                        definitions)))
    (if (null? let-body)
      body
      (list (append (list 'let let-body) set-body rest-of-lambda)))))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-the-frame! var val frame))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))

; In order to use our evaluator we need several more primitives

; procedure to set up basic elements of the global environment
(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))
(define (primitive-implementation proc)
  (show "GETTING-PRIMITIVE-IMPLEMENTATION" proc)
  (cadr proc))

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)
        (list '= =)))

(define (primitive-procedure-names)
  (map car primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define (setup-environment)
  (let ((initial-env
          (extend-environment (primitive-procedure-names)
                              (primitive-procedure-objects)
                              the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))
(define the-global-environment (setup-environment))

(define (apply-primitive-procedure proc args)
  (show "APPLY-PRIMITIVE-PROCEDURE" proc args)
  (apply-in-underlying-scheme (primitive-implementation proc) args))

;(show primitive-procedures)
;(show (primitive-procedure-objects))

; REPL

(define input-prompt ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value:")
(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (eval input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))
(define (prompt-for-input string)
  (newline) (newline) (show string) (newline))

(define (announce-output string)
  (newline) (show string) (newline))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))

;(driver-loop)
