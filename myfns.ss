; ; Erik Thompson
; 3341 Project6

(define (myinterpreter x)
	(mainEval (cadr x) '())
)
;Main evaluation of Statements
(define (mainEval x bindings)
	;(display x) (newline)
	(cond
		((integer? x) x)
		((mbr x '(a b c d e f g h i j k l m n o p q r s t u v w x y z)) 
			(getFirstLet x bindings)
		)
		;((symbol? x) 
		;	(getFirstLet (car x) bindings)
		;)
		((equal? (car x) 'planAdd)
			(evalPlanAdd (cdr x) bindings)
		)
		((equal? (car x) 'planMul)
			(evalPlanMul (cdr x) bindings)
		)
		((equal? (car x) 'planSub)
			(evalPlanSub (cdr x) bindings)
		)
		((equal? (car x) 'planIf)
			(evalPlanIf (cdr x) bindings)
		)
		((equal? (car x) 'planLet)
			(evalPlanLet (cdr x) bindings)
		)
	)
)

; Adds two expressions
(define (evalPlanAdd x bindings)
	(+
		(mainEval (car x) bindings)
		(mainEval (cadr x) bindings)
	)
)
;Multiplies two expressions
(define (evalPlanMul x bindings)
	(*
		(mainEval (car x) bindings)
		(mainEval (cadr x) bindings)
	)
)
;Subtracts two expressions
(define (evalPlanSub x bindings)
	(-
		(mainEval (car x) bindings)
		(mainEval (cadr x) bindings)
	)
)
;If first expression > 0 expression 2 else expression 3
(define (evalPlanIf x bindings)
	(cond
		((> (mainEval (car x) bindings) 0) 
			(mainEval (cadr x) bindings)
		)
		(else (mainEval (caddr x) bindings))
	)
)

;Store expr1 to id1 and evaluate expr2.
(define (evalPlanLet x bindings)
	;(display x) (newline)
	;(display (car x)) (newline)
	(mainEval (caddr x) (cons (cons (car x) (mainEval (cadr x) bindings)) bindings))
)

;Get first binding for a letter.
(define (getFirstLet a bindings)
	;(display a) (newline)
	;(display bindings) (newline)
	(cond
		((equal? a (caar bindings))
			(cdar bindings)
		)
		(else (getFirstLet a (cdr bindings)))
	)
)

;Member function from lecture slides referenced
(define (mbr x list)
	(cond
		( (null? list) #f )
		( #t (cond
			( (equal? x (car list)) #t )
			( #t (mbr x (cdr list)) ) ) )
	)
)