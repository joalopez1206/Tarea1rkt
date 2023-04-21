#lang play

(require "env.rkt")

;; Parte 1

#|
<prog>   ::= {<fundef>* <expr>}

<fundef> ::= {define {<id> <id>*} <expr>}

<expr>   ::= <num>
           | <id>
           | <bool>
           | {cons <expr> <expr>}
           | {add1 <expr>}
           | {+ <expr> <expr>}
           | {< <expr> <expr>}
           | {= <expr> <expr>}
           | {! <expr>}
           | {&& <expr> <expr>}
           | {|| <expr> <expr>}
           | {fst <expr>}
           | {snd <expr>}
           | {if <expr> <expr> <expr>}
           | {with {{<id> <expr>}*} <expr>}
           | {<id> <expr>*}
|#
(deftype Prog
  (prog fundefs main))

(deftype Fundef
  (fundef name arg body))

(deftype Expr
  (num n)
  (pair l r)
  (id x)
  (bool b)
  (add l r)
  (add1 e)
  (lt l r)
  (eq l r)
  (not0 b)
  (or0 l r)
  (and0 l r)
  (fst l)
  (snd l)
  (if0 c t f)
  (with lst body)
  (app name arg-expr))
;; tipo inductivo para los valores del lenguaje
(deftype Val
  (numV n)
  (boolV b)
  (pairV lV rV))

;; parse :: s-program -> program
(define (parse sp)
  (match sp
    [(list ds ... e) (prog (map parse-fundef ds) (parse-expr e))] ;; ds es la lista de definiciones, e es la expresion principal
    ))

;; parse-expr :: s-expr -> expr
(define (parse-expr se)
  (match se
    [(? number?) (num se)]
    [(? symbol?) (id se)]
    [(? boolean?) (bool se)]
    [(list 'cons e1 e2) (pair (parse-expr e1) (parse-expr e2))]
    [(list '+ e1 e2) (add (parse-expr e1) (parse-expr e2))]
    [(list '< e1 e2) (lt (parse-expr e1) (parse-expr e2))]
    [(list '= e1 e2) (eq (parse-expr e1) (parse-expr e2))]
    [(list '! e) (not0 (parse-expr e))]
    [(list '&& e1 e2) (and0 (parse-expr e1) (parse-expr e2))]
    [(list '|| e1 e2) (or0 (parse-expr e1) (parse-expr e2))]
    [(list 'fst e) (fst (parse-expr e))]
    [(list 'snd e) (snd (parse-expr e))]
    [(list 'if  c t f) (if0 (parse-expr c) (parse-expr t) (parse-expr f))]
    [(list 'with lst body) (with (map parse-tuples lst) (parse-expr body))]
    [(? list?) (app (car se) (map parse-expr (cdr se)))]
    ))


(define (parse-tuples lst)
  (match lst
    [(list i e) (list (parse-expr i) (parse-expr e))]
    ['() '()]))

;; parse-fundef :: s-function -> function
(define (parse-fundef sf)
  (match sf
    [(list 'define lst body) (fundef (car lst) (map parse-expr (cdr lst)) (parse-expr body))]
    [_ (error "xd")]))

;; interp :: expr list[var] list [fun] -> Val
(define (interp e env funs)
  (match e
    [(num n) (numV n)]
    [(id x) (env-lookup x env)]
    [(bool b) (boolV b)]
    [(add l r) (numV+ (interp l env funs) (interp r env funs))]
    [(lt l r) (boolV< (interp l env funs) (interp r env funs))]
    [(eq l r) (numV= (interp l env funs) (interp r env funs))]
    [(and0 l r) (numV+ (interp l env funs) (interp r env funs))]
    [(not0 b) (boolVnot (interp b env funs))]
    [(fst p) (interp (pairV-lV p) env funs)]
    [(snd p) (interp (pairV-rV p) env funs)]
    [(if0 c t f) (if (interp c env funs) (interp t env funs) (interp f env funs))]
    [(with vars body) ()]
    [_ (error "not yet implemented")]
    ))

(define (numV+ l r)
  (numV (+ (numV-n l) (numV-n r))))

(define (numV= l r)
  (boolV (equal? ((numV-n l) (numV-n r)))))

(define (boolVnot e)
  (boolV (not (boolV-b e))))

(define (boolV< l r)
  (boolV (< (numV-n l) (numV-n r))))

(define (boolVand l r)
  (boolV (and (boolV-b l) (boolV-b r))))

(define (boolVor l r)
  (boolV (or (boolV-b l) (boolV-b r))))


;; run :: s-program -> val
(define (run sp)
  (def (prog funs main) (parse sp))
  (interp main empty-env funs))

(test (run '{5}) (numV 5))
(test (run '{#t}) (boolV #t))
;(test (run '{+ 1 1}) (numV 2))
;(test
(parse '{ ;; Programa de Ejemplo 2
             {with {{x 5} {y 23} {z {cons 11 -3}}}
                   z}
             })
(parse '{ ;; Programa de Ejemplo 1
             {define {sum x y z} {+ x {+ y z}}}
             {define {cadr x} {fst {snd x}}}
             {with {{x 9} {y {cons 1 {cons 3 4}}}}
                   {sum x {fst y} {cadr y}} }
             })