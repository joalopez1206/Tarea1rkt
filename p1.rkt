#lang play

(require "env.rkt")


(define (lookup-fundef f fundefs)
  (match fundefs
    ['() (error "undefined function:" f)]
    [(cons fd rest)
     (if (equal? (fundef-name fd) f)
         fd
         (lookup-fundef f rest))]))
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
    [(list 'with lst body) (with (map parse-tuple lst) (parse-expr body))]
    [(? list?) (app (car se) (map parse-expr (cdr se)))]
    ))

;; parse-tuples :: list[list[id, val]] -> list[tuple[id, val]] 
(define (parse-tuple tuple)
  (match tuple
    [(list i e) (cons i (parse-expr e))]))

;; parse-fundef :: s-function -> function
(define (parse-fundef sf)
  (match sf
    [(list 'define lst body) (fundef (car lst) (map parse-expr (cdr lst)) (parse-expr body))]
    ))

;; interp :: expr list[var] list [fun] -> Val
(define (interp e env funs)
  (match e
    [(num n) (numV n)]
    [(id x) (env-lookup x env)]
    [(bool b) (boolV b)]
    [(pair l r) (pairV (interp l env funs) (interp r env funs))]
    [(add l r) (numV+ (interp l env funs) (interp r env funs))]
    [(lt l r) (boolV< (interp l env funs) (interp r env funs))]
    [(eq l r) (numV= (interp l env funs) (interp r env funs))]
    [(and0 l r) (boolVand (interp l env funs) (interp r env funs))]
    [(or0 l r) (boolVor (interp l env funs) (interp r env funs))]
    [(not0 b) (boolV! (interp b env funs))]
    [(fst p) (pairV-lV (interp p env funs))]
    [(snd p) (pairV-rV (interp p env funs))]
    [(if0 c t f) (if (interp c env funs)
                     (interp t env funs)
                     (interp f env funs))]
    [(with vars body)
     (interp body (extend-env-vars env vars funs env) funs)]
    [(app f value)
     (def (fundef _ args body) (lookup-fundef f funs))
     (def vars (map cons (map id-x args) value))
     (interp body (extend-env-vars empty-env vars funs env) funs)]
    ))

;; extend-env-vars :: env x list[tuple[id, expr]] list[funs] x env -> env
(define (extend-env-vars env vars funs outside-env)
  (match vars
    ['() env]
    [(cons (cons i val) tail)
     (def value (interp val outside-env funs))
     (def new-env (extend-env i value env))
     (extend-env-vars new-env tail funs  outside-env)]))
;;
(define (numV+ l r)
  (numV (+ (numV-n l) (numV-n r))))
;;
(define (numV= l r)
  (boolV (equal? (numV-n l) (numV-n r))))

(define (boolV! e)
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
#|(run '{ ;; Programa de Ejemplo 1
             {define {sum x y z} {+ x {+ y z}}}
             {define {cadr x} {fst {snd x}}}
             {with {{x 9} {y {cons 1 {cons 3 4}}}}
                   {sum x {fst y} {cadr y}} }
             })|#

#|(run '{{define {triple x} {+ x {+ x x}}}
             {define {add2 x} {+ 2 x}}
             {with {{x {triple 2}} {y {add2 x}}}
                   {if {< x 10} y #f}}
             })|#