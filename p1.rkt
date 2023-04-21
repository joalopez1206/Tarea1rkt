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
  (_num n)
  (_id x)
  (_bool b)
  (_add l r)
  (_add1 e)
  (_lt l r)
  (_gt l r)
  (_eq l r)
  (_not b)
  (_or l r)
  (_and l r)
  (_fst l)
  (_snd l)
  (_if cond body other)
  (_with lst body)
  (_app name arg-expr)
)
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
    [(? number?) (_num se)]
    [(? symbol?) (_id se)]
    [(? boolean?) (_bool se)]
    [(list '+ e1 e2) (_add (parse-expr e1) (parse-expr e2))]
    [(list '< e1 e2) (_lt (parse-expr e1) (parse-expr e2))]
    [(list '> e1 e2) (_gt (parse-expr e1) (parse-expr e2))]
    [(list '= e1 e2) (_eq (parse-expr e1) (parse-expr e2))]
    [(list '! e1 e2) (_not (parse-expr e1) (parse-expr e2))]
    [(list '&& e1 e2) (_and (parse-expr e1) (parse-expr e2))]
    [(list '|| e1 e2) (_or (parse-expr e1) (parse-expr e2))]
    [(list 'fst e) (_fst (parse-expr e))]
    [(list 'snd e) (_snd (parse-expr e))]
    [(list 'if e1 e2) (_lt (parse-expr e1) (parse-expr e2))]

    [_ (error "not yet implemented")]
    ))

;; parse-fundef :: s-function -> function
(define (parse-fundef sf)
  ; ...
  (error "not yet implemented"))


;; interp :: expr list[var] list [fun] -> Val
(define (interp e env funs)
  (match e
    [(_num n) (numV n)]
    [(_id x) (env-lookup x env)]
    [(_bool b) (boolV b)]
    [(_add l r) (sumV (interp l env funs) (interp r env funs))]
    [_ (error "not yet implemented")]
    ))

()
;; run :: s-program -> val
(define (run sp)
  (def (prog funs main) (parse sp))
  (interp main empty-env funs))

(test (run '{5}) (numV 5))
(test (run '{#t}) (boolV #t))
(test (run '{+ 1 1}) (numV 2))
;(test
(parse '{ ;; Programa de Ejemplo 1
             {define {sum x y z} {+ x {+ y z}}}
             {define {cadr x} {fst {snd x}}}
             {with {{x 9} {y {cons 1 {cons 3 4}}}}
                   {sum x {fst y} {cadr y}} }
             })
      ;(numV 13))