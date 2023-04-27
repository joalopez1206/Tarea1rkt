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

;; mytype :: Val -> String
(define (mytype v)
  (match v
    [(numV _)    "Number"]
    [(boolV _)   "Bool"]
    [(pairV _ _) "Pair"]))

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
    [(list 'add1 e) (add (num 1) (parse-expr e))]
    [(list '= e1 e2) (eq (parse-expr e1) (parse-expr e2))]
    [(list '! e) (not0 (parse-expr e))]
    [(list '&& e1 e2) (and0 (parse-expr e1) (parse-expr e2))]
    [(list '|| e1 e2) (or0 (parse-expr e1) (parse-expr e2))]
    [(list 'fst e) (fst (parse-expr e))]
    [(list 'snd e) (snd (parse-expr e))]
    [(list 'if c t f) (if0 (parse-expr c) (parse-expr t) (parse-expr f))]
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

;; interp :: expr list[var] list [fun] -> Val/error
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
    [(fst p)
     (pairV-lV (interp p env funs))]
    [(snd p)
     (pairV-rV (interp p env funs))]
    [(if0 c t f) (if (boolV-b (interp c env funs))
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

;; numV+ :: numV x numV -> numV/err
(define (numV+ l r)
  (checknumV l)
  (checknumV r)
  (numV (+ (numV-n l) (numV-n r))))

;; numV= :: numV x numV -> boolV/err
(define (numV= l r)
  (checknumV l)
  (checknumV r)
  (boolV (equal? (numV-n l) (numV-n r))))

;; boolV! :: boolV -> boolV/err
(define (boolV! e)
  (checkboolV e)
  (boolV (not (boolV-b e))))

;; boolV! :: numV x numV -> boolV/err
(define (boolV< l r)
  (checknumV l)
  (checknumV r)
  (boolV (< (numV-n l) (numV-n r))))

;; numV+ numV x numV -> boolV/err
(define (boolVand l r)
  (checkboolV l)
  (checkboolV r)
  (boolV (and (boolV-b l) (boolV-b r))))

;; numV+ numV x numV -> boolV/err
(define (boolVor l r)
  (checkboolV l)
  (checkboolV r)
  (boolV (or (boolV-b l) (boolV-b r))))

;; checknumV :: Val -> null/err
(define (checknumV n)
  (match n
    [(numV _) null]
    [else (error "Runtime type error: expected Number found" (mytype n))]))

;; checkboolV :: Val -> null/err
(define (checkboolV n)
  (match n
    [(boolV _) null]
    [else (error "Runtime type error: expected Bool found" (mytype n))]))

;; checkpairV :: Val -> null/err
(define (checkpairV n)
  (match n
    [(pairV _ _) null]
    [else (error "Runtime type error: expected Pair found" (mytype n))]))

;; run :: s-program -> val
(define (run sp)
  (def (prog funs main) (parse sp))
  (interp main empty-env funs)) 
#|
<fundef> ::= {define {<id> {arg}*} [: <type>] <expr>}

<arg>    ::= {<id> : <type>}

<expr>   ::= ... | {with { {<id> [: <type>] <expr>}* } <expr>}  ; los otros casos no cambian

<type>   ::= Num | Bool | {Pair <type> <type>}
|#

(deftype Arg
  (idx x)
  (type t))

(deftype Type
  (numT)
  (boolT)
  (pairT lT rT))

;; typecheck-expr :: expr x env x funs -> Type/err
(define (typecheck-expr e env funs)
  (match e
    [(num n) (numT)]
    [(bool b) (boolT)]
    [(pair lT rT) (pairT)]
    [(add lT rT) (if (eq? (typecheck-expr lT) numT) (numT) (typecheck-expr rT env funs))]
    [(lt lT rT) (if (eq? (typecheck-expr lT) numT) (numT) (typecheck-expr rT env funs))]
    [(eq lT rT) (if (eq? (typecheck-expr lT) numT) (numT) (typecheck-expr rT env funs))]
    [(and0 lT rT) (if (eq? (typecheck-expr lT) boolT) (boolT) (typecheck-expr rT env funs))]
    [(or0 lT rT) (if (eq? (typecheck-expr lT) boolT) (boolT) (typecheck-expr rT env funs))]
    [(not0 vT) (typecheck-expr vT env funs)]
    [_ (error "not yet implemented")]
    ))

;; typecheck-fundef :: ...
(define (typecheck-fundef f)
  ; ...
  (error "not yet implemented"))

;; typecheck :: Prog -> Type/err
(define (typecheck p)
  (def (prog funs main) p)
  (begin
    (map typecheck-fundef funs)
    (typecheck-expr main empty-env funs)))

