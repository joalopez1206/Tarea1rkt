#lang play

;;;;
#|  COPIE Y PEGUE SU CODIGO DE LA PREGUNTA UNO   |#
#| LUEGO MODIFIQUELO SIGUIENDO LAS INSTRUCCIONES |#
;;;;

;;;; luego de copiar su codigo elimine las definiciones de Exp, Prog, parse y run
(deftype Exp
  (num n)
  (bool b))

(deftype Prog
  (prog funs main))
(define (parse p) (error "replace parse with your own implementation"))
;;;;

#|
<fundef> ::= {define {<id> {arg}*} [: <type>] <expr>}

<arg>    ::= {<id> : <type>}

<expr>   ::= ... | {with { {<id> [: <type>] <expr>}* } <expr>}  ; los otros casos no cambian

<type>   ::= Num | Bool | {Pair <type> <type>}
|#

(deftype Type
  (numT)
  (boolT)
  (pairT lT rT))

;; typecheck-expr :: ...
(define (typecheck-expr e)
  (match e
    [(num n) (numT)]
    [(bool b) (boolT)]
    ; ...
    [_ (error "not yet implemented")]
    ))

;; typecheck-fundef :: ...
(define (typecheck-fundef f)
  ; ...
  (error "not yet implemented"))

;; typecheck :: ...
(define (typecheck p)
  (def (prog funs main) p)
  (begin
    (map typecheck-fundef funs)
    (typecheck-expr main)))

