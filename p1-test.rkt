#lang play
(require "p1.rkt")

(test (run '{5}) (numV 5))
(test (run '{#t}) (boolV #t))
;; test que engloba casi todo
(test (run '{ ;; Programa de Ejemplo 1
             {define {sum x y z} {+ x {+ y z}}}
             {define {cadr x} {fst {snd x}}}
             {with {{x 9} {y {cons 1 {cons 3 4}}}}
                   {sum x {fst y} {cadr y}} }
             })
      (numV 13))
;; test para pares 
(test (run '{ ;; Programa de Ejemplo 2
             {with {{x 5} {y 23} {z {cons 11 -3}}}
                   z}
             })
      (pairV (numV 11) (numV -3)))
;; test varias funciones
(test (run '{ 
             {define {triple x} {+ x {+ x x}}}
             {define {add2 x} {+ 2 x}}
             {add2 {triple 2}}
             })
      (numV 8))
;; test para la igualdad
(test (run '{
             {with {{x 3} {y {+ 1 2}}}
                   {if {= x y} x y}}
             })
      (numV 3))
;; test con with nario para el parse
(test (parse-expr '{with {{x 1}{y 2}{z 3}}
              {+ x z}})
  (with 
    (list (cons 'x (num 1)) 
          (cons 'y (num 2))
          (cons 'z (num 3)))
    (add (id 'x) (id 'z))))

;; test con el if y desigualdad
(test (run '{{define {triple x} {+ x {+ x x}}}
             {define {add2 x} {+ 2 x}}
             {with {{x {triple 2}} {y {add2 2}}}
                   {if {< x 10} y x}}
             })(numV 4))

;; test con el else y desigualdad
(test (run '{{define {triple x} {+ x {+ x x}}}
             {define {add2 x} {+ 2 x}}
             {with {{x {triple 3}} {y {add2 18}}}
                   {if {< x 9} y x}}
             })
      (numV 9))

;; test add1
(test (run '{{add1 1}})
      (numV 2))


;; test de scope
(test/exn (run '{{define {f x} {+ x n}}
                 {with {{n 5}} {f 1}}}) "env-lookup: free identifier: n")

;;test runtime error
(test/exn (run '{{+ 5 (cons 1 2)}}
                 )"Runtime type error: expected Number found")

;; test recursivo!
(test (run '{{define {sumacion x} {if {< 0 x} {+ x {sumacion {+ x -1}}} 0}}
            {with {{k 5}}
                  {sumacion k}}
            }) (numV 15))

