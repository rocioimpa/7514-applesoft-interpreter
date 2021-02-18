(require '[clojure.test :refer [is deftest run-tests]])

(load-file "basic.clj")

(deftest test-palabra-reservada?
   (is (= true (palabra-reservada? 'REM)))
   (is (= false (palabra-reservada? 'SPACE)))
)

(deftest test-operador?
   (is (= true (operador? '+)))
   (is (= true (operador? (symbol "+"))))
   (is (= false (operador? (symbol "%"))))
)

(deftest test-eliminar-cero-decimal
   (is (= 0 (eliminar-cero-decimal ".")))
   (is (= 1.5 (eliminar-cero-decimal 1.5)))
   (is (= 1.5 (eliminar-cero-decimal 1.50)))
   (is (= 1 (eliminar-cero-decimal 1.0)))
   (is (= 'A (eliminar-cero-decimal 'A)))
   (is (= 1 (eliminar-cero-decimal 1)))
)

(deftest test-eliminar-cero-entero
   (is (= nil (eliminar-cero-entero nil)))
   (is (= "A" (eliminar-cero-entero 'A)))
   (is (= "0" (eliminar-cero-entero 0)))
   (is (= "1.5" (eliminar-cero-entero 1.5)))
   (is (= "1" (eliminar-cero-entero 1)))
   (is (= "-1" (eliminar-cero-entero -1)))
   (is (= "-1.5" (eliminar-cero-entero -1.5)))
   (is (= ".5" (eliminar-cero-entero 0.5)))
   (is (= "-.5" (eliminar-cero-entero -0.5)))
)

(deftest test-variable-float?
   (is (= true (variable-float? 'X)))
   (is (= false (variable-float? 'X%)))
   (is (= false (variable-float? 'X$)))
)

(deftest test-variable-integer?
   (is (= false (variable-integer? 'X)))
   (is (= true (variable-integer? 'X%)))
   (is (= false (variable-integer? 'X$)))
)

(deftest test-variable-string?
   (is (= false (variable-string? 'X)))
   (is (= false (variable-string? 'X%)))
   (is (= true (variable-string? 'X$)))
)

(deftest test-anular-invalidos 
   (is (= '(IF X nil * Y < 12 THEN LET nil X = 0) (anular-invalidos '(IF X & * Y < 12 THEN LET ! X = 0))))
)

(deftest test-cargar-linea
   (is (= '[((10 (PRINT X))) [:ejecucion-inmediata 0] [] [] [] 0 {}] (cargar-linea '(10 (PRINT X)) [() [:ejecucion-inmediata 0] [] [] [] 0 {}])))
   (is (= '[((10 (PRINT X)) (20 (X = 100))) [:ejecucion-inmediata 0] [] [] [] 0 {}] (cargar-linea '(20 (X = 100)) ['((10 (PRINT X))) [:ejecucion-inmediata 0] [] [] [] 0 {}])))
   (is (= '[((10 (PRINT X)) (15 (X = X + 1)) (20 (X = 100))) [:ejecucion-inmediata 0] [] [] [] 0 {}] (cargar-linea '(15 (X = X + 1)) ['((10 (PRINT X)) (20 (X = 100))) [:ejecucion-inmediata 0] [] [] [] 0 {}])))
   (is (= '[((10 (PRINT X)) (15 (X = X - 1)) (20 (X = 100))) [:ejecucion-inmediata 0] [] [] [] 0 {}] (cargar-linea '(15 (X = X - 1)) ['((10 (PRINT X)) (15 (X = X + 1)) (20 (X = 100))) [:ejecucion-inmediata 0] [] [] [] 0 {}])))
)

(deftest test-expandir-nexts
   (is (= '((PRINT 1) (NEXT A) (NEXT B)) (expandir-nexts (list '(PRINT 1) (list 'NEXT 'A (symbol ",") 'B))) ))
   (is (= '((PRINT 1)) (expandir-nexts (list '(PRINT 1)))) )
   (is (= '((NEXT A)) (expandir-nexts (list '(NEXT A))) ))
   (is (= '((NEXT A) (NEXT B)) (expandir-nexts (list (list 'NEXT 'A (symbol ",") 'B))) ))
)

(deftest test-dar-error 
   (is (= "?SYNTAX ERROR" (with-out-str (dar-error 16 [:ejecucion-inmediata 4]))))
   (is (= "?ERROR DISK FULL" (with-out-str (dar-error "?ERROR DISK FULL" [:ejecucion-inmediata 4]))))
   (is (= "?ERROR DISK FULL IN 100" (with-out-str (dar-error "?ERROR DISK FULL" [100 3]))))
   (is (= "?SYNTAX ERROR IN 100" (with-out-str (dar-error 16 [100 3]))))
)

;; (deftest test-contar-sentencias
;;    (is (= 2 (contar-sentencias 10 [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [10 1] [] [] [] 0 {}])))
;;    (is (= 1 (contar-sentencias 15 [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [10 1] [] [] [] 0 {}])))
;;    (is (= 2 (contar-sentencias 20 [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [10 1] [] [] [] 0 {}])))
;; )

; (deftest test-buscar-lineas-restantes

; )

; (deftest test-continuar-linea

; )

; (deftest test-extraer-data

; )

; (deftest test-ejecutar-asignacion 

; )

; (deftest test-preprocesar-expresion

; )

; (deftest test-desambiguar

; )

(deftest test-precedencia
   (is (= 1 (precedencia 'OR)))
   (is (= 2 (precedencia 'AND)))
   (is (= 6 (precedencia '*)))
   (is (= 7 (precedencia '-u)))
   (is (= 9 (precedencia 'MID$)))
)

(deftest test-aridad
   (is (= 0 (aridad 'THEN)))
   (is (= 1 (aridad 'SIN)))
   (is (= 2 (aridad '*)))
   (is (= 3 (aridad 'MID3$)))
)

(run-tests)
  
