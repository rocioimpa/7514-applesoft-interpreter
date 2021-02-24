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
   (is (= [(list '(10 (PRINT X))) [:ejecucion-inmediata 0] [] [] [] 0 {}] (cargar-linea '(10 (PRINT X)) [() [:ejecucion-inmediata 0] [] [] [] 0 {}])))
   (is (= [(list '(10 (PRINT X)) '(20 (X = 100))) [:ejecucion-inmediata 0] [] [] [] 0 {}] (cargar-linea '(20 (X = 100)) ['((10 (PRINT X))) [:ejecucion-inmediata 0] [] [] [] 0 {}])))
   (is (= [(list '(10 (PRINT X)) '(15 (X = X + 1)) '(20 (X = 100))) [:ejecucion-inmediata 0] [] [] [] 0 {}] (cargar-linea '(15 (X = X + 1)) ['((10 (PRINT X)) (20 (X = 100))) [:ejecucion-inmediata 0] [] [] [] 0 {}])))
   (is (= [(list '(10 (PRINT X)) '(15 (X = X - 1)) '(20 (X = 100))) [:ejecucion-inmediata 0] [] [] [] 0 {}] (cargar-linea '(15 (X = X - 1)) ['((10 (PRINT X)) (15 (X = X + 1)) (20 (X = 100))) [:ejecucion-inmediata 0] [] [] [] 0 {}])))
)

(deftest test-expandir-nexts
   (is (= (list '(PRINT 1) '(NEXT A) '(NEXT B)) (expandir-nexts (list '(PRINT 1) (list 'NEXT 'A (symbol ",") 'B))) ))
   (is (= (list '(PRINT 1)) (expandir-nexts (list '(PRINT 1)))) )
   (is (= (list '(NEXT A)) (expandir-nexts (list '(NEXT A))) ))
   (is (= (list '(NEXT A) '(NEXT B)) (expandir-nexts (list (list 'NEXT 'A (symbol ",") 'B))) ))
)

(deftest test-dar-error 
   (is (= "?SYNTAX ERROR" (with-out-str (dar-error 16 [:ejecucion-inmediata 4]))))
   (is (= "?ERROR DISK FULL" (with-out-str (dar-error "?ERROR DISK FULL" [:ejecucion-inmediata 4]))))
   (is (= "?ERROR DISK FULL IN 100" (with-out-str (dar-error "?ERROR DISK FULL" [100 3]))))
   (is (= "?SYNTAX ERROR IN 100" (with-out-str (dar-error 16 [100 3]))))
)

(deftest test-contar-sentencias
   (is (= 2 (contar-sentencias 10 [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [10 1] [] [] [] 0 {}])))
   (is (= 1 (contar-sentencias 15 [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [10 1] [] [] [] 0 {}])))
   (is (= 2 (contar-sentencias 20 [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [10 1] [] [] [] 0 {}])))
)

(deftest test-buscar-lineas-restantes
   (is (= nil (buscar-lineas-restantes [() [:ejecucion-inmediata 0] [] [] [] 0 {}])))
   (is (= nil (buscar-lineas-restantes ['((PRINT X) (PRINT Y)) [:ejecucion-inmediata 2] [] [] [] 0 {}])))
   (is (= (list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [10 2] [] [] [] 0 {}]))) 
   (is (= (list '(10 (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [10 1] [] [] [] 0 {}])))
   (is (= (list '(10) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [10 0] [] [] [] 0 {}])))
   (is (= (list '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [15 1] [] [] [] 0 {}])))
   (is (= (list '(15) (list 20 (list 'NEXT 'I (symbol ",") 'J))) (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [15 0] [] [] [] 0 {}])))
   (is (= (list '(20 (NEXT I) (NEXT J))) (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [20 3] [] [] [] 0 {}])))
   (is (= (list '(20 (NEXT I) (NEXT J))) (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [20 2] [] [] [] 0 {}])))
   (is (= (list '(20 (NEXT J))) (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [20 1] [] [] [] 0 {}])))
   (is (= (list '(20)) (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [20 0] [] [] [] 0 {}])))
   (is (= (list '(20)) (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [20 -1] [] [] [] 0 {}])))
   (is (= nil (buscar-lineas-restantes [(list '(10 (PRINT X) (PRINT Y)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [25 0] [] [] [] 0 {}])))
)

(deftest test-continuar-linea
   (def var-test1 [(list '(10 (PRINT X)) '(15 (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [20 3] [] [] [] 0 {}])
   (is (= "?RETURN WITHOUT GOSUB ERROR IN 20[nil [((10 (PRINT X)) (15 (X = X + 1)) (20 (NEXT I , J))) [20 3] [] [] [] 0 {}]]"
           (str (with-out-str (continuar-linea var-test1)) (continuar-linea var-test1))))
   (def var-test2 [(list '(10 (PRINT X)) '(15 (GOSUB 100) (X = X + 1)) (list 20 (list 'NEXT 'I (symbol ",") 'J))) [20 3] [[15 2]] [] [] 0 {}])
   (is (= "[:omitir-restante [((10 (PRINT X)) (15 (GOSUB 100) (X = X + 1)) (20 (NEXT I , J))) [15 1] [] [] [] 0 {}]]"
            (str (continuar-linea var-test2))))
)

(deftest test-extraer-data
   (is (= (list '()) (extraer-data '(()))))
   (is (= (list '("HOLA" "MUNDO" 10 20)) (extraer-data (list '(10 (PRINT X) (REM ESTE NO) (DATA 30)) '(20 (DATA HOLA)) (list 100 (list 'DATA 'MUNDO (symbol ",") 10 (symbol ",") 20))))))
)

(deftest test-ejecutar-asignacion 
   (is (= [(list '(10 (PRINT X))) [10 1] [] [] [] 0 '{X 5}] (ejecutar-asignacion '(X = 5) ['((10 (PRINT X))) [10 1] [] [] [] 0 {}])))
   (is (= [(list '(10 (PRINT X))) [10 1] [] [] [] 0 '{X 5}] (ejecutar-asignacion '(X = 5) ['((10 (PRINT X))) [10 1] [] [] [] 0 '{X 2}])))
   (is (= [(list '(10 (PRINT X))) [10 1] [] [] [] 0 '{X 3}] (ejecutar-asignacion '(X = X + 1) ['((10 (PRINT X))) [10 1] [] [] [] 0 '{X 2}])))
   (is (= [(list '(10 (PRINT X))) [10 1] [] [] [] 0 '{X$ "HOLA MUNDO"}] (ejecutar-asignacion '(X$ = X$ + " MUNDO") ['((10 (PRINT X))) [10 1] [] [] [] 0 '{X$ "HOLA"}])))
)

(deftest test-preprocesar-expresion
   (is (= (list "HOLA" (symbol "+") " MUNDO" (symbol "+") "") (preprocesar-expresion '(X$ + " MUNDO" + Z$) ['((10 (PRINT X))) [10 1] [] [] [] 0 '{X$ "HOLA"}])))
   (is (= (list 5 (symbol "+") 0 (symbol "/") 2 (symbol "*") 0) (preprocesar-expresion '(X + . / Y% * Z) ['((10 (PRINT X))) [10 1] [] [] [] 0 '{X 5 Y% 2}])))
)

(deftest test-desambiguar
   (is (= (list '(symbol "(") '-u 2 * (symbol "(") '-u 3 + 5 - (symbol "(") 2 / 7 (symbol ")") (symbol ")"))) (desambiguar (list '- 2 '* (symbol "(") '- 3 '+ 5 '- (symbol "(") '+ 2 '/ 7 (symbol ")") (symbol ")"))))
   (is (= (list '(symbol "(") 'MID$ (symbol "(") 1 (symbol ",") 2 (symbol ")")(symbol ")"))) (desambiguar (list 'MID$ (symbol "(") 1 (symbol ",") 2 (symbol ")"))))
   (is (= (list 'MID3$ (symbol "(") 1 (symbol ",") 2 (symbol ",") 3 (symbol ")")) (desambiguar (list 'MID$ (symbol "(") 1 (symbol ",") 2 (symbol ",") 3 (symbol ")")))))
   (is (= (list 'MID3$ (symbol "(") 1 (symbol ",") '-u 2 (symbol "+") 'K (symbol ",") 3 (symbol ")")) (desambiguar (list 'MID$ (symbol "(") 1 (symbol ",") '- 2 '+ 'K (symbol ",") 3 (symbol ")")))))
)

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
  
