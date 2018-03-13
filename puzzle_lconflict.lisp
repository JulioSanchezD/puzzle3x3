; Este programa resuelve rompecabezas de 3x3 utilizando
; el algoritmo A*.
; La función heurística es la distancia manhattan mas el conflicto lineal.
; Si dos rompecabezas tienen el mismo costo tendrá prioridad
; el rompecabezas de menor nivel.

; La estructura del rompecabezas es:
; ((a b c) (d e f) (g h i))
; donde a, b, c son los números del renglón superior
; d, e, f son los números del renglón de en medio
; g, h, i son los números del renglón inferior

; La estructura de un nodo es:
; (id id-p niv costo op rom)
; donde id es el identificador del nodo
; id-p es el identificador del nodo de donde proviene
; niv es el nivel en el que fue expandido
; costo es el costo calculado con la función heurística
; op es el número de operador utilizado
;     1 derecha 2 izquierda 3 arriba 4 abajo
; rom es el rompecabezas

;;; main: función principal
;;; parámetros: ei y em
;;; donde ei y em son rompecabezas correspondiendo al
;;; estado inicial y el estado meta.
;;; variables: fin, ini, obs, no-obs
;;; fin es una variable que guarda a em
;;; ini es el nodo inicial construido con ei
;;; donde obs es una lista que contiene a los nodos observados
;;; no-obs contiene a los nodos no observados
(defun main (ei em)
      (setq fin em e-aux ei ini (append (list 1) (list 'nil) (list 1)
            (list (costo e-aux fin)) (list nil) (list ei))
            obs nil no-obs (list ini))
      (if (essolucionable e-aux fin) (b-sol) nil))

;;; costo: calcula costo
;;; Parámetros: ea y em
;;; donde ea es el estado actual
;;; y em es el estado meta
(defun costo (ea em)
      (setq aux nil r 0 c 0)
      (compara ea em))

(defun sumaL (lst) (setq sum 0) (suma lst))

(defun suma (lst) (setq obj (car lst))
        (cond   ((null lst) sum)
                                ((atom obj)
                                        (cond ((numberp obj) (incf sum obj) (suma (cdr lst)))
                                                (t (suma (cdr lst)))))
                                (t (suma obj) (suma (cdr lst)))))

(defun compara (ea em) (setq x (car ea))
                (cond   ((null ea) (sumaL aux))
                                        ((listp x) (incf r) (setq c 0) (compara x em) (compara (cdr ea) em))
                                        (t (incf c) (encuentra x em) (push (+ (abs (- r2 r)) (abs (- c2 c))) aux) (compara (cdr ea) em))))

(defun compara (ea em) (setq x (car ea))
                (cond   ((null ea) (sumaL aux))
                                        ((listp x) (incf r) (setq c 0) (compara x em) (compara (cdr ea) em))
                                        (t (incf c) (encuentra x em) (push (+ (abs (- r2 r)) (abs (- c2 c))) aux) (compara (cdr ea) em))))

(defun encuentra (obj em) (setq r2 0 c2 0 flag nil) (encuentra2 obj em))

(defun encuentra2 (obj em)
                (cond   ((null em))
                                        ((listp (car em)) (incf r2) (setq c2 0) (encuentra2 obj (car em)) (if (equal flag nil) (encuentra2 obj (cdr em))))
                                        (t (incf c2) (if (equal (car em) obj) (setq flag T) (encuentra2 obj (cdr em))))))

(defun linearconflict (lst listafinal)
(setq list (sixth lst) costo (fourth lst) aux1 (first list) aux2 (first listafinal)  a1 (first aux1) a2 (second aux1) a3 (third aux1) b1 (first aux2) b2 (second aux2) b3 (third aux2) )
(cond ((and (= a1 b2) (= a2 b1) (or (= (+ 1 a1) a2) (= (+ 1 a2) a1) ))(setf costo (+ 2 costo)))(t costo))
(cond ((and (= a1 b3) (= a2 b2) (or (= (+ 1 a1) a2) (= (+ 1 a2) a1) ))(setf costo (+ 2 costo)))(t costo))
(cond ((and (= a2 b3) (= a3 b2) (or (= (+ 1 a3) a2) (= (+ 1 a2) a3) ))(setf costo (+ 2 costo)))(t costo))
(cond ((and (= a2 b2) (= a3 b1) (or (= (+ 1 a3) a2) (= (+ 1 a2) a3) ))(setf costo (+ 2 costo)))(t costo))
(setq aux1 (second list) aux2 (second listafinal) a1 (first aux1) a2 (second aux1) a3 (third aux1) b1 (first aux2) b2 (second aux2) b3 (third aux2) )
(cond ((and (= a1 b2) (= a2 b1) (or (= (+ 1 a1) a2) (= (+ 1 a2) a1) ))(setf costo (+ 2 costo)))(t costo))
(cond ((and (= a1 b3) (= a2 b2) (or (= (+ 1 a1) a2) (= (+ 1 a2) a1) ))(setf costo (+ 2 costo)))(t costo))
(cond ((and (= a2 b3) (= a3 b2) (or (= (+ 1 a3) a2) (= (+ 1 a2) a3) ))(setf costo (+ 2 costo)))(t costo))
(cond ((and (= a2 b2) (= a3 b1) (or (= (+ 1 a3) a2) (= (+ 1 a2) a3) ))(setf costo (+ 2 costo)))(t costo))
(setq aux1 (third list) aux2 (third listafinal) a1 (first aux1) a2 (second aux1) a3 (third aux1) b1 (first aux2) b2 (second aux2) b3 (third aux2) )
(cond ((and (= a1 b2) (= a2 b1) (or (= (+ 1 a1) a2) (= (+ 1 a2) a1) ))(setf costo (+ 2 costo)))(t costo))
(cond ((and (= a1 b3) (= a2 b2) (or (= (+ 1 a1) a2) (= (+ 1 a2) a1) ))(setf costo (+ 2 costo)))(t costo))
(cond ((and (= a2 b3) (= a3 b2) (or (= (+ 1 a3) a2) (= (+ 1 a2) a3) ))(setf costo (+ 2 costo)))(t costo))
(cond ((and (= a2 b2) (= a3 b1) (or (= (+ 1 a3) a2) (= (+ 1 a2) a3) ))(setf costo (+ 2 costo)))(t costo))
(setq a1 (first (first list)) a2 (first (second list)) a3 (first (third list)) b1 (first (first listafinal)) b2 (first (second listafinal)) b3 (first (third listafinal)))
(cond ((and (= a1 b2) (= a2 b1) (or (= (+ 1 a1) a2) (= (+ 1 a2) a1) ))(setf costo (+ 2 costo)))(t costo))
(cond ((and (= a1 b3) (= a2 b2) (or (= (+ 1 a1) a2) (= (+ 1 a2) a1) ))(setf costo (+ 2 costo)))(t costo))
(cond ((and (= a2 b3) (= a3 b2) (or (= (+ 1 a3) a2) (= (+ 1 a2) a3) ))(setf costo (+ 2 costo)))(t costo))
(cond ((and (= a2 b2) (= a3 b1) (or (= (+ 1 a3) a2) (= (+ 1 a2) a3) ))(setf costo (+ 2 costo)))(t costo))
(setq a1 (second (first list)) a2 (second (second list)) a3 (second (third list)) b1 (second (first listafinal)) b2 (second (second listafinal)) b3 (second (third listafinal)))
(cond ((and (= a1 b2) (= a2 b1) (or (= (+ 1 a1) a2) (= (+ 1 a2) a1) ))(setf costo (+ 2 costo)))(t costo))
(cond ((and (= a1 b3) (= a2 b2) (or (= (+ 1 a1) a2) (= (+ 1 a2) a1) ))(setf costo (+ 2 costo)))(t costo))
(cond ((and (= a2 b3) (= a3 b2) (or (= (+ 1 a3) a2) (= (+ 1 a2) a3) ))(setf costo (+ 2 costo)))(t costo))
(cond ((and (= a2 b2) (= a3 b1) (or (= (+ 1 a3) a2) (= (+ 1 a2) a3) ))(setf costo (+ 2 costo)))(t costo))
(setq a1 (third (first list)) a2 (third (second list)) a3 (third (third list)) b1 (third (first listafinal)) b2 (third (second listafinal)) b3 (third (third listafinal)))
(cond ((and (= a1 b2) (= a2 b1) (or (= (+ 1 a1) a2) (= (+ 1 a2) a1) ))(setf costo (+ 2 costo)))(t costo))
(cond ((and (= a1 b3) (= a2 b2) (or (= (+ 1 a1) a2) (= (+ 1 a2) a1) ))(setf costo (+ 2 costo)))(t costo))
(cond ((and (= a2 b3) (= a3 b2) (or (= (+ 1 a3) a2) (= (+ 1 a2) a3) ))(setf costo (+ 2 costo)))(t costo))
(cond ((and (= a2 b2) (= a3 b1) (or (= (+ 1 a3) a2) (= (+ 1 a2) a3) ))(setf costo (+ 2 costo)))(t costo))
(setf (fourth lst) (+ costo (fourth lst))))

;;; essolucionable: existe solución
;;; metodo que verifica si existe solución
(defun essolucionable (lstInicial lstFinal)(setf list nil a 0)(aplana lstInicial)(setf p1 (solucionable list) )(setf list nil a 0)(aplana lstFinal)(setf p2 (solucionable list))(cond((= p1 p2) t)(t nil)))

(defun aplana (lst)(setq list (append (first lst)(second lst)(third lst))))

(defun soluciona (b lst a)(cond ((null (cdr lst))(if (and (not (= (car lst) 0))(> b (car lst)))(incf a)a))(t  (cond ((and (not (= (car lst) 0))(> b (car lst)))(incf a 1)(soluciona b (cdr lst) a))(t (soluciona b (cdr lst) a))))))

(defun solucionable (lst )(cond ((null (cdr lst))(cond ((= 0 (mod a 2)) 2)(t 3)))(t (setf a (soluciona (car lst) (cdr lst) a))(solucionable (cdr lst)))))

;;; b-sol: busca solución
;;; Regresa lista de operadores
(defun b-sol ()
      (loop while (not (res)) do
            (setq n-aux (pop no-obs))
            (push n-aux obs)
            (expan n-aux))
      (sol (car no-obs)))

;;; res: ¿Está resuelto?
(defun res ()
      (= (fourth (car no-obs)) 0))

;;; exp: expandir nodo
;;; parámetros: n
;;; donde n es el nodo a expandir
(defun expan (n)
      (loop for i from 1 to 4 do
            (setq r-aux (mov i n) new-n nil)
            (when (and (not (prev r-aux)) r-aux)
                  (setq new-n
                  (append (list (+ (length obs) (length no-obs) 1))
                  (list (car n)) (list (+ (third n) 1))
                  (list (costo r-aux fin)) (list i) (list r-aux))))
            (when new-n (linearconflict new-n fin))
            (when new-n (ins new-n))))

;;; mov: realiza movimiento.
;;; Parámetros: i n
;;; donde i es el número de movimiento a realizar
;;; n el nodo al que se le aplica el movimiento.
;;; Regresa un rompecabezas.
(defun mov (i n)
      (setq LISTA (copy-tree (car (last n))))
      (if (COND ((= i 1)(MOVERDERECHA LISTA))
      ((= i 2)(MOVERIZQUIERDA LISTA))
      ((= i 3)(MOVERARRIBA LISTA))
      (T (MOVERABAJO LISTA)))
      LISTA nil))

(DEFUN MOVER (LISTA N)
   (if (COND ((= N 1)(MOVERDERECHA LISTA))
   ((= N 2)(MOVERIZQUIERDA LISTA))
   ((= N 3)(MOVERARRIBA LISTA))
   (T (MOVERABAJO LISTA)))
   LISTA nil))

(DEFUN MOVERDERECHA (LISTA)
   (COND ((OR (= 0 (THIRD(FIRST LISTA))) (= 0 (THIRD(SECOND LISTA))) (= 0 (THIRD(THIRD LISTA)))) (return-from moverderecha nil))
   ((MEMBER 0 (FIRST LISTA))(SETQ AUX (FIRST LISTA) A 1))
   ((MEMBER 0 (SECOND LISTA))(SETQ AUX (SECOND LISTA) A 2))
   (T (SETQ AUX (THIRD LISTA) A 3)))
   (IF (= 0 (FIRST AUX)) (SETF (FIRST AUX) (SECOND AUX) (SECOND AUX) 0) (SETF (SECOND AUX) (THIRD AUX) (THIRD AUX) 0))
   (COND ((= A 1) (SETF (FIRST LISTA) AUX))
   (( = A 2) (SETF (SECOND LISTA) AUX))
   (T (SETF(THIRD LISTA) AUX))))

(DEFUN MOVERIZQUIERDA (LISTA)
   (COND ((OR (= 0 (FIRST(FIRST LISTA))) (= 0 (FIRST(SECOND LISTA))) (= 0 (FIRST(THIRD LISTA)))) (return-from moverizquierda nil))
   ((MEMBER 0 (FIRST LISTA))(SETQ AUX (FIRST LISTA) A 1))
   ((MEMBER 0 (SECOND LISTA))(SETQ AUX (SECOND LISTA) A 2))
   (T (SETQ AUX (THIRD LISTA) A 3)))
   (IF (= 0 (THIRD AUX))(SETF (THIRD AUX) (SECOND AUX) (SECOND AUX) 0) (SETF (SECOND AUX) (FIRST AUX) (FIRST AUX) 0))
   (COND ((= A 1) (SETF (FIRST LISTA) AUX))
   (( = A 2)(SETF (SECOND LISTA) AUX))
   (T (SETF(THIRD LISTA) AUX))))

(DEFUN MOVERARRIBA (LISTA)
   (COND ((MEMBER 0 (FIRST LISTA)) (return-from moverarriba nil))
   ((MEMBER 0 (SECOND LISTA))(SETQ AUX2 (SECOND LISTA) AUX1 (FIRST LISTA) A 1))
   (T (SETQ AUX2 (THIRD LISTA) AUX1 (SECOND LISTA) A 2)))
   (COND ((= 0 (FIRST AUX2))(SETF (FIRST AUX2) (FIRST AUX1) (FIRST AUX1) 0))
   ((= 0 (SECOND AUX2))(SETF  (SECOND AUX2) (SECOND AUX1) (SECOND AUX1) 0))
   (T (SETF (THIRD AUX2) (THIRD AUX1) (THIRD AUX1) 0)))
   (COND ((= A 1)(SETF (FIRST LISTA) AUX1 (SECOND LISTA) AUX2))
   (T (SETF (SECOND LISTA) AUX1 (THIRD LISTA) AUX2))))

(DEFUN MOVERABAJO (LISTA)
   (COND ((MEMBER 0 (THIRD LISTA)) (return-from moverabajo nil))
   ((MEMBER 0 (SECOND LISTA))(SETQ AUX1 (SECOND LISTA) AUX2 (THIRD LISTA) A 1))
   (T (SETQ AUX1 (FIRST LISTA) AUX2 (SECOND LISTA) A 2)))
   (COND ((= 0 (FIRST AUX1))(SETF (FIRST AUX1) (FIRST AUX2) (FIRST AUX2) 0))
   ((= 0 (SECOND AUX1))(SETF  (SECOND AUX1) (SECOND AUX2) (SECOND AUX2) 0))
   (T (SETF (THIRD AUX1) (THIRD AUX2) (THIRD AUX2) 0)))
   (COND ((= A 1)(SETF (SECOND LISTA) AUX1 (THIRD LISTA) AUX2))
   (T (SETF (FIRST LISTA) AUX1 (SECOND LISTA) AUX2))))

;;; prev: verifica si un rompecabezas ya había sido creado (es previo).
;;; Parámetros: r
;;; donde r es un rompecabezas.
(defun prev (r)
      (loop for x in obs do
            (when (equal r (car (last x))) (return-from prev t)))
      (loop for y in no-obs do
            (when (equal r (car (last y))) (return-from prev t))))

;;; ins: inserta en no-obs ordenado por costo.
;;; Si dos nodos tienen el mismo costo tiene prioridad
;;; el nodo de menor nivel.
;;; Parámetros: n
;;; donde n es un nodo a insertar
(defun ins (n)
      (cond ((null no-obs) (setq no-obs (list n)))
      (t (setq i nil lst nil)
      (loop for x in no-obs do
            (cond ((not i) (cond ((= (fourth x) (fourth n))
                              (if (< (third n) (third x)) (and (setq lst (cons n lst))
                              (setq lst (cons x lst)) (setq i t))
                                    (setq lst (cons x lst))))
                              ((< (fourth x) (fourth n)) (setq lst (cons x lst)))
                              (t (setq lst (cons n lst)) (setq lst (cons x lst)) (setq i t))))
            (t (setq lst (cons x lst)))))
      (when (not i) (setq lst (cons n lst)))
      (setq no-obs (reverse lst)))))

;;; sol: recupera la solución
;;; Parámetros: n
;;; donde n es el nodo que alcanzó la solución
(defun sol (n)
      (unless (null (fifth n)) (append (sol (trae (second n) obs)) (list (fifth n)))))

;;; trae: trae un nodo de ina lista
;;; Parámetros: id lst
;;; donde id es el identificador de un nodo
;;; lst la lista donde buscará este nodo.
(defun trae (id lst)
      (loop for x in lst do
            (when (= id (car x)) (return-from trae x))))
