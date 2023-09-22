;; Definire una struttura dati per rappresentare le quantit� fisiche
(defstruct quantity
	value      ; Valore numerico della quantit�
	dimension  ; Dimensione della quantit� (es. '(* m s -1))
)

;; Definire una struttura dati per rappresentare le unit� SI
(defstruct si-unit
  name       ; Nome dell'unit� (es. "metro")
  symbol     ; Simbolo dell'unit� (es. 'm)
  base       ; Indica se l'unit� � una base unit (T o NIL)
  expansion  ; Espansione in termini di unit� base (es. '(* m))
)

(defstruct si-prefix
  name     ; Nome del prefisso (es. "kilo")
  symbol   ; Simbolo del prefisso (es. 'k)
  factor   ; Fattore di scala (es. 10^3 per "kilo")
)

(defvar *si-units*
  (list

   ;; Unit� di base
   (make-si-unit :name 'meter
                 :symbol 'm
                 :base t
                 :expansion '(* m))

   (make-si-unit :name 'kilogram
                 :symbol 'kg
                 :base t
                 :expansion '(* kg))

   (make-si-unit :name 'second
                 :symbol 's
                 :base t
                 :expansion '(* s))

   (make-si-unit :name 'ampere
                 :symbol 'A
                 :base t
                 :expansion '(* A))

   (make-si-unit :name 'kelvin
                 :symbol 'K
                 :base t
                 :expansion '(* K))

   (make-si-unit :name 'mole
                 :symbol 'mol
                 :base t
                 :expansion '(* mol))

   (make-si-unit :name 'candela
                 :symbol 'cd
                 :base t
                 :expansion '(* cd))

   ;; Unit� derivate
   (make-si-unit :name 'becquerel 
                 :symbol 'Bq 
                 :base nil 
                 :expansion '(** s -1))
   (make-si-unit :name 'degreecelsius 
                 :symbol 'DC 
                 :base nil 
                 :expansion 'K)
   (make-si-unit :name 'coulomb 
                 :symbol 'C 
                 :base nil 
                 :expansion '(* A s))
   (make-si-unit :name 'farad 
                 :symbol 'F 
                 :base nil 
                 :expansion '(* (** A 2) (** kg -1) (** m -2) (** s 4)))       
   (make-si-unit :name 'gray 
                 :symbol 'Gy 
                 :base nil 
                 :expansion '(* (** m 2) (** s -2)))
   (make-si-unit :name 'hertz 
                 :symbol 'Hz 
                 :base nil 
                 :expansion '(** s -1))
   (make-si-unit :name 'henry 
                 :symbol 'H 
                 :base nil 
                 :expansion '(* (** A -2) (** kg 1) (** m 2) (** s -2)))
   (make-si-unit :name 'joule 
                 :symbol 'J 
                 :base nil 
                 :expansion '(* kg (** m 2) (** s -2)))
   (make-si-unit :name 'katal 
                 :symbol 'kat 
                 :base nil 
                 :expansion '(* mol (** s -1)))
   (make-si-unit :name 'lumen 
                 :symbol 'lm 
                 :base nil 
                 :expansion '(* cd (** m 2) (** m -2)))
   (make-si-unit :name 'lux 
                 :symbol 'lx 
                 :base nil 
                 :expansion '(* cd (** m 2) (** m -4)))
   (make-si-unit :name 'newton 
                 :symbol 'N 
                 :base nil 
                 :expansion '(* kg m (** s -2)))
   (make-si-unit :name 'ohm 
                 :symbol 'Omega 
                 :base nil 
                 :expansion '(* (** A -2) kg (** m 2) (** s -3)))
   (make-si-unit :name 'pascal 
                 :symbol 'Pa 
                 :base nil 
                 :expansion '(* kg (** m -1) (** s -2)))
   (make-si-unit :name 'radian 
                 :symbol 'rad 
                 :base nil 
                 :expansion '(* m (** m -1)))
   (make-si-unit :name 'siemens 
                 :symbol 'S 
                 :base nil 
                 :expansion '(* (** A 2) (** kg -1) (** m -2) (** s 3)))
   (make-si-unit :name 'sievert 
                 :symbol 'Sv 
                 :base nil 
                 :expansion '(* (** m 2) (** s -2)))
   (make-si-unit :name 'steradian 
                 :symbol 'sr 
                 :base nil 
                 :expansion '(* (** m 2) (** m -2)))
   (make-si-unit :name 'tesla 
                 :symbol 'T 
                 :base nil 
                 :expansion '(* (** A -1) (** kg 1) (** s -2)))
   (make-si-unit :name 'volt 
                 :symbol 'V 
                 :base nil 
                 :expansion '(* (** A -1) kg (** m 2) (** s -3)))
   (make-si-unit :name 'watt 
                 :symbol 'W 
                 :base nil 
                 :expansion '(* kg (** m 2) (** s -3)))
   (make-si-unit :name 'weber 
                 :symbol 'Wb 
                 :base nil 
                 :expansion '(* (** A -1) (** kg 1) (** m 2) (** s -2)))
   ))

(defvar *si-prefixes*
  (list
    (make-si-prefix :name "kilo" :symbol 'k :factor 3)
    (make-si-prefix :name "hecto" :symbol 'h :factor 2)
    (make-si-prefix :name "deca" :symbol 'da :factor 1)
    (make-si-prefix :name "deci" :symbol 'd :factor -1)
    (make-si-prefix :name "centi" :symbol 'c :factor -2)
    (make-si-prefix :name "milli" :symbol 'm :factor -3)
  ))

;;Funzioni da implementare
(defun is-siu (s)
  (or
	(find-unit-by-symbol s)
	(has-prefix s)))

(defun is-base-siu (s)
  "Verifica se il simbolo S rappresenta un'unit� SI di base."
  (let ((unit (find-unit-by-symbol s)))
    (if unit
        (si-unit-base unit) ; Controlla direttamente se � una base unit
        nil)))

(defun siu-name (s)
  "Restituisce il nome di SI con il simbolo S o NIL se non � presente."
  (let ((unit (find-unit-by-symbol s)))
    (if unit
        (si-unit-name unit)
        nil)))

(defun siu-symbol (n)
  "Restituisce il simbolo di SI con il nome N o NIL se non � presente."
  (let ((unit (find-unit-by-name n)))
    (if unit
        (si-unit-symbol unit)
        nil)))

(defun siu-base-expansion (s)
  "Restituisce l'espansione in forma canonica dell'unit� S con solo base si."
  (let ((unit (find-unit-by-symbol s)))
    (if unit
        (si-unit-expansion unit)
        nil)))

(defun is-dimension (d)
    (cond
		((null d) t)
		((atom d) (is-siu d))
		((equal (car d) '*) (if (null (cdr d))
									nil
									(is-dimension-times (cdr d))))
		((equal (car d) '**) (is-dimension-power d))
		(t nil)))

(defun is-quantity (q)
  "Verifica se Q � una quantit� valida."
  (and (numberp (first q)) (is-dimension (second q))))

(defun norm (lista)
    (if (and (atom lista) (is-siu lista))
        lista
        (let ((simply-list (simplify (flatten-list lista))))
             (cond
                 ((equal (car simply-list) '*) 
                    (append (list '*) 
                            (sort (cdr simply-list) #'compare-element)))
                 ((equal (car simply-list) '**) 
                    (append (list '**) 
                            (sort (cdr simply-list) #'compare-element)))))))   

(defun compare-units (u1 u2)
    (cond 
        ((and (or (null u1) (null u2)) 
              (not (or (or (is-siu u1) (has-prefix u1))
                       (or (is-siu u2) (has-prefix u2)))))
            (error "inserire 2 unita\'"))
        ((equal u1 u2) '=)
        ((equal u1 'kg) '>)
        ((equal u2 'kg) '<)
        ((equal u1 'm) '>)
        ((equal u2 'm) '<)
        ((equal u1 's) '>)
        ((equal u2 's) '<)
        ((equal u1 'A) '>)
        ((equal u2 'A) '<)
        ((equal u1 'K) '>)
        ((equal u2 'K) '<)
        ((equal u1 'cd) '>)
        ((equal u2 'cd) '<)
        ((equal u1 'mol) '>)
        ((equal u2 'mol) '<)
        ((not (string> u1 u2)) '<)
        (t '>)))

(defun q (n d)
  "Costruisce una quantità in forma (Q N D'), dove D' è D in forma canonica."  
  (if (and (numberp n) (is-dimension d))
      (if (atom d)
          (if (has-prefix d)
              (list 'Q
                    (* n (expt 10 (norm-ten-exp d)))
                    (norm (expandall (norm-exp d))))
              (list 'Q
                    n
                    (norm (expandall d))))
          (let ((norm-d (norm d)))
               (list 'Q
                     (* n (expt 10 (norm-ten-exp (cdr norm-d))))
                     (norm (expandall (norm-exp norm-d))))))
      (error "D deve essere una quantità valida")))

(defun qsum (q1 q2)
    (if (equal (third q1) (third q2))
        (q (+ (second q1) (second q2)) (third q1))
        (error "Dimensioni non compatibili")))

(defun qsub (q1 q2)
    (if (equal (third q1) (third q2))
        (q (- (second q1) (second q2)) (third q1))
        (error "Dimensioni non compatibili")))

(defun qtimes (q1 q2)
    (q (* (second q1) (second q2)) 
       (norm (append (third q1) (third q2)))))

(defun qdiv (q1 q2)
	(if (zerop (second q2))
		(error "Non puoi dividere per 0")
		(q (/ (second q1) (second q2)) 
		   (norm (append (third q1) (invert (third q2)))))))

(defun qexpt (q1 n)
    (cond
        ((zerop n) (if (zerop (second q1))
                         (error "Non puoi elevare 0 alla 0")
                         1))
        ((equal n 1) q1)
        (t (q (expt (second q1) n) (dimension-exp (third q1) n)))))
                            

;;Funzioni di ausilio
(defun find-unit-by-symbol (symbol)
  "Trova un'unit� SI dato il simbolo in unit-list."
  (find-if (lambda (unit) (eq (si-unit-symbol unit) symbol)) *si-units*))

(defun find-unit-by-name (name)
  "Trova un'unit� SI dato il nome in unit-list."
  (find-if (lambda (unit) (string= (si-unit-name unit) name)) *si-units*))
                         
(defun is-dimension-times (lista)
	(cond
		((null lista) t)
		((atom (car lista)) (is-dimension-times (cdr lista)))
		((listp (car lista)) (cond
								((equal (car (car lista)) '*)
									(and 
										(is-dimension-times (cdr (car lista))) 
										(is-dimension-times (cdr lista))))
								((equal (car (car lista)) '**)
									(and 
										(is-dimension-power (car lista)) 
										(is-dimension-times (cdr lista))))))
		(t nil)))
		
(defun is-dimension-power (lista)
	(if (equal (length lista) 3)
		(cond
			((and (atom (second lista)) (numberp (third lista)))
				(is-siu (second lista)))
			((and (listp (second lista)) (numberp (third lista)))
				(if (equal (car (second lista)) '**)
					(is-dimension-power (second lista))
					nil))
			(t nil))
		nil))
			
                            
(defun invert (dim)
    (cond
        ((equal (first dim) '*)
             (dolist (element (cdr dim))
                          (cond 
                              ((atom element) 
                                (replace-element-in-list (cdr dim) 
                                                         element 
                                                         '(** element -1)))
                              ((listp element) (invert element)))))
        ((equal (first dim) '**)
                      (cond
                          ((atom (second dim)) 
                            (norm-exp-single (cdr dim) 
                                             (second dim) 
                                             (- 0 (third dim))))))))
                     
(defun simplify (lista)
    (cond
        ((null lista) nil)
        ((equal (car lista) '*) (append '(*) (simplify (cdr lista))))
        ((and (atom (car lista)) (is-siu (car lista))) 
            (if (contains-key (cdr lista) (car lista))
                (simplify (update-key (cdr lista) (car lista) 1))
                (append (list (car lista)) (simplify (cdr lista)))))
        ((list (car lista)) 
            (if (contains-key (cdr lista) (second (car lista)))
                (simplify (update-key (cdr lista) 
                                      (second (car lista))
                                      (third (car lista))))
                (append (list (car lista)) (simplify (cdr lista)))))))
                                  

(defun flatten-list (lista)
  "Livella una lista in un'unica lista secondo le regole specificate."
  (cond
    ((null lista) '()) ; Se la lista � vuota, restituisci una lista vuota.
    ;((atom lista) (list lista))
    ((equal (car lista) '*) (append (list (car lista)) (flatten-list (cdr lista)))) ; Se l'elemento � un atomo, inseriscilo in una lista.
    ((and (atom (car lista)) (is-siu (car lista))) (append (list (car lista)) (flatten-list (cdr lista))))
    ((listp (car lista))
         (cond
             ((equal (car (car lista)) '*) (append (flatten-list (cdr (car lista))) (flatten-list (cdr lista))))
             ((equal (car (car lista)) '**) (if (atom (second (car lista)))
                                                (append (list (car lista)) (flatten-list (cdr lista)))
                                                (append (list (list '** (first (flatten-list-exp (second (car lista)))) (* (third (car lista)) (second (flatten-list-exp (second (car lista)))))))
                                                        (flatten-list (cdr lista)))
                                                        ))))))

(defun flatten-list-exp (lista)
    (if (atom (second lista))
        (list (second lista) (third lista))
        (list (first (flatten-list-exp lista)) (* (third lista) (third (flatten-list-exp lista))))))
         
(defun contains-key (lista key)
    (dolist (element lista)
        (cond
            ((atom element) (if (equal element key)
                                (return t) nil))
            ((list element) (if (contains-key element key)
                                (return t) nil))
            )))

(defun update-key (lista key value)
    (cond
        ((equal (car lista) key) (if (equal (+ 1 value) 0)
                                     (cdr lista)
                                     (append (list (list '** key (+ 1 value))) (cdr lista))))
        ((and (listp (car lista)) (contains-key (car lista) key)) (if (equal (+ (third (car lista)) value) 0)
                                                                      (cdr lista)
                                                                      (append (list (list '** key (+ (third (car lista)) value))) (cdr lista))))
        (t (append (list (car lista)) (update-key (cdr lista) key value))))) 
		
(defun has-prefix (s)
    (if (is-kg s)
        (has-prefix-kg s)
        (has-prefix-help (mapcar #'si-prefix-symbol *si-prefixes*) s)))

(defun get-prefix (s)
     (if (is-kg s)
         (get-prefix-kg s)
         (get-prefix-help (mapcar #'si-prefix-symbol *si-prefixes*) s)))

(defun get-prefix-factor (symbol)
  "Restituisce il factor corrispondente a un simbolo di prefisso."
  (let ((prefix-entry (find symbol *si-prefixes* :key #'si-prefix-symbol)))
    (if (not (null prefix-entry))
        (si-prefix-factor prefix-entry)
        0)))
		
(defun has-prefix-help (prefixies s)
    (cond
        ((null prefixies) nil)
        ((find-substring-in-atom (symbol-name (car prefixies)) (symbol-name s)) 
            t)
        (t (has-prefix-help (cdr prefixies) s))
        ))
		
(defun get-prefix-help (prefixies s)
    (cond
        ((null prefixies) nil)
        ((find-substring-in-atom (symbol-name (car prefixies)) (symbol-name s)) (intern (symbol-name (car prefixies))))
        (t (get-prefix-help (cdr prefixies) s))
        ))

(defun get-symbol-prefix (prefixies s)
    (dolist (prefix prefixies)
        (cond
            ((and (equal (symbol-name prefix) (subseq (symbol-name s) 0 1)) 
                  (is-siu (intern(subseq (symbol-name s) 1)))) 
                (return (intern(subseq (symbol-name s) 1))))
            ((and (equal (symbol-name prefix) (subseq (symbol-name s) 0 2)) (is-siu (intern(subseq (symbol-name s) 2)))) (return (intern(subseq (symbol-name s) 2)))))))
            

(defun find-substring-in-atom (substring string)
  "Cerca la sottostringa nel nome dell'atomo."
  (cond
      ((equal (length string) 1) nil)
      ((and (equal substring (subseq string 0 1)) (is-siu (intern(subseq string 1)))) t)
      ((and (>= (length string) 3) (equal substring (subseq string 0 2)) (is-siu (intern(subseq string 2)))) t)
      (t nil)))
	  
(defun sort-norm (lista)
	(cond
		((null lista) nil)
		((equal (car lista) '*) (if (equal (length lista) 2)
									lista
									(sort-help lista 1)))
		((equal (car lista) '**) (if (equal (length lista) 3)
									lista))))
									
(defun sort-help (lista n)
	(cond
		((>= n (length lista)) nil)))
		

(defun get-symbol (element)
	(cond
		((null element) nil)
		((atom element) element)
		(t (second element))))

(defun norm-ten-exp (lista)
    (cond
        ((null lista) 0)
        ((atom lista) (if (has-prefix lista)
                          (get-prefix-factor (get-prefix lista))
                          0))
        ((has-prefix (get-symbol (car lista))) (if (atom (car lista))
                                                   (+ (get-prefix-factor (get-prefix (get-symbol (car lista)))) (norm-ten-exp (cdr lista)))
                                                   (+ (* (get-prefix-factor (get-prefix (get-symbol (car lista)))) (third (car lista))) (norm-ten-exp (cdr lista)))))
        (t (norm-ten-exp (cdr lista)))))

(defun norm-exp (lista)
    (norm (norm-exp-help lista)))

(defun norm-exp-help (lista)
    (cond
        ((null lista) nil)
        ((atom lista) (if (has-prefix (get-symbol lista))
                          (get-symbol-prefix (mapcar #'si-prefix-symbol *si-prefixes*) lista)
                          lista))
        ((equal (car lista) '*) (append (list (car lista)) (norm-exp-help (cdr lista))))
        ((atom (car lista)) (if (has-prefix (get-symbol (car lista)))
                              (append (list (get-symbol-prefix (mapcar #'si-prefix-symbol *si-prefixes*) (car lista))) (norm-exp-help (cdr lista)))
                              (append (list (car lista)) (norm-exp-help (cdr lista)))))
        ((listp (car lista)) (if (has-prefix (get-symbol (car lista))) 
                                 (append (list (list '** (get-symbol-prefix (mapcar #'si-prefix-symbol *si-prefixes*) (get-symbol (car lista))) (third (car lista)))) (norm-exp-help (cdr lista)))
                                 (append (list (car lista)) (norm-exp-help (cdr lista)))))
        ((has-prefix (car lista)) (append (list (get-symbol-prefix (mapcar #'si-prefix-symbol *si-prefixes*) (get-symbol (car lista)))) (norm-exp-help (cdr lista))))
        (t (append (list (car lista)) (norm-exp-help (cdr lista))))))
		
(defun expandall (d)
	(cond
		((null d) nil)
		((atom d) (if (not (is-base-siu d))
						(siu-base-expansion d)
						d))
		((equal (car d) '*) (append (list (car d)) (expandall (cdr d))))
		((atom (car d)) (if (not (is-base-siu (car d)))
							(append (list (siu-base-expansion (car d))) (expandall (cdr d)))
							(append (list (car d)) (expandall (cdr d)))))
		((is-base-siu (get-symbol (car d))) (append (list (car d)) (expandall (cdr d))))
		(t (append (list '** (siu-base-expansion (get-symbol (car d))) (third (car d))) (expandall (car d))))))

(defun dimension-exp (dim n)
    (cond
        ((null dim) nil)
        ((atom dim) (list '** dim n))
        ((equal (car dim) '*) (append (list '*) (dimension-exp (cdr dim) n)))
        ((equal (car dim) '**) (list '** (second dim) (* (third dim) n)))
        ((atom (car dim)) (append (list (list '** dim n)) (dimension-exp (cdr dim) n)))
        ((listp (car dim)) (append (list '** (second dim) (* (third dim) n)) (dimension-exp (cdr dim) n)))))						
	  
(defun is-kg (s)
    (if (equal (subseq (symbol-name s) (- (length (symbol-name s)) 1)) "G")
        t nil))

(defun has-prefix-kg (s)
    (if (is-base-siu s)
        nil t))

(defun get-prefix-kg (s)
    (if (equal (length (symbol-name s)) 1)
        nil
        (intern (subseq (symbol-name s) 0 1))))

(defun get-prefix-factor-kg (symbol)
  "Restituisce il factor corrispondente a un simbolo di prefisso."
  (let ((prefix-entry (find symbol *si-prefixes* :key #'si-prefix-symbol)))
    (if (not (null prefix-entry))
        (- (si-prefix-factor prefix-entry) 3)
        -3)))

(defun compare-element (e1 e2)
    (cond
        ((and (atom e1) (atom e2)) (equal (compare-symbols e1 e2) '<))
        ((and (listp e1) (atom e2)) (equal (compare-symbols (get-symbol e1) e2) '<))
        ((and (listp e2) (atom e1)) (equal (compare-symbols e1 (get-symbol e2)) '<))
        (t (get-symbol e2) (equal (compare-symbols (get-symbol e1) (get-symbol e2)) '<))))

(defun compare-symbols (u1 u2)
    (cond
        ((equal u1 u2) '=)
        ((null u1) '<)
        ((null u2) '>)
        ((and (atom u1) (atom u2)) (dolist (element (mapcar #'si-unit-symbol *si-units*))
                                       (cond
                                           ((equal element u1) (return '<))
                                           ((equal element u2) (return '>)))))
        (t '=)))

;;TEST
(defun test-is-dimension ()
    (let ((tester '(* dam (** kg 3) (** (** kg 3) 2) (* A (* mm hK) mol) (* A (** (** m 4) 2) (* hmol K)))))
         (format t "Testing is-dimension:~%")
         (format t "~a~%" tester)
         (format t "Is a valid dimension? ~a~%" (is-dimension tester))
  
         (format t "Test completed.~%")))

(defun test-norm ()
    (format t "Testing norm:~%")
    (let ((expression '((* (** m -2) A (** s -2) kg (** m 2) (** K 2) (** s -1)))))
         (dolist (element expression)
             (format t "Original ~a~%" element)
             (format t "Normalization ~a~%" (norm element)))))
			 
;; Esempio di utilizzo:
;(setq nested-list '(* m (** kg 3) (** (** kg 3) 2) (* A (* m K) mol) (* A (** (** m 4) 2) (* mol K))))
;(setq flattened-list (flatten-list nested-list))
;(format t "Lista annidata: ~a~%" nested-list)
;(format t "Lista livellata: ~a~%" flattened-list) ;(** (* kg A K) 2)
;(format t "Lista semplificata: ~a~%" (simplify flattened-list))
;(format t "Lista Normalizzata: ~a~%" (norm nested-list))

;;TEST
(defun test-compare-units ()
    (format t "Testing prefix:~%")
    (format t "Compare Unit KM: ~a~%" (get-prefix 'mm))
    (format t "HAS PREFIX KM: ~a~%" (has-prefix 'km))
    (format t "Symbol KM: ~a~%" (get-prefix-factor (get-prefix 'mm))))
    ;(format t "Comparing Unity: ~a~%" (compare-units 'm 'km))
    ;(format t "Comparing Unity: ~a~%" (compare-units 'm 's))
    ;(format t "Comparing Unity: ~a~%" (compare-units 'mm 'km))
    ;(format t "Comparing Unity: ~a~%" (compare-units 'm 'm))
    ;(format t "Comparing Dimension: ~a~%" (get-prefix-factor (get-prefix 'mm)))

(defun test-q ()
    (format t "TESTING Q~%")
    (format t "> ~a~%" (q 8 'm))
    (format t "> ~a~%" (q 8 'cm))
    (format t "> ~a~%" (q 8 '(* m (** ms -2))))
    (format t "> ~a~%" (q 8 '(* m (** kg -2))))
(let ((q1 (q 42 'm))
        (q2 (q 42 'cm)))
    (format t "q1 = ~a~%" q1)
    (format t "q2 = ~a~%" q2)
    (format t "qsum(q1, q2) = ~a~%" (qsum q1 q2))))

(defun test-kg ()
    (format t "Testing KG ~%")
    (format t "Is ~a Kg? ~a~%" 'kg (is-kg 'kg))
    (format t "Is ~a Kg? ~a~%" 'g (is-kg 'g))
    (format t "Is ~a Kg? ~a~%" 'mg (is-kg 'mg))
    (format t "Get prefix ~a: ~a~%" 'kg (get-prefix-kg 'kg))
    (format t "Get prefix ~a: ~a~%" 'g (get-prefix-kg 'g))
    (format t "Get prefix ~a: ~a~%" 'mg (get-prefix-kg 'mg))
    (format t "Has prefix ~a: ~a~%" 'kg (has-prefix-kg 'kg))
    (format t "Has prefix ~a: ~a~%" 'g (has-prefix-kg 'g))
    (format t "Has prefix ~a: ~a~%" 'mg (has-prefix-kg 'mg))
    (format t "Prefix Power ~a: ~a~%" 'kg (get-prefix-factor-kg (get-prefix-kg 'kg)))
    (format t "Prefix Power ~a: ~a~%" 'g (get-prefix-factor-kg (get-prefix-kg 'g)))
    (format t "Prefix Power ~a: ~a~%" 'mg (get-prefix-factor-kg (get-prefix-kg 'mg)))
    
    )

;; Esegui il test
;(test-is-dimension)
;(test-norm)
;(test-compare-units)
;(test-q)
;(test-kg)
