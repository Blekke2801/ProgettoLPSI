;;;; -*- Mode: Lisp -*-
;;;; 886259 Comi Emanuele
;;;; 869222 Melpignano Umberto
;;;; 869218 Mocellin Andrea

; Ritorna T quando Q è una “quantità”
; In caso contrario ritorna NIL
(defun is-quantity (q)
    (and (listp q)
         (eq 'q (first q))
         (let ((n (second q))
               (d (third q)))
              (and (numberp n)
                   (is-dimension d)))))

; Ritorna T quando S è un simbolo che denota un’unità S.I. base.
; La funzione ritorna NIL in caso contrario
(defun is-base-siu (unit)
    (not (null (member unit '(kg m s A K cd mol)))))
       
; Ritorna T quando S è un simbolo che denota un’unità S.I.
; (base o derivata).
; La funzione ritorna NIL in caso contrario
(defun is-siu (unit)
  (or (not (null (member unit '(
             Bq dc C F g Gy Hz H J kat lm lx N Omega Pa 
             rad S Sv sr T V W Wb))))
      (is-base-siu unit)))

; Ritorna il nome N (un simbolo Common Lisp) dell’unità
; il cui simbolo è S
; Ritorna NIL se non si trova l’associazione
(defun siu-name (unit)
  (cond ((eq unit 'kg) 'kilogram)
        ((eq unit 'm) 'metre)
        ((eq unit 's) 'second)
        ((eq unit 'A) 'Ampere)
        ((eq unit 'K) 'Kelvin)
        ((eq unit 'cd) 'candela)
        ((eq unit 'mol) 'mole)
        ((eq unit 'Bq) 'Becquerel)
        ((eq unit 'dc) 'degreecelsius)
        ((eq unit 'C) 'Coulomb)
        ((eq unit 'F) 'Farad)
        ((eq unit 'Gy) 'Gray)
        ((eq unit 'Hz) 'Hertz)
        ((eq unit 'H) 'Henry)
        ((eq unit 'J) 'Joule)
        ((eq unit 'kat) 'Katal)
        ((eq unit 'lm) 'lumen)
        ((eq unit 'lx) 'lux)
        ((eq unit 'N) 'Newton)
        ((eq unit 'Omega) 'ohm)
        ((eq unit 'Pa) 'Pascal)
        ((eq unit 'rad) 'radian)
        ((eq unit 'S) 'Siemens)
        ((eq unit 'Sv) 'Sievert)
        ((eq unit 'sr) 'steradian)
        ((eq unit 'T) 'Tesla)
        ((eq unit 'V) 'Volt)
        ((eq unit 'W) 'Watt)
        ((eq unit 'Wb) 'Weber)
        (t nil))) ; Restituisci nil se non è un'unità conosciuta

; Ritorna il simbolo S (un simbolo Common Lisp) dell’unità
; il cui nome è N
; Ritorna NIL se non si trova l’associazione
(defun siu-symbol (name)
  (cond ((eq name 'kilogram) 'kg)
        ((eq name 'metre) 'm)
        ((eq name 'second) 's)
        ((eq name 'Ampere) 'A)
        ((eq name 'Kelvin) 'K)
        ((eq name 'candela) 'cd)
        ((eq name 'mole) 'mol)
        ((eq name 'Becquerel) 'Bq)
        ((eq name 'degreecelsius) 'dc)
        ((eq name 'Coulomb) 'C)
        ((eq name 'Farad) 'F)
        ((eq name 'Gray) 'Gy)
        ((eq name 'Hertz) 'Hz)
        ((eq name 'Henry) 'H)
        ((eq name 'Joule) 'J)
        ((eq name 'Katal) 'kat)
        ((eq name 'lumen) 'lm)
        ((eq name 'lux) 'lx)
        ((eq name 'Newton) 'N)
        ((eq name 'ohm) 'Omega)
        ((eq name 'Pascal) 'Pa)
        ((eq name 'radian) 'rad)
        ((eq name 'Siemens) 'S)
        ((eq name 'Sievert) 'Sv)
        ((eq name 'steradian) 'sr)
        ((eq name 'Tesla) 'T)
        ((eq name 'Volt) 'V)
        ((eq name 'Watt) 'W)
        ((eq name 'Weber) 'Wb)
        (t nil))) ; Restituisci nil se non è un nome conosciuto

; Ritorna l’espansione in forma canonica dell’unità S
; L’espansione deve contenere solo unità base.
(defun siu-base-expansion (symbol)
  (cond ((eq symbol 'Bq)   '(** s -1))
        ((eq symbol 'dc)   'K)
        ((eq symbol 'C)    '(* s A))
        ((eq symbol 'F)    '(* (** kg -1) (** m -2) (** s 4) (** A 2)))
        ((eq symbol 'Gy)   '(* (** m 2) (** s -2)))
        ((eq symbol 'Hz)   '(** s -1))
        ((eq symbol 'H)    '(* kg (** m 2) (** s -2) (** A -2)))
        ((eq symbol 'J)    '(* kg (** m 2) (** s -2)))
        ((eq symbol 'kat)  '(* mol (** s -1)))
        ((eq symbol 'lm)   '(* cd (** m 2) (** m -2)))
        ((eq symbol 'lx)   '(* cd (** m 2) (** m -4)))
        ((eq symbol 'N)    '(* kg m (** s -2)))
        ((eq symbol 'Omega) '(* kg (** m 2) (** s -3) (** A -2)))
        ((eq symbol 'Pa)   '((* kg (** m -1) * s ** -2)))
        ((eq symbol 'rad)  '(* m (** m -1)))
        ((eq symbol 'S)    '(* (** kg -1) (** m -2) (** s 3) (** A 2)))
        ((eq symbol 'Sv)   '(* (** m 2) (** s -2)))
        ((eq symbol 'sr)   '(* (** m 2) (** m -2)))
        ((eq symbol 'T)    '(* kg (** s -2) (** A -1)))
        ((eq symbol 'V)    '(* kg (** m 2) (** s -3) (** A -1)))
        ((eq symbol 'W)    '(* kg (** m 2) (** s -3)))
        ((eq symbol 'Wb)   '(* kg (** m 2) (** s -2) (** A -1)))
        ((is-base-siu symbol) symbol)
        (t nil))) ; default case

; Espande e trasforma unità complesse in un formato
; più standardizzato o base
(defun expand-all (unit)
  (cond ((is-siu unit) 
          (siu-base-expansion unit))
        ((is-potenza unit) 
          (expt-dimension (expand-all (cadr unit)) (caddr unit)))
        ((and (listp unit) (eql '* (car unit)))
          (if (> (length (cdr unit)) 1)
            (append '(*) (mapcar 'expand-all (cdr unit)))
            (expand-all (cadr unit))))
        (t nil)))

; Verifica se un dato elemento rappresenta una dimensione elevata
; a una potenza
(defun is-potenza (a)
  (if (and (listp a)
           (equal (car a) '**)
           (numberp (caddr a)))
      (if (atom (cadr a))
        (or (is-siu (cadr a)) (not (null (exp-prefix (cadr a)))))
        (is-dimension (cadr a)))))

; Ritorna T quando D è una “dimensione”
; In caso contrario ritorna NIL
(defun is-dimension (unit)
  (cond ((null unit) t)
        ((atom unit)
        (cond ((not (null (exp-prefix unit))) t)
              ((is-siu unit) t)
              (t nil)))
        ((is-potenza unit) t)
        ((and (listp unit) (eql '* (car unit)) (is-dimension (cadr unit)) 
          (if (> (length (cddr unit)) 1) 
            (is-dimension (append '(*) (cddr unit)))
            (is-dimension (caddr unit)))))
        (t nil)))

; Ritorna il valore del prefisso passato
; Altrimenti restituisce NIL
(defun expansion (prefix)
  (cond ((eq prefix 'k)  (expt 10 3))
        ((eq prefix 'h)  (expt 10 2))
        ((eq prefix 'da) 10)
        ((eq prefix 'd)  (float (expt 10 -1)))
        ((eq prefix 'c)  (float (expt 10 -2)))
        ((eq prefix 'm)  (float (expt 10 -3)))
        (t nil))) ; default case, ritorna nil se il prefisso non è riconosciuto

; Riconosce se unit ha prefisso o no, e in caso positivo
; riconosce e divide il prefisso dall'unità, resituendone il valore
(defun exp-prefix (unit)
  (if (is-siu unit) 
    (list 1 unit)
    (if (> (length (string unit)) 1)
      (let* ((u (string unit))
            (siu (read-from-string (subseq u 1 (length u)))))
        (cond ((and (> (length u) 2) 
                    (is-siu (read-from-string (subseq u 2 (length u))))
                    (eql (subseq u 0 2) "da"))
                (list (expansion 'da) 
                      (subseq u 2 (length u))))
              ((and (> (length u) 1) 
                    (is-siu siu))
                (case (read-from-string (subseq u 0 1))
                  ('k (list (expansion 'k) siu))
                  ('h (list (expansion 'h) siu))
                  ('d (list (expansion 'd) siu))
                  ('c (list (expansion 'c) siu))
                  ('m (list (expansion 'm) siu))))
              (t nil)))
        nil)))

; Ritorna come Result uno dei simboli <, >, o =
; Result rappresenta la relazione d’ordine tra le unità U1 e U2
(defun compare-units (u1 u2)
    (cond 
        ((and (is-potenza u1) (not (is-potenza u2)))
          (compare-units (cadr u1) u2))
        ((and (is-potenza u2) (not (is-potenza u1)))
          (compare-units u1 (cadr u2)))
        ((and (is-potenza u1) (is-potenza u2))
          (compare-units (cadr u1) (cadr u2)))
        ((and (or (null u1) (null u2)) 
              (not (or (or (is-siu u1) (not (null (exp-prefix u1))))
                       (or (is-siu u2) (not (null (exp-prefix u1)))))))
            (error "inserire 2 unita\'"))
        ((or (listp u1) (listp u2)) 
          'error)
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
        ((and (equal u1 'c) (equal u2 'dc)) '<)
        ((and (equal u2 'c) (equal u1 'dc)) '>)
        ((string> u1 u2) '<)
        (t '>)))

; Ritorna la forma canonica della dimensione D
(defun norm (dim)
  (if (is-dimension dim)
    (cond ((is-potenza dim)
                dim)
          ((and (atom dim) (or (is-siu dim) (not (null (exp-prefix dim)))))
            dim)
          (t 
            (let* ((sussy (merge-sort (cdr dim) 'compare-units)))
              (if (null sussy)
                nil
                (if (> (length sussy) 1)
                  (append '(*) sussy)
                  (car sussy))))))
    (error "non e\' una dimensione")))

; Esegue l'algoritmo base di Merge Sort
(defun merge-sort (lst comparator)
  (if (and (<= (length lst) 1) (not (listp (car lst))))
      lst
      (if (and (eql (length lst) 1) (listp (car lst)))
        (list (norm (car lst)))
        (let* ((mid (floor (/ (length lst) 2)))
              (left (subseq lst 0 mid))
              (right (subseq lst mid)))
          (my-merge (merge-sort left comparator)
                (merge-sort right comparator)
                comparator)))))

; Fonde due liste ordinate e gestisce il caso in cui due
; elementi sono uguali, unificandoli in un unico elemento
(defun my-merge (left right comparator)
  (cond
    ((null left) right)
    ((null right) left)
    (t (case (funcall comparator (car left) (car right))
         ('> (append (list (car left)) 
                     (my-merge (cdr left) right comparator)))
         ('< (append (list (car right)) 
                     (my-merge left (cdr right) comparator)))
         ('= (let* ((r (unify-powers (car left) (car right)))
              (e (caddr r)))
              (if (zerop e)
                  (my-merge (cdr left) (cdr right) comparator)
                  (append (list r) 
                          (my-merge (cdr left) (cdr right) comparator)))))
         (otherwise (append (list (car left) (car right)) 
                    (my-merge (cdr left) (cdr right) comparator)))))))

; Unifica due unità di potenza in una singola unità, in base
; alle regole definite da "valuta"
(defun unify-powers (u1 u2)
  (case (valuta u1 u2)
    ('1 (list '** (cadr u1) (+ (caddr u1) (caddr u2))))
    ('2 (list '** (cadr u1) (1+ (caddr u1))))
    ('3 (list '** u1 (1+ (caddr u2))))
    ('4 (list '** u1 2))))

; Determina le relazioni tra due unità rispetto al fatto che
; siano o meno rappresentate come potenze
(defun valuta (u1 u2)
  (cond ((and (is-potenza u1) (is-potenza u2)) '1)
        ((and (is-potenza u1) (not (is-potenza u2))) '2)
        ((and (not (is-potenza u1)) (is-potenza u2)) '3)
        ((and (not (is-potenza u1)) (not (is-potenza u2))) '4)))

; Ordina e combina due input sotto forma di lista o meno
; In caso di liste, confronta i secondi elementi di ciascuna
(defun scambiatore (lista nonlista)
  (if (and (listp lista)
           (listp nonlista))
    (let ((a (second lista))
        (b (second nonlista)))
        (cond
          ((eql (compare-units a b) '<)
          (list nonlista lista))
          ((eql (compare-units a b) '>)
            (list lista nonlista))
          (t
            (list '** a (+ (third lista) (third nonlista))))))
    (let ((a (second lista))
          (b nonlista))
          (cond
            ((eql (compare-units a b) '<)
            (list nonlista lista))
            ((eql (compare-units a b) '>)
              (list lista nonlista))
            (t
              (list '** a (+ (third lista) 1)))))))

; Rappresentazione delle quantità
; n indica il valore
; d indica la dimensione
(defun q (n d)
(if (numberp n)
  (cond ((null d) (list 'Q n 1))
        ((is-dimension d)
          (list 'Q (float n) (norm d)))
        (t 
          (error "Non è una dimensione valida")))
  (error "Non può essere trasformato in quantità")))

; Ritorna il risultato (in forma canonica) della somma
; delle quantità Q1 e Q2. La somma è valida solo se
; le due quantità hanno dimesioni compatibili; in caso contrario
; la funzione genera un errore (chiamando la funzione error).
(defun qsum (q1 q2)
  (if (and (is-quantity q1) (is-quantity q2))
    (let ((n1 (second q1))
          (d1 (third q1))
          (n2 (second q2))
          (d2 (third q2)))
      (cond 
        ((minusp (+ n1 n2)) 
            "Non puo\' andare sotto zero")
        ((equal d1 d2)
          (q (+ n1 n2) d1))
        ((equal (expand-all d1) (expand-all d2))
          (q (+ n1 n2) d1))
        ((and (atom d1) 
              (atom d2)
              (equal (second (exp-prefix d1)) (second (exp-prefix d2))))
          (let ((pn1 (* (first (exp-prefix d1)) n1))
                (pn2 (* (first (exp-prefix d2)) n2)))
            (q (+ pn1 pn2) d1)))
        (t 
          (error "Le quantità non sono compatibili."))))
      (error "Non sono quantità valide")))

; Ritorna il risultato (in forma canonica) della sottrazione
; delle quantità Q1 e Q2. La sottrazione è valida solo se
; le due quantità hanno dimesioni compatibili; in caso contrario
; la funzione genera un errore (chiamando la funzione error).
(defun qsub (q1 q2)
  (if (and (is-quantity q1) (is-quantity q2))
    (let ((n1 (second q1))
          (d1 (third q1))
          (n2 (second q2))
          (d2 (third q2)))
      (cond 
        ((minusp (- n1 n2)) 
            "Non puo\' andare sotto zero")
        ((equal d1 d2)
          (q (- n1 n2) d1))
        ((equal (expand-all d1) (expand-all d2))
          (q (- n1 n2) d1))
        ((and (atom d1) 
              (atom d2)
              (equal (second (exp-prefix d1)) (second (exp-prefix d2))))
          (let ((pn1 (* (first (exp-prefix d1)) n1))
                (pn2 (* (first (exp-prefix d2)) n2)))
            (q (- pn1 pn2) d1)))
        (t 
          (error "Le quantità non sono compatibili."))))
      (error "Non sono quantità valide")))

; Ritorna il risultato (in forma canonica) della moltiplicazione
; delle quantità Q1 e Q2.
(defun qtimes (q1 q2)
  (if (and (is-quantity q1) (is-quantity q2))
    (if (minusp (* (second q1) (second q2))) 
      (format t "~%Non valgono unità sotto lo zero")
      (let ((value1 (second q1))
            (dim1 (third q1))
            (value2 (second q2))
            (dim2 (third q2)))
        (if (and (listp dim2) (eql (car dim2) '*))
          (q 
                (* value1 value2)
                (norm (append '(*) (append (list dim1) (cdr dim2)))))
          (q 
                (* value1 value2)
                (norm (append '(*) (append (list dim1) (list dim2))))))))
    (error "Non sono quantità valide")))

; Ritorna il risultato (in forma canonica) della divisione
; delle quantità Q1 e Q2.
; Se Q2 ha valore 0, viene segnalato
; un errore mediante la funzione error
(defun qdiv (q1 q2)
  (if (and (is-quantity q1) (is-quantity q2))
    (cond
        ((zerop (second q2)) 
          (error "Non puoi dividere per 0"))
        ((not (equal (minusp (second q1)) (minusp (second q2)))) 
          (format t "~%Non valgono unità sotto lo zero"))
        (t (qtimes q1 (qexpt q2 -1))))
    (error "Non sono quantità valide")))

; Ritorna il risultato (in forma canonica) dell’elevamento
; alla potenza N delle quantità Q
(defun qexpt (quantity exponent)
  (if (and (is-quantity quantity) (numberp exponent))
    (let* ((value (second quantity))
            (dimension (third quantity))
            (new-value (expt value exponent))
            (new-dimension (expt-dimension dimension exponent)))
      (if (minusp (second quantity))
        (format t "~%Non valgono unità sotto lo zero")
        (q new-value new-dimension)))
    (error "q non valido o esponente non valido")))

; Consente di elevare a potenza dimensioni atomiche o il prodotto
; di due dimensioni
(defun expt-dimension (dim exponent)
  (cond 
   ;; Caso base: dimensione atomica
   ((atom dim) (list '** dim exponent))
   
   ;; Caso: [* A B]
   ((and (listp dim) 
         (eq (first dim) '*)) 
    (let ((left (second dim))
          (right (third dim)))
      (list '* (expt-dimension left exponent) 
               (expt-dimension right exponent))))

   ;; Caso: [** A B]
   ((and (listp dim) 
         (eq (first dim) '**)) 
    (let ((a (second dim))
          (b (third dim)))
      (list '** a (* b exponent))))

   ;; Caso di default
   (t dim)))
