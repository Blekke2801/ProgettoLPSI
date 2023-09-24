(defun is-quantity (q)
    (and (listp q)
         (eq 'q (first q))
         (let ((n (second q))
               (d (third q)))
              (and (numberp n)
                   (is-dimension d)))))


(defun is-base-siu (unit)
    (not (null (member unit '(kg m s A K cd mol)))))
       

(defun is-siu (unit)
  (or (not (null (member unit '(
             Bq dc C F g Gy Hz H J kat lm lx N Omega Pa 
             rad S Sv sr T V W Wb))))
      (is-base-siu unit)))

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


(defun siu-base-expansion (symbol)
  (cond ((eq symbol 'Bq)   '(s ** -1))
        ((eq symbol 'dc)   'K)
        ((eq symbol 'C)    '(s * A))
        ((eq symbol 'F)    '((kg ** -1) * (m ** -2) * (s ** 4) * (A ** 2)))
        ((eq symbol 'Gy)   '((m ** 2) * (s ** -2)))
        ((eq symbol 'Hz)   '(s ** -1))
        ((eq symbol 'H)    '((kg * (m ** 2) * (s ** -2) * (A ** -2))))
        ((eq symbol 'J)    '((kg * (m ** 2) * (s ** -2))))
        ((eq symbol 'kat)  '(mol * (s ** -1)))
        ((eq symbol 'lm)   '((cd * m ** 2) * m ** -2))
        ((eq symbol 'lx)   '((cd * m ** 2) * m ** -4))
        ((eq symbol 'N)    '((kg * m) * s ** -2))
        ((eq symbol 'Omega) '((kg * m ** 2) * s ** -3 * A ** -2))
        ((eq symbol 'Pa)   '((kg * m ** -1) * s ** -2))
        ((eq symbol 'rad)  '(m * m ** -1))
        ((eq symbol 'S)    '((kg ** -1 * m ** -2 * s ** 3 * A ** 2)))
        ((eq symbol 'Sv)   '((m ** 2) * s ** -2))
        ((eq symbol 'sr)   '((m ** 2) * m ** -2))
        ((eq symbol 'T)    '((kg) * s ** -2 * A ** -1))
        ((eq symbol 'V)    '((kg * m ** 2) * s ** -3 * A ** -1))
        ((eq symbol 'W)    '((kg * m ** 2) * s ** -3))
        ((eq symbol 'Wb)   '((kg * m ** 2) * s ** -2 * A ** -1))
        ((is-base-siu symbol) symbol)
        (t nil))) ; default case


(defun expansion (prefix)
  (cond ((eq prefix 'k)  (expt 10 3))
        ((eq prefix 'h)  (expt 10 2))
        ((eq prefix 'da) 10)
        ((eq prefix 'd)  (float (expt 10 -1)))
        ((eq prefix 'c)  (float (expt 10 -2)))
        ((eq prefix 'm)  (float (expt 10 -3)))
        (t nil))) ; default case, ritorna nil se il prefisso non è riconosciuto


(defun prefix-and-unit (atom)
  (let ((str (symbol-name atom)))
    (and (= (length str) 2)
         (expansion (intern (string (elt str 0))))
         (is-siu (intern (string (elt str 1)))))))


(defun is-dimension (dim)
  (cond 
   ;; Caso: [* A B]
   ((and (listp dim) 
         (eq (first dim) '*) 
         (is-dimension (second dim)) 
         (is-dimension (third dim)))
    t)
   
   ;; Caso: [** A B] con A come unità SI e B come numero
   ((and (listp dim) 
         (eq (first dim) '**) 
         (is-siu (second dim)) 
         (numberp (third dim)))
    t)
   
   ;; Caso: [** A B] con A come prefisso SI e B come numero
   ((and (listp dim) 
         (eq (first dim) '**) 
         (expansion (second dim))
         (numberp (third dim)))
    t)

   ;; Caso: [** A B] con A come dimensione e B come numero
   ((and (listp dim) 
         (eq (first dim) '**)
         (is-dimension (second dim))
         (numberp (third dim)))
    t)

   ;; Caso: D come atom e unità SI
   ((and (atom dim) 
         (is-siu dim))
    t)
   
   ;; Caso: X come atom e prefisso SI
   ((and (atom dim) 
         (expansion dim))
    t)
   
   ;; Caso: atom come combinazione di prefisso e unità
   ((and (atom dim) 
         (prefix-and-unit dim))
    t)

   ;; Caso: D come compound
   ((and (listp dim) 
         (is-dimension (list dim)))
    t)
   
   ;; Caso di default
   (t 
    nil)))


;;Dovete comparare solo se le unità sono composte da unità base.
;;Le unità si confrontano secondo l'ordine lessicografico, non quello della tabella.
;;A presto
;;Marco Antoniotti venerdì, 1 settembre 2023, 15:05
(defun compare-units (u1 u2)
    (cond 
        ((and (is-potenza u1) (not (is-potenza u2)))
          (compare-units (cadr u1) u2))
        ((and (is-potenza u2) (not (is-potenza u1)))
          (compare-units u1 (cadr u2)))
        ((and (is-potenza u1) (is-potenza u2))
          (compare-units (cadr u1) (cadr u2)))
        ((and (or (null u1) (null u2)) 
              (not (or (or (is-siu u1) (not (null (expansion u1))))
                       (or (is-siu u2) (not (null (expansion u1)))))))
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

(defun norm (dim)
  (if (is-dimension dim)
    (cond ((is-potenza dim)
                dim)
          ((or (is-siu dim) (not (null (expansion dim))))
            dim)
          (t 
            (let* ((sussy (merge-sort (cdr dim) 'compare-units)))
              (if (null sussy)
                nil
                (append '(*) sussy)))))
    (error "non e\' una dimensione")))

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

(defun unify-powers (u1 u2)
  (case (valuta u1 u2)
    ('1 (list '** (cadr u1) (+ (caddr u1) (caddr u2))))
    ('2 (list '** (cadr u1) (1+ (caddr u1))))
    ('3 (list '** (cadr u1) (1+ (caddr u2))))
    ('4 '(** u1 2))))

(defun valuta (u1 u2)
  (cond ((and (is-potenza u1) (is-potenza u2)) '1)
        ((and (is-potenza u1) (not (is-potenza u2))) '2)
        ((and (not (is-potenza u1)) (is-potenza u2)) '3)
        ((and (not (is-potenza u1)) (not (is-potenza u2))) '4)))

(defun is-potenza (a)
  (and (listp a)
       (equal (car a) '**)
       (or (is-siu (cadr a)) (not (null (expansion (cadr a)))))
       (numberp (caddr a))))

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

(defun q (n d)
  (if (and (numberp n) (is-dimension d))
   (list 'Q (float n) (norm d))
   (error "Non può essere trasformato in quantità")))



(defun qsum (q1 q2)
  (cond 
      ((minusp (+ (second q1) (second q2))) (format t "~%Non valgono unità sotto lo zero")))
     (if
      (equal (norm (third q1)) (norm (third q2)))
      (q(+ (second q1) (second q2)) (third q1)) 
      (error "Le quantità non sono compatibili in termini di dimensioni.")))

(defun qsub (q1 q2)
    (cond 
      ((minusp (- (second q1) (second q2))) (format t "~%Non valgono unità sotto lo zero")))
  (if (equal (norm (third q1)) (norm (third q2)))
   (q(- (second q1) (second q2)) (third q1)) 
   (error "Le quantità non sono compatibili in termini di dimensioni.")))


(defun qtimes (q1 q2)
    (cond 
      ((minusp (* (second q1) (second q2))) (format t "~%Non valgono unità sotto lo zero")))
      (let ((value1 (second q1))
            (dim1 (third q1))
            (value2 (second q2))
            (dim2 (third q2)))
        (q 
              (* value1 value2)
              (norm (append '(*) (append (list dim1) (list dim2)))))))

(defun qdiv (q1 q2)
    (cond
        ((zerop (second q2)) (error "Non puoi dividere per 0"))
        ((minusp (/ (second q1) (second q2))) (format t "~%Non valgono unità sotto lo zero")))
  (if (and (is-quantity q1) (is-quantity q2))
      (let ((value1 (second q1))
            (dim1 (third q1))
            (value2 (second q2))
            (dim2 (third q2)))
        (q 
              (float(/ value1 value2))
              (norm (list '* dim1 dim2))))
    (error "Non sono quantità valide")))


(defun qexpt (quantity exponent)
     (cond
        ((minusp(expt (second quantity) exponent)) (format t "~%Non valgono unità sotto lo zero")))
  (if (is-quantity quantity)
      (let* ((value (second quantity))
             (dimension (third quantity))
             (new-value (expt value exponent))
             (new-dimension (expt-dimension dimension exponent)))
        (list 'q new-value new-dimension))
      (error "Not a valid quantity")))

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

;;    (print(q 20 'm))
;;    (terpri)
;;    (print(qsum (q -20 'm) (q 10 'm)))
;;    (terpri)
;;    (print(qsub (q -20 'm) (q 10 'm)))
;;    (terpri)
;;    (print(qtimes(q 20 'm)(q 10 '(* s m))))
;;    (terpri)
    (print(qtimes(q -20 'm)(q 10 '(* s m))))
;;    (terpri)
    (print(qtimes(q 20 'm)(q 10 '(** m -1)))) ;;TEST ROTTO
    ;;(print(qdiv(q 20 'm)(q 10 'm))) ;;TEST ROTTO
;;    (terpri)
;;    (print(qexpt(q 20 'm) 2))
;;    (terpri)
;;    (print(qdiv(q 20 'm) (q 10 's)))
;;    (terpri)
;;    (print(qdiv(q -20 'm) (q 10 's)))
;;    (terpri)
;;    (print(qexpt(q 20 'm) -1))
;;    (terpri)
;;    (print(qexpt(q -2 'm) 3))
;;    (terpri)
