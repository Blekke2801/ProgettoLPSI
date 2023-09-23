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
   ((and (listp dim) (eq (first dim) '*) (is-dimension (second dim)) (is-dimension (third dim)))
    t)
   
   ;; Caso: [** A B] con A come unità SI e B come numero
   ((and (listp dim) (eq (first dim) '**) (is-siu (second dim)) (numberp (third dim)))
    t)
   
   ;; Caso: [** A B] con A come prefisso SI e B come numero
   ((and (listp dim) (eq (first dim) '**) (expansion (second dim)) (numberp (third dim)))
    t)

   ;; Caso: [** A B] con A come dimensione e B come numero
   ((and (listp dim) (eq (first dim) '**) (is-dimension (second dim)) (numberp (third dim)))
    t)

   ;; Caso: D come atom e unità SI
   ((and (atom dim) (is-siu dim))
    t)
   
   ;; Caso: X come atom e prefisso SI
   ((and (atom dim) (expansion dim))
    t)
   
   ;; Caso: atom come combinazione di prefisso e unità
   ((and (atom dim) (prefix-and-unit dim))
    t)

   ;; Caso: D come compound
   ((and (listpp dim) (is-dimension (apply 'list dim)))
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
        ((and (or (null u1) (null u2)) 
              (not (or (or (is-siu u1) (not (null (expansion u1))))
                       (or (is-siu u2) (not (null (expansion u1)))))))
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

(defun norm (dim)
  (if (is-dimension dim)
    (cond ((is-potenza dim)
                dim)
          ((or (is-siu dim) (not (null (expansion dim))))
            dim)
          (t 
            (list '* (merge-sort (cdr dim)))))
    (error "non e\' una dimensione")))

(defun merge-sort (list)
  (if (small list) list
	  (my-merge
		(merge-sort (left-half list))
		(merge-sort (right-half list)))))

(defun right-half (list)
  (last list (ceiling (/ (length list) 2))))

(defun left-half (list)
  (ldiff list (right-half list)))

(defun small (list)
  (or (null list) (atom list) (is-potenza list)))

(defun divide (lst)
  (let ((middle (floor (/ (length lst) 2))))
    (values (subseq lst 0 middle) (subseq lst middle))))

(defun my-merge (lst1 lst2)
  (cond
   ((null (car lst1)) lst2)
   ((null (car lst2)) lst1)
   (t 
    (let ((a (car lst1))
          (b (car lst2)))
      (cond ((and (is-potenza a)
              (not (listp b)))
          (append (scambiatore a b) (my-merge (cdr lst1) (cdr lst2))))
        ((and (is-potenza b)
              (not (listp a))
          (append (scambiatore b a) (my-merge (cdr lst1) (cdr lst2)))))
        ((and (is-potenza a)
              (is-potenza b))
          (append (scambiatore a b) (my-merge (cdr lst1) (cdr lst2))))
        ((or (listp a) 
             (listp b))
          (list a (my-merge lst1 (cdr lst2))))
        (t
          (cond
            ((eql (compare-units a b) '<)
            (list b (my-merge (cdr lst1) lst2)))
            ((eql (compare-units a b) '>)
              (list a (my-merge lst1 (cdr lst2)))))
            (t
              (list '(** a 2) (my-merge (cdr lst1) (cdr lst2))))))))))

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

