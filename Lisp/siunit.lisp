;; Definire una struttura dati per rappresentare le quantità fisiche
(defstruct quantity
  dimension  ; Dimensione della quantità (es. '(* m s -1))
  value      ; Valore numerico della quantità
)

;; Definire una struttura dati per rappresentare le unità SI
(defstruct si-unit
  name       ; Nome dell'unità (es. "metro")
  symbol     ; Simbolo dell'unità (es. 'm)
  base       ; Indica se l'unità è una base unit (T o NIL)
  expansion  ; Espansione in termini di unità base (es. '(* m))
)

(defvar *si-units*
  (list

   ;; Unità di base
   (make-si-unit :name "meter" :symbol 'm :base t :expansion '(* m))
   (make-si-unit :name "kilogram" :symbol 'kg :base t :expansion '(* kg))
   (make-si-unit :name "second" :symbol 's :base t :expansion '(* s))
   (make-si-unit :name "ampere" :symbol 'A :base t :expansion '(* A))
   (make-si-unit :name "kelvin" :symbol 'K :base t :expansion '(* K))
   (make-si-unit :name "mole" :symbol 'mol :base t :expansion '(* mol))
   (make-si-unit :name "candela" :symbol 'cd :base t :expansion '(* cd))

   ;; Unità derivate
   (make-si-unit :name "becquerel" :symbol 'Bq :base nil :expansion '(** s -1))
   (make-si-unit :name "degreecelsius" :symbol 'DC :base nil :expansion 'K)
   (make-si-unit :name "coulomb" :symbol 'C :base nil :expansion '(* A s))
   (make-si-unit :name "farad" :symbol 'F :base nil :expansion '(* (** A 2) (** kg -1) (** m -2) (** s 4)))
   (make-si-unit :name "gray" :symbol 'Gy :base nil :expansion '(* (** m 2) (** s -2)))
   (make-si-unit :name "hertz" :symbol 'Hz :base nil :expansion '(** s -1))
   (make-si-unit :name "henry" :symbol 'H :base nil :expansion '(* (** A -2) (** kg 1) (** m 2) (** s -2)))
   (make-si-unit :name "joule" :symbol 'J :base nil :expansion '(* kg (** m 2) (** s -2)))
   (make-si-unit :name "katal" :symbol 'kat :base nil :expansion '(* mol (** s -1)))
   (make-si-unit :name "lumen" :symbol 'lm :base nil :expansion '(* cd (** m 2) (** m -2)))
   (make-si-unit :name "lux" :symbol 'lx :base nil :expansion '(* cd (** m 2) (** m -4)))
   (make-si-unit :name "newton" :symbol 'N :base nil :expansion '(* kg m (** s -2)))
   (make-si-unit :name "ohm" :symbol 'Omega :base nil :expansion '(* (** A -2) kg (** m 2) (** s -3)))
   (make-si-unit :name "pascal" :symbol 'Pa :base nil :expansion '(* kg (** m -1) (** s -2)))
   (make-si-unit :name "radian" :symbol 'rad :base nil :expansion (* m (** m -1))
   (make-si-unit :name "siemens" :symbol 'S :base nil :expansion '(* (** A 2) (** kg -1) (** m -2) (** s 3)))
   (make-si-unit :name "sievert" :symbol 'Sv :base nil :expansion '(* (** m 2) (** s -2)))
   (make-si-unit :name "steradian" :symbol 'sr :base nil :expansion '(* (** m 2) (** m -2))
   (make-si-unit :name "tesla" :symbol 'T :base nil :expansion '(* (** A -1) (** kg 1) (** s -2)))
   (make-si-unit :name "volt" :symbol 'V :base nil :expansion '(* (** A -1) kg (** m 2) (** s -3)))
   (make-si-unit :name "watt" :symbol 'W :base nil :expansion '(* kg (** m 2) (** s -3)))
   (make-si-unit :name "weber" :symbol 'Wb :base nil :expansion '(* (** A -1) (** kg 1) (** m 2) (** s -2)))
   ))

;;Funzioni da implementare
(defun is-siu (s)
  "Verifica se il simbolo S rappresenta un'unità SI (base o derivata)."
  (let ((unit (find-unit-by-symbol s *si-units*)))
    (if unit
        t
        nil)))

(defun is-base-siu (s)
  "Verifica se il simbolo S rappresenta un'unità SI di base."
  (let ((unit (find-unit-by-symbol s *si-units*)))
    (if unit
        (si-unit-base unit) ; Controlla direttamente se è una base unit
        nil)))

(defun siu-name (s)
  "Restituisce il nome dell'unità SI con il simbolo S o NIL se non è presente."
  (let ((unit (find-unit-by-symbol s *si-units*)))
    (if unit
        (si-unit-name unit)
        nil)))

(defun siu-symbol (n)
  "Restituisce il simbolo dell'unità SI con il nome N o NIL se non è presente."
  (let ((unit (find-unit-by-name n *si-units*)))
    (if unit
        (si-unit-symbol unit)
        nil)))

(defun siu-base-expansion (s)
  "Restituisce l'espansione in forma canonica dell'unità S con solo unità di base."
  (let ((unit (find-unit-by-symbol s *si-units*)))
    (if unit
        (si-unit-expansion unit)
        nil)))

(defun is-dimension (d)
  "Verifica se D è una dimensione valida in forma canonica."
  (if (and (listp d) (canonical-dimension-p d))
      t
      nil))

(defun is-quantity (q)
  "Verifica se Q è una quantità valida."
  (and (numberp (quantity-value q)) (is-dimension (quantity-dimension q))))

;;Funzione Compare-Unit


   (defun norm (dim)
  "Restituisce la forma canonica della dimensione D."
  (if (null dim)
      '()  ; Dimensione vuota è già in forma canonica.
      (canonicalize-dimension dim)))

;;Funzioni di ausilio
(defun find-unit-by-symbol (symbol unit-list)
  "Trova un'unità SI dato il simbolo in unit-list."
  (find-if (lambda (unit) (eq (si-unit-symbol unit) symbol)) unit-list))

(defun find-unit-by-name (name unit-list)
  "Trova un'unità SI dato il nome in unit-list."
  (find-if (lambda (unit) (string= (si-unit-name unit) name)) unit-list))

(defun canonical-dimension-p (d)
  "Verifica se D è una dimensione in forma canonica con unità valide."
  (cond
    ((null d) t) ; Una dimensione vuota è valida.
    ((symbolp (car d))
     (and (is-siu (car d)) (canonical-dimension-p (cdr d))))
    ((listp (car d))
     (and (canonical-dimension-p (car d)) (canonical-dimension-p (cdr d))))
    (t nil)))

(defun canonicalize-dimension (dim)
  "Semplifica gli esponenti delle unità di misura uguali nella dimensione."
  (let ((unit-exponents '()))
    (dolist (element dim)
      (if (is-base-siu element)
          (push element unit-exponents)
          (if (and (listp element) (= (length element) 3) (eq (first element) '**))
              (let* ((unit (second element))
                     (exponent (third element))
                     (existing-exponent (cdr (assoc unit unit-exponents))))
                (if existing-exponent
                    (setf (cdr (assoc unit unit-exponents)) (+ existing-exponent exponent))
                    (push (list unit exponent) unit-exponents)))
              (error "Elemento non valido nella dimensione")))))
    (let ((simplified-dim '()))
      (dolist (unit-exp unit-exponents)
        (let ((unit (car unit-exp))
              (exponent (cdr unit-exp)))
          (if (= exponent 0)
              ;; Ignora le unità con esponente zero.
              ()
              (if (= exponent 1)
                  (push unit simplified-dim)
                  (push (list '** unit exponent) simplified-dim)))))
      (sort simplified-dim #'(lambda (a b) (string< (symbol-name (cadr a)) (symbol-name (cadr b))))))))
