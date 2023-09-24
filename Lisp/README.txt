;;;; -*- Mode: Lisp -*-
;;;; 886259 Comi Emanuele
;;;; 869222 Melpignano Umberto
;;;; 869218 Mocellin Andrea

(is-dimension 'm))---> T 
(is-dimension '(* m s)))---> T
(is-dimension '(* s m)))---> T
(is-dimension '(* m A)))---> T
(is-dimension '(* m A (** z 4))---> NIL
(is-dimension '(* m (** (** A 2) 4))--> T
(is-dimension '(* m Bq s)))---> T

(siu-symbol 'metre)---> M
(siu-symbol 'ohm)---> OMEGA
(siu-symbol 'degreecelsius)---> DC
(siu-name 'm)---> METRE
(siu-name 'Bq)---> BECQUEREL

(compare-units 'm 'm)---> =
(compare-units 'm 'kg)---> <
(compare-units 'm 'kg)---> >

(qtimes (q 42 'm) (q 0.5 '(* (** s 3) (** m -3))))
--->(q 21.0 '(* (** m -2) (** s 3))))
(qtimes (q 42 'm) (q 0.5 'm))--->(q 21.0 '(** m 2))

(qdiv (q 21 'm) (q 0.5 's))--->(q 42.0 '(* m (** s -1)))
(qdiv (q 21 'm) (q 0 's))---> ERROR
(qdiv (q 21 'z) (q 0.5 's)---> ERROR

(qexpt (q 2 'm) 4)---> (q 16 (** m 4))
(qexpt (q 2 '(** m 2)) 4)---> (q 16 (** m 8))
(qexpt (q 2 '(** z 2)) 3))---> ERROR

(qsum (q 42 'm) (q 0.5 '(* (** s 3) (** m -3))))
(qsum (q 42 'm) (q 42 'cm))---> (q 42.42 'm)
(qsum (q 42 'ms) (q 42 'kg)))---> ERROR
(qsum (q 42 's) (q 42 's))---> (q 84 's)

(qsub (q 42 'm) (q 44 'm))---> ERROR
(qsub (q 42 's) (q 42 's))---> (q 0 's)
(qsub (q 42 'cm) (q 44 'm))---> ERROR

(norm '(* (** m -2) A (** s -2) kg (** m 2) (** K 2) (** s -1)))
---> (* kg (** s -3) A (** K 2))




