%%%% -*- Mode: Prolog -*-
%%%% 886259 Comi Emanuele
%%%% 869222 Melpignano Umberto
%%%% 869218 Mocellin Andrea

TESTS

IS_DIMENSION)
?- is_dimension(m).
true.

?- is_dimension(m*s).
true.

?- is_dimension(s*m).
true.

?- is_dimension(m * 'A').
true.

?- is_dimension(m * 'A' * z ** 4).
false.

?- is_dimension(m * ('A' ** 2) ** 4).
true.

?- is_dimension(m * 'Bq' * s).
true.

SIU_SYMBOL)
?- siu_symbol(ohm, S).
S = 'Omega'.

?- siu_symbol(metre, S).
S = m.

SIU_NAME)
?- siu_name(m, S).
S = metre.

?- siu_name('Bq', S).
S = 'Becquerel'.

COMPARE_UNITS)
?- compare_units(X,m,m).
X = (=).

?- compare_units(X,a,a).
false.

?- compare_units(X,m,kg).
X = (<).

?- compare_units(X,m,'Bq').
X = (>).

QSUM)
?- qsum(q(42,m), q(42,cm),R).
R = q(42.42, m).

?- qsum(q(42,ms), q(42,kg),R).
false.

?- qsum(q(42,ms), q(42,ms),R).
R = q(84, ms).

QSUB)
?- qsub(q(42,m), q(44,m),R).
false.

ࠀ?- qsub(q(42,ms), q(42,ms),R).
R = q(0, ms).

?- qsub(q(42,cm), q(44,m),R).
false.

QTIMES)
?- qtimes(q(42, m), q(0.5, (s ** 3) * (m ** -3)), R).
R = q(21.0, m*(m** -3*s**3)) .

?- qtimes(q(42,m),q(0.5,m),X).
X = q(21.0, m**2) .

QDIV)
?- qdiv(q(21,m),q(0.5,s),X).
X = q(42.0, m*s** -1) .

?- qdiv(q(21,m),q(0,s),X).
false.

?- qdiv(q(21,a),q(0,s),X).
false.

QEXPT)
?- qexpt(q(2,m),4,X).
X = q(16, m**4).

?- qexpt(q(2,m**2),4,X).
X = q(16, m**8).

?- qexpt(q(2,a**2),3,X).
false.

NORM)
?- norm((m ** -2) * 'A' * (s ** -2) * kg * (m ** 2) * ('K' ** 2) * (s ** -1), ND).
ND = kg*s** -3*'A'*'K'**2 .


