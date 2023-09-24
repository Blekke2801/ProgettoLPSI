%%%% -*- Mode: Prolog -*-
%%%% Emanuele Comi 886259
%%%% Umberto Melpignano 869222
%%%% Andrea Mocellin 869218

% Espande dimensioni e unità di misura
% in termini delle loro dimensioni base

expand_all(D, D) :-
    is_base_siu(D),
    !.

expand_all(D, DR)  :-
    siu_base_expansion(D, DR),
    !.

expand_all(D, DR) :-
    prefix_expansion(D, DR),
    !.

% Espande l'elevamento a potenza

expand_all(A**N, DR):-
    siu_base_expansion(A, DRM),
    !,
    uexpt(DRM, N, DR).

expand_all(A**N, DR):-
    prefix_expansion(A, DRM),
    !,
    uexpt(DRM, N, DR).

expand_all([*, A, B], DR) :-
    siu_base_expansion(A, DA),
    siu_base_expansion(B, DB),
    !,
    DR =.. [*, DA, DB].

expand_all([*, A, B], DR) :-
    siu_base_expansion(A, DA),
    !,
    compound(B),
    !,
    expand_all(B, DB),
    DR =.. [*, DA, DB].

expand_all([*, A, B], DR) :-
    compound(A),
    siu_base_expansion(B, DB),
    !,
    expand_all(A, DA),
    DR =.. [*, DA, DB].

expand_all([*, A, B], DR) :-
    compound(A),
    compound(B),
    expand_all(A, DA),
    expand_all(B, DB),
    DR =.. [*, DA, DB].

expand_all(D, DR) :-
    is_dimension(D),
    D =.. [*, A, B],
    expand_all([*, A, B], DR).

% Vero quando S è un simbolo che denota
% un’unità SI base

is_base_siu(kg).
is_base_siu(m).
is_base_siu(s).
is_base_siu('A').
is_base_siu('K').
is_base_siu(cd).
is_base_siu(mol).


% Vero quando S è un simbolo che denota
% un’unità SI (base o derivata).

is_siu('Bq').
is_siu(dc).
is_siu('C').
is_siu('F').
is_siu(g).
is_siu('Gy').
is_siu('Hz').
is_siu('H').
is_siu('J').
is_siu(kat).
is_siu(lm).
is_siu(lx).
is_siu('N').
is_siu('Omega').
is_siu('Pa').
is_siu(rad).
is_siu('S').
is_siu('Sv').
is_siu(sr).
is_siu('T').
is_siu('V').
is_siu('W').
is_siu('Wb').
is_siu(X) :-
    atom(X),
    !,
    is_base_siu(X),
    !.

% Vero quando N è il nome
% dell’unità il cui simbolo è S

siu_name(kg, kilogram).
siu_name(m, metre).
siu_name(s, second).
siu_name('A', 'Ampere').
siu_name('K', 'Kelvin').
siu_name(cd, candela).
siu_name(mol, mole).
siu_name('Bq', 'Becquerel').
siu_name(dc, degreecelsius).
siu_name('C', 'Coulomb').
siu_name('F', 'Farad').
siu_name('Gy', 'Gray').
siu_name('Hz', 'Hertz').
siu_name('H', 'Henry').
siu_name('J', 'Joule').
siu_name(kat, 'Katal').
siu_name(lm, lumen).
siu_name(lx, lux).
siu_name('N', 'Newton').
siu_name('Omega', ohm).
siu_name('Pa', 'Pascal').
siu_name(rad, radian).
siu_name('S', 'Siemens').
siu_name('Sv', 'Sievert').
siu_name(sr, steradian).
siu_name('T', 'Tesla').
siu_name('V', 'Volt').
siu_name('W', 'Watt').
siu_name('Wb', 'Weber').

% Esegue l'inverso di siu_name
% Vero quando N è il nome dell'unità

% il cui simbolo è S
siu_symbol(N, S) :-
    siu_name(S, N).

% Vero quando Expansion è l’espansione in forma
% canonica dell’unità S
% L’espansione deve contenere solo unità base

siu_base_expansion('Bq', (s ** -1)).
siu_base_expansion(dc, 'K' - 273).
siu_base_expansion('C', s * 'A').
siu_base_expansion('F', (kg  ** -1) * (m  ** -2) * (s ** 4) * ('A' ** 2)).
siu_base_expansion('Gy', (m ** 2) * (s  ** -2)).
siu_base_expansion('Hz', (s  ** -1)).
siu_base_expansion('H', kg * (m ** 2) * (s  ** -2) * ('A'  ** -2)).
siu_base_expansion('J', kg * (m ** 2) * (s  ** -2)).
siu_base_expansion(kat, mol * (s  ** -1)).
siu_base_expansion(lm, cd * (m ** 2) * (m ** -2)).
siu_base_expansion(lx, cd * (m ** 2) * (m ** -4)).
siu_base_expansion('N', kg * m * (s  ** -2)).
siu_base_expansion('Omega', kg * (m ** 2) * (s  ** -3) * ('A'  ** -2)).
siu_base_expansion('Pa', kg * (m  ** -1) * (s  ** -2)).
siu_base_expansion(rad, m * (m ** -1)).
siu_base_expansion('S', (kg  ** -1) * (m  ** -2) * (s ** 3) * ('A' ** 2)).
siu_base_expansion('Sv', (m ** 2) * (s  ** -2)).
siu_base_expansion(sr, (m ** 2) * (m ** -2)).
siu_base_expansion('T', kg * (s  ** -2) * ('A'  ** -1)).
siu_base_expansion('V', kg * (m ** 2) * (s  ** -3) * ('A'  ** -1)).
siu_base_expansion('W', kg * (m ** 2) * (s  ** -3)).
siu_base_expansion('Wb', kg * (m ** 2) * s  ** -2 * ('A'  ** -1)).

siu_base_expansion(S, S) :-
    is_base_siu(S),
    !.

% Scompone l'unità di misura derivata
% nella sua forma di base

prefix_expansion(Final, _) :-
    \+atom(Final),
    !,
    false.

prefix_expansion(kg, g * 10 ** 3):-!.

prefix_expansion(Var, Var * 10 ** 0):-
    is_siu(Var).

% Conversione del prefisso "deca"

prefix_expansion(Final, Exp) :-
    atom_length(Final, L),
    L > 2,
    atom_concat(da, Unit, Final),
    is_siu(Unit),
    Exp =.. [*, Unit, 10 ** 1],!.


prefix_expansion(Final, _) :-
    atom_length(Final, L),
    L =< 1,
    !,
    false.

% Conversione dei prefissi "k,h,d,c,m"

prefix_expansion(Final, Exp) :-
    atom_concat(k, Unit, Final),
    is_siu(Unit),
    Exp =.. [*, Unit, 10 ** 3],!.

prefix_expansion(Final, Exp) :-
    atom_concat(h, Unit, Final),
    is_siu(Unit),
    Exp =.. [*, Unit, 10 ** 2],!.


prefix_expansion(Final, Exp) :-
    atom_concat(d, Unit, Final),
    is_siu(Unit),
    Exp =.. [*, Unit, 10 ** (-1)],!.

prefix_expansion(Final, Exp) :-
    atom_concat(c, Unit, Final),
    is_siu(Unit),
    Exp =.. [*, Unit, 10 ** (-2)],!.

prefix_expansion(Final, Exp) :-
    atom_concat(m, Unit, Final),
    is_siu(Unit),
    Exp =.. [*, Unit, 10 ** (-3)],!.

% Valori dei prefissi delle
% unità derivate

expansion(k, 10 ** 3).
expansion(h, 10 ** 2).
expansion(da, 10).
expansion(d, 10 ** (-1)).
expansion(c, 10 ** (-2)).
expansion(m, 10 ** (-3)).

% Vero quando D è una “dimensione”

is_dimension([*, A, B]) :-
    !,
    is_dimension(A),
    is_dimension(B).

is_dimension([**, A, B]) :-
    is_siu(A),
    !,
    number(B).

is_dimension([**, A, B]) :-
    prefix_expansion(A, _),
    !,
    number(B).

is_dimension([**, A, B]) :-
    !,
    is_dimension(A),
    number(B).

is_dimension(D) :-
    atom(D),
    is_siu(D),
    !.

is_dimension(X):-
    atom(X),
    !,
    prefix_expansion(X, _).

is_dimension(D) :-
    compound(D),
    D =.. List,
    is_dimension(List).

% Vero quando Q è una “quantità”.

is_quantity(q(N, D)) :-
    number(N),
    is_dimension(D).

% Vero quando Result è uno dei simboli
% <, >, o =
% Result rappresenta la relazione d’ordine
% tra le unità U1 e U2

compare_units(>, U, PU):-
    is_base_siu(U),
    prefix_expansion(PU, U * 10 ** N),
    integer(N),
    !.

compare_units(<, PU, U):-
    is_base_siu(U),
    prefix_expansion(PU, U * 10 ** N),
    integer(N),
    !.

compare_units(_, A, _):-
    \+is_siu(A),
    !,
    false.

compare_units(=, U, U):- !.

compare_units(_, _, B):-
    \+is_siu(B),
    !,
    false.


compare_units(>, kg, _):- !.
compare_units(<, _, kg):- !.

compare_units(>, m, X):-
    X \= kg,
    !.

compare_units(<, X, m):-
    compare_units(>, m, X),
    !.

compare_units(>, s, X):-
    X \= kg,
    X \= m,
    !.

compare_units(<, X, s):-
    compare_units(>, s, X),
    !.

compare_units(>, 'A', X):-
    X \= kg,
    X \= m,
    X \= s,
    !.

compare_units(<, X, 'A'):-
    compare_units(>, 'A', X),
    !.

compare_units(>, 'K', X):-
    X \= kg,
    X \= m,
    X \= 'A',
    X \= s,
    !.

compare_units(<, X, 'K'):-
    compare_units(>, 'K', X),
    !.

compare_units(>, cd, X):-
    X \= kg,
    X \= m,
    X \= 'A',
    X \= 'K',
    X \= s,
    !.

compare_units(<, X, cd):-
    compare_units(>, cd, X),
    !.

compare_units(>, mol, X):-
    \+is_base_siu(X),
    !.

compare_units(<, X, mol):-
    compare_units(>, m, X),
    !.


compare_units(<,'C',dc).
compare_units(>,dc,'C').

compare_units(<, U1, U2) :-
\+is_base_siu(U1),
\+is_base_siu(U2),
siu_name(U1, N1),
siu_name(U2, N2),
N1 @> N2,
!.

compare_units(>, U1, U2) :-
    \+is_base_siu(U1),
    \+is_base_siu(U2).

% Vero quando QR è il risultato (in forma canonica)
% della somma delle quantità Q1 e Q2
% La somma è valida solo se le due quantità
% hanno dimesioni compatibili

qsum(q(N, D), _, _) :-
    \+is_quantity(q(N, D)),
    !,
    false.

qsum(_, q(N, D), _) :-
    \+is_quantity(q(N, D)),
    !,
    false.

qsum(q(N1, D1), q(N2, D1), q(NR, DR)) :-
    !,
    norm(D1, DR),
    NR is N1 + N2.

qsum(q(N1, D1), q(N2, D2), q(NR, DC)) :-
    expand_all(D2, D2C),
    expand_all(D1, D1C),
    norm(D2C, DC),
    norm(D1C, DC),
    !,
    NR is N1 + N2.

qsum(q(N1, D1), q(N2, D2), q(NR, DP)):-
    prefix_expansion(D1, DP * 10  ** E1),
    prefix_expansion(D2, DP * 10  ** E2),
    NR1 is N1*(10 ** E1),
    NR2 is N2*(10 ** E2),
    NR is NR1 + NR2.

% Vero quando QR è il risultato (in forma canonica)
% della sottrazione delle quantità Q1 e Q2
% La sottrazione è valida solo se le due quantità
% hanno dimesioni compatibili

qsub(q(N, D), _, _) :-
    \+is_quantity(q(N, D)),
    !,
    false.

qsub(_, q(N, D), _) :-
    \+is_quantity(q(N, D)),
    !,
    false.

qsub(q(N1, D1), q(N2, D2), q(NR, DP)):-
    prefix_expansion(D1, DP * 10  ** E1),
    prefix_expansion(D2, DP * 10  ** E2),
    NR1 is N1 * (10 ** E1),
    NR2 is N2 * (10 ** E2),
    NR1 > NR2,
    NR is NR1 - NR2.

qsub(q(N1, D1), q(N2, D2), _):-
    prefix_expansion(D1, DP * 10  ** E1),
    prefix_expansion(D2, DP * 10  ** E2),
    NR1 is N1 * (10 ** E1),
    NR2 is N2 * (10 ** E2),
    NR1 < NR2,
    write('Non puo\' andare sotto zero, viva il 42'),
    !,
    false.

qsub(q(N1, _), q(N2, _), _):-
    N1 < N2,
    write('Non puo\' andare sotto zero, viva il 42'),
    !,
    false.

qsub(q(N1, D1), q(N2, D2), q(NR, DC)) :-
    expand_all(D2, D2C),
    expand_all(D1, D1C),
    norm(D2C, DC),
    norm(D1C, DC),
    !,
    NR is N1 - N2.

qsub(q(N1, D1), q(N2, D1), q(NR, DR)) :-
    norm(D1, DR),
    NR is N1 - N2.

qsub(q(N1, D1), q(N2, D2), q(NR, D1)) :-
    norm(D2, DC),
    siu_base_expansion(D1, DC),
    NR is N1 - N2.


% Vero quando QR è il risultato (in forma canonica)
% della moltiplicazione delle quantità Q1 e Q2

qtimes(q(N, D), _, _) :-
    \+is_quantity(q(N, D)),
    !,
    false.

qtimes(_, q(N, D), _) :-
    \+is_quantity(q(N, D)),
    !,
    false.

qtimes(q(N1, D1), q(N2, D1), q(NR, DR)) :-
    qexpt(q(N1, D1), 2, q(_, DR)),
    NR is N1 * N2.

qtimes(q(N1, D1), q(N2, D2), q(NR, DR)) :-
    extract_elements(D2, D2C),
    mul_dim(D1, D2C, DR),
    NR is N1 * N2.

% Moltiplica ricorsivamente una dimensione data
% per una lista di altre dimensioni
% Se la lista è vuota, ritorna la dimensione originale
% Altrimenti, procede moltiplicando attraverso gli elementi della lista

mul_dim(D, [], D).

mul_dim(D1, [D2 | DRest], R) :- 
    list_to_expression(D2, D2E),
    norm(D1 * D2E, D1M),
    mul_dim(D1M, DRest, R).

% Vero quando QR è il risultato (in forma canonica)
% dell'elevamento alla potenza di N
% della quantità Q
    
qexpt(q(N, D), _, _) :-
    \+is_quantity(q(N, D)),
    !,
    false.

qexpt(_, N, _) :-
    \+integer(N),
    !,
    false.

qexpt(q(Number,U), N, q(NR, UR)):-
    NR is Number ** N,
    norm(U, URN),
    uexpt(URN, N, UR).

% Gestisce l'elevamento a potenza di unità di misura,
% considerando sia i prefissi che le unità composte

uexpt(SI ** E, N, SI ** N1):-
    prefix_expansion(SI, _),
    number(N),
    number(E),
    N1 is N * E.

uexpt(SI, N, SI ** N):-
    prefix_expansion(SI, _),
    number(N).

uexpt(U, N, UR):-
    is_dimension(U),
    integer(N),
    U =.. [*, X, Y],
    uexpt(X, N, UR1),
    uexpt(Y, N, UR2),
    UR =.. [*, UR1, UR2].

% Vero quando QR è il risultato (in forma canonica)
% della moltiplicazione delle quantità Q1 e Q2

qdiv(q(N, D), _, _) :-
    \+is_quantity(q(N, D)),
    !,
    false.

qdiv(_, q(N, D), _) :-
    \+is_quantity(q(N, D)),
    !,
    false.

qdiv(_, q(N, _), _) :-
    N == 0,
    !,
    false.

qdiv(q(N1, D1), q(N2, D1), q(NR, 1)):-
    NR is N1 / N2.

qdiv(q(N1, D1), q(N2, D2), QR) :-
    qexpt(q(N2, D2), -1, Q2),
    qtimes(q(N1, D1), Q2, QR).

unify_units([], []).
unify_units([X], [X]).

unify_units([X, X | Rest], [[X, 2] | NRest]):-
    !,
    is_siu(X),
    unify_units(Rest, NRest).

unify_units([[X, N], X | Rest], [[X, N1] | NRest]):-
    is_siu(X),
    number(N),
    !,
    N1 is N + 1,
    unify_units(Rest, NRest).

unify_units([X, [X, N] | Rest], [[X, N1] | NRest]):-
    is_siu(X),
    number(N),
    !,
    N1 is N + 1,
    unify_units(Rest, NRest).

unify_units([[X, N1], [X, N2] | Rest], NRest):-
    is_siu(X),
    number(N1),
    number(N2),
    0 is N1 + N2,
    !,
    unify_units(Rest, NRest).

unify_units([[X, N1], [X, N2] | Rest], [[X, NR] | NRest]):-
    is_siu(X),
    number(N1),
    number(N2),
    !,
    NR is N1 + N2,
    unify_units(Rest, NRest).

unify_units([X | Rest], [XU | NRest]) :-
    is_list(X),
    !,
    unify_units(X, XU),
    unify_units(Rest, NRest).

unify_units([X, Y | Rest], [X | NRest]) :-
    unify_units([Y | Rest], NRest).

% Algoritmo Merge Sort Base
merge_sort([], []).
merge_sort([A], [A]).

merge_sort([A, B | Rest], S) :-
    divide([A, B | Rest], L1, L2),
    merge_sort(L1, S1),
    merge_sort(L2, S2),
    my_merge(S1, S2, S).

% Divide una lista in due sottoliste

divide([], [], []).
divide([A], [A], []).

divide([A, B | R], [A | Ra], [B | Rb]) :-
    divide(R, Ra, Rb).

% Unisce due liste basandosi su una logica di comparazione di unità,
% gestendo vari formati di elementi nelle liste
% e ordinando eventuali sottoliste

my_merge(A, [], A).
my_merge([], B, B).

my_merge([A], [N], [A, N]):-
    \+is_list(A),
    number(N),
    !.

my_merge([A | Ra], [[B, N] | Rb], [A | M]) :-
    number(N),
    compare_units(X, A, B),
    X = >,
    my_merge(Ra, [[B, N] | Rb], M).

my_merge([[A, N] | Ra], [B | Rb], [[A, N] | M]) :-
    number(N),
    compare_units(X, A, B),
    X = >,
    my_merge(Ra, [B | Rb], M).

my_merge([[A, N1] | Ra], [[B, N2] | Rb], [[A, N1] | M]) :-
    number(N1),
    number(N2),
    compare_units(X, A, B),
    X = >,
    my_merge(Ra, [[B, N2] | Rb], M).

my_merge([A | Ra], [B | Rb], [A | M]) :-
    compare_units(X, A, B),
    X = >,
    my_merge(Ra, [B | Rb], M).

my_merge([A | Ra], [[B, N] | Rb], [[B, N] | M]) :-
    number(N),
    compare_units(X, A, B),
    X = <,
    my_merge(Ra, [A | Rb], M).

my_merge([[A, N] | Ra], [B | Rb], [B | M]) :-
    number(N),
    compare_units(X, A, B),
    X = <,
    my_merge(Ra, [[A, N] | Rb], M).

my_merge([[A, N1] | Ra], [[B, N2] | Rb], [[B, N2] | M]) :-
    number(N1),
    number(N2),
    compare_units(X, A, B),
    X = <,
    my_merge(Ra, [[A, N1] | Rb], M).

my_merge([A | Ra], [B | Rb], [B | M]) :-
  compare_units(X, A, B),
    X = <,
  my_merge([A | Ra], Rb, M).

  my_merge([A | Ra], [[A, N] | Rb], [[A, N] | M]) :-
    number(N),
    my_merge(Ra, [A | Rb], M).

my_merge([[A, N] | Ra], [A | Rb], [A | M]) :-
    number(N),
    my_merge(Ra, [[A, N] | Rb], M).

my_merge([[A, N1] | Ra], [[A, N2] | Rb], [[A, N2] | M]) :-
    number(N1),
    number(N2),
    my_merge(Ra, [[A, N1] | Rb], M).

my_merge([A | Ra], [B | Rb], [AS | M]) :-
    is_list(A),
    \+is_list(B),
    merge_sort(A, AS),
    my_merge(Ra, [B | Rb], M).

my_merge([A | Ra], [B | Rb], [A | M]) :-
    \+is_list(A),
    is_list(B),
    merge_sort(B, BS),
    my_merge(Ra, [BS | Rb], M).

my_merge([A | Ra], [B | Rb], [AS | M]) :-
    is_list(A),
    is_list(B),
    !,
    merge_sort(A, AS),
    merge_sort(B, BS),
    my_merge(Ra, [BS | Rb], M).

my_merge([A | Ra], [A | Rb], [A | M]) :-
  my_merge(Ra, [A | Rb], M).


% Predicato per ordinare una moltiplicazione mantenendo le parentesi

norm(A ** N, A ** N):-
    prefix_expansion(A, _),
    number(N),
    !.

norm(A, A):-
    is_siu(A),
    !.

norm(A , A):-
    prefix_expansion(A, _),
    !.

norm(Expr, SortedExpr) :-
    is_dimension(Expr),
    extract_elements(Expr, FactorList),
    merge_sort(FactorList, SortedFactorList),
    unify_units(SortedFactorList, UnifiedList),
    list_to_expression(UnifiedList, SortedExpr).

% Predicato per convertire un'espressione in una lista di fattori

extract_elements(A, [A]):-
    atom(A),
    is_siu(A).

extract_elements(A, [A]):-
    atom(A),
    !,
    prefix_expansion(A, _),
    !.

extract_elements(A ** N, [[A, N]]):-
    number(N),
    atom(A),
    is_siu(A),
    !.

extract_elements(A ** N, [[A, N]]):-
    number(N),
    atom(A),
    !,
    prefix_expansion(A, _),
    !.

extract_elements(Comp, List):-
    compound(Comp),
    compound_name_arguments(Comp, *, [A, B ** N]),
    atom(B),
    !,
    extract_elements(A, L1),
    extract_elements(B ** N, L2),
    append(L1, L2, List).

extract_elements(Comp, List):-
    compound(Comp),
    compound_name_arguments(Comp, *, [A, B]),
    compound(B),
    !,
    extract_elements(A, L1),
    extract_elements(B, L2),
    append(L1, [L2], List).

extract_elements(Comp, List):-
    compound(Comp),
    compound_name_arguments(Comp, *, [A, B]),
    extract_elements(A, L1),
    extract_elements(B, L2),
    append(L1, L2, List).

% Predicato per convertire una lista di fattori in un'espressione

list_to_expression([Factor], Factor):- 
    \+is_list(Factor),
    !.

list_to_expression([Factor], Factors):- 
    list_to_expression(Factor, Factors),
    !.

list_to_expression([A, N], (A ** N)):-
    number(N),
    !.

list_to_expression([Factor1, Factor2 | Rest], Expr) :-
    \+is_list(Factor1),
    is_list(Factor2),
    !,
    list_to_expression(Factor2, NewFactor),
    list_to_expression([Factor1 * (NewFactor) | Rest], SubExpr),
    Expr = SubExpr.

list_to_expression([Factor1, Factor2 | Rest], Expr) :-
    is_list(Factor1),
    \+is_list(Factor2),
    !,
    list_to_expression(Factor1, NewFactor),
    list_to_expression([(NewFactor) * Factor2 | Rest], SubExpr),
    Expr = SubExpr.

list_to_expression([Factor1, Factor2 | Rest], Expr) :-
    is_list(Factor1),
    is_list(Factor2),
    !,
    list_to_expression(Factor1, NewFactor1),
    list_to_expression(Factor2, NewFactor2),
    list_to_expression([(NewFactor1) * (NewFactor2) | Rest], SubExpr),
    Expr = SubExpr.

list_to_expression([Factor1, Factor2 | Rest], Expr) :-
    list_to_expression([Factor1 * Factor2 | Rest], SubExpr),
    Expr = SubExpr.

