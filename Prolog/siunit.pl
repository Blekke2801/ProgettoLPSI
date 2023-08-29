is_base_siu(kg).
is_base_siu(m).
is_base_siu(s).
is_base_siu('A').
is_base_siu('K').
is_base_siu(cd).
is_base_siu(mol).

is_siu(X) :-
    atom(X),
    !,
    is_base_siu(X),
    !.

is_siu('Bq').
is_siu(dc).
is_siu('C').
is_siu('F').
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
siu_name('Omega', 'Ohm').
siu_name('Pa', 'Pascal').
siu_name(rad, radian).
siu_name('S', 'Siemens').
siu_name('Sv', 'Sievert').
siu_name(sr, steradian).
siu_name('T', 'Tesla').
siu_name('V', 'Volt').
siu_name('W', 'Watt').
siu_name('Wb', 'Weber').

siu_symbol(N, S) :-
    siu_name(S, N).

siu_base_expansion(S, S) :-
    is_base_siu(S),
    !.

siu_base_expansion('Bq', (s ** -1)).
siu_base_expansion(dc, 'K').
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

is_dimension([*, A, B]) :-
    is_dimension(A),
    is_dimension(B).

is_dimension([**, A, B]) :-
    is_dimension(A),
    number(B).

is_dimension(D) :-
    atom(D),
    !,
    is_siu(D).

is_dimension(D) :-
    compound(D),
    D =.. List,
    is_dimension(List).

is_quantity(q(N, D)) :- 
    number(N), 
    is_dimension(D).

compare_units(=, U1, U2) :-
    is_base_siu(U1),
    is_base_siu(U2).

compare_units(>, U1, U2) :-
    is_base_siu(U1),
    !,
    \+is_base_siu(U2).

compare_units(=, U1, U2) :-
    \+is_base_siu(U1),
    \+is_base_siu(U2).

compare_units(<, U1, U2) :-
    \+is_base_siu(U1),
    is_base_siu(U2).

