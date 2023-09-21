:-[siunit].

test_dimensione_m :- 
    is_dimension(m).

test_dimensione_ms :- 
    is_dimension(m*s).

test_dimensione_sm :- 
    is_dimension(s*m).

test_dimensione_ma :- 
    is_dimension(m*'A').

test_dimensione_maz4 :- 
    not(is_dimension(m*'A'*z**4)).

test_dimensione_ma24 :- 
    is_dimension(m*('A'**2)**4).

test_qtimes :- 
    qtimes(q(42, m), q(0.5, (s ** 3) * (m ** -3)), q(21.0 , (m ** -2) * (s ** 3))).

test_qsum_false :- 
    not(qsum(q(42, m), q(0.5, (s ** 3) * (m ** -3)), _)).

test_symbol_ohm :- 
    siu_symbol(ohm, 'Omega').

test_symbol_dc :- 
    siu_symbol(degreecelsius, dc).

test_norm :- 
    norm((m ** -2) * 'A' * (s ** -2) * kg * (m ** 2) * ('K' ** 2) * (s ** -1), kg * (s ** -3) * 'A' * ('K' ** 2)).

test_qsum_prefixes :- 
    qsum(q(2, m), q(20, cm), q(2.2, m)).

test_once(Predicate) :-
    write(Predicate),
    call(Predicate),
    write(": ✅\n").

test_all :-
    test_once(test_dimensione_m),
    test_once(test_dimensione_ms),
    test_once(test_dimensione_sm),
    test_once(test_dimensione_ma),
    test_once(test_dimensione_maz4),
    test_once(test_dimensione_ma24),
    test_once(test_qtimes),
    test_once(test_qsum_false),
    test_once(test_symbol_ohm),
    test_once(test_symbol_dc),
    test_once(test_norm),
    test_once(test_qsum_prefixes).

reloadt :- [test], tty_clear.
    
