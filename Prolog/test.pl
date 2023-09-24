:-[siunit].
test_assurdamente_lungo(X) :-
    norm((m** -2) * 'A' *
     (s** -2) * kg * 
     (m ** 2) * 
     ('K' ** 2 ) * 
     (s ** -1 ) * 'Wb' * 
     ('W' ** 2) * ('W' ** -2) * 'T' * 'Sv' * 
     (rad ** 2) * 'Pa' * 'N' * 
     ('Omega' ** 2) * (lx * lm * kat * 'J') * 
     ('H' ** 2) * 'Hz' * ('Gy' **4) * 'F' * 'C' * dc, X).
    
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

test_dimensione_mBqs :-
    is_dimension(m * 'Bq' * s).

test_symbol_metre :-
    siu_symbol(metre, m).

test_name_m :-
    siu_name(m, metre).

test_name_Bq :-
    siu_name('Bq', 'Becquerel').

test_compare_mm :-
    compare_units(=,m,m).

test_compare_aa :-
    not(compare_units(_,a,a)).

test_compare_mkg :-
    compare_units(<,m,kg).

test_compare_mBq :-
    compare_units(>,m,'Bq').

test_qtimes :- 
    qtimes(q(42, m), q(0.5, (s ** 3) * (m ** -3)), q(21.0 , (m ** -2) * (s ** 3))).

test_qtimes_quad :-
    qtimes(q(42,m),q(0.5,m), q(21.0, m**2)).

test_qdiv_normal :- 
    qdiv(q(21,m),q(0.5,s),q(42.0, m*s** -1)).

test_qdiv_per0 :-
    not(qdiv(q(21,m),q(0,s),_)).

test_qdiv_nonquantity :-
    not(qdiv(q(21,a),q(0.5,s),_)).

test_qexpt_normale :-
    qexpt(q(2,m),4,q(16, m**4)).

test_qexpt_conexp :-
    qexpt(q(2,m**2),4,q(16, m**8)).

test_qexpt_false :-
    not(qexpt(q(2,a**2),3,_)).

test_qsum_false :- 
    not(qsum(q(42, m), q(0.5, (s ** 3) * (m ** -3)), _)).

test_symbol_ohm :- 
    siu_symbol(ohm, 'Omega').

test_symbol_dc :- 
    siu_symbol(degreecelsius, dc).

test_norm :- 
    norm((m ** -2) * 'A' * (s ** -2) * kg * (m ** 2) * ('K' ** 2) * (s ** -1), kg * (s ** -3) * 'A' * ('K' ** 2)).

test_norm_svuotato :-
    norm((m ** -2) * (m ** 2), 1).

test_qsum_prefixes :- 
    qsum(q(42, m), q(42, cm), q(42.42, m)).

test_qsum_false_2 :- 
    not(qsum(q(42,ms), q(42,kg) , _)).

test_norm_single :- 
    norm(m*m, m**2).

test_qsum_ms :-
    qsum(q(42,ms), q(42,ms), q(84, ms)).

test_qsub_negative :-
    not(qsub(q(42,m), q(44,m), _)).

test_qsub_normal :-
    qsub(q(42,ms), q(42,ms),q(0, ms)).

test_qsub_negative_prefixes :-
    not(qsub(q(42,cm), q(44,m),_)).


test_once(Predicate) :-
    write(Predicate),
    call(Predicate),
    !,
    write(": 'V'\n").

test_once(_) :-
    write(": 'X'\n").

test_all :-
    test_once(test_dimensione_m),
    test_once(test_dimensione_ms),
    test_once(test_dimensione_sm),
    test_once(test_dimensione_ma),
    test_once(test_dimensione_maz4),
    test_once(test_dimensione_ma24),
    test_once(test_dimensione_mBqs),
    test_once(test_symbol_metre),
    test_once(test_name_m),
    test_once(test_name_Bq),
    test_once(test_norm_single),
    test_once(test_compare_mm),
    test_once(test_compare_aa),
    test_once(test_compare_mkg),
    test_once(test_compare_mBq),
    test_once(test_qtimes),
    test_once(test_qtimes_quad),
    test_once(test_qdiv_normal),
    test_once(test_qdiv_per0),
    test_once(test_qdiv_nonquantity),
    test_once(test_qexpt_normale),
    test_once(test_qexpt_conexp),
    test_once(test_qexpt_false),
    test_once(test_symbol_ohm),
    test_once(test_symbol_dc),
    test_once(test_norm),
    test_once(test_norm_svuotato),
    test_once(test_qsum_prefixes),
    test_once(test_qsum_false),
    test_once(test_qsum_ms),
    test_once(test_qsub_negative),
    test_once(test_qsub_normal),
    test_once(test_qsub_negative_prefixes),
    test_once(test_qsum_false_2).
    

reloadt :- [test], tty_clear.
    
