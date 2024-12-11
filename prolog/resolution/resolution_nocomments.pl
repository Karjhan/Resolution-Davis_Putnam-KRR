load_kb(File, KB):-
    see(File),
    read_clauses(KB),
    seen.

read_clauses([]):-
    at_end_of_stream,
    !.
read_clauses([Clause|Rest]):-
    \+ at_end_of_stream,
    read(Clause),
    (Clause \= end_of_file ->
        read_clauses(Rest)
    ;   read_clauses(Rest)
    ).

solve_resolution(File):-
    load_kb(File, KB),
    res(KB).

complementary(Literal1, Literal2) :-
    Literal1 = n(Literal2).
complementary(n(Literal1), Literal2) :-
    Literal1 = Literal2.

choose_clauses(KB, Clause1, Clause2) :-
    member(Clause1, KB),
    member(Clause2, KB),
    Clause1 \= Clause2.

resolve(Clause1, Clause2, Resolvent) :-
    select(Literal, Clause1, Rest1),
    complementary(Literal, OppositeLiteral),
    select(OppositeLiteral, Clause2, Rest2),
    append(Rest1, Rest2, ResolventUnsorted),
    sort(ResolventUnsorted, Resolvent).

res(KB) :-
    member([], KB),
    !,
    write('UNSATISFIABLE'), nl.

res(KB) :-
    choose_clauses(KB, Clause1, Clause2),
    resolve(Clause1, Clause2, Resolvent),
    \+ member(Resolvent, KB),
    res([Resolvent | KB]).

res(_) :-
    write('SATISFIABLE'), nl.
