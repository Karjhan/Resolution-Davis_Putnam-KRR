% Load knowledge base from file
load_kb(File, KB):- 
    see(File), 
    read_clauses(KB), 
    seen.

% Read clauses from file, skipping end_of_file
read_clauses([]):- 
    at_end_of_stream, 
    !.  % End of stream, return empty list
read_clauses([Clause|Rest]):- 
    \+ at_end_of_stream,
    read(Clause),
    (Clause \= end_of_file -> 
        write('Read clause: '), write(Clause), nl,  % Debugging: show each clause read
        read_clauses(Rest)
    ;   read_clauses(Rest)  % Skip end_of_file and continue reading
    ).

% Main entry point
solve_resolution(File):- 
    load_kb(File, KB), 
    write('Loaded KB: '), write(KB), nl,  % Debugging: show the loaded KB
    res(KB).

% Complementary literals (i.e., negation of each other)
complementary(Literal1, Literal2) :- 
    Literal1 = n(Literal2).
complementary(n(Literal1), Literal2) :- 
    Literal1 = Literal2.

% Choose two distinct clauses from KB
choose_clauses(KB, Clause1, Clause2) :- 
    member(Clause1, KB),
    member(Clause2, KB),
    Clause1 \= Clause2.

% Resolve two clauses and produce a resolvent
resolve(Clause1, Clause2, Resolvent) :-
    select(Literal, Clause1, Rest1),  % Select a literal from the first clause
    complementary(Literal, OppositeLiteral),  % Find its complementary literal
    select(OppositeLiteral, Clause2, Rest2),  % Select the complementary literal from the second clause
    append(Rest1, Rest2, ResolventUnsorted),  % Combine the remaining literals
    sort(ResolventUnsorted, Resolvent).  % Sort to eliminate duplicates

% Resolution process that checks for unsatisfiability
res(KB) :- 
    member([], KB),  % If an empty clause is found, return UNSATISFIABLE
    !,
    write('UNSATISFIABLE'), nl.

res(KB) :-
    choose_clauses(KB, Clause1, Clause2),  % Select two clauses for resolution
    resolve(Clause1, Clause2, Resolvent),  % Resolve the two clauses
    \+ member(Resolvent, KB),  % If the resolvent is not already in the KB
    res([Resolvent | KB]).  % Add the resolvent to the KB and continue the process

res(_) :-
    write('SATISFIABLE'), nl.  % If no empty clause is found, return SATISFIABLE
