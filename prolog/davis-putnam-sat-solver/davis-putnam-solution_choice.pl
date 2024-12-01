% Main Davis-Putnam procedure
dp([], [], _) :- !.
dp(L, _, _) :-
    member([], L),  % Unsatisfiable if there's an empty clause
    !, fail.
dp(L, [C/true | Solution], Strategy) :-
    choose_atom(L, C, Strategy), 
    perform_operation(L, C, true, L1),  % Assign true to C
    dp(L1, Solution, Strategy).
dp(L, [C/false | Solution], Strategy) :-
    choose_atom(L, C, Strategy),  
    perform_operation(L, C, false, L2),  % Assign false to C
    dp(L2, Solution, Strategy).

% Perform the operation
perform_operation([], _, _, []) :- !.
perform_operation([Clause | Rest], Atom, Value, NewClauses) :-
    (member(Atom, Clause), Value = true ->
        perform_operation(Rest, Atom, Value, NewClauses);
     member(n(Atom), Clause), Value = false ->
        perform_operation(Rest, Atom, Value, NewClauses);
     \+ member(Atom, Clause), \+ member(n(Atom), Clause) ->
        perform_operation(Rest, Atom, Value, NewRest),
        NewClauses = [Clause | NewRest];
     simplify_clause(Clause, Atom, Value, SimplifiedClause),
     perform_operation(Rest, Atom, Value, NewRest),
     NewClauses = [SimplifiedClause | NewRest]
    ).

% Simplify a clause by removing the literal that is false
simplify_clause(Clause, Atom, true, Simplified) :-
    delete(Clause, n(Atom), Simplified).
simplify_clause(Clause, Atom, false, Simplified) :-
    delete(Clause, Atom, Simplified).

% Choose an atom based on the given strategy
choose_atom(L, Atom, most_frequent) :-
    choose_most_frequent(L, Atom).
choose_atom(L, Atom, shortest_clause) :-
    choose_shortest_clause_atom(L, Atom).

% Strategy: Choose the atom that appears in the most clauses
choose_most_frequent(L, Atom) :-
    findall(A, (member(Clause, L), member(A, Clause), \+ A = n(_)), Positives),
    findall(A, (member(Clause, L), member(n(A), Clause)), Negatives),
    append(Positives, Negatives, AllAtoms),
    msort(AllAtoms, SortedAtoms),
    clumped(SortedAtoms, CountedAtoms),
    sort(2, @>=, CountedAtoms, [Atom-_|_]).

% Utility to count occurrences
clumped([], []).
clumped([H | T], [H-N | Clumped]) :-
    clump(H, T, N, Rest),
    clumped(Rest, Clumped).

clump(_, [], 1, []).
clump(X, [X | T], N, Rest) :-
    clump(X, T, N1, Rest),
    N is N1 + 1.
clump(X, [Y | T], 1, [Y | T]) :-
    X \= Y.

% Strategy: Choose the atom that appears in the shortest clause
choose_shortest_clause_atom(L, Atom) :-
    ( member([], L) -> 
        fail  % If an empty clause is found, the formula is unsatisfiable
    ; sort(2, @=<, L, [ShortestClause | _]),
      member(Atom, ShortestClause)
    ).

% Read clauses from a file
read_clauses(File, Clauses) :-
    open(File, read, Stream),
    read(Stream, Clauses),
    close(Stream).

% Top-level predicate with strategy selection
solve(File, 1) :-
    Strategy = most_frequent,
    read_clauses(File, Clauses),
    (dp(Clauses, Solution, Strategy) ->
        writeln('YES'),
        writeln('Solution:'),
        writeln(Solution);
     writeln('NO')).

solve(File, 0) :-
    Strategy = shortest_clause,
    read_clauses(File, Clauses),
    (dp(Clauses, Solution, Strategy) ->
        writeln('YES'),
        writeln('Solution:'),
        writeln(Solution);
     writeln('NO')).

solve(_, _) :-
    writeln('Invalid strategy. Use 1 for most frequent or 0 for shortest clause.'),
    fail.
