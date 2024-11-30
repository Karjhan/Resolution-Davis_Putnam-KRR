% The main Davis-Putnam procedure
dp([], []).  % Satisfiable if there are no clauses
dp(L, _) :-
    member([], L),  % Unsatisfiable if there's an empty clause
    !,
    fail.
dp(L, [C/true | Solution]) :-
    choose_atom(L, C),  % Choose an atom C based on a strategy
    perform_operation(L, C, true, L1),  % Assign true to C
    dp(L1, Solution).
dp(L, [C/false | Solution]) :-
    choose_atom(L, C),  % Choose an atom C based on a strategy
    perform_operation(L, C, false, L2),  % Assign false to C
    dp(L2, Solution).

% Perform the • operation
perform_operation([], _, _, []).  % Base case: no more clauses
perform_operation([Clause | Rest], Atom, Value, NewClauses) :-
    (member(Atom, Clause), Value = true ->
        % Clause satisfied, exclude it
        perform_operation(Rest, Atom, Value, NewClauses);
     member(n(Atom), Clause), Value = false ->
        % Clause satisfied by ¬Atom, exclude it
        perform_operation(Rest, Atom, Value, NewClauses);
     \+ member(Atom, Clause), \+ member(n(Atom), Clause) ->
        % Atom not in the clause, keep it unchanged
        perform_operation(Rest, Atom, Value, NewRest),
        NewClauses = [Clause | NewRest];
     % Atom conflicts, simplify the clause
     simplify_clause(Clause, Atom, Value, SimplifiedClause),
     perform_operation(Rest, Atom, Value, NewRest),
     NewClauses = [SimplifiedClause | NewRest]
    ).

% Simplify a clause by removing the literal that is false
simplify_clause(Clause, Atom, true, Simplified) :-
    delete(Clause, n(Atom), Simplified).
simplify_clause(Clause, Atom, false, Simplified) :-
    delete(Clause, Atom, Simplified).

% Choose an atom based on a strategy
choose_atom(L, Atom) :- choose_most_frequent(L, Atom).

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
    sort(2, @=<, L, [ShortestClause | _]),
    member(Atom, ShortestClause).

read_clauses(File, Clauses) :-
    open(File, read, Stream),
    read(Stream, Clauses),
    close(Stream).

solve(File) :-
    read_clauses(File, Clauses),
    (dp(Clauses, Solution) ->
        writeln('YES'),
        writeln('Solution:'),
        writeln(Solution);
     writeln('NO')).
