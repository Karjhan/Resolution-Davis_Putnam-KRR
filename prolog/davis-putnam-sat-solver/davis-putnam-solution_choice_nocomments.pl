% Main Davis-Putnam procedure
% Base case: If there are no clauses left, the formula is satisfiable.
dp([], [], _) :- !.
% If an empty clause exists, the formula is unsatisfiable.
dp(L, _, _) :-
    member([], L),
    !, fail.
% Try assigning true to the chosen atom and proceed recursively.
dp(L, [C/true | Solution], Strategy) :-
    choose_atom(L, C, Strategy),  % Choose an atom based on the strategy
    perform_operation(L, C, true, L1),  % Simplify the clauses by assigning true to the atom
    dp(L1, Solution, Strategy).
% Try assigning false to the chosen atom and proceed recursively.
dp(L, [C/false | Solution], Strategy) :-
    choose_atom(L, C, Strategy),  % Choose an atom based on the strategy
    perform_operation(L, C, false, L2),  % Simplify the clauses by assigning false to the atom
    dp(L2, Solution, Strategy).

% Perform the operation
% Base case: If there are no more clauses, return an empty list.
perform_operation([], _, _, []) :- !.
% Recursively process each clause and simplify it based on the atom's truth value.
perform_operation([Clause | Rest], Atom, Value, NewClauses) :-
    (
        % Case 1: Clause contains the atom and its value is true, so the clause is satisfied and removed.
        member(Atom, Clause), Value = true ->
        perform_operation(Rest, Atom, Value, NewClauses);
        % Case 2: Clause contains the negated atom and its value is false, so the clause is satisfied and removed.
        member(n(Atom), Clause), Value = false ->
        perform_operation(Rest, Atom, Value, NewClauses);
        % Case 3: Clause does not contain the atom, keep it unchanged.
        \+ member(Atom, Clause), \+ member(n(Atom), Clause) ->
        perform_operation(Rest, Atom, Value, NewRest),
        NewClauses = [Clause | NewRest];
        % Case 4: Clause contains the atom but it is false; simplify the clause by removing the literal.
        simplify_clause(Clause, Atom, Value, SimplifiedClause),
        perform_operation(Rest, Atom, Value, NewRest),
        NewClauses = [SimplifiedClause | NewRest]
    ).

% Simplify a clause by removing the literal that is false
simplify_clause(Clause, Atom, true, Simplified) :-
    delete(Clause, n(Atom), Simplified).  % Remove the negated atom
simplify_clause(Clause, Atom, false, Simplified) :-
    delete(Clause, Atom, Simplified).  % Remove the atom

% Choose an atom based on the given strategy
choose_atom(L, Atom, most_frequent) :-
    choose_most_frequent(L, Atom).  % Strategy: Select the most frequent atom
choose_atom(L, Atom, shortest_clause) :-
    choose_shortest_clause_atom(L, Atom).  % Strategy: Select an atom from the shortest clause

% Strategy: Choose the atom that appears in the most clauses
choose_most_frequent(L, Atom) :-
    findall(A, (member(Clause, L), member(A, Clause), \+ A = n(_)), Positives),  % All positive literals
    findall(A, (member(Clause, L), member(n(A), Clause)), Negatives),  % All negated literals
    append(Positives, Negatives, AllAtoms),  % Combine all literals
    msort(AllAtoms, SortedAtoms),  % Sort the literals
    clumped(SortedAtoms, CountedAtoms),  % Count occurrences of each literal
    sort(2, @>=, CountedAtoms, [Atom-_|_]).  % Sort by frequency and select the most frequent atom

simplify_clause(Clause, Atom, false, Simplified) :-
    delete(Clause, Atom, Simplified).  % Remove the positive literal from the clause when assigning false

% Strategy: Choose an atom that appears in the most clauses
choose_most_frequent(L, Atom) :-
    findall(A, (member(Clause, L), member(A, Clause), \+ A = n(_)), Positives),  % Collect positive literals
    findall(A, (member(Clause, L), member(n(A), Clause)), Negatives),  % Collect negated literals
    append(Positives, Negatives, AllAtoms),  % Combine all literals
    msort(AllAtoms, SortedAtoms),  % Sort literals
    clumped(SortedAtoms, CountedAtoms),  % Count occurrences of each literal
    sort(2, @>=, CountedAtoms, [Atom-_|_]).  % Sort by frequency in descending order and choose the most frequent

% Utility to group and count occurrences of atoms
clumped([], []).  % Base case: If the list is empty, return an empty count
clumped([H | T], [H-N | Clumped]) :-
    clump(H, T, N, Rest),  % Count occurrences of the first atom
    clumped(Rest, Clumped).  % Recur for the remaining list

% Count occurrences of the current atom
clump(_, [], 1, []).  % Base case: Atom appears once in the list
clump(X, [X | T], N, Rest) :-
    clump(X, T, N1, Rest),  % Recur to count more occurrences of X
    N is N1 + 1.  % Increment the count for X

clump(X, [Y | T], 1, [Y | T]) :-
    X \= Y.  % Stop counting if a different atom is found

% Strategy: Choose an atom from the shortest clause
choose_shortest_clause_atom(L, Atom) :-
    ( member([], L) ->  % If an empty clause exists, the formula is unsatisfiable
        fail;
    sort(2, @=<, L, [ShortestClause | _]),  % Sort clauses by length and pick the shortest
    member(Atom, ShortestClause)  % Select any atom from the shortest clause
    ).

% Read clauses from a file
read_clauses(File, Clauses) :-
    open(File, read, Stream),  % Open the file for reading
    read(Stream, Clauses),  % Parse the clauses from the file
    close(Stream).  % Close the file after reading

% Top-level predicate: Solve a CNF formula with a selected strategy
solve(File, 1) :-
    Strategy = most_frequent,  % Select the most frequent atom strategy
    read_clauses(File, Clauses),  % Load the CNF formula from the file
    (dp(Clauses, Solution, Strategy) ->  % Try solving the formula
        writeln('YES'),  % Output "YES" if it is satisfiable
        writeln('Solution:'),
        writeln(Solution);  % Print the satisfying assignments
     writeln('NO')).  % Output "NO" if it is unsatisfiable

solve(File, 0) :-
    Strategy = shortest_clause,  % Select the shortest clause strategy
    read_clauses(File, Clauses),  % Load the CNF formula from the file
    (dp(Clauses, Solution, Strategy) ->  % Try solving the formula
        writeln('YES'),  % Output "YES" if it is satisfiable
        writeln('Solution:'),
        writeln(Solution);  % Print the satisfying assignments
     writeln('NO')).  % Output "NO" if it is unsatisfiable

solve(_, _) :-
    writeln('Invalid strategy. Use 1 for most frequent or 0 for shortest clause.'),
    fail.  % Output an error message for invalid strategy inputs
