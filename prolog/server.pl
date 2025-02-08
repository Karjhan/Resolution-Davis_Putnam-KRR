:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).

% Load the Davis-Putnam solution logic
:- ensure_loaded('davis-putnam-sat-solver/davis-putnam-solution_choice.pl').
:- ensure_loaded('resolution/resolution.pl').

% Define the HTTP entry point
:- http_handler('/solve-davis-putnam', handle_solve, []).
:- http_handler('/solve-resolution', handle_solve_resolution, []).

% Start the server
server(Port) :-
    http_server(http_dispatch, [port(Port)]).

% Handle the "solve" query
handle_solve(Request) :-
    http_read_json_dict(Request, Dict),
    % Extract 'path' and 'strategy' from the JSON body
    _{path: Path, strategy: Strategy} :< Dict,
    % Process the path and strategy using the solve/2 predicate
    solve_query(Path, Strategy, Result),
    % Send the response as JSON
    reply_json_dict(Result).

% Use the solve/2 predicate from davis-putnam-solution_choice.pl
solve_query(Path, Strategy, Result) :-
    catch(
        % Redirect the output of solve/2 to a string
        (with_output_to(string(Output),
            (solve(Path, Strategy) -> true; true)), 
         Result = _{status: "success", output: Output}),
        Error,
        Result = _{status: "error", message: Error}
    ).

% Handle the "solve-resolution" query
handle_solve_resolution(Request) :-
    http_read_json_dict(Request, Dict),
    % Extract 'path' from the JSON body
    _{path: Path} :< Dict,
    atom_string(AtomPath, Path), 
    solve_resolution_query(AtomPath, Result),
    % Send the response as JSON
    reply_json_dict(Result).

% Solve resolution query
solve_resolution_query(Path, Result) :-
    catch(
        % Redirect the output of solve_resolution/1 to a string
        (with_output_to(string(Output),
            (solve_resolution(Path) -> true; true)),
         Result = _{status: "success", output: Output}),
        Error,
        Result = _{status: "error", message: Error}
    ).
