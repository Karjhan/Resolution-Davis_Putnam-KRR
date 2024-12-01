:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).

% Load the Davis-Putnam solution logic
:- ensure_loaded('davis-putnam-solution_choice.pl').

% Define the HTTP entry point
:- http_handler('/solve', handle_solve, []).

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
            (solve(Path, Strategy) -> true; true)),  % Run solve/2 and capture all its output
         Result = _{status: "success", output: Output}),
        Error,
        Result = _{status: "error", message: Error}
    ).
