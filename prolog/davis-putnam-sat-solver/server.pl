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
    % Read JSON request body
    http_read_json_dict(Request, Dict),
    % Extract 'path' from the JSON body
    _{path: Path} :< Dict,
    % Process the path using the solve/1 predicate
    solve_query(Path, Result),
    % Send the response as JSON
    reply_json_dict(Result).

% Use the solve/1 predicate from davis-putnam-solution.pl
solve_query(Path, Result) :-
    catch(
        % Redirect the output of solve/1 to a string
        (with_output_to(string(Output),
            (solve(Path) -> true; true)),  % Run solve/1 and capture all its output
         Result = _{status: "success", output: Output}),
        Error,
        Result = _{status: "error", message: Error}
    ).
