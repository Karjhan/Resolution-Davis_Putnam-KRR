:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).

:- ensure_loaded('davis-putnam-sat-solver/davis-putnam-solution_choice.pl').
:- ensure_loaded('resolution/resolution.pl').

:- http_handler('/solve-davis-putnam', handle_solve, []).
:- http_handler('/solve-resolution', handle_solve_resolution, []).

server(Port) :-
    http_server(http_dispatch, [port(Port)]).

handle_solve(Request) :-
    http_read_json_dict(Request, Dict),
    _{path: Path, strategy: Strategy} :< Dict,
    solve_query(Path, Strategy, Result),
    reply_json_dict(Result).

solve_query(Path, Strategy, Result) :-
    catch(
        (with_output_to(string(Output),
            (solve(Path, Strategy) -> true; true)), 
         Result = _{status: "success", output: Output}),
        Error,
        Result = _{status: "error", message: Error}
    ).

handle_solve_resolution(Request) :-
    http_read_json_dict(Request, Dict),
    _{path: Path} :< Dict,
    atom_string(AtomPath, Path), 
    solve_resolution_query(AtomPath, Result),
    reply_json_dict(Result).

solve_resolution_query(Path, Result) :-
    catch(
        (with_output_to(string(Output),
            (solve_resolution(Path) -> true; true)),
         Result = _{status: "success", output: Output}),
        Error,
        Result = _{status: "error", message: Error}
    ).
