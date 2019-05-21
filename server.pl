% include the required libraries
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/thread_httpd)).

% include the file alexa_mod
:- use_module(alexa_mod).

% Prolog Webserver Part I

% determine on which port the application should run
server(Port) :-
  http_server(http_dispatch, [ port(Port)]).

% define in which predicate the reply should be generated and which options should be considered
:- http_handler(/, alexa, [
  methods([
    get,
    head,
    options,
    post
  ]),
  prefix
]).

% main/0 starts the server on localhost 8080
main :- server(8080).
