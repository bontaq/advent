% Top level things are called "predicates"

:- use_module(library(pure_input)).
:- use_module(library(dcg/basics)).

input([X|Data]) -->
    integer(X),
    "\n",
    input(Data).

input([]) --> eos.

load_data(Data) :-
    open('./data.txt', read, Stream),
    phrase_from_stream(input(Data), Stream).

relate([]).
relate([_]).
relate([A, B|Data]) :-
    % store this relationship into the database
    assert(parent(A, B)),
    relate([B|Data]).

part_one :-
    load_data(Numbers),
    relate(Numbers),
    % queries the database
    findall(X, (parent(X, Y), X < Y), Result), length(Result, Out),
    format("Count: ~p ~n", [Out]).

triple(Child, Result) :-
    % ! means cut, which stops backtracking.
    % I think it was finding alternative chains up,
    % which was giving results like 172,000 for
    % the final count.
    parent(B, Child), !,
    parent(C, B), !,
    Result is Child + B + C.

part_two :-
    findall(
        X,
        (parent(X, Y),
         triple(X, X1),
         triple(Y, Y1),
         X1 < Y1),
        Result
    ),
    length(Result, Out),
    format("Count: ~p ~n", [Out]).
