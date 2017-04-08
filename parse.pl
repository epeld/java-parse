
:- module(parse_utils, [upper_char//1, lower_word//1]).
:- use_module(library(dcg/basics)).

:- set_prolog_flag(double_quotes, codes).

upper_char(C, [C | Rest], Rest) :-
    ground(C), member(C, "ABCDEFGHIJKLMNOPQRSTUVXYZ"), !.

upper_char(C, [C | Rest], Rest) :-
    member(C, "ABCDEFGHIJKLMNOPQRSTUVXYZ").


lower_word(Word) --> 
    string_without(",.; \t-_!{}()<>\n", Word),
    {
        Word = [_ | _Rest],
        string_chars(String, Word),
        string_lower(String, String)
    }.

interspersed([], _, []).
interspersed([X], _, [X]) :- !.
interspersed([X, X2 | XRest], Sep, [ X, Sep | YRest ]) :-
    interspersed([X2 | XRest], Sep, YRest).


remainder(X, X, []).

:- begin_tests(parse_utils).

:- set_prolog_flag(double_quotes, codes).

test(upper_char) :-
    phrase(upper_char(C), "C"),
    [C] = "C".


test(test_file) :-
    phrase_from_file((lower_word(X), blanks_to_nl), "hello.txt"),
    X = "hello".

test(strings_are_codes) :-
    atom_codes('hello', Codes),
    Codes = "hello".

test(lower_word) :-
    phrase(parse_utils:lower_word(W), "hello"),

    W = "hello".

:- end_tests(parse_utils).
