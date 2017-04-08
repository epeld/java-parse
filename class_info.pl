
:- module(qualified_class, []).
:- use_module(library(dcg/basics)).

:- set_prolog_flag(double_quotes, codes).

qualified_class(QualifiedClass, Package, Class) :-
    class(QualifiedClass, Class),
    package(QualifiedClass, Package),
    qualified_class(QualifiedClass).

qualified_class([qualified_class, Package, Class]) :-
    forall(member(Part, Package),
           compound(Part)),
    compound(Class),

    Class = [First | _],
    is_upper(First).

is_upper(C) :-
    to_upper(C, X),
    X = C.


codified([qualified_class, Package, Class], String) :-
    phrase(java:fully_qualified_class(Package, Class), String).

stringified(QualifiedClass, String) :-
    codified(QualifiedClass, Codes),
    string_codes(String, Codes).


package([qualified_class, Package, _], Package).
class([qualified_class, _, Class], Class).


:- begin_tests(qualified_class).

:- set_prolog_flag(double_quotes, codes).

test(encode) :-
    qualified_class:stringified([qualified_class, ["foo", "bar", "baz"], "Hello"], _String).

test(decode) :-
    qualified_class:codified(_C, "foo.bar.baz.Hello"), !.

:- end_tests(qualified_class).
