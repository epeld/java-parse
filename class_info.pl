
:- module(class_info, []).
:- use_module(library(dcg/basics)).

:- set_prolog_flag(double_quotes, codes).

class_info(ClassInfo, Package, Class) :-
    class(ClassInfo, Class),
    package(ClassInfo, Package),
    class_info(ClassInfo).

class_info([class_info, Package, Class]) :-
    forall(member(Part, Package),
           compound(Part)),
    compound(Class),

    Class = [First | _],
    is_upper(First).

is_upper(C) :-
    to_upper(C, X),
    X = C.


codified([class_info, Package, Class], String) :-
    phrase(java:fully_qualified_class(Package, Class), String).

stringified(ClassInfo, String) :-
    codified(ClassInfo, Codes),
    string_codes(String, Codes).


package([class_info, Package, _], Package).
class([class_info, _, Class], Class).


:- begin_tests(class_info).

:- set_prolog_flag(double_quotes, codes).

test(encode) :-
    class_info:stringified([class_info, ["foo", "bar", "baz"], "Hello"], _String).

test(decode) :-
    class_info:codified(_C, "foo.bar.baz.Hello"), !.

:- end_tests(class_info).
