
:- module(qualified_class, []).
:- use_module(library(dcg/basics)).

:- set_prolog_flag(double_quotes, codes).

% TODO consider introducing a predicate 'known_class'
% to distinguish existing classes

qualified_class(QualifiedClass, Package, Class) :-
    class(QualifiedClass, Class),
    package(QualifiedClass, Package),
    qualified_class(QualifiedClass).

qualified_class([qualified_class, _, [class, Class]]) :-
    Class = [First | _],
    is_upper(First).


builtin_class(QualifiedClass) :-
    builtin_class(QualifiedClass, _).

builtin_class(QualifiedClass, Class) :-
    lang_class(Class),
    lang_package(Pkg),
    qualified_class(QualifiedClass, Pkg, Class).


% TODO
codes([_X | _Rest]).


package([X | Rest]) :-
    codes(X), package(Rest).
package([]).


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


builtin(Class) -->
    { builtin_class(Class) },
    Class.

lang_package(Pkg) :-
    phrase(java:package(Pkg), "java.lang").

lang_class("String").
lang_class("Integer").

builtin_class_name(Class, Name) :-
    builtin_class(Class),
    class(Class, Name).


:- begin_tests(qualified_class).

:- set_prolog_flag(double_quotes, codes).

test(encode) :-
    qualified_class:stringified([qualified_class,
                                 [package, ["foo", "bar", "baz"]],
                                 [class, "Hello"]],
                                _String).

test(decode) :-
    qualified_class:codified(_C, "foo.bar.baz.Hello"), !.

:- end_tests(qualified_class).
