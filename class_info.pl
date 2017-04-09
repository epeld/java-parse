
:- module(class_info, []).
:- use_module(library(dcg/basics)).

:- set_prolog_flag(double_quotes, codes).


class_info([class_info, QualifiedClass, Imports], QualifiedClass, Imports).
    %qualified_class:qualified_class(QualifiedClass),
    %maplist(qualified_class:qualified_class, Imports).


class([class_info, Class | _], Class).

imports_list([class_info, _, Imports | _], Imports).

imports_class(ClassInfo, Import) :-
    imports_list(ClassInfo, Imports),
    member(Import, Imports).

codified(ClassInfo, Codes) :-
    phrase(java:class_info(ClassInfo), Codes).

stringified(ClassInfo, String) :-
    codified(ClassInfo, Codes),
    string_codes(String, Codes).
