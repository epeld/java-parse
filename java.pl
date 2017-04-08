:- module(java, []).
:- use_module(library(dcg/basics)).

:- set_prolog_flag(double_quotes, codes).

import(Package, Class) -->
    "import ", package(Package), ".", class(Class), ";".

import(ClassInfo) -->
    { class_info:class(ClassInfo, Class), class_info:package(ClassInfo, Package) },
    import(Package, Class).

package_declaration(Package) --> 
    "package ", blanks,
    package(Package), blanks,
    ";".

fully_qualified_class(Package, Class) -->
    package(Package), ".", class(Class).

package([Something | Rest])        -->
    parse_utils:lower_word(Something),
    sub_package(Rest).


sub_package([]) --> [].
sub_package([Sub1 | Rest]) -->
    ".",
    package([Sub1 | Rest]).



class([C | Rest]) -->
    parse_utils:upper_char(C),
    parse_utils:lower_word(Rest).


import_list([]) --> [].
import_list([ClassInfo | Imports]) -->
    import(ClassInfo),
    java_blank,
    import_list(Imports).

line_comment(Text) -->
    blanks,
    "//",
    string_without("\n", Text),
    blanks_to_nl.

java_blank() -->
    blanks,
    (line_comment(_), java_blank; { true } ).

file_header(Package, Imports) -->
    package_declaration(Package),
    java_blank,
    import_list(Imports),
    java_blank.

file(ClassInfo, Imports) -->
    {
        class_info:class(ClassInfo, Class),
        class_info:package(ClassInfo, Package)
    },
    file_header(Package, Imports),
    class_declaration(Class, _Visibility).


class_declaration(Name, Visibility) -->
    visibility(Visibility), blank, "class", blank, class(Name), blank, class_body(_).

class_body(_) --> parse_utils:remainder(_). % TODO

visibility(public) --> "public".
visibility(protected) --> "protected".
visibility(private) --> "private".
visibility(package) --> "".


class_member() -->
    visibility(_), blank, java_type(_), blank(), ";".


initial_value_assignment(_) -->
    blank,
    "=",
    blank,
    % TODO:
    string_without(";").

% Higher order DCG parser. Optionally uses its first argument to parse,
% Otherwise succeeds without consuming any input
optional(Pred, Before, After) :-
    (
        call(Pred, Before, After);
        Before = After
    ).
    

name(Name) -->
    string_without("; .-(){}[],", "", Name).

java_type(int) --> "int".
java_type(boolean) --> "boolean".
java_type(void) --> "void".
java_type(BuiltinClass) --> builtin_class(BuiltinClass).

builtin_class(Class) :-
    builtin_class(Class, _).

builtin_class("String", ["java", "lang"]).
builtin_class("Integer", ["java", "lang"]).

:- begin_tests(java_parsing).

:- set_prolog_flag(double_quotes, codes).

test(file_header) :-
    phrase_from_file(file(ClassInfo, Imports), "Foo.java"),
    
    class_info:class_info(Integer, ["java", "lang"], "Integer"),
    class_info:class_info(List, ["java", "lang"], "Integer"),
    class_info:class_info(Frame, ["java", "lang"], "Integer"),
    
    member(Integer, Imports),
    member(List, Imports),
    member(Frame, Imports),
    
    class_info:package(ClassInfo, ["foo", "bar", "baz"]),
    !.

test(import_list) :-
    phrase(import_list(_I), "import foo.bar.Baz; \nimport gaga.momo.Foo;"), !.

test(package_declaration) :-
    phrase(package_declaration(_I), "package fafa.googo;"), !.

test(file_header2) :-
    phrase(file_header(_I, _X), "package fafa.googo; \n \n  import mama.fafa.Boo;"),
    !.


test(blanks_to_nl) :-
    phrase(blanks_to_nl, "").

test(class) :-
    phrase(class(W), "Foo"),

    W = "Foo".

test(import, [nondet]) :- 
    phrase(import(A, B), "import foo.bar.Baz;"),
                                  
    A = ["foo", "bar"],
    B = "Baz".

test(package, [nondet]) :-
    phrase(package(A), "foo.bar"),
    A = ["foo", "bar"].


test(line_comment) :-
    phrase(line_comment("Hello"), "//Hello").

test(blanks) :-
    phrase(blanks, "").

test(blanks2) :-
    phrase((blanks, "hello", blanks), " hello").

:- end_tests(java_parsing).
