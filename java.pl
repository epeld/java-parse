:- module(java, []).
:- use_module(library(dcg/basics)).

:- set_prolog_flag(double_quotes, codes).

import(Package, Class) -->
    "import ", package(Package), ".", class(Class), ";".

import([qualified_class, Package, Class]) -->
    import(Package, Class).

package_declaration(Package) --> 
    "package ", blanks,
    package(Package), blanks,
    ";".

fully_qualified_class(QualifiedClass) -->
    fully_qualified_class(Pkg, Class),
    { qualified_class:qualified_class(QualifiedClass, Pkg, Class) }.

fully_qualified_class(Package, Class) -->
    package(Package), ".", class(Class).

package([package, Package])        -->
    packageL(Package).

packageL([Something | Rest]) -->
    parse_utils:lower_word(Something),
    sub_packageL(Rest).


sub_packageL([]) --> [].
sub_packageL([Sub1 | Rest]) -->
    ".",
    packageL([Sub1 | Rest]).


java_class(Class) --> class(Class).
java_class(Class) --> fully_qualified_class(Class).

class([class, [C | Rest]]) -->
    parse_utils:upper_char(C),
    parse_utils:lower_word(Rest).


import_list([]) --> [].
import_list([QualifiedClass | Imports]) -->
    import(QualifiedClass),
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

file(ClassInfo) -->
    {
        class_info:class_info(ClassInfo, QualifiedClass, Imports),
        
        qualified_class:class(QualifiedClass, Class),
        qualified_class:package(QualifiedClass, Package)
    },
    file_header(Package, Imports),
    class_declaration(Class, _Visibility).


class_declaration(Name, Visibility) -->
    visibility(Visibility), blank, "class", blank, class(Name), blank, class_body(_).

class_body(_) --> parse_utils:remainder(_). % TODO

visibility(public) --> "public".
visibility(protected) --> "protected".
visibility(private) --> "private".

class_level(class) --> "static".

myblank --> " ".
myblanks --> [].
myblanks --> myblank, myblanks.

% We want to accept many blanks when parsing,
% but output only none when encoding
% NOTE: put extrablanks --> myblanks
% for purity..
extrablanks(Before, After) :-
    ( nonvar(Before), var(After), myblanks(Before, After) ;
      (var(Before) ; nonvar(After) ), ( Before = After )).
%extrablanks --> myblanks.

class_member([class_member, Name, Type, Visibility, ClassLevel]) -->
    ( visibility(Visibility),
      myblank, extrablanks ;
      { Visibility = none } ),
    
    ( class_level(ClassLevel),
      myblank, extrablanks ;
      { ClassLevel = instance } ),
    
    java_type(Type), myblank, extrablanks,
    name(Name), extrablanks, ";".


initial_value_assignment(_) -->
    blank,
    "=",
    blank,
    % TODO:
    string_without(";").
  

name(Name) -->
    { Name = [_ | _Rest] },
    string_without(",; .-(){}[]", Name).

simple_java_type(int) --> "int".
simple_java_type(boolean) --> "boolean".
simple_java_type(void) --> "void".

simple_java_type(QualifiedClass) -->
    fully_qualified_class(QualifiedClass).

simple_java_type(Class) -->
    class(Class).

composite_java_type(arrayOf(Type), 0) -->
    simple_java_type(Type), "[]".

composite_java_type(arrayOf(Type), N) -->
    { between(1, 10, N), succ(N0, N) },
    composite_java_type(Type, N0), "[]".

generic_java_class([generic_class, Class, [params, Params]]) -->
    java_class(Class), "<", generic_params(Params), ">".

generic_java_class([generic_class, Class, [params, implied]]) -->
    java_class(Class), "<>".

generic_java_class([generic_class, Class, [params, []]]) -->
    java_class(Class).

generic_params([params, [Param]]) -->
    generic_java_class(Param).

generic_params([params, [Param1, Param2 | Rest]]) -->
    generic_java_class(Param1),
    ",", myblank, extrablanks,
    generic_params([Param2 | Rest]).
    


java_type(Type) --> simple_java_type(Type).
java_type(Type) --> composite_java_type(Type, _).


mutable(mutable) --> [].
final(final) --> "final".

argument([argument, Name, Type, Mutability]) -->
    ( { Mutability = mutable } ;
      final(Mutability), myblank, extrablanks ),
    java_type(Type), myblank, extrablanks, name(Name).


argument_list(Args) --> "(", extrablanks, arguments(Args), ")".

arguments([]) --> [].
arguments([Arg]) --> argument(Arg).
arguments([Arg, Arg2 | Args]) -->
    argument(Arg), extrablanks, ",", extrablanks, arguments([Arg2 | Args]).


method([method, Name, Type, ClassLevel, Mutability, Visibility, Arguments, Body, Throws]) -->
    ( visibility(Visibility),
      blank, extrablanks ;
      { Visibility = package } ),

    ( class_level(ClassLevel),
      blank, extrablanks ;
      { ClassLevel = instance } ),
    
    ( final(Mutability),
      blank, extrablanks ;
      mutable(Mutability) ),
    
    java_type(Type),
    blank, extrablanks,
    name(Name),
    extrablanks,
    argument_list(Arguments),
    blank, extrablanks,
    
    ( throws_declaration(Throws),
      blank, extrablanks ;
      { Throws = [] } ),
    
    block(Body).

throws_declaration([throws, "Exception"]) --> "throws Exception".
    


block([block, Text]) -->
    "{", block_body(Text), "}".

block_body([Text]) -->
    string_without("{}", Text).

block_body([Text, InnerBlock | More]) -->
    string_without("{}", Text),
    ( block(InnerBlock), block_body(More) ;
      { More = [] } ).
     

:- begin_tests(java_parsing).

:- set_prolog_flag(double_quotes, codes).

test(file_header) :-
    phrase_from_file(file(ClassInfo), "Foo.java"),

    class_info:class(ClassInfo, QualifiedClass),
    
    qualified_class:qualified_class(Integer, [package, ["java", "lang"]], [class, "Integer"]),
    qualified_class:qualified_class(List, [package, ["java", "lang"]], [class, "Integer"]),
    qualified_class:qualified_class(Frame, [package, ["java", "lang"]], [class, "Integer"]),

    class_info:imports_class(ClassInfo, Integer),
    class_info:imports_class(ClassInfo, List),
    class_info:imports_class(ClassInfo, Frame),
    
    qualified_class:package(QualifiedClass, [package, ["foo", "bar", "baz"]]),
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
    phrase(class([class, W]), "Foo"),

    W = "Foo".

test(import, [nondet]) :- 
    phrase(import(A, B), "import foo.bar.Baz;"),
                                  
    A = [package, ["foo", "bar"]],
    B = [class, "Baz"].

test(package, [nondet]) :-
    phrase(package(A), "foo.bar"),
    A = [package, ["foo", "bar"]].


test(line_comment) :-
    phrase(line_comment("Hello"), "//Hello").

test(blanks) :-
    phrase(blanks, "").

test(blanks2) :-
    phrase((blanks, "hello", blanks), " hello").

test(class_member) :-
    phrase(class_member(_), "public   int  hello ;"), !.

test(class_member2) :-
    phrase(class_member(_), "java.lang.Integer   hello ;"), !.

test(class_member3) :-
    phrase(class_member(_), "static int   hello ;"), !.

test(class_member4) :-
    phrase(class_member(_), "private static String   hello ;"), !.

test(class_member5) :-
    phrase(java:class_member([class_member, "testing", int, protected, instance]), _S), !.

test(class_member6) :-
    phrase(java:class_member(_X), "protected int testing;"), !.

test(method) :-
    phrase(java:method(_X), "protected void superfn(int arg1, int arg2) { return 3; }"), !.

test(method3) :-
    phrase(java:method(_X), "void superfn(boolean[] foo, boolean fafa) throws Exception { if {return 3;} else {} }"), !.

test(method2) :-
    phrase(java:method(_X), "final int superfn2(boolean flag, boolean[] flags) throws Exception { if(flag) {return 3;} else {return 1;} }"), !.


test(method4) :-
    phrase(java:method(_X), "protected static final void superfn() { return 3; }"), !.

test(argument_list1) :-
    phrase(java:argument_list(_X), "(int arg1, int arg2)"), !.

test(block) :-
    phrase(java:block(_X), "{ return 3; }"), !.


test(argument_list) :-
    phrase(java:argument_list(_Args), "(int myint_3, final boolean aFlag, String[] arrayOfStrings)"), !.

test(arg) :-
    phrase(java:argument(_Arg), "int myint_3"), !.

test(arg2) :-
    phrase(java:argument(_Arg), "final boolean aFlag"), !.

test(arg3) :-
    phrase(java:argument(_Arg), "String[] array"), !.

test(generics) :-
    phrase(java:generic_java_class(_X), "java.util.List<String, java.lang.Integer>"), !.

test(generics) :-
    phrase(java:generic_java_class(_X), "java.util.List<>"), !.


:- end_tests(java_parsing).
