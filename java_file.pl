:- module(java_file, []).

:- set_prolog_flag(double_quotes, codes).

without_file_suffix(File, Name) :-
    file_base_name(File, Base),
    split_string(Base, ".", "", [Name | _]).


expected_class_name(File, Class) :-
    without_file_suffix(File, Class),
    string_codes(Class, Codes),
    [C | _] = Codes,
    member(C, "ABCDEFGHIJKLMNOPQRSTUVXYZ"), !.


read_file(File) :-
    atom(File),
    absolute_file_name(File, AbsFile),
    
    phrase_from_file(java:file(QualifiedClass, Imports), AbsFile ),
    
    verify_class_names(QualifiedClass, AbsFile),

    atom_string(AbsFile, SAbsFile),
    qualified_class:stringified(QualifiedClass, S),

    write_term(class_definition(S, SAbsFile), [nl(true)]),
    write_imports(Imports, QualifiedClass), !.


verify_class_names(QualifiedClass, AbsFile) :-
    expected_class_name(AbsFile, Expected),
    qualified_class:class(QualifiedClass, Real),

    ( atom_codes(Expected, Real), ! ;
      
      atom_codes(SReal, Real),
      atom_codes(SExpected, Expected),
    
      write_term(warning:class_names_differ(SExpected, SReal, AbsFile), [nl(true)])
    ).

write_imports(Imports, QualifiedClass) :-
    qualified_class:stringified(QualifiedClass, Str),
    forall(member(Import, Imports),
           (
               qualified_class:stringified(Import, S),
               write_term(imports(Str, S), [nl(true)])
           )).
