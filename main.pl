
hello :-
  parse_test_file(ClassProps),
  
  Version = version(_Major, _Minor),
  member(Version, ClassProps),
  format("~w~n~n", [Version]),
  
  Constants = constants(Consts),
  member(Constants, ClassProps),
  format("Constants: ~n---~n"),
  forall(nth1(Ix, Consts, C),
         (
          format("~a\t", [Ix]),
          portray_constant(C),
          format("~n", [C])
         )),
  format("---~n~n"),
  
  format("Interfaces: ~n---~n"),
  Interfaces = interfaces(Ifs),
  member(Interfaces, ClassProps),
  forall(member(If, Ifs),
         format("~w~n", [If])),
  
  !.


portray_constant(utf8(S)) :- !,
  format("\"~a\"", [S]).

portray_constant(C) :-
  write(C).