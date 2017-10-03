
:- module(inspect, []).

:- use_module(spec_parse).


print_constants(ClassContents) :-
  constant_pool(ClassContents, Cs),
  forall(nth1(Ix, Cs, C),
         print_constant(Ix, C)).


print_constant(Ix, C) :-
  constant_type(C, Type),
  constant_summary(Type, C, Summary),
  format("~t~d~3+ ~w~t~30+~w~t~30+~n", [Ix, Type, Summary]).
  

constant_summary(constant_utf8_info, C, Summary) :-
  member(bytes(Codes), C),
  format(string(Summary), "\"~s\"", [Codes]).

constant_summary(Tag, C, Summary) :-
  \+ Tag = constant_utf8_info,
  format(string(LongSummary), "~w", [C]),
  truncated_string(LongSummary, 70, Summary).

truncated_string(LongString, Length, ShortString) :-
  Length0 is Length - 3,
  sub_string(LongString, 0, Length0, _After, ShortString0), 
  !,
  string_concat(ShortString0, "...", ShortString).

truncated_string(ShortString, _Length, ShortString).

constant_pool(ClassContents, ConstantPool) :-
    member(constant_pool(ConstantPool), ClassContents).

pretty_constants(ClassContents, Constants) :-
  constant_pool(ClassContents, Constants0),
  maplist(pretty_constant, Constants0, Constants).

pretty_constant(C0, C) :-
  member(tag(Tag), C0),
  spec_parse:cp_info_tag(Name, Tag),
  standard_constant(Name),
  functor(F, Name, 1),
  arg(1, F, C0),
  F = C.

pretty_constant(C0, C) :-
  member(tag(Tag), C0),
  spec_parse:cp_info_tag(constant_utf8_info, Tag),
  member(bytes(B), C0),
  string_codes(C, B).

standard_constant(Name) :-
  \+ Name = constant_utf8_info.

constant_type(C, Type) :-
  member(tag(T), C),
  spec_parse:cp_info_tag(Type, T).