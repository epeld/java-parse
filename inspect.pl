
:- module(inspect, []).

:- use_module(spec_parse).

pretty_constants(ClassContents, Constants) :-
  member(constant_pool(Constants0), ClassContents),
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

