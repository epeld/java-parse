
:- module(spec_parse, [test1/1, test2/1]).

:- use_module([library(http/dcg_basics)]).

:- set_prolog_flag(double_quotes, codes).

%
% Field Parsing
%
test2(Spec) :-
  phrase_from_file(spec(Spec), "./classfile.spec"),
  format("Spec read ~n"),
  phrase_from_file(parse_field(Spec, field(foobar, classfile), [], R), "./Hello.class", [type(binary)]).

parse_fields(Specs, [Field | Fields], ResultsBefore, [Result | ResultsBefore], OctetsBefore, OctetsAfter) :-
  parse_field(Specs, Field, ResultsBefore, Result, OctetsBefore, OctetsAfter0),
  parse_fields(Specs, Fields, [ Result | ResultsBefore], ResultsBefore, OctetsAfter0, OctetsAfter).

parse_field(_Specs, field(Identifier, Parser), _, Result, OctetsBefore, OctetsAfter) :-
  primitive(Parser),
  call(Parser, R, OctetsBefore, OctetsAfter),
  parse_result(Identifier, R, Result).

parse_field(Specs, field(Identifier, Parser), _, Result, OctetsBefore, OctetsAfter) :-
  spec_fields(Specs, Parser, Fields),
  parse_fields(Specs, Fields, [], R, OctetsBefore, OctetsAfter),
  parse_result(Identifier, R, Result).


spec_fields(Specs, Parser, Fields) :-
  member(spec(Parser, Fields), Specs), !.

spec_fields(_Specs, Parser, _) :-
  throw(spec_not_found(Parser)).

parse_result(Identifier, Result, Term) :-
  functor(Term, Identifier, 1),
  arg(1, Term, Result).


test1(Spec) :-
  phrase(spec(Spec), "CONSTANT_Class_info {
    u1 tag;
    u2 name_index;
}").


%
% Spec Parsing
%
spec(spec(Name, Fields)) -->
  identifier(Name),
  " {",
  field_specs(Fields),
  "\n", whites, 
  "}", whites.


field_spec(field(Identifier, ParserType)) -->
  "\n", whites,
  parser(ParserType),
  whites,
  identifier(Identifier),
  ";".

field_specs([]) --> [].

field_specs([FieldSpec | Rest]) -->
  field_spec(FieldSpec),
  field_specs(Rest).



parser(ParserType) -->
  nonblanks(CParserType),
  {
   CParserType = [_Char | _Rest],
   atom_codes(ParserType, CParserType)
  }.

identifier(Identifier) -->
  string_without("; \n\t", CIdentifier),
  {
   CIdentifier = [_C | _Rest],
   atom_codes(UIdentifier, CIdentifier),
   downcase_atom(UIdentifier, Identifier)
  }.