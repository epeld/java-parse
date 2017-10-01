
:- module(spec_parse, [test1/1, test2/1]).

:- use_module([library(http/dcg_basics)]).
:- use_module([parse_primitives]).

:- set_prolog_flag(double_quotes, codes).

%
% Field Parsing
%
test2(Spec) :-
  format("Reading spec..~n"),
  phrase_from_file(specs(Spec), "./full.spec"),
  format("- OK ~n"),
  phrase_from_file(parse_field(Spec, field(foobar, classfile), [], R), "./Hello.class", [type(binary)]).

parse_fields(Specs, [Field], ResultsBefore, [Result], OctetsBefore, OctetsAfter) :-
  parse_field(Specs, Field, ResultsBefore, Result, OctetsBefore, OctetsAfter).


parse_fields(Specs, [Field | Fields], ResultsBefore, [Result | Rest], OctetsBefore, OctetsAfter) :-
  parse_field_or_fail(Specs, Field, ResultsBefore, Result, OctetsBefore, OctetsAfter0),
  parse_fields(Specs, Fields, [Result | ResultsBefore], Rest, OctetsAfter0, OctetsAfter).


parse_field_or_fail(Specs, Field, ResultsBefore, Result, OctetsBefore, OctetsAfter) :-
  ( parse_field(Specs, Field, ResultsBefore, Result, OctetsBefore, OctetsAfter) *->
    true
  ; throw(parser_failed(Field)) ).


parse_field(Specs, field(Identifier, cp_info, Atom - 1), Prev, Result, OctetsBefore, OctetsAfter) :-
  functor(F, Atom, 1),
  member(F, Prev),
  arg(1, F, Count1),
  Count is Count1 - 1,
  true. % TODO

parse_field(Specs, field(Identifier, cp_info), _, Result, OctetsBefore, OctetsAfter) :-
  cp_info_parser(Parser),
  parse_field_or_fail(Specs, field(Identifier, Parser), _, Result, OctetsBefore, OctetsAfter).

parse_field(_Specs, field(Identifier, Parser), _, Result, OctetsBefore, OctetsAfter) :-
  primitive(Parser),
  format("Running ~a (~a)", [Parser, Identifier]),
  call(Parser, R, OctetsBefore, OctetsAfter),
  format(" - OK ~n"),
  parse_result(Identifier, R, Result).

parse_field(Specs, field(Identifier, Parser), _, Result, OctetsBefore, OctetsAfter) :-
  \+ primitive(Parser),
  spec_fields(Specs, Parser, Fields),
  parse_fields(Specs, Fields, [], R, OctetsBefore, OctetsAfter),
  parse_result(Identifier, R, Result).

cp_info_parser(Atom) :- cp_info_tag(Atom, _Tag).
cp_info_tag(constant_class_info, 7).
cp_info_tag(constant_fieldref_info, 9).
cp_info_tag(constant_methodref_info, 10).
cp_info_tag(constant_interfacemethodref_info, 11).
cp_info_tag(constant_string_info, 8).
cp_info_tag(constant_integer_info, 3).
cp_info_tag(constant_float_info, 4).
cp_info_tag(constant_long_info, 5).
cp_info_tag(constant_double_info, 6).
cp_info_tag(constant_nameandtype_info, 12).
cp_info_tag(constant_utf8_info, 1).
cp_info_tag(constant_methodhandle_info, 15).
cp_info_tag(constant_methodtype_info, 16).
cp_info_tag(constant_invokedynamic_info, 18).


spec_fields(Specs, Parser, Fields) :-
  member(spec(Parser, Fields), Specs).

spec_fields(Specs, Parser, _) :-
  \+ member(spec(Parser, _), Specs),
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
  "}", whites, "\n".

specs([Spec | Specs]) -->
  spec(Spec),
  {
   Spec = spec(Name, _Fields),
   format("Read ~a~n", [Name])
   },
  blanks,
  specs(Specs), blanks.

specs([]) --> [].

field_spec(field(Identifier, ParserType)) -->
  "\n", whites,
  parser(ParserType),
  whites,
  identifier(Identifier),
  ";".

field_spec(field(Identifier, ParserType, CountTerm)) -->
  "\n", whites,
  parser(ParserType),
  whites,
  identifier(Identifier),
  "[",
  ( string_without("-]", S), { atom_codes(CountTerm, S) }
  ; string_without("-]", S1), "-1", { atom_codes(A1, S1), CountTerm = A1 - 1 }),
  "];".

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
  string_without("[; \n\t", CIdentifier),
  {
   CIdentifier = [_C | _Rest],
   atom_codes(UIdentifier, CIdentifier),
   downcase_atom(UIdentifier, Identifier)
  }.