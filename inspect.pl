
:- module(inspect, []).

:- use_module(spec_parse).


%
% Top-Level API
%

referenced_classes(ClassContents, References) :-
  referenced_class_names(ClassContents, ClassNames),
  class_name(ClassContents, ThisName),
  bagof(references(ThisName, Name), member(Name, ClassNames), References).

referenced_class_names(ClassContents, ClassNames) :-
  setof(ClassName, constant_class_name(ClassContents, ClassName), ClassNames).


constant_pool(ClassContents, ConstantPool) :-
    member(constant_pool(ConstantPool), ClassContents).

%
% Helpers
%

constant_class_name(ClassContents, ClassName) :-
  constant_pool(ClassContents, Constants),
  nth1(Index, Constants, ClassRef),
  constant_type(ClassRef, constant_class_info),
  constant_string(ClassContents, Index, ClassName).


% Return the string contents of a Utf8Constant
utf8_string(Utf8, String) :-
  member(bytes(Codes), Utf8),
  string_codes(String, Codes).


constant_type(C, Type) :-
  member(tag(T), C),
  spec_parse:cp_info_tag(Type, T).


class_name(ClassContents, Name) :-
  class_string_prop(ClassContents, this_class, Name).

super_class_name(ClassContents, Name) :-
  class_string_prop(ClassContents, super_class, Name).

%
% Find the StringValue of any class string property
class_string_prop(ClassContents, PropName, StringValue) :-
  functor(Prop, PropName, 1),
  arg(1, Prop, StringIndex),
  member(Prop, ClassContents),
  constant_string(ClassContents, StringIndex, StringValue).

%
% Follow the index references down to a bottom constant string
constant_string(ClassContents, Index, String) :-
  constant_index(ClassContents, Index, Constant),
  constant_type(Constant, constant_utf8_info),
  utf8_string(Constant, String).

constant_string(ClassContents, Index, String) :-
  constant_index(ClassContents, Index, Constant),
  \+ constant_type(Constant, constant_utf8_info),
  non_tag_member(Prop, Constant),
  arg(1, Prop, Index2),
  constant_string(ClassContents, Index2, String).

constant_index(ClassContents, Index, Constant) :-
  constant_pool(ClassContents, Constants),
  nth1(Index, Constants, Constant).

non_tag_member(Prop, Constant) :-
  member(Prop, Constant),
  \+ Prop = tag(_).