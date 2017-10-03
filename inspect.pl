
:- module(inspect, []).

:- use_module(spec_parse).


%
% Top-Level API
%

reference_groups(ClassReferences, [G1 | Groups]) :-
  reference_group(ClassReferences, G1),
  
  % Remove the 'used' edges
  % TODO
  
  reference_groups(ClassReferences1, Groups).

reference_group(ClassReferences, Group) :-
  ClassReferences = [references(ClassName, _) | _Rest],
  reference_group(ClassReferences, ClassName, Group).

reference_group(ClassReferences, ClassName, Group) :-
  setof(ClassName2, indirectly_references(ClassReferences, ClassName, ClassName2), DownStream),
  setof(ClassName2, indirectly_references(ClassReferences, ClassName2, ClassName), UpStream),
  union(DownStream, UpStream, Group).
                  
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

indirectly_references(AllReferences, Source, Target) :-
  member(references(Source, Target), AllReferences).

indirectly_references(AllReferences, Source, Target) :-
  Source \= Target, % Avoid recursion
  member(references(Source, Target0), AllReferences),
  indirectly_references(AllReferences, Target0, Target).

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