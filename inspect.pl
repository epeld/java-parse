
:- module(inspect, []).

:- use_module(spec_parse).


%
% Top-Level API
%

referenced_classes(ClassContents, ClassNames) :-
  constant_pool(ClassContents, Constants),
  setof(ClassName,
        constant_class_name(Constants, ClassName),
        ClassNames).


constant_pool(ClassContents, ConstantPool) :-
    member(constant_pool(ConstantPool), ClassContents).

%
% Helpers
%

constant_class_name(Constants, ClassName) :-
  member(ClassRef, Constants),
  constant_type(ClassRef, constant_class_info),
  member(name_index(Ix), ClassRef),
  nth1(Ix, Constants, Utf8),
  utf8_string(Utf8, ClassName).


utf8_string(Utf8, String) :-
  member(bytes(Codes), Utf8),
  string_codes(String, Codes).


constant_type(C, Type) :-
  member(tag(T), C),
  spec_parse:cp_info_tag(Type, T).