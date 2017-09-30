
:- use_module([library(http/dcg_basics)]).


class_file(version(Major, Minor), ConstantPool) -->
  magic_value,
  minor_version(Minor),
  major_version(Major),
  constant_pool_count(ConstantPoolCount),
  constant_pool(ConstantPoolCount, ConstantPool),
  remainder(_Rem).

magic_value -->
  [0xCA, 0xFE, 0xBA, 0xBE].

minor_version(Minor) --> u2(Minor).
major_version(Major) --> u2(Major).
constant_pool_count(Count) --> u2(Count).


%
% Constant Pool Parsing
%
constant_pool(1, []) --> [].
constant_pool(Count, Pool) -->
  {
   Count > 1,
   Count1 is Count - 1,
   length(Pool, Count1),
   Pool = [Entry | Pool1]
  },
  u1(Tag),
  constant_pool_entry(Tag, Entry),
  constant_pool(Count1, Pool1).

constant_pool_entry(1, Utf8) -->
  utf8_info(Utf8).

constant_pool_entry(3, Integer) -->
  integer_info(Integer).

constant_pool_entry(4, Float) -->
  float_info(Float).

constant_pool_entry(5, Long) -->
  long_info(5, Long).

constant_pool_entry(6, Double) -->
  double_info(6, Double).

constant_pool_entry(7, ClassInfo) -->
  class_info(ClassInfo).

constant_pool_entry(8, String) -->
  string_info(String).

constant_pool_entry(9, FieldRefInfo) -->
  field_ref_info(FieldRefInfo).

constant_pool_entry(10, MethodRefInfo) -->
  method_ref_info(MethodRefInfo).

constant_pool_entry(11, InterfaceMethodRefInfo) -->
  interface_method_ref_info(InterfaceMethodRefInfo).

constant_pool_entry(12, NameAndTypeInfo) -->
  name_and_type_info(NameAndTypeInfo).

constant_pool_entry(15, MethodHandle) -->
  method_handle_info(MethodHandle).

constant_pool_entry(16, MethodType) -->
  method_type_info(MethodType).

constant_pool_entry(18, Invoke) -->
  invoke_dynamic_info(Invoke).


%
% Info-Structures
%

string_info(string_index(StringIndex)) --> 
  u2(StringIndex).

integer_info(integer(Integer)) --> 
  u4(Integer).

long_info(integer(Long)) --> 
  u8(Long).

float_info(float(Float)) --> 
  [Byte1, Byte2, Byte3, Byte4],
  bytes_to_float([Byte1, Byte2, Byte3, Byte4], Float).

double_info(float(todo([Byte1, Byte2, Byte3, Byte4, Byte21, Byte22, Byte23, Byte24]))) --> 
  [Byte1, Byte2, Byte3, Byte4], 
  [Byte21, Byte22, Byte23, Byte24].
            

name_and_type_info(name_and_type_info(NameIndex, DescriptorIndex)) -->
  u2(NameIndex),
  u2(DescriptorIndex).

class_info(class_info(NameIndex)) -->
  u2(NameIndex).

ref_info(ClassIndex, NameAndTypeIndex) -->
  u2(ClassIndex),
  u2(NameAndTypeIndex).


field_ref_info(field_ref(ClassIndex, NameAndTypeIndex)) --> 
  ref_info(ClassIndex, NameAndTypeIndex).

method_ref_info(method_ref(ClassIndex, NameAndTypeIndex)) --> 
  ref_info(ClassIndex, NameAndTypeIndex).

interface_method_ref_info(interface_method_ref(ClassIndex, NameAndTypeIndex)) --> 
  ref_info(ClassIndex, NameAndTypeIndex).

utf8_info(utf8(Utf8)) -->
  u2(Length),
  {
   length(Utf8, Length)
  },
  % NOTE: THIS IS A SIMPLIFICATION. MAYBE USE ACTUAL UTF8-FORMAT LATER
  Utf8.


method_type_info(method_type_info(DescriptorIndex)) -->
  u2(DescriptorIndex).

method_handle_info(method_handle(ReferenceKind, ReferenceIndex)) -->
  u1(ReferenceKind),
  u2(ReferenceIndex).

invoke_dynamic_info(invoke_dynamic_info(BootstrapMethodAttrIndex, NameAndTypeIndex)) -->
  u2(BootstrapMethodAttrIndex),
  u2(NameAndTypeIndex).


bytes_to_float([0x7f, 0x80, 0, 0], infinity(positive)).
bytes_to_float([0xff, 0x80, 0, 0], infinity(negative)).

bytes_to_float([Byte1, Byte2, Byte3, Byte4], Float) :-
  Int is Byte1 << 24 + Byte2 << 16 + Byte3 << 8 + Byte4,
  bytes_int_to_float(Int, Float).

bytes_int_to_float(Int, nan) :-
  0x7f800001 =< Int, Int =< 0x7fffffff, !.

bytes_int_to_float(Int, nan) :-
  0xff800001 =< Int, Int =< 0xffffffff, !.

bytes_int_to_float(_Int, Float) :-
  todo_float(Float).


%
% Low-Level Primitives
%
u1(U1) --> [U1].

u2(U2) -->
  [Byte1, Byte2],
  {
   U2 is Byte1 << 8 + Byte2
  }.

u4(U4) -->
  u2(First),
  u2(Second),
  {
   U4 is First << 16 + Second
  }.

u8(U8) -->
  u4(First),
  u4(Second),
  {
   U8 is First << 32 + Second
  }.


try_it(Version, Constants) :-
  phrase_from_file(class_file(Version, Constants), "./Hello.class", [type(binary)]).
