
:- module(parse_primitives, [u2/3, u4/3, u8/3, primitive/1]).


primitive(u1).
primitive(u2).
primitive(u4).
primitive(u8).


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
