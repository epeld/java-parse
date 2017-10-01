lower_alphabet(Alphabet) :-
  "abcdefghijklmnopqrstuvwxyz".

upper_alphabet(Alphabet) :-
  "ABCDEFGHIJKLMNOPQRSTUVWXYZ".

alphabet(A) :- lower_alphabet(A) ; upper_alphabet(A).

downcased(Char, DownChar) :-
  alphabet(A),
  lower_alphabet(LA),
  nth0(Ix, A, Char),
  nth0(Ix, LA, DownChar).

upcased(Char, UpChar) :-
  alphabet(A),
  upper_alphabet(UA),
  nth0(Ix, A, Char),
  nth0(Ix, UA, UpChar).

letter(L) :-
  alphabet(A),
  member(L, A).

digit(N) :-
  member(N, "1234567890").


camelcase([Letter | Snake], [LowLetter | Camel]) :-
  downcased(Letter, LowLetter),
  camelcase(Snake, Camel).
         
camelcase([Digit | Snake], [Digit | Camel]) :-
  digit(Digit),
  camelcase(Snake, Camel).

% camelcase([95, Letter | Snake], [UpLetter | Camel]) :-