:- include('../../lib/input.pl').

main :-
    read_lines(Lines),
    part1(Lines, P1),
    write("Part 1: "), write(P1), nl,
    part2(Lines, P2),
    write("Part 2: "), write(P2), nl.

% Part 1

part1([], 0).
part1([Line|Lines], Result) :-
    findFirstNumber(Line, FirstNum),
    findLastNumber(Line, LastNum),
    part1(Lines, Results),
    Result is FirstNum * 10 + LastNum + Results.

findFirstNumber(S, Number) :-
    findall(Num, (sub_string(S, 0, _, _, Sub), number(Sub, Num)), [Number]).
findFirstNumber(S, Number) :-
    findall(Num, (sub_string(S, 0, _, _, Sub), number(Sub, Num)), []),
    sub_string(S, 1, _, 0, Rest),
    findFirstNumber(Rest, Number).

findLastNumber(S, Number) :-
    findall(Num, (sub_string(S, _, _, 0, Sub), number(Sub, Num)), [Number]).
findLastNumber(S, Number) :-
    findall(Num, (sub_string(S, _, _, 0, Sub), number(Sub, Num)), []),
    sub_string(S, 0, _, 1, Rest),
    findLastNumber(Rest, Number).

number("0", 0).
number("1", 1).
number("2", 2).
number("3", 3).
number("4", 4).
number("5", 5).
number("6", 6).
number("7", 7).
number("8", 8).
number("9", 9).

% Part 2

part2([], 0).
part2([Line|Lines], Result) :-
    findFirstWordNumber(Line, FirstNum),
    findLastWordNumber(Line, LastNum),
    part2(Lines, Results),
    Result is FirstNum * 10 + LastNum + Results.

findFirstWordNumber(S, Number) :-
    findall(Num, (sub_string(S, 0, _, _, Sub), wordNumber(Sub, Num)), [Number]).
findFirstWordNumber(S, Number) :-
    findall(Num, (sub_string(S, 0, _, _, Sub), wordNumber(Sub, Num)), []),
    sub_string(S, 1, _, 0, Rest),
    findFirstWordNumber(Rest, Number).

findLastWordNumber(S, Number) :-
    findall(Num, (sub_string(S, _, _, 0, Sub), wordNumber(Sub, Num)), [Number]).
findLastWordNumber(S, Number) :-
    findall(Num, (sub_string(S, _, _, 0, Sub), wordNumber(Sub, Num)), []),
    sub_string(S, 0, _, 1, Rest),
    findLastWordNumber(Rest, Number).

wordNumber("zero", 0).
wordNumber("one", 1).
wordNumber("two", 2).
wordNumber("three", 3).
wordNumber("four", 4).
wordNumber("five", 5).
wordNumber("six", 6).
wordNumber("seven", 7).
wordNumber("eight", 8).
wordNumber("nine", 9).
wordNumber(Word, Num) :- 
    number(Word, Num).
