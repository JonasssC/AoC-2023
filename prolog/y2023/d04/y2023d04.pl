:- include('../../lib/input.pl').
:- include('../../lib/lineparser.pl').

main :-
    read_lines(Lines),
    maplist(parseCard, Lines, Cards),
    part1(Cards, P1),
    write("Part 1: "), write(P1), nl,
    part2(Cards, P2),
    write("Part 2: "), write(P2), nl.

parseCard(Line, card(Id, Winning, Numbers)) :-
    parseLine(
        [
            literal("Card"),
            whitespace, 
            capture(number), 
            literal(":"),
            whitespace, 
            capture(
                splitOn(
                    number, 
                    whitespace
                )
            ),
            whitespace,
            literal("|"),
            whitespace,
            capture(
                splitOn(
                    number, 
                    whitespace
                )
            )
        ], 
        Line,
        [], 
        [Id, Winning, Numbers]
    ).

% Part 1

part1([], 0).
part1([card(_, Winning, Numbers)|Cards], Res) :-
    intersection(Winning, Numbers, WinningNumbers),
    length(WinningNumbers, N),
    part1(Cards, Sum),
    ( N = 0 ->
        Res is Sum
    ;
        Res is Sum + (2 ^ (N - 1))
    ).

% Part 2

part2(Cards, Res) :-
    length(Cards, Len),
    length(Counts, Len),
    maplist(=(1), Counts),
    part2Helper(Cards, Counts, Res).

part2Helper([], [], 0).
part2Helper([card(_, Winning, Numbers)|Cards], [Count|Counts], Res) :-
    intersection(Winning, Numbers, WinningNumbers),
    length(WinningNumbers, N),
    increaseCounts(Count, N, Counts, NewCounts),
    part2Helper(Cards, NewCounts, SubRes),
    Res is Count + SubRes.

increaseCounts(_, 0, Counts, Counts).
increaseCounts(_, N, [], []) :- 
    N > 0.
increaseCounts(X, N, [Count|Counts], [NewCount|NewCounts]) :-
    N > 0,
    NewN is N - 1,
    NewCount is Count + X,
    increaseCounts(X, NewN, Counts, NewCounts).
