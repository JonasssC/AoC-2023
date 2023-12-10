:- include('../../lib/input.pl').
:- include('../../lib/lineparser.pl').

main :-
    read_lines(Lines),
    maplist(parseGame, Lines, Games),
    part1(Games, P1),
    write("Part 1: "), write(P1), nl,
    part2(Games, P2),
    write("Part 2: "), write(P2), nl.

parseGame(Line, game(Id, Pulls)) :-
    parseLine(
        [
            literal("Game "), 
            capture(number), 
            literal(": "), 
            capture(
                splitOn(
                    splitOn([
                        capture(number), 
                        whitespace, 
                        capture(string(alpha))
                        ], 
                        literal(", ")
                    ), 
                    literal("; ")
                )
            )
        ], 
        Line, 
        [], 
        [Id, Pulls]
    ).

% Part 1

part1([], 0).
part1([game(Id, Pulls)|Games], Res) :-
    ( validPulls(Pulls) ->
        Outcome is Id
    ;
        Outcome is 0
    ),
    part1(Games, Sum),
    Res is Sum + Outcome.

validPulls([]).
validPulls([Pull|Pulls]) :-
    validPull(Pull),
    validPulls(Pulls).

validPull([]).
validPull([[Count, "red"]|Rest]) :-
    Count =< 12,
    validPull(Rest).
validPull([[Count, "green"]|Rest]) :-
    Count =< 13,
    validPull(Rest).
validPull([[Count, "blue"]|Rest]) :-
    Count =< 14,
    validPull(Rest).

% Part 2

part2([], 0).
part2([game(_, Pulls)|Games], Res) :-
    flattenedPulls(Pulls, FlattenedPulls),
    findall(Count, member([Count, "red"], FlattenedPulls), RedCounts),
    findall(Count, member([Count, "green"], FlattenedPulls), GreenCounts),
    findall(Count, member([Count, "blue"], FlattenedPulls), BlueCounts),
    max_member(RedMin, RedCounts),
    max_member(GreenMin, GreenCounts),
    max_member(BlueMin, BlueCounts),
    part2(Games, Sum),
    Res is Sum + (RedMin * GreenMin * BlueMin).


flattenedPulls([], []).
flattenedPulls([Pull | Pulls], FlattenedPulls) :-
    flattenedPulls(Pulls, Flattened),
    append(Pull, Flattened, FlattenedPulls).
