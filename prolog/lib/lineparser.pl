parseLine(Types, Line, Rest, Captures) :-
    string_chars(Line, Chars),
    parseLineHelper(Types, Chars, Rest, Captures).

parseLineHelper([], Line, Line, []).
parseLineHelper([Type|Types], Line, LeftOver, Captures) :-
    Type \= capture(_),
    parsePart(Type, Line, _, Rest),
    parseLineHelper(Types, Rest, LeftOver, Captures).
parseLineHelper([capture(Type)|Types], Line, LeftOver, [Capture|Captures]) :-
    parsePart(Type, Line, Capture, Rest),
    parseLineHelper(Types, Rest, LeftOver, Captures).

parsePart(splitOn(Types, SplitOn), Line, [Capture|Captures], Rest) :-
    is_list(Types),
    parseLineHelper(Types, Line, LeftOver, Capture),
    parsePart(SplitOn, LeftOver, _, LeftOver2),
    parsePart(splitOn(Types, SplitOn), LeftOver2, Captures, Rest).

parsePart(splitOn(Types, SplitOn), Line, [Capture], LeftOver) :-
    is_list(Types),
    parseLineHelper(Types, Line, LeftOver, Capture),
    \+ parsePart(SplitOn, LeftOver, _, _).

parsePart(splitOn(Types, _), Line, [], Line) :-
    is_list(Types),
    \+ parseLineHelper(Types, Line, _, _).

parsePart(splitOn(Type, SplitOn), Line, [Capture|Captures], Rest) :-
    \+ is_list(Type),
    parsePart(Type, Line, Capture, LeftOver),
    parsePart(SplitOn, LeftOver, _, LeftOver2),
    parsePart(splitOn(Type, SplitOn), LeftOver2, Captures, Rest).

parsePart(splitOn(Type, SplitOn), Line, [Capture], LeftOver) :-
    \+ is_list(Type),
    parsePart(Type, Line, Capture, LeftOver),
    \+ parsePart(SplitOn, LeftOver, _, _).

parsePart(splitOn(Type, _), Line, [], Line) :-
    \+ is_list(Type),
    \+ parsePart(Type, Line, _, _).

parsePart(whitespace, Line, Part, Rest) :-
    collectCharsOfType(space, Line, Whitespaces, Rest),
    string_chars(Part, Whitespaces).

parsePart(string, Line, String, Rest) :-
    append(Part, Rest, Line),
    string_chars(String, Part).

parsePart(string(CharType), Line, String, Rest) :-
    append(Part, Rest, Line),
    isStringOfType(CharType, Part),
    string_chars(String, Part).

parsePart(literal(Part), Line, Part, Rest) :-
    string_chars(Part, Chars),
    append(Chars, Rest, Line).

parsePart(number, Line, Part, Rest) :-
    collectCharsOfType(digit, Line, Digits, Rest),
    length(Digits, Length),
    Length > 0,    
    number_chars(Part, Digits).

collectCharsOfType(_, [], [], []).
collectCharsOfType(Type, [Char|Line], [Char|Part], Rest) :-
    char_type(Char, Type),
    collectCharsOfType(Type, Line, Part, Rest).
collectCharsOfType(Type, [Char|Line], [], [Char|Line]) :-
    \+ char_type(Char, Type).

isStringOfType(_, []).
isStringOfType(Type, [Char|String]) :-
    char_type(Char, Type),
    isStringOfType(Type, String).
