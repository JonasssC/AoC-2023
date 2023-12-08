use_module(library(list_util)).

read_str(Content) :-
    read_file_to_string('input.txt', Content, []).

read_lines(Lines) :-
    read_str(Content),
    split_string(Content, "\n", "", Lines).
    
read_lines_as_nums(Numbers) :-
    read_lines(Lines),
    numbers_strings(Numbers, Lines).

numbers_strings([], []).
numbers_strings([Number|Numbers], [Line|Lines]) :-
    number_string(Number, Line),
    numbers_strings(Numbers,Lines).

% read_split_on_empty_line(Segments) :-
%     read_lines(Lines),
%     split(Lines, "", Segments).