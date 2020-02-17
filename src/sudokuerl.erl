-module(sudokuerl).

%% API exports
-export([main/1]).

-ifdef(TEST).
    -compile([export_all]).
-endif.

%%==============================================================================
%% API functions
%%==============================================================================

%% escript Entry point
main(Args) ->
    {In, Out} = case length(Args) of
        1 -> {hd(Args), "solved"};
        2 -> {hd(Args), hd(tl(Args))};
        _ -> usage()
    end,
    Sudoku = read_sudoku(In),
    print_sudoku(Sudoku),
    erlang:halt(0).

%%==============================================================================
%% I/O functions
%%==============================================================================

usage() ->
    io:format("usage: sudokuerl input_filename [output_filename]\n", []),
    erlang:halt(1).

read_sudoku(Filename) ->
    {ok, Fd} = file:open(Filename, read),
    SudokuMap = read_lines(Fd, maps:new(), 1),
    N = get_sudoku_side(SudokuMap),
    SudokuMap#{side => N}.

% RowNumbers start from 1
read_lines(Fd, AccMap, RowNumber) ->
    case file:read_line(Fd) of
        {error, Reason} -> throw({error, Reason});
        eof -> AccMap;
        {ok, Line} ->
            Trim = string:trim(Line, both),
            Row = re:split(Trim, "\s+", [{return, list}]),
            NewAccMap = row_to_map(Row, RowNumber, 1, AccMap),
            read_lines(Fd, NewAccMap, RowNumber+1)
    end.

row_to_map([], _, _, AccMap) -> AccMap;
row_to_map([H|TL], RowNumber, ColNumber, AccMap) ->
    {N, _} = string:to_integer(H),
    row_to_map(TL, RowNumber, ColNumber+1,
               AccMap#{{RowNumber, ColNumber} => N}).

get_sudoku_side(SudokuMap) ->
    N = maps:size(SudokuMap),
    Side = round(math:sqrt(N)),
    case Side*Side of
        N -> Side;
        _ -> throw({error, bad_input_sudoku})
    end.

print_sudoku(Sudoku) ->
    Side = maps:get(side, Sudoku),
    io:format("[~p x ~p]\n",[Side, Side]),
    print_sudoku(Sudoku, 1, 1, Side).

print_sudoku(Sudoku, Side, Side, Side)->
    Val = maps:get({Side, Side}, Sudoku),
    Format = "\~"++
             integer_to_list(round(1+math:floor(math:log10(Side))))
             ++"b~n",
    io:format(Format, [Val]);
print_sudoku(Sudoku, Row, Side, Side) ->
    Val = maps:get({Row, Side}, Sudoku),
    Format = "\~"++
             integer_to_list(round(1+math:floor(math:log10(Side))))++
             "b~n",
    io:format(Format, [Val]),
    maybe_square_sep(Row, Side, "\n"),
    print_sudoku(Sudoku, Row+1, 1, Side);
print_sudoku(Sudoku, Row, Col, Side) ->
    Val = maps:get({Row, Col}, Sudoku),
    Format = "\~"++
             integer_to_list(round(1+math:floor(math:log10(Side))))++
             "b ",
    io:format(Format, [Val]),
    maybe_square_sep(Col, Side, " "),
    print_sudoku(Sudoku, Row, Col+1, Side).

maybe_square_sep(Col, Side, Str) ->
    case Col rem round(math:sqrt(Side)) of
        0 -> io:format(Str);
        _ -> ok
    end.
