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
%% solving logic
%%==============================================================================

solve(Sudoku) ->
    % build digit count map
    SortedOccurences = sorted_occurrences(Sudoku),
    SquaresData = empty_squares_data(Sudoku),
    solve_loop(Sudoku, SortedOccurences, SquaresData),
    ok.

solve_loop(Sudoku, [], SquaresData) -> SquaresData;
solve_loop(Sudoku, [{Num,_Occ}|SortedOccTL], SquaresData) ->
    Side = maps:get(side, Sudoku),
    NewSquaresData = squares_loop(Sudoku, Num, 1, Side, SquaresData),
    ok.

squares_loop(Sudoku, Num, NSquare, Side, SquaresData) when NSquare > Side ->
    SquaresData;
squares_loop(Sudoku, Num, NSquare, Side, SquaresData) ->
    {FreePos, MissingDigits} = maps:get(NSquare, SquaresData),
    % check whether Num is already in square
    case lists:member(Num, maps:keys(MissingDigits)) of
        false ->
            % already there, move to next square
            squares_loop(Sudoku, Num, NSquare+1, Side, SquaresData);
        true ->
            % the number is not in the square, check how many places it can go
            AvailablePositions =
                find_available_positions(Sudoku, Num, maps:keys(FreePos), []),
            case length(AvailablePositions) of
                0 -> 
                    throw(invalid_sudoku);
                1 ->
                    insert(Sudoku, Num, hd(AvailablePositions), SquaresData),
                    % TODO
                    more;
                _ ->
                    NewSquaresData = update_squares_data(NSquare, Num,
                                            AvailablePositions, SquaresData),
                    squares_loop(Sudoku, Num, NSquare+1, Side, NewSquaresData)
            end
    end.

insert(Sudoku, Num, Pos, SquaresData) ->
    % riempio nel sudoku la posizone corrispondente
    NewSudoku = maps:put(Pos, Num, Sudoku),

    % rimuovo la posizione da FreePos, che implica l'andare a rimuovere da
    % MissingDigits la posizione di tutti i numeri trovati in FreePos
    % (Num compreso)
    SqNum = get_square_from_pos(maps:get(sqrt, Sudoku), Pos),
    {FreePos, MissingDigits} = maps:get(SqNum, SquaresData),
    DigitsInPos = maps:get(Pos, FreePos),
    NewFreePos = maps:remove(Pos, FreePos),
    MissingDigits1 = lists:foldl(
        fun(Digit, TmpMissingDigits) ->
            maps:update_with(Digit, fun(L) -> lists:delete(Pos,L) end,
                             TmpMissingDigits)
        end,
        MissingDigits,
        DigitsInPos
    ),

    % rimuovo completamente Num dalla mappa MissingDigits
    MissingDigits2 = maps:remove(Num, MissingDigits1),

    NewSquaresData = 
        maps:update(SqNum, {NewFreePos, MissingDigits2}, SquaresData),

    % guardo in tutti i quadranti che hanno in comune la riga o la colonna
    % ed elimino eventuali possibili posizioni trovate per lo stesso numero
    % cioè: genero tutte le posizioni che hanno in comune la riga o la colonna
    % con Pos, per ogni posizione vado nel quadrante e, se trovo la posizione
    % in FreePos e se c'è dentro il numero in questione, lo elimino da quel
    % FreePos e vado ad eliminare quella Pos anche dal MissingDigits

    % Cerco in tutti i quadranti coinvolti se adesso ci sia qualche numero con
    % una sola posizione possibile e nel caso ripeto l'INSERIMENTO

    {NewSudoku, NewSquaresData}.

update_squares_data(_NSquare, _Num, [], SquaresData) -> SquaresData;
update_squares_data(NSquare, Num, [NewPos|TL], SquaresData) ->
    NewData = maps:update_with(
        NSquare,
        fun({FreePos, MissingDigits}) ->
                NewFreePos = 
                    maps:update_with(NewPos, fun(L) -> [Num|L] end, FreePos),
                NewMissingDigits =
                    maps:update_with(Num, fun(L) -> [NewPos|L] end,
                                     MissingDigits),
                {NewFreePos, NewMissingDigits}
        end,
        SquaresData
    ),
    update_squares_data(NSquare, Num, TL, NewData).

% this function determines whether a number can go in a certain position inside
% a square (assuming it is not already in the square) by looking at the row and
% the column of the position.
find_available_positions(Sudoku, Num, [], Acc) -> lists:reverse(Acc);
find_available_positions(Sudoku, Num, [Pos={I,J}|TL], Acc) ->
    case 
        (check_rules([Num]++get_sudoku_row(Sudoku,I)) =/= invalid) and
        (check_rules([Num]++get_sudoku_col(Sudoku,J)) =/= invalid) of

        true -> find_available_positions(Sudoku, Num, TL, [Pos|Acc]);
        false -> find_available_positions(Sudoku, Num, TL, Acc)
    end.

% this function builds a map that for every square holds
% a tuple {FreePos, MissingDigits} where:
% - FreePos is a map mapping free positions to a list of numbers that could
%   take that position
% - MissingDigits is a map mapping digits that are not present in the square
%   to the list of positions that digit could take
empty_squares_data(Sudoku) ->
    Side = maps:get(side, Sudoku),
    Sqrt = maps:get(sqrt, Sudoku),
    lists:foldl(
        fun(SqNum, Acc) ->
            % generate all possible positions for the square and check
            % whether they are empty or not
            FirstRow = 1 + ((SqNum-1) div Sqrt) * Sqrt,
            FirstCol = 1 + ((SqNum-1) rem Sqrt) * Sqrt,
            FreePos = maps:from_list([
                {{FirstRow+I, FirstCol+J}, []} ||
                I <- lists:seq(0, Sqrt-1),
                J <- lists:seq(0, Sqrt-1),
                maps:get({FirstRow+I, FirstCol+J}, Sudoku) == 0
            ]),
            % missing digits are easy to find with -- operator
            Missing = lists:seq(1,Side) -- get_sudoku_square(Sudoku, SqNum),
            MissingDigits = maps:from_list([{D,[]} || D <- Missing]),
            maps:put(SqNum, {FreePos, MissingDigits}, Acc)
        end,
        #{},
        lists:seq(1,Side)
    ).

sorted_occurrences(Sudoku) ->
    Side = maps:get(side, Sudoku),
    Empty = maps:from_list([{I, 0} || I <- lists:seq(1, Side)]),
    Map = lists:foldl(
        fun(I, Acc) -> count_digits_in_row(Sudoku, I, Acc) end,
        Empty,
        lists:seq(1,Side)
    ),
    Occurrences = [X || X <- maps:to_list(Map), X =/= 0],
    lists:sort(fun({A1,A2},{B1,B2}) -> {A2,A1} >= {B2,B1} end, Occurrences).

count_digits_in_row(Sudoku, N, DGCountMap) ->
    lists:foldl(
        fun (0, Acc) -> Acc;
            (X, Acc) -> maps:update_with(X, fun(M) -> M+1 end, Acc)
        end,
        DGCountMap,
        get_sudoku_row(Sudoku, N)
    ).

%%==============================================================================
%% get sudoku parts and check rules
%%==============================================================================

% takes a list that can be either a row, a column or a square
check_rules(Section) ->
    Side = length(Section),
    Filtered = [X || X <- Section, X =/= 0],
    Len = length(Filtered),
    case length(lists:usort(Filtered)) of
        Side -> full;
        Len -> ok;
        _ -> invalid
    end.

check_sudoku(Sudoku) ->
    try check_sudoku0(Sudoku) of
        Res -> Res
    catch
        Exception:invalid -> invalid
    end.

check_sudoku0(Sudoku) ->
    Side = maps:get(side, Sudoku),
    RR = check_rows(Sudoku, 1, Side, full),
    RC = check_cols(Sudoku, 1, Side, RR),
    check_squares(Sudoku, 1, Side, RC).

check_rows(Sudoku, Side, Side, Res) ->
    case check_rules(get_sudoku_row(Sudoku, Side)) of
        ok -> ok;
        full -> Res;
        invalid -> throw(invalid)
    end;
check_rows(Sudoku, N, Side, Res) ->
    case check_rules(get_sudoku_row(Sudoku, N)) of
        ok -> check_rows(Sudoku, N+1, Side, ok);
        full -> check_rows(Sudoku, N+1, Side, Res);
        invalid -> throw(invalid)
    end.

check_cols(Sudoku, Side, Side, Res) ->
    case check_rules(get_sudoku_col(Sudoku, Side)) of
        ok -> ok;
        full -> Res;
        invalid -> throw(invalid)
    end;
check_cols(Sudoku, N, Side, Res) ->
    case check_rules(get_sudoku_col(Sudoku, N)) of
        ok -> check_cols(Sudoku, N+1, Side, ok);
        full -> check_cols(Sudoku, N+1, Side, Res);
        invalid -> throw(invalid)
    end.

check_squares(Sudoku, Side, Side, Res) ->
    case check_rules(get_sudoku_square(Sudoku, Side)) of
        ok -> ok;
        full -> Res;
        invalid -> throw(invalid)
    end;
check_squares(Sudoku, N, Side, Res) ->
    case check_rules(get_sudoku_square(Sudoku, N)) of
        ok -> check_squares(Sudoku, N+1, Side, ok);
        full -> check_squares(Sudoku, N+1, Side, Res);
        invalid -> throw(invalid)
    end.

get_sudoku_row(Sudoku, RowNum) ->
    Side = maps:get(side, Sudoku),
    [maps:get({RowNum,I}, Sudoku) || I <- lists:seq(1,Side)].

get_sudoku_col(Sudoku, ColNum) ->
    Side = maps:get(side, Sudoku),
    [maps:get({I,ColNum}, Sudoku) || I <- lists:seq(1,Side)].

% squares are numbered from the top left moving down right
% representations are linearized row by row
get_sudoku_square(Sudoku, SqNum) ->
    Sqrt = maps:get(sqrt, Sudoku),
    FirstRow = 1 + ((SqNum-1) div Sqrt) * Sqrt,
    FirstCol = 1 + ((SqNum-1) rem Sqrt) * Sqrt,
    [maps:get({FirstRow+I, FirstCol+J}, Sudoku) ||
        I <- lists:seq(0, Sqrt-1),
        J <- lists:seq(0, Sqrt-1)].

% function that from a position return the quadrant number
get_square_from_pos(Sqrt, {I,J}) ->
    ((I-1) div Sqrt) * Sqrt + 1 + ((J-1) div Sqrt).

%%==============================================================================
%% I/O functions
%%==============================================================================

usage() ->
    io:format("usage: sudokuerl input_filename [output_filename]\n", []),
    erlang:halt(1).

read_sudoku(Filename) ->
    {ok, Fd} = file:open(Filename, read),
    SudokuMap = read_lines(Fd, maps:new(), 1),
    file:close(Fd),
    N = get_sudoku_side(SudokuMap),
    Sqrt = round(math:sqrt(N)),
    SudokuMap#{side => N, sqrt => Sqrt}.

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
