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
    try solve(Sudoku) of
        never_here -> ok
    catch
        {win, CompletedSudoku} ->
            io:format("Sudoku Completed!\n"),
            print_sudoku(CompletedSudoku);
            % TODO: also write to outfile
        invalid_sudoku ->
            io:format("The sudoku was incorrect (or something went wrong)\n")
    end,
    erlang:halt(0).

%%==============================================================================
%% solving logic
%%==============================================================================

solve(Sudoku) ->
    SortedOccurences = sorted_occurrences(Sudoku),
    SquaresData = empty_squares_data(Sudoku),
    solve_loop(Sudoku, SortedOccurences, SquaresData).

solve_loop(Sudoku, [], SquaresData) ->
    % when we reach the end of the list it can only happen that
    % we have to guess between possibilites
    % TODO: guessing part
    todo_guessing;
solve_loop(Sudoku, [{Num,_Occ}|TL], SquaresData) ->

    % io:format("DEBUG: solve_loop with Num:~p~n", [Num]),

    Side = maps:get(side, Sudoku),
    {NewSudoku, NewSquaresData} =
        squares_loop(Sudoku, Num, 1, Side, SquaresData),

    % recalculate the occurrences
    NewSortedOcc = sorted_occurrences(NewSudoku),

    % io:format("DEBUG: NewSortedOcc: ~p~n", [NewSortedOcc]),

    case proplists:get_value(Num, NewSortedOcc) of
        Side ->
            % the number is full, we move on to the next one
            ok;
        _ ->
            % the number is not full. We have to check,
            % if it cannot go anywhere, the sudoku is wrong
            check_if_wrong(Num, NewSquaresData)
    end,

    % move on to the next most frequent number (that wasn't used before (TL))
    NewSortedOcc1 = [{N,Occ} || {N,Occ} <- NewSortedOcc,
                                proplists:is_defined(N, TL)],

    % io:format("DEBUG: NewSortedOcc1: ~p~n", [NewSortedOcc1]),

    solve_loop(NewSudoku, NewSortedOcc1, NewSquaresData).

check_if_wrong(Num, SquaresData) ->
    case lists:foldl(
        fun({_SqNum, {_FreePos, MissingDigits}}, Count) ->
            Count + length(maps:get(Num, MissingDigits, []))
        end,
        0,
        maps:to_list(SquaresData)
    ) of
        0 -> throw(invalid_sudoku);
        _ -> ok
    end.

squares_loop(Sudoku, Num, NSquare, Side, SquaresData) when NSquare > Side ->
    {Sudoku, SquaresData};
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
                    {NewSudoku, NewSquaresData} = 
                        insert(Sudoku, Num,
                               hd(AvailablePositions), SquaresData),
                    
                    % when we return from insert check if we won
                    case check_sudoku(NewSudoku) of
                        full ->
                            throw({win, NewSudoku});
                        ok ->
                            % if we didn't win, move on to the next square
                            squares_loop(NewSudoku, Num, NSquare+1, 
                                         Side, NewSquaresData)
                    end;
                _ ->
                    NewSquaresData = update_squares_data(NSquare, Num,
                                            AvailablePositions, SquaresData),
                    squares_loop(Sudoku, Num, NSquare+1, Side, NewSquaresData)
            end
    end.

insert(Sudoku, Num, Pos, SquaresData) ->

    io:format("\n**************************************\n"),
    io:format("insert(Sudoku, ~p, ~p, SquaresData)\n", [Num, Pos]),
    io:format("where Sudoku:\n"),
    print_sudoku(Sudoku),
    % io:format("and SquaresData:\n"),
    % io:format("~p~n", [SquaresData]),
    io:format("**************************************\n"),

    % fill Pos with Num in the Sudoku
    NewSudoku1 = maps:put(Pos, Num, Sudoku),

    % remove Pos from FreePos in its quadrant. Also remove from MissingDigits
    % all numbers found in Pos (Num included, it's ok).
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

    % completely remove Num from MissingDigit
    MissingDigits2 = maps:remove(Num, MissingDigits1),

    NewSquaresData1 = 
        maps:update(SqNum, {NewFreePos, MissingDigits2}, SquaresData),

    % search all positions that share the row or column wih Pos and delete
    % Num from any FreePos found this way. If Num is found we also have to
    % update the MissingDigits field for the Square
    {Row,Col} = Pos,
    NewSquaresData2 =
        update_same_row_data(NewSudoku1, Row, Num, NewSquaresData1),
    NewSquaresData3 =
        update_same_col_data(NewSudoku1, Col, Num, NewSquaresData2),

    % check how many digits are missing from the row and the col
    % if it's just 1 we have a free insert
    {NewSudoku2, NewSquaresData4} =
        case check_row_missing_one_digit(NewSudoku1, Row) of
            {FDigitRow, FPosRow} ->
                insert(NewSudoku1, FDigitRow, FPosRow, NewSquaresData3);
            _ -> {NewSudoku1, NewSquaresData3}
        end,

    {NewSudoku3, NewSquaresData5} =
        case check_col_missing_one_digit(NewSudoku2, Col) of
            {FDigitCol, FPosCol} ->
                insert(NewSudoku2, FDigitCol, FPosCol, NewSquaresData4);
            _ -> {NewSudoku2, NewSquaresData4}
        end,

    % io:format("DEBUG: NewSquaresData5:\n"),
    % io:format("~p~n", [NewSquaresData5]),

    % search all squares for MissingDigits with only 1 entry.
    % When we find one, we recursively call insert
    {NewSudoku4, NewSquaresData6} = lists:foldl(
        fun(N, {AccSudoku, AccSquaresData}) ->
            check_can_insert_sq(AccSudoku, N, AccSquaresData)
        end,
        {NewSudoku3, NewSquaresData5},
        lists:seq(1,maps:get(side, Sudoku))
    ),

    {NewSudoku4, NewSquaresData6}.

check_row_missing_one_digit(Sudoku, Row) ->
    Side = maps:get(side, Sudoku),
    RawRow = get_sudoku_row(Sudoku, Row),
    case lists:seq(1,Side) -- RawRow of
        [MissingDigit] ->
            Col = find_first(0, RawRow, 1),
            {MissingDigit, {Row, Col}};
        _ ->
            false
    end.

check_col_missing_one_digit(Sudoku, Col) ->
    Side = maps:get(side, Sudoku),
    RawCol = get_sudoku_col(Sudoku, Col),
    case lists:seq(1,Side) -- RawCol of
        [MissingDigit] ->
            Row = find_first(0, RawCol, 1),
            {MissingDigit, {Row, Col}};
        _ ->
            false
    end.

% positions start from 1
find_first(N, [N|TL], Pos) -> Pos;
find_first(N, [_|TL], Pos) -> find_first(N, TL, Pos+1).

check_can_insert_sq(Sudoku, SqNum, SquaresData) ->
    {_, MissingDigits} = maps:get(SqNum, SquaresData),
    lists:foldl(
        fun(MissingDigit, {AccSudoku, AccSquaresData}) ->
            {_, AccMissingDigits} = maps:get(SqNum, AccSquaresData),
            % it can be that we come back here after a recursive call so
            % the key might not be in the map anymore. So we return a list
            % that will not be used
            case maps:get(MissingDigit, AccMissingDigits, not_found) of
                [Pos] ->
                    insert(AccSudoku, MissingDigit, Pos, AccSquaresData);
                _ -> {AccSudoku, AccSquaresData}
            end
        end,
        {Sudoku, SquaresData},
        maps:keys(MissingDigits)
    ).

update_same_row_data(Sudoku, Row, Num, SquaresData) ->
    lists:foldl(
        fun(N, AccSquaresData) ->
            remove_free_pos_from_data(Sudoku, {Row,N}, Num, AccSquaresData) 
        end,
        SquaresData,
        lists:seq(1,maps:get(side, Sudoku))
    ).

update_same_col_data(Sudoku, Col, Num, SquaresData) ->
    lists:foldl(
        fun(N, AccSquaresData) ->
            remove_free_pos_from_data(Sudoku, {N, Col}, Num, AccSquaresData) 
        end,
        SquaresData,
        lists:seq(1,maps:get(side, Sudoku))
    ).

remove_free_pos_from_data(Sudoku, Pos, Num, SquaresData) ->
    TSqNum = get_square_from_pos(maps:get(sqrt, Sudoku), Pos),
    {FreePos, MissingDigits} = maps:get(TSqNum, SquaresData),
    case maps:get(Pos, FreePos, undefined) of
        undefined -> SquaresData;
        L ->
            case lists:member(Num, L) of
                false -> SquaresData;
                true ->
                    NewFreePos = maps:update(Pos, L--[Num], FreePos),
                    NewMissingDigits =
                        % it may be that we already removed the number
                        % from the missing digits map
                        case maps:is_key(Num, MissingDigits) of
                            false -> MissingDigits;
                            true ->
                                maps:update_with(Num, 
                                                 fun(L1) -> L1--[Pos] end,
                                                 MissingDigits)
                        end,
                    maps:update(TSqNum, {NewFreePos, NewMissingDigits},
                                SquaresData)
            end
    end.


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
    lists:sort(fun({A1,A2},{B1,B2}) -> {A2,A1} >= {B2,B1} end,
               maps:to_list(Map)).

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
