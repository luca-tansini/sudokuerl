-module(sudokuerl_tests).

-include_lib("eunit/include/eunit.hrl").

sudoku_sizes_test() ->
    [sudoku_sizes_test(Size) || Size <- [4, 9, 16, 25, 36]].

sudoku_sizes_test(Size) ->
    {ok, Fd} = file:open("sudoku_sizes_test_tmp", write),
    [begin
        L = lists:seq(I, Size)++lists:seq(1, I-1),
        [io:format(Fd, "~p ", [X]) || X <- L],
        io:format(Fd, "\n", [])
     end
    || I <- lists:seq(1, Size)],
    file:close(Fd),
    Sudoku = sudokuerl:read_sudoku("sudoku_sizes_test_tmp"),
    Size = maps:get(side, Sudoku),
    Sqrt = round(math:sqrt(Size)),
    Sqrt = maps:get(sqrt, Sudoku),
    sudokuerl:print_sudoku(Sudoku),
    file:delete("sudoku_sizes_test_tmp").

check_rules_test() ->
    full = sudokuerl:check_rules([1,2,3,4]),
    full = sudokuerl:check_rules([9,5,3,4,7,8,1,2,6]),
    ok = sudokuerl:check_rules([0,0,0,0,0,0,0,0,0]),
    ok = sudokuerl:check_rules([1,0,4,0,8,9,5,7,3]),
    invalid = sudokuerl:check_rules([9,5,3,4,9,8,1,2,6]),
    invalid = sudokuerl:check_rules([9,9,3,4,9,8,1,2,6]),
    invalid = sudokuerl:check_rules([0,0,0,0,0,0,0,1,1]).
    
get_sudoku_row_test() ->
    Sudoku = sudokuerl:read_sudoku("test/sudoku.txt"),
    [0, 0, 0, 0, 0, 0, 0, 4, 7] = sudokuerl:get_sudoku_row(Sudoku, 1),
    [0, 0, 0, 0, 4, 5, 8, 0, 0] = sudokuerl:get_sudoku_row(Sudoku, 3),
    [0, 0, 0, 0, 0, 6, 9, 7, 0] = sudokuerl:get_sudoku_row(Sudoku, 6),
    [3, 8, 0, 0, 0, 0, 0, 0, 0] = sudokuerl:get_sudoku_row(Sudoku, 9).

get_sudoku_col_test() ->
    Sudoku = sudokuerl:read_sudoku("test/sudoku.txt"),
    [0, 0, 0, 0, 7, 0, 0, 0, 3] = sudokuerl:get_sudoku_col(Sudoku, 1),
    [0, 3, 0, 5, 0, 0, 0, 0, 8] = sudokuerl:get_sudoku_col(Sudoku, 2),
    [0, 2, 0, 1, 0, 0, 8, 0, 0] = sudokuerl:get_sudoku_col(Sudoku, 4),
    [7, 0, 0, 0, 2, 0, 0, 0, 0] = sudokuerl:get_sudoku_col(Sudoku, 9).

get_sudoku_square_test() ->
    Sudoku = sudokuerl:read_sudoku("test/sudoku.txt"),
    [0, 0, 0, 0, 3, 5, 0, 0, 0] = sudokuerl:get_sudoku_square(Sudoku, 1),
    [0, 0, 0, 2, 0, 0, 0, 4, 5] = sudokuerl:get_sudoku_square(Sudoku, 2),
    [0, 5, 8, 7, 0, 0, 0, 0, 0] = sudokuerl:get_sudoku_square(Sudoku, 4),
    [0, 0, 0, 0, 0, 2, 9, 7, 0] = sudokuerl:get_sudoku_square(Sudoku, 6),
    [0, 0, 0, 2, 1, 0, 0, 0, 0] = sudokuerl:get_sudoku_square(Sudoku, 9).

check_sudoku_test() ->
    Sudoku = sudokuerl:read_sudoku("test/sudoku.txt"),
    ok = sudokuerl:check_sudoku(Sudoku),
    Sudoku9Invalid = sudokuerl:read_sudoku("test/sudoku9_invalid.txt"),
    invalid = sudokuerl:check_sudoku(Sudoku9Invalid),
    Sudoku4Full = sudokuerl:read_sudoku("test/sudoku4_full.txt"),
    full = sudokuerl:check_sudoku(Sudoku4Full).

sorted_occurrences_test() ->
    Sudoku = sudokuerl:read_sudoku("test/sudoku.txt"),
    [{8,4}, {7,3}, {5,3}, {3,3}, {2,3}, {9,2}, {6,2}, {4,2}, {1,2}] =
        sudokuerl:sorted_occurrences(Sudoku).

empty_squares_data_test() ->
    Sudoku = sudokuerl:read_sudoku("test/sudoku.txt"),
    Side = maps:get(side, Sudoku),
    EmptySquaresData = sudokuerl:empty_squares_data(Sudoku),
    9 = maps:size(EmptySquaresData),
    [] = lists:seq(1,Side) -- maps:keys(EmptySquaresData),
    Sq1Actual = maps:get(1, EmptySquaresData),
    Sq1Model = {
        #{{1,1} => [], {1,2} => [], {1,3} => [],
          {2,1} => [],
          {3,1} => [], {3,2} => [], {3,3} => []},
        #{1 => [], 2 => [], 4 => [], 6 => [], 7 => [], 8 => [], 9 => []}
    },
    ?assertEqual(Sq1Model, Sq1Actual),
    Sq9Actual = maps:get(9, EmptySquaresData),
    Sq9Model = {
        #{{7,7} => [], {7,8} => [], {7,9} => [],
          {8,9} => [],
          {9,7} => [], {9,8} => [], {9,9} => []},
        #{3 => [], 4 => [], 5 => [], 6 => [], 7 => [], 8 => [], 9 => []}
    },
    ?assertEqual(Sq9Model, Sq9Actual).

find_available_positions_test() ->
    Sudoku = sudokuerl:read_sudoku("test/sudoku.txt"),
    Num = 7,
    TestPos = [{1,1}, {9,9}, {9,8}, {8,5}, {3,2}],
    Expected = [{8,5}, {3,2}],
    Actual = sudokuerl:find_available_positions(Sudoku, Num, TestPos, []),
    ?assertEqual(Expected, Actual).

update_squares_data_test() ->
    Sudoku = sudokuerl:read_sudoku("test/sudoku.txt"),
    EmptySquaresData = sudokuerl:empty_squares_data(Sudoku),
    Num = 4,
    NSquare = 5,
    NewPositions = [{6,4}, {5,6}],
    NewSquaresData =
        sudokuerl:update_squares_data(NSquare, Num, NewPositions,
                                      EmptySquaresData),
    {NewFreePos, NewMissingDigits} = maps:get(NSquare, NewSquaresData),
    [Num] = maps:get({6,4}, NewFreePos),
    [Num] = maps:get({5,6}, NewFreePos),
    [{5,6}, {6,4}] = maps:get(Num, NewMissingDigits).

get_quad_from_pos_test() ->
    1 = sudokuerl:get_square_from_pos(3, {1,1}),
    1 = sudokuerl:get_square_from_pos(3, {3,3}),
    4 = sudokuerl:get_square_from_pos(2, {3,3}),
    2 = sudokuerl:get_square_from_pos(3, {3,4}),
    4 = sudokuerl:get_square_from_pos(3, {4,3}),
    5 = sudokuerl:get_square_from_pos(3, {5,5}),
    3 = sudokuerl:get_square_from_pos(3, {3,7}),
    9 = sudokuerl:get_square_from_pos(3, {7,7}).

insert_test() ->
    Sudoku = sudokuerl:read_sudoku("test/sudoku.txt"),
    SquaresData = sudokuerl:empty_squares_data(Sudoku),
    Num = 4,
    Pos = {2,1},
    SqNum = sudokuerl:get_square_from_pos(maps:get(sqrt, Sudoku), Pos),
    {FreePos, MissingDigits} = maps:get(SqNum, SquaresData),
    
    % handcraft some data
    HandCraftMissingDigits1 = maps:update(2, [{2,1},{3,1}], MissingDigits),
    HandCraftMissingDigits2 = 
        maps:update(6, [{1,2},{2,1}], HandCraftMissingDigits1),
    HandCraftMissingDigits3 = maps:update(8, [{2,2}], HandCraftMissingDigits2),
    HandCraftFreePos = maps:update(Pos, [2,4,6,8], FreePos),
    HandCraftSquaresData = maps:update(SqNum,
                                       {HandCraftFreePos,
                                        HandCraftMissingDigits3},
                                       SquaresData),
    
    {NewSudoku, NewSquaresData} =
        sudokuerl:insert(Sudoku, Num, Pos, HandCraftSquaresData),
    Num = maps:get(Pos, NewSudoku),
    {NewFreePos, NewMissingDigits} = maps:get(SqNum, NewSquaresData),
    undefined = maps:get(Pos, NewFreePos, undefined),
    undefined = maps:get(Num, NewMissingDigits, undefined),
    [{3,1}] = maps:get(2, NewMissingDigits),
    [{1,2}] = maps:get(6, NewMissingDigits),
    [{2,2}] = maps:get(8, NewMissingDigits).

