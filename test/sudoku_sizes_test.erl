-module(sudoku_sizes_test).

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
    sudokuerl:print_sudoku(Sudoku),
    file:delete("sudoku_sizes_test_tmp").
