-module(num_util_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

all() -> [{group, exported}, {group, internal}].

groups() ->
	[{exported,
		[shuffle],
		[bytes,
		 log2_est,
		 p2_short,
		 p2_long,
		 hamming]},
	 {internal, [shuffle], [p2_64, hamm14]}].

bytes(_) ->
	F = fun(N) -> num_util:bytes(N) end,
	1 = F(0),
	1 = F(1),
	1 = F(255),
	2 = F(256),
	2 = F(65535),
	3 = F(65536),
	3 = F(16777215),
	ok.

log2_est(_) ->
	F = fun(N) -> num_util:log2_est(N) end,
	
	% values less than 256
	0 = F(0),
	1 = F(1),
	2 = F(16#02),
	3 = F(16#04),
	3 = F(16#05),
	3 = F(16#06),
	3 = F(16#07),
	4 = F(16#08),
	5 = F(16#10),
	6 = F(16#20),
	7 = F(16#40),
	8 = F(16#80),
	
	% "large" values
	9 = F(16#100),
	49 = F(1 bsl 48),
	50 = F(1 bsl 49),
	50 = F(3 bsl 48),
	ok.

p2_64(_) ->
	F = fun(N) -> num_util:p2(N) end,
	% using lists:seq(0, 63) would prevent p2_64(1) from being tested.
	R = [F(1 bsl I) || I <- lists:seq(1, 64)],
	R = lists:seq(1, 64),
	ok.

p2_short(_) ->
	F = fun(N) -> num_util:p2(N) end,
	0 = F(0),
	0 = F(1),
	0 = F(12345),
	5 = F(12345 bsl 5),
	ok.

p2_long(_) ->
	F = fun(N) -> num_util:p2(N) end,
	R = F(1234567890123456789012), % long integer with few trailing 0
	R = p(1234567890123456789012),
	2039 = F(1 bsl 2039), % long integer with at least 64 trailing 0
	2040 = F(1 bsl 2040), % very long integer with at least 64 trailing 0
	ok.

hamm14(_) ->
	ok = hamming_check(lists:seq(0, 16383)),
	ok.

hamming(_) ->
	F = fun(N) -> num_util:hamming(N) end,
	All_1 = [F(1 bsl I) || I <- lists:seq(0, 63)],
	All_1 = lists:duplicate(64, 1),

	All_2 = [F(3 bsl I) || I <- lists:seq(0, 62)],
	All_2 = [F(1 bsl (I+1) + 1) || I <- lists:seq(0, 62)],
	All_2 = lists:duplicate(63, 2),

	% using a number of 89 bits to cover all case of hamming_bin/2
	4 = F(1 bsl 88 + 1 bsl 66 + 1 bsl 44 + 1 bsl 22),
	ok.

% counting bits the slow and safe way
h(N) -> h(N, 0).

h(0, C) -> C;
h(N, C) when N band 1 =:= 1 -> h(N bsr 1, C + 1);
h(N, C) -> h(N bsr 1, C).

hamming_check(L) ->
	F = fun(N) -> num_util:hamming(N) end,
	R = [F(N) || N <- L],
	R = [h(N) || N <- L],
	ok.

% very slow way of counting the number of trailing 0s
p(N) -> p(lists:reverse(integer_to_list(N, 2)), 0).

p([$0 | T], C) -> p(T, C + 1);
p(_, C) -> C.
