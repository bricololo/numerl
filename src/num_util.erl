-module(num_util).

-define(N31, 2147483648). % 1 bsl 31

-export([log2_est/1, p2/1]).

log2_est(N) when N < 256 -> log2_bin(N);
log2_est(N) when N < 65536 -> 8 + log2_bin(N bsr 8);
log2_est(N) when N < 16777216 -> 16 + log2_bin(N bsr 16);
log2_est(N) when N < 4294967296 -> 24 + log2_bin(N bsr 24);
log2_est(N) ->
	case term_to_binary(N) of
		<<131, 111, S:32, R/binary>> -> 8 * S - 8 + log2_bin(binary:last(R));
		<<131, 110, S, R/binary>> -> 8 * S - 8 + log2_bin(binary:last(R))
	end.

log2_bin(N) when N > 15 -> log2_bin_l(N);
log2_bin(N) -> log2_bin_s(N).

log2_bin_l(N) when N > 63 -> (N band 128) bsr 7 + 7;
log2_bin_l(N) -> (N band 32) bsr 5 + 5.

log2_bin_s(0) -> 0;
log2_bin_s(N) when N > 3 -> (N band 8) bsr 3 + 3;
log2_bin_s(N) -> (N band 2) bsr 1 + 1.

% the largest power of 2 dividing N
p2(0) -> 0;
p2(N) when N band 1 =:= 1 -> 0;
p2(N) when N < ?N31 -> p2(N, 0);
p2(N) ->
	case term_to_binary(N) of
		<<131, 110, _:16, Bin/binary>> -> p2bin(0, Bin);
		<<131, 111, _:40, Bin/binary>> -> p2bin(0, Bin)
	end.

p2(N, P) when N band 1 =/= 0 -> P;
p2(N, P) when N band 2 =/= 0 -> P + 1;
p2(N, P) when N band 4 =/= 0 -> P + 2;
p2(N, P) when N band 8 =/= 0 -> P + 3;
p2(N, P) -> p2(N bsr 4, P + 4).

p2bin(P, <<0:72, Bin/binary>>) -> p2bin(P + 72, Bin);
p2bin(P, <<0:24, Bin/binary>>) -> p2bin(P + 24, Bin);
p2bin(P, <<0:8, Bin/binary>>) -> p2bin(P + 8, Bin);
p2bin(P, <<V:8, _/binary>>) -> p2(V, P).
