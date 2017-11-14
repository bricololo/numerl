-module(num_util).

-define(N32, 4294967296). % 1 bsl 32

-export([log2_est/1, p2/1]).

log2_est(N) when N < 256 -> log2_bin(N);
log2_est(N) ->
	BN = binary:encode_unsigned(N),
	<<H:8, _/binary>> = BN,
	byte_size(BN) bsl 3 - 8 + log2_bin(H).

log2_bin(N) when N > 15 -> log2_bin_l(N);
log2_bin(N) -> log2_bin_s(N).

log2_bin_l(N) when N > 63 -> (N band 128) bsr 7 + 7;
log2_bin_l(N) -> (N band 32) bsr 5 + 5.

log2_bin_s(N) when N > 3 -> (N band 8) bsr 3 + 3;
log2_bin_s(0) -> 0;
log2_bin_s(N) -> (N band 2) bsr 1 + 1.

% the largest power of 2 dividing N
p2(0) -> 0;
p2(N) when N band 1 =:= 1 -> 0;
p2(N) when N < ?N32 -> p2(N, 0);
p2(N) ->
	case term_to_binary(N) of
		<<131, 110, _:16, Bin/binary>> -> p2bin(0, Bin);
		<<131, 111, _:40, Bin/binary>> -> p2bin(0, Bin)
	end.

p2_8(N) ->
	case N bxor (N - 1) of
		1 -> 0;
		3 -> 1;
		7 -> 2;
		15 -> 3;
		31 -> 4;
		63 -> 5;
		127 -> 6;
		255 -> 7
	end.

p2(N, P) when N < 256 -> P + p2_8(N);
p2(N, P) ->
	case N band 255 of
		0 -> p2(N bsr 8, P + 8);
		V -> P + p2_8(V)
	end.

p2bin(P, <<0:32, Bin/binary>>) -> p2bin(P + 32, Bin);
p2bin(P, <<0:8, Bin/binary>>) -> p2bin(P + 8, Bin);
p2bin(P, <<V:8, _/binary>>) -> p2(V, P).
