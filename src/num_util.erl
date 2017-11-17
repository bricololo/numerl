-module(num_util).

-define(N64, 18446744073709551616). % 1 bsl 64

-export([bytes/1, log2_est/1, p2/1, hamming/1]).

% minimal number of bytes to represent N in binary
bytes(N) -> byte_size(binary:encode_unsigned(N)).

% minimal number of bits to represent N in binary, for 0, returns 0.
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
p2(N) when N < ?N64 -> p2_64(N);
p2(N) ->
	case N band 16#ffffffffffffffff of
		0 ->
			case term_to_binary(N) of
				<<131, 110, _:80, Bin/binary>> -> p2_bin(64, Bin);
				<<131, 111, _:104, Bin/binary>> -> p2_bin(64, Bin)
			end;
		V -> p2_64(V)
	end.

p2_64(N) ->
	lists:nth(
		(N bxor (N - 1)) rem 67, % the remainder can't be 0, 33 or 66
		[0,38,1,14,39,22,2,11,15,58,40,18,23,53,3,63,12,9,16,61,59,27,41,29,19,
		50,24,43,54,46,4,31,bad,37,13,21,10,57,17,52,62,8,60,26,28,49,42,45,30,
		36,20,56,51,7,25,48,44,35,55,6,47,34,5,33,32]).

p2_bin(P, <<0:64, Bin/binary>>) -> p2_bin(P + 64, Bin);
p2_bin(P, <<T:8/binary, _/binary>>) -> p2_f(P, T);
p2_bin(P, Bin) -> p2_f(P, Bin).

p2_f(P, T) ->
	P +
		p2_64(
			binary:decode_unsigned(
				list_to_binary(lists:reverse(binary_to_list(T))))).

% count the numbers of bits set to 1 in N.
hamming(N) when N < 16384 -> hamm14(N);
hamming(N) when N < 268435456 -> hamming28(N);
hamming(N) -> hamming_bin(binary:encode_unsigned(N), 0).

hamming28(N) -> hamm14(N band 16#3fff) + hamm14((N band 16#fffc000) bsr 14).

hamm14(N) -> ((N * 16#200040008001) band 16#111111111111111) rem 15.

hamming_bin(<<A:14, B:14, C:14, D:14, Bin/binary>>, P) ->
	hamming_bin(Bin, P + hamm14(A) + hamm14(B) + hamm14(C) + hamm14(D));
hamming_bin(<<A:4, B:14, C:14, Bin/binary>>, P) ->
	hamming_bin(Bin, P + hamm14(A) + hamm14(B) + hamm14(C));
hamming_bin(Bin, P) -> P + hamming28(binary:decode_unsigned(Bin)).
