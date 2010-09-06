-module(explane_packet).
-export([from_raw/1, to_raw/1]).
-include_lib("eunit/include/eunit.hrl").


%% Each packet has the following format: (variable/type/size in bytes)
%% header/string/5
%%   then N times
%% index/int/4, val1/float/4, val2/float/4, ... val8/float/4
%%   where N is the number of data items enabled in X-plane 
to_from_raw_test() ->
    Raw = [68,65,84,65,48,
           0,0,0,0,0,0,0,64,0,0,0,64,0,0,0,64,0,0,0,64,
           0,0,128,63,0,0,128,63,0,0,128,63,0,0,128,63,
           3,0,0,0,0,0,0,64,0,0,0,64,0,0,0,64,0,0,0,64,
           0,0,128,63,0,0,128,63,0,0,128,63,0,0,128,63],
    Packet = {"DATA0",
              [
               {0, [2.0, 2.0, 2.0, 2.0, 1.0, 1.0, 1.0, 1.0]},
               {3, [2.0, 2.0, 2.0, 2.0, 1.0, 1.0, 1.0, 1.0]}
              ]},
    [
     ?assertMatch(Packet, from_raw(Raw)),
     ?assertMatch(Raw, to_raw(Packet))
    ].
    


from_raw(Raw) ->
    {Header, Blocks} = lists:split(5, Raw),
    Binaries = split_blocks(Blocks),
    Rows = lists:map(fun parse_block/1, Binaries),
    Sorted = lists:sort(fun({A, _}, {B, _}) -> A =< B end, Rows),
    {Header, Sorted}.


to_raw({Header, Blocks}) ->
    RawBlocks = lists:foldl(fun(Block, Acc) ->
                                    RawBlock = raw_block(Block),
                                    << Acc/binary, RawBlock/binary >>
                                        end,
                            <<>>,
                            Blocks),
    Header ++ binary_to_list(RawBlocks).

%%
%% parse_block/1 will take a single block in the form 
%% index/int/4, val1/float/4, val2/float/4, ... val8/float/4
%% and output a tuple
%% {index, [val1, val2, ... val8]}
parse_block_test() ->
    RawBlock = <<
                3,0,0,0,
                0,0,0,64, 0,0,0,64, 0,0,0,64, 0,0,0,64,
                0,0,128,63, 0,0,128,63, 0,0,128,63, 0,0,128,63
                >>,
    ParsedBlock = {3, [2.0, 2.0, 2.0, 2.0, 1.0, 1.0, 1.0, 1.0]},
    [
     ?assertMatch(ParsedBlock, parse_block(RawBlock)),
     ?assertMatch(RawBlock, raw_block(ParsedBlock))
    ].

parse_block(Block) ->
    <<Index:32/integer-little, 
     A:32/float-little, B:32/float-little, C:32/float-little, D:32/float-little, 
     E:32/float-little, F:32/float-little, G:32/float-little, H:32/float-little>> = Block,
    {Index, [A, B, C, D, E, F, G, H]}.

raw_block({Index, [A, B, C, D, E, F, G, H]}) ->
    <<Index:32/integer-little, 
     A:32/float-little, B:32/float-little, C:32/float-little, D:32/float-little, 
     E:32/float-little, F:32/float-little, G:32/float-little, H:32/float-little>>.

%%
%% split_blocks/1 will take a list of 36-byte blocks, and parse the blocks
%% into separate binaries
%%
split_blocks_test() ->
    D = [3,0,0,0,117,74,170,64,151,175,250,64,84,202,250,64,13,81,93,
         55,0,192,121,196,155,247,195,64,92,77,16,65,198,175,126,55,
         20,0,0,0,50,199,245,191,240,169,109,194,224,226,114,65,202,
         110,75,63,0,192,121,196,215,226,114,65,0,0,64,192,0,0,116,194],
    Split = split_blocks(D),
    [
     ?assertMatch([
                    <<20,0,0,0,50,199,245,191,240,169,109,194,224,226,114,65,202,
                     110,75,63,0,192,121,196,215,226,114,65,0,0,64,192,0,0,116,194>>,
                    <<3,0,0,0,117,74,170,64,151,175,250,64,84,202,250,64,13,81,93,
                    55,0,192,121,196,155,247,195,64,92,77,16,65,198,175,126,55>>
                   ],
                   Split)
    ].

split_blocks(Blocks) ->
    split_blocks(Blocks, []).

split_blocks([], Acc) ->
    Acc;
split_blocks(Blocks, Acc) ->
    {Row, Rest} = lists:split(36, Blocks),
    split_blocks(Rest, [list_to_binary(Row)|Acc]).


