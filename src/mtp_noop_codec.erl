%%% @author Sergey <me@seriyps.ru>
%%% @copyright (C) 2018, Sergey
%%% @doc
%%% Fake codec that returns it's input as output.
%%% Used in downstream handshake flow
%%% @end
%%% Created : 31 Oct 2018 by Sergey <me@seriyps.ru>

-module(mtp_noop_codec).
-behaviour(mtp_codec).
-export([new/0,
         try_decode_packet/2,
         encode_packet/2]).
-export_type([codec/0]).

-opaque codec() :: ?MODULE.

-spec new() -> codec().
new() ->
 io_lib:format("mtp_noop_codec      new ~n"),
    ?MODULE.

-spec try_decode_packet(binary(), codec()) -> {ok, binary(), binary(), codec()}.
try_decode_packet(<<>>, ?MODULE) ->
 io_lib:format("mtp_noop_codec      try_decode_packet1 ~n"),
    {incomplete, ?MODULE};
try_decode_packet(Data, ?MODULE) ->
 io_lib:format("mtp_noop_codec      try_decode_packet2 ~n"),
    {ok, Data, <<>>, ?MODULE}.

-spec encode_packet(binary(), codec()) -> {binary(), codec()}.
encode_packet(Data, ?MODULE) ->
 io_lib:format("mtp_noop_codec      encode_packet ~n"),
    {Data, ?MODULE}.
