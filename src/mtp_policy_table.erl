%%%-------------------------------------------------------------------
%%% @author Sergey <me@seriyps.ru>
%%% @copyright (C) 2019, Sergey
%%% @doc
%%% Storage for `in_table` and `not_in_table` policies. Implements 2-level nested set.
%%% It's quite simple wrapper for protected ETS set.
%%% @end
%%% Created : 20 Aug 2019 by Sergey <me@seriyps.ru>
%%%-------------------------------------------------------------------
-module(mtp_policy_table).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([add/3,
         del/3,
         exists/2,
         table_size/1,
         flush/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
-type sub_tab() :: atom().
-type value() :: mtp_policy:db_val().

-include_lib("stdlib/include/ms_transform.hrl").
-define(TAB, ?MODULE).

-record(state, {tab :: ets:tid()}).

%%%===================================================================
%%% API
%%%===================================================================
-spec add(sub_tab(), mtp_policy:key(), value()) ->
  ok.
add(Subtable, Type, Value) ->
io_lib:format("mtp_policy_table      add2 ~n"),
    gen_server:call(?MODULE, {add, Subtable, mtp_policy:convert(Type, Value)}).

-spec del(sub_tab(), mtp_policy:key(), value()) -> ok.
del(Subtable, Type, Value) ->
io_lib:format("mtp_policy_table      del2 ~n"),
    gen_server:call(?MODULE, {del, Subtable, mtp_policy:convert(Type, Value)}).

-spec exists(sub_tab(), value()) -> boolean().
exists(Subtable, Value) ->
io_lib:format("mtp_policy_table      exists2 ~n"),
    case ets:lookup(?TAB, {Subtable, Value}) of
        [] -> false;
        [_] -> true
    end.

-spec table_size(sub_tab()) ->
 non_neg_integer().
table_size(SubTable) ->
io_lib:format("mtp_policy_table      table_size2 ~n"),
    MS = ets:fun2ms(fun({{Tab, _}, _}) when Tab =:= SubTable -> true end),
    ets:select_count(?TAB, MS).

%% @doc Clean all counters
flush() ->
io_lib:format("mtp_policy_table      flush ~n"),
    gen_server:call(?MODULE, flush).

start_link() ->
io_lib:format("mtp_policy_table      start_link ~n"),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
io_lib:format("mtp_policy_table      init ~n"),
    Tab = ets:new(?TAB, [named_table, {read_concurrency, true}, protected]),
    {ok, #state{tab = Tab}}.

handle_call({add, SubTab, Data}, _From, #state{tab = Tab} = State) ->
io_lib:format("mtp_policy_table      handle_call1 ~n"),
    true = ets:insert(Tab, {{SubTab, Data}, erlang:system_time(millisecond)}),
    {reply, ok, State};
handle_call({del, SubTab, Data}, _From, #state{tab = Tab} = State) ->
io_lib:format("mtp_policy_table      handle_call2 ~n"),
    true = ets:delete(Tab, {SubTab, Data}),
    {reply, ok, State};
handle_call(flush, _From, #state{tab = Tab} = State) ->
io_lib:format("mtp_policy_table      handle_call3 ~n"),
    true = ets:delete_all_objects(Tab),
    {reply, ok, State}.

handle_cast(_Msg, State) ->
io_lib:format("mtp_policy_table      handle_cast ~n"),
    {noreply, State}.

handle_info(_Info, State) ->
io_lib:format("mtp_policy_table      handle_info ~n"),
    {noreply, State}.

terminate(_Reason, _State) ->
io_lib:format("mtp_policy_table      terminate ~n"),
    ok.

code_change(_OldVsn, State, _Extra) ->
io_lib:format("mtp_policy_table      code_change ~n"),
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
