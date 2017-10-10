%%%-------------------------------------------------------------------
%%% @author huber
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. Oct 2017 14:35
%%%-------------------------------------------------------------------
-module(kaboose_server).
-behavior(gen_server).
-author("huber").

%% API
-export([init/1, handle_call/3, handle_cast/2, start/0]).



%% server registered locally with id: "kaboose_server"
start() -> gen_server:start_link({local,kaboose_server},?MODULE,[],[]).

init(_) -> {ok,[]}.


%% Sync
handle_call({get_a_room}, _ , Server) ->
  [H|T] = server:get_a_room(Server),
  {reply, {ok, H}, [H|T]}.


%% Async
handle_cast(Request, State) ->
  erlang:error(not_implemented).




handle_info(_Message, Server) ->
  {noreply, Server}.

terminate(Reason, _Value) ->
  io:format("Server stopped.~n"),
  Reason.