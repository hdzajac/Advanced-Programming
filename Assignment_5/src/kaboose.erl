%%%-------------------------------------------------------------------
%%% @author huber
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. Oct 2017 09:18
%%%-------------------------------------------------------------------
-module(kaboose).
-author("huber").
-behavior(gen_server).


%% API
-export([get_a_room/1, add_question/2, start/0, get_questions/1, init/1, handle_call/3, handle_cast/2, play/1, handle_info/2, terminate/2, code_change/3, next/1, timesup/1, join/2, leave/2, rejoin/2, guess/3]).

start() -> gen_server:start_link({local,kaboose_server},kaboose_server,[],[]),
          {ok, kaboose_server}.

get_a_room(Server) ->
  gen_server:call(Server,{get_a_room}).

add_question(Room, {Description, Answers}) ->
  gen_server:call(kaboose_server,{add_question,Room, {Description, Answers}}).

get_questions(Room) ->
  gen_server:call(kaboose_server,{get_questions, Room}).

play(Room) ->
  gen_server:start_link(?MODULE, Room, []),
  gen_server:call(kaboose_server,{play, Room}).

next(ActiveRoom) ->
  gen_server:call(ActiveRoom, {next_question}).

timesup(ActiveRoom) ->
  gen_server:call(ActiveRoom, {timesup}).

join(ActiveRoom, Nick) ->
  gen_server:call(ActiveRoom, {join, Nick}).

leave(ActiveRoom, Ref) ->
  gen_server:call(ActiveRoom, {leave, Ref}).

rejoin(ActiveRoom, Ref) ->
  gen_server:call(ActiveRoom, {join, Ref}).

guess(ActiveRoom, Ref, Index) ->
  gen_server:call(ActiveRoom, {guess, Ref, Index}).

init(_) -> {ok, #{}}.

handle_call({_CRef, {player_joined, _Name, Active}}, _, State) ->
  Result = maps:is_key(active, State),
  if Result == true -> State = State#{active => Active};
      true -> State = State#{active => 1}
  end;

handle_call({_CRef, {player_left, _Name, Active}}, _, State) ->
  Result = maps:is_key(active, State),
  if Result == true -> State = State#{active => Active};
    true -> {error, "Sorry something went wrong buddy"}
  end.


handle_cast(_Request, _State) ->
  erlang:error(not_implemented).

handle_info(_Message, _Server) ->
  {noreply, _Server}.

terminate(_Reason, _Value) ->
  io:format("Server stopped.~n"),
  _Reason.

code_change(_OldVsn, [], _Extra) ->
  {ok, []}.
