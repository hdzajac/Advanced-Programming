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
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(room, {id, questions}).



%% server registered locally with id: "kaboose_server"

init(_) -> {ok,[]}.



%% Sync

%% returning room id as an opaque datatype
handle_call({get_a_room}, _ , State) ->
  [H|T] = get_a_room(State),
  {reply, {ok, H#room.id}, [H|T]};

handle_call({add_question, Room, {Description, Answers}}, _, State) ->
  [H|T] = add_question(State, Room, {Description, Answers}),
  {reply, ok, [H|T]};

handle_call({get_questions, RoomID}, _, State) ->
  Room = get_questions(State, RoomID),
  {reply, Room#room.questions, State};

handle_call({play, RoomID, CondPID}, _From, State) ->
  {ok, ActiveRoom} = gen_server:start_link(active_room,[lists:keyfind(RoomID, 2, State), CondPID],[]),
  {reply, {ActiveRoom, CondPID}, State}.



%% Async
handle_cast(_Request, _State) ->
  erlang:error(not_implemented).

handle_info(_Message, _Server) ->
  {noreply, _Server}.

terminate(_Reason, _Value) ->
  io:format("Server stopped.~n"),
  _Reason.

code_change(_OldVsn, [], _Extra) ->
  {ok, []}.



find_room([H|T],ID) when is_record(H, room) ->
  if
    H#room.id == ID -> H;
    true -> find_room(T,ID)
  end;

find_room([],_) -> false.


get_a_room([])-> [#room{id = 0, questions = []}];
get_a_room([H|T]) -> [#room{id = H#room.id + 1, questions = [] }|[H|T]].


%% TODO: check answers
add_question([], _ , _ ) -> {error, "Can't add questions to empty server"};
add_question([H|T], RoomID, {Description, Answers}) ->
  case find_room([H|T],RoomID) of
    false -> {error, "Can't add questions to room that doesn't exist."};
    Q -> lists:keyreplace(RoomID,2,[H|T],Q#room{questions = Q#room.questions ++ [{Description, Answers}]})
  end.


get_questions([], _ ) -> {error, "Can't get questions from an empty server"};
get_questions([H|T], RoomID) ->
  case find_room([H|T],RoomID) of
    false -> {error, "Can't get questions from a room that doesn't exist."};
    _ -> lists:keyfind(RoomID,2,[H|T])
  end.
