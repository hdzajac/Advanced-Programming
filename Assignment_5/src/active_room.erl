%%%-------------------------------------------------------------------
%%% @author huber
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. Oct 2017 22:37
%%%-------------------------------------------------------------------
-module(active_room).
-author("huber").
-behavior(gen_server).

%% API
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(room, {id, questions}).
-record(result, {dist, lastQ, total, final}).



init([Room, From]) ->
  Map = #{},
  Map1 = maps:put(id, Room#room.id, Map),
  Results = #result{dist = #{}, lastQ = #{}, total = #{}, final = false },
  Map15 = maps:put(results, Results, Map1),
  Map2 = maps:put(questions, array:from_list(Room#room.questions), Map15),
  Map3 = maps:put(currentQuestion, 0, Map2),
  Map4 = maps:put(active, true, Map3),
  Map5 = maps:put(conductor, From, Map4),
  Map6 = maps:put(players, [], Map5),
  Map7 = maps:put(activationTime, 0, Map6),
  {ok, Map7}.

handle_call({next_question}, From, State) ->
  Conductor = maps:get(conductor, State),
  if Conductor == From ->
      I = maps:get(currentQuestion, State),
      Questions  = maps:get(questions, State),
      Size = array:size(Questions),
      Active = maps:get(active, State),
      if I >= Size ->
          {reply, {error, "Out of questions :c"}};
        Active == true ->
          {reply, {error, has_active_question}, State};
        true ->
          State3 = maps:update(active, true, State),
          State4 = maps:update(activationTime, erlang:system_time(millisecond ),State3),
          Question = array:get(I, Questions),
          {reply, {ok, Question}, State4}
      end;
    true ->
      {reply, {error, who_are_you}, State}
  end;


handle_call({timesup}, From, State) ->
  Conductor = maps:get(conductor, State),
  if
    Conductor == From ->
      I = maps:get(currentQuestion, State),
      Active = maps:get(active, State),
      Size = array:size(maps:get(questions, State)),
      if
        Active == false ->
          {reply, no_question_asked, State};
        true ->
          State1 = maps:update(active, false, State),
          State2 = maps:update_with(currentQuestion,fun(V) -> V + 1 end, State1),
          if
            I + 1 == Size ->
              State3 = maps:update(results, false, maps:get(results, State2)#result{final = true}),
              {reply, {ok, maps:get(results, State3)}, State3};
            true ->
              {reply, {ok, maps:get(results, State2)}, State2}
          end
      end;
    true -> {reply, nice_try, State}
end;

handle_call({join, Nick}, _From, State) ->
  case maps:get(players, State) of
    [] ->
      State1 = State#{players => [{Nick}]},
      gen_server:call(maps:get(conductor, State1), {maps:get(conductor, State1),{player_joined, Nick, 1}}),
      {reply, {ok, Nick}, State1};
    List ->
      case lists:keyfind(Nick, 1, List) of
        false ->
          State1 = State#{players => [{Nick}|maps:get(players, State)]},
          gen_server:call(maps:get(conductor, State1), {maps:get(conductor, State1),{player_joined, Nick, size(maps:get(players, State1))}}),
          {reply, {ok, Nick}, State1};
        _ ->
          {reply, {error, Nick, is_taken}, State}
      end
  end;

handle_call({leave, Nick}, _From, State) ->
  case maps:get(players, State) of
    [] ->
      {reply, {error, "Can't leave an empty room", State}};
    List ->
      case lists:keyfind(Nick, 1, List) of
        false ->
          {reply, {error, "Can't leave room you haven't entered"}, State};
        _Yes ->
          State1 = State#{players => lists:keydelete(Nick, 1, List)},
          gen_server:call(maps:get(conductor, State1), {maps:get(conductor, State1),{player_left, Nick, size(maps:get(players, State1))}}),
          {reply, {ok, Nick}, State1}
      end
  end.

%%handle_call({guess, Ref, Index}, _From, State) ->
%%  case maps:get(active, State) of
%%    false ->
%%      {reply, {error, "No question was asked", State}};
%%    true ->
%%      {Description, Answers} = array:get(maps:get(currentQuestion, State),maps:get(questions, State)),
%%      Size = size(Answers),
%%      if
%%        Index >= Size ->
%%          {reply, {error, "Index out of bound, can't pick and answer that doesn't exist"}, State};
%%        true ->
%%          Res = maps:get(results, State),
%%          Res2 = Res#result{dist = maps:update_with(Index, fun(X) -> X + 1 end, State)},
%%          State1 = maps:update(results, Res2, State),
%%          case is_answer_correct(Answers, Index) of
%%            false ->
%%              {reply, {ok, false}, State1};
%%            true ->
%%
%%
%%          end
%%      end
%%      _ ->
%%      {reply, {error, "Weird error in guess"}, State}
%%  end.




handle_cast(_Request, _State) ->
  erlang:error(not_implemented).

handle_info(_Message, _Server) ->
  {noreply, _Server}.

terminate(_Reason, _Value) ->
  io:format("Server stopped.~n"),
  _Reason.

code_change(_OldVsn, [], _Extra) ->
  {ok, []}.
