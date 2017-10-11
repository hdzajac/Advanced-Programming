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
  Map4 = maps:put(active, false, Map3),
  Map5 = maps:put(conductor, From, Map4),
  Map6 = maps:put(players, [], Map5),
  Map7 = maps:put(activationTime, 0, Map6),
  {ok, Map7}.

handle_call({next_question}, {From, _Tag}, State) ->
  Conductor = maps:get(conductor, State),
  if
    Conductor == From ->
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


handle_call({timesup}, {From, _Tag}, State) ->
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
          erlang:display(I),
          erlang:display(Size),
          erlang:display(State2),
          if
            I + 1 == Size ->
              erlang:display("In da last loop"),
              Res = maps:get(results, State2),
              State3 = maps:update(results, Res#result{final = true}, State2 ),
              {_Desc, Answers} = array:get(maps:get(currentQuestion, State3) - 1, maps:get(questions, State3)),
              Response = {ok, make_me_a_list(Res#result.dist, length(Answers),0), Res#result.lastQ, Res#result.total, Res#result.final},
              {reply, Response, State3};
            I + 1 < Size ->
              Res = maps:get(results, State2),
              {_Desc, Answers} = array:get(maps:get(currentQuestion, State2) -1, maps:get(questions, State2)),
              Response = {ok, make_me_a_list(Res#result.dist, length(Answers),0), Res#result.lastQ, Res#result.total, Res#result.final},
              {reply, Response, State2}
          end
      end;
    true -> {reply, nice_try, State}
end;

handle_call({join, Nick}, _From, State) ->
  case maps:get(players, State) of
    [] ->
      State1 = State#{players => [{Nick}]},
      maps:get(conductor, State1) ! {maps:get(conductor, State1),{player_joined, Nick, 1}},
      {reply, {ok, Nick}, State1};
    List ->
      case lists:keyfind(Nick, 1, List) of
        false ->
          State1 = State#{players => [{Nick}|maps:get(players, State)]},
          maps:get(conductor, State1) ! {maps:get(conductor, State1),{player_joined, Nick, length(maps:get(players, State1))}},
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
          maps:get(conductor, State1) ! {maps:get(conductor, State1),{player_left, Nick, length(maps:get(players, State1))}},
          {reply, {ok, Nick}, State1}
      end
  end;

handle_call({guess, Ref, I}, _From, State) ->
  Index = I - 1,
  Time = erlang:system_time(millisecond),
  case maps:get(active, State) of
    false ->
      {reply, {error, "No question was asked", State}};
    true ->
      {_Description, Answers} = array:get(maps:get(currentQuestion, State),maps:get(questions, State)),
      Size = length(Answers),
      if
        Index >= Size ->
          {reply, {error, "Index out of bound, can't pick and answer that doesn't exist"}, State};
        Index < Size ->
          Res = maps:get(results, State),
          case maps:is_key(Index, Res#result.dist) of
            true ->
              Res2 = Res#result{dist = maps:update_with(Index, fun(X) -> X + 1 end, Res#result.dist)};
            false ->
              Dist = Res#result.dist,
              Res2 = Res#result{dist = Dist#{Index => 1}}
          end,
          State1 = maps:update(results, Res2, State),
          case is_answer_correct(Answers, Index) of
            false ->
              {reply, {ok, false}, State1};
            true ->
              Points = calculate_points(State1, Time),
              Res3 = maps:get(results, State1),
              LastQ = Res3#result.lastQ,
              State2 = maps:update(results, Res3#result{lastQ = maps:put(Ref, Points, LastQ)},State1),
              Total = Res3#result.total,
              case maps:get(currentQuestion, State2) of
                0 ->
                  State3 = maps:update(results, Res3#result{total = Total#{Ref => Points}},State2);
                _ ->
                  State3 = maps:update(results, Res3#result{total = Total#{Ref => Points + maps:get(Ref, Res3#result.total)}},State2)
              end,
              {reply, {ok, "Correct Answer " }, State3}
          end
      end;
      _ ->
        {reply, {error, "Weird error in guess"}, State}
  end.




handle_cast(_Request, _State) ->
  erlang:error(not_implemented).

handle_info(_Message, _Server) ->
  {noreply, _Server}.

terminate(_Reason, _Value) ->
  io:format(_Reason),
  io:format("Server stopped.~n"),
  _Reason.



code_change(_OldVsn, [], _Extra) ->
  {ok, []}.


%% Utils
is_answer_correct([], _Index) ->
  false;

is_answer_correct([{correct, _}|_T], 0) ->
  true;

is_answer_correct([_H|_T], 0) ->
  false;

is_answer_correct([_H|T], Index) ->
  is_answer_correct(T, Index - 1).



calculate_points(#{activationTime := ActTime}, Time) ->
  Diff = Time - ActTime,
  if
    Diff >= 500 -> 500;
    true -> 1000
  end.


make_me_a_list(_Map, 0,_Index)->
  [];

make_me_a_list(Map, Answers, Index)->
  case maps:is_key(Index, Map) of
    true -> [maps:get(Index, Map) | make_me_a_list(Map, Answers - 1, Index + 1)] ;
    false ->  [0 | make_me_a_list(Map, Answers - 1, Index + 1)]
  end.
