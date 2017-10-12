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
-record(result, {ok, dist, lastQ, total, final}).
-record(state, {results, questions, currentQuestion, active, conductor, players, activationTime}).



init([Room, From]) ->
  Results = #result{ok = ok, dist = [], lastQ = #{}, total = #{}, final = false },
  State = #state{results = Results,
    questions = array:from_list(Room#room.questions),
    currentQuestion = 0,
    active = false,
    conductor = From,
    players = [],
    activationTime = 0},
  {ok, State}.

handle_call({next_question}, {From, _Tag}, S) ->
  Conductor = S#state.conductor,
  if
    Conductor == From ->
      I = S#state.currentQuestion,
      Questions = S#state.questions,
      Size = array:size(Questions),
      Active = S#state.active,
      if
        I >= Size ->
          {reply, {error, "Out of questions :c"}, S};
        Active == true ->
          {reply, {error, has_active_question}, S};
        true ->
          Question = array:get(I, Questions),
          S0 = S#state{results = S#state.results#result{dist = list_init(length(element(2,Question)))}},
          S1 = S0#state{results = S0#state.results#result{lastQ = #{}}},
          S2 = S1#state{active = true},
          S3 = S2#state{activationTime = erlang:system_time(millisecond)},
          {reply, {ok, Question}, S3}
      end;
    true ->
      {reply, {error, who_are_you}, S}
  end;


handle_call({timesup}, {From, _Tag}, S) ->
  Conductor = S#state.conductor,
  if
    Conductor == From ->
      I = S#state.currentQuestion,
      Active = S#state.active,
      Size = array:size(S#state.questions),
      if
        Active == false ->
          {reply, no_question_asked, S};
        true ->
          S1 = S#state{active = false},
          S2 = S1#state{currentQuestion = S1#state.currentQuestion + 1},
          if
            I + 1 == Size ->
              Res = S2#state.results,
              S3 = S2#state{results = Res#result{final = true}},
              {_Desc, Answers} = array:get(S3#state.currentQuestion - 1, S3#state.questions),
              {reply, S3#state.results, S3};
            I + 1 < Size ->
              {_Desc, Answers} = array:get(S2#state.currentQuestion - 1, S2#state.questions),
              {reply, S2#state.results, S2}
          end
      end;
    true -> {reply, nice_try, S}
end;

handle_call({join, Nick}, _From, S) ->
  case S#state.players of
    [] ->
      S1 = S#state{players = [{Nick}]},
      S1#state.conductor ! {S1#state.conductor,{player_joined, Nick, 1}},
      {reply, {ok, Nick}, S1};
    List ->
      case lists:keyfind(Nick, 1, List) of
        false ->
          S1 = S#state{players = [{Nick} | S#state.players]},
          S1#state.conductor ! {S1#state.conductor,{player_joined, Nick, length(List) + 1}},
          {reply, {ok, Nick}, S1};
        _ ->
          {reply, {error, Nick, is_taken}, S}
      end
  end;

handle_call({leave, Nick}, _From, S) ->
  case S#state.players of
    [] ->
      {reply, {error, "Can't leave an empty room", S}};
    List ->
      case lists:keyfind(Nick, 1, List) of
        false ->
          {reply, {error, "Can't leave room you haven't entered"}, S};
        _Yes ->
          S1 = S#state{players = lists:keydelete(Nick, 1, List)},
          S1#state.conductor ! {S1#state.conductor,{player_left, Nick, length(List) - 1}},
          {reply, {ok, Nick}, S1}
      end
  end;

handle_call({guess, Ref, I}, _From, S) ->
  Index = I - 1,
  Time = erlang:system_time(millisecond),
  case S#state.active of
    false ->
      {reply, {error, "No question was asked", S}};
    true ->
      {_Description, Answers} = array:get(S#state.currentQuestion, S#state.questions),
      Size = length(Answers),
      if
        Index >= Size ->
          {reply, {error, "Index out of bound, can't pick and answer that doesn't exist"}, S};
        Index < Size ->
          Dist = S#state.results#result.dist,
          S1 = S#state{results = S#state.results#result{dist = add_try(Dist, Index)}},
          case is_answer_correct(Answers, Index) of
            false ->
              {reply, {ok, false}, S1};
            true ->
              Points = calculate_points(S1, Time),
              Res3 = S1#state.results,
              LastQ = Res3#result.lastQ,
              S2 = S1#state{results = Res3#result{lastQ = maps:put(Ref, Points, LastQ)}},
              Res4 = S2#state.results,
              Total = Res4#result.total,
              case maps:is_key(Ref, Total) of
                true -> S3 = S2#state{results = Res4#result{total = Total#{Ref => Points + maps:get(Ref, Res4#result.total)}}};
                false -> S3 = S2#state{results = Res4#result{total = Total#{Ref => Points}}}
              end,
              {reply, {ok, "Correct Answer " }, S3}
          end
      end;
      _ ->
        {reply, {error, "Weird error in guess"}, S}
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



calculate_points(S, Time) ->
  ActTime = S#state.activationTime,
  Diff = Time - ActTime,
  if
    Diff >= 500 -> 500;
    true -> 1000
  end.

list_init(0)->
  [];

list_init(I) ->
  [0|list_init(I-1)].


add_try([H|T],0) ->
  [H+1|T];

add_try([H|T], I) ->
  [H|add_try(T,I-1)].