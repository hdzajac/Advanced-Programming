%%%-------------------------------------------------------------------
%%% @author huber
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. Oct 2017 09:18
%%%-------------------------------------------------------------------
-module(server).
-author("huber").

%% API
-export([getRoom/1]).


-record(player, {username, score}).
-record(room, {id, players, questions}).


getRoom([])-> [#room{id = 0, players = [], questions = []}];
getRoom([H|T]) -> [#room{id = H#room.id + 1, players = [], questions = [] }|[H|T]].