%%%%%-------------------------------------------------------------------
%%%%% @author huber
%%%%% @copyright (C) 2017, <COMPANY>
%%%%% @doc
%%%%%
%%%%% @end
%%%%% Created : 10. Oct 2017 08:57
%%%%%-------------------------------------------------------------------
-module(test).
-author("huber").

%% API
-export([]).



-include_lib("eunit/include/eunit.hrl").

% run gen_server:stop(kaboose_server) before testing


init_test() ->

  ?assertMatch({ok,kaboose_server},kaboose:start()),
  ?assertMatch({error,_}, kaboose:get_questions(1)),
  ?assertMatch({ok,0},kaboose:get_a_room(kaboose_server)),
  ?assertMatch({ok,1},kaboose:get_a_room(kaboose_server)),
  ?assertMatch({ok,2},kaboose:get_a_room(kaboose_server)),
  ?assert(kaboose:get_questions(1)=:=[]),
  ?assert(kaboose:get_questions(0)=:=[]),
  ?assert(kaboose:get_questions(2)=:=[]),
  ?assertMatch({error,_}, kaboose:get_questions(100)),
  ?assertMatch([], kaboose:get_questions(1)).