%%%-------------------------------------------------------------------
%%% @author Hubert
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. sty 2015 02:48
%%%-------------------------------------------------------------------
-module(rAddressBookSupOTP).
-author("Hubert").
-version('1.0').
-behaviour(supervisor).

%API
-export([start_link/0,init/1]).

start_link() ->
  supervisor:start_link({local, rAddressBookSupOTP}, ?MODULE, []).

init(_) ->
  {ok, {{one_for_all, 2, 2000},[{rAddressBookOTP,{rAddressBookOTP, start_link, []},permanent, brutal_kill, worker, [rAddressBookOTP]}]}}.