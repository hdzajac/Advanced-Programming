%%%-------------------------------------------------------------------
%%% @author Hubert
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. gru 2014 15:40
%%%-------------------------------------------------------------------
-module(rAddressBookSup).
-author("Hubert").

%% API
-export([start/0]).
-export([loop/0]).

start() -> process_flag(trap_exit, true),
           loop().

loop() ->
          receive
            {'EXIT',From,Reason} -> register(myBook,spawn_link(rAddressBook,loop,[[]])),
                                    loop()
          end.