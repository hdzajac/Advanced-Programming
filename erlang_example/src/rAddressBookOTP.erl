%%%-------------------------------------------------------------------
%%% @author Hubert
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. sty 2015 23:45
%%%-------------------------------------------------------------------
-module(rAddressBookOTP).
-behaviour(gen_server).
-author("Hubert").

%% API

-export([start_link/0,init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
%%-export([crash/0]).
%%-export([stop/0]).
%%-export([addContact/2]).
%%-export([addEmail/3]).
%%-export([addPhone/3]).
%%-export([addBirthDate/3]).
%%-export([removeContact/2]).
%%-export([removeEmail/1]).
%%-export([removePhone/1]).
%%-export([removeBirthDate/2]).
%%-export([getEmails/2]).
%%-export([getPhones/2]).
%%-export([findByEmail/1]).
%%-export([findByPhone/1]).
%%-export([findByDate/2]).


start_link() -> gen_server:start_link({local,rAddressBookOTP},?MODULE,[],[]).

init(_) -> {ok,[]}.

%%addContact(Name,Surname) ->
%%    gen_server:call(rAddressBookOTP,{addContact,Name,Surname}).
%%
%%addEmail(Name,Surname,Email) ->
%%    gen_server:call(rAddressBookOTP,{addEmail,Name,Surname,Email}).
%%
%%addPhone(Name,Surname,Phone) ->
%%    gen_server:call(rAddressBookOTP,{addPhone,Name,Surname,Phone}).
%%
%%addBirthDate(Name,Surname,{Day,Month,Year}) ->
%%    gen_server:call(rAddressBookOTP,{addBirthDate,Name,Surname,{Day,Month,Year}}).
%%
%%removeContact(Name,Surname) ->
%%    gen_server:call(rAddressBookOTP,{removeContact,Name,Surname}).
%%
%%removeEmail(Email) ->
%%    gen_server:call(rAddressBookOTP,{removeEmail,Email}).
%%
%%removePhone(Phone) ->
%%    gen_server:call(rAddressBookOTP,{removePhone,Phone}).
%%
%%removeBirthDate(Name,Surname) ->
%%    gen_server:call(rAddressBookOTP,{removeBirthDate,Name,Surname}).
%%
%%getEmails(Name,Surname) ->
%%    gen_server:call(rAddressBookOTP,{getEmails,Name,Surname}).
%%
%%getPhones(Name,Surname) ->
%%    gen_server:call(rAddressBookOTP,{getPhones,Name,Surname}).
%%
%%findByEmail(Email) ->
%%    gen_server:call(rAddressBookOTP,{findByEmail,Email}).
%%
%%findByPhone(Phone) ->
%%    gen_server:call(rAddressBookOTP,{findByPhone,Phone}).
%%
%%findByDate({D1,M1,Y1},{D2,M2,Y2}) ->
%%    gen_server:call(rAddressBookOTP,{findByDate,{D1,M1,Y1},{D2,M2,Y2}}).
%%
%%crash() ->
%%    gen_server:cast(rAddressBookOTP,crash).
%%
%%stop() ->
%%    gen_server:cast(rAddressBookOTP,stop).



handle_call({addContact,Name,Surname}, _ , MyBook) ->
  {reply,ok,addressBook:addContact(MyBook,Name,Surname)};

handle_call({addEmail,Name,Surname,Email}, _ , MyBook) ->
  {reply,ok,addressBook:addEmail(MyBook,Name,Surname,Email)};

handle_call({addPhone,Name,Surname,Phone}, _ , MyBook) ->
  {reply,ok,addressBook:addPhone(MyBook,Name,Surname,Phone)};

handle_call({addBirthDate,Name,Surname,{Day,Month,Year}}, _ , MyBook) ->
  {reply,ok,addressBook:addBirthDate(MyBook,Name,Surname,{Day,Month,Year})};

handle_call({removeContact,Name,Surname}, _ , MyBook) ->
  {reply,ok,addressBook:removeContact(MyBook,Name,Surname)};

handle_call({removeEmail,Email}, _ , MyBook) ->
  {reply,ok,addressBook:removeEmail(MyBook,Email)};

handle_call({removePhone,Phone}, _ , MyBook) ->
  {reply,ok,addressBook:removePhone(MyBook,Phone)};

handle_call({getEmails,Name,Surname}, _ , MyBook) ->
  {reply,ok,addressBook:getEmails(MyBook,Name,Surname)};

handle_call({getPhones,Name,Surname}, _ , MyBook) ->
  {reply,ok,addressBook:getPhones(MyBook,Name,Surname)};

handle_call({findByEmail,Email}, _ , MyBook) ->
  {reply,ok,addressBook:findByEmail(MyBook,Email)};

handle_call({findByPhone,Phone}, _ , MyBook) ->
  {reply,ok,addressBook:findByPhone(MyBook,Phone)};

handle_call({findByDate,{D1,M1,Y1},{D2,M2,Y2}}, _ , MyBook) ->
  {reply,ok,addressBook:findByDate(MyBook,{D1,M1,Y1},{D2,M2,Y2})}.

handle_cast(stop, Mybook) ->
  {stop, normal, Mybook};

handle_cast(crash, Mybook) ->
  1/0,
  {noreply, Mybook}.

handle_info(_Message, AddressBook) ->
  {noreply, AddressBook}.

terminate(Reason, _Value) ->
  io:format("Server stopped.~n"),
  Reason.

code_change(_OldVsn, AddressBook, _Extra) ->
  {ok, AddressBook}.



