%%%-------------------------------------------------------------------
%%% @author Hubert
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. gru 2014 15:00
%%%-------------------------------------------------------------------
-module(rAddressBook).
-version('1.0').
-author("Hubert").

%% API
-export([start/0]).
-export([stop/0]).
-export([init/0]).
-export([crash/0]).
-export([loop/1]).
-export([getBook/0]).
-export([addContact/2]).
-export([addEmail/3]).
-export([addPhone/3]).
-export([addBirthDate/3]).
-export([removeContact/2]).
-export([removeEmail/1]).
-export([removePhone/1]).
-export([removeBirthDate/2]).
-export([getEmails/2]).
-export([getPhones/2]).
-export([findByEmail/1]).
-export([findByPhone/1]).
-export([findByDate/2]).


start() -> register(myBook,spawn(rAddressBook,init,[])).

stop() -> myBook ! {self(),stop}.

init() ->  register(mySup,spawn_link(rAddressBookSup,start,[])),
            rAddressBook:loop(addressBook:createAddressBook()).


loop(MyBook) ->
                receive
                  {PID,stop} -> PID ! ok;
                  {PID,addContact,N,S} ->  loop(addressBook:addContact(MyBook,N,S));
                  {PID,addEmail,N,S,E} ->  loop(addressBook:addEmail(MyBook,N,S,E));
                  {PID,addPhone,N,S,P} ->  loop(addressBook:addPhone(MyBook,N,S,P));
                  {PID,addBirthDate,N,S,{D,M,Y}} ->  loop(addressBook:addBirthDate(MyBook,N,S,{D,M,Y}));
                  {PID,removeContact,N,S} -> PID ! ok,
                                             loop(addressBook:removeContact(MyBook,N,S));
                  {PID,removeEmail,E} -> PID ! ok,
                                         loop(addressBook:removeEmail(MyBook,E));
                  {PID,removePhone,P} -> PID ! ok,
                                         loop(addressBook:removePhone(MyBook,P));
                  {PID,removeBirthDate,N,S} -> PID ! ok,
                                         loop(addressBook:removeBirthDate(MyBook,N,S));
                  {PID,getEmails,N,S} -> PID ! addressBook:getEmails(MyBook,N,S),
                                         loop(MyBook);
                  {PID,getPhones,N,S} -> PID ! addressBook:getPhones(MyBook,N,S),
                                         loop(MyBook);
                  {PID,getBook} -> PID ! MyBook,
                                   loop(MyBook);
                  {PID,findByEmail,E} -> PID ! addressBook:findByEmail(MyBook,E),
                                          loop(MyBook);
                  {PID,findByPhone,P} -> PID ! addressBook:findByPhone(MyBook,P),
                                          loop(MyBook);
                  {PID,findByDate,{D1,M1,Y1},{D2,M2,Y2}} -> PID ! addressBook:findByDate(MyBook,{D1,M1,Y1},{D2,M2,Y2}),
                                          loop(MyBook);
                  {PID,crash} -> 1/0
                end.

crash() -> myBook ! {self(),crash}.

addContact(Name,Surname) -> myBook ! {self(),addContact,Name,Surname}.
addEmail(Name,Surname,Email) -> myBook ! {self(),addEmail,Name,Surname,Email}.
addPhone(Name,Surname,Phone) -> myBook ! {self(),addPhone,Name,Surname,Phone}.
addBirthDate(Name,Surname,{Day,Month,Year}) -> myBook ! {self(),addBirthDate,Name,Surname,{Day,Month,Year}}.
removeContact(Name,Surname) -> myBook ! {self(),removeContact,Name, Surname}.
removeEmail(Email) -> myBook ! {self(),removeEmail,Email},
                      receive
                        ok -> ok
                      end.
removePhone(Phone) -> myBook ! {self(),removePhone,Phone},
                      receive
                       ok -> ok
                      end.
removeBirthDate(Name,Surname) -> myBook ! {self(),removeBirthDate,Name,Surname},
                                  receive
                                    ok -> ok
                                  end.
getEmails(Name,Surname) -> myBook ! {self(),getEmails,Name,Surname},
                            receive
                              Emails -> Emails
                            end.
getPhones(Name,Surname) -> myBook ! {self(),getPhones,Name,Surname},
                           receive
                             Phones -> Phones
                           end.
getBook() -> myBook ! {self(),getBook},
            receive
            Book -> Book
            end.
findByEmail(Email) -> myBook ! {self(),findByEmail, Email},
                  receive
                    Name -> Name
                  end.
findByPhone(Phone) -> myBook ! {self(),findByPhone, Phone},
                  receive
                    Name -> Name
                  end.
findByDate({D1,M1,Y1},{D2,M2,Y2}) -> myBook ! {self(),findByDate, {D1,M1,Y1},{D2,M2,Y2}},
                  receive
                   Name -> Name
                  end.



