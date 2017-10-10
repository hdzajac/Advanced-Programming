%%%-------------------------------------------------------------------
%%% @author Hubert
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. lis 2014 15:00
%%%-------------------------------------------------------------------
-module(addressBook).
-author("Hubert").

-export([createAddressBook/0]).
-export([containsName/2]).
-export([containsEmail/2]).
-export([containsPhoneNmbr/2]).
-export([addEmail/4]).
-export([addContact/3]).
-export([addPhone/4]).
-export([addBirthDate/4]).
-export([removeContact/3]).
-export([removeEmail/2]).
-export([removePhone/2]).
-export([removeBirthDate/3]).
-export([getEmails/3]).
-export([getPhones/3]).
-export([findByEmail/2]).
-export([findByPhone/2]).
-export([findByDate/3]).
-export([isBetween/3]).


-record(person,{name_surname,phoneNmbr, email, birthDate}).

%% API

createAddressBook() -> [].

%%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Swietne funkcje ktore napisalem bo nie przeczytalem dokumentacji~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

containsName([H|T],{N,S}) -> if
                               H#person.name_surname == {N,S} -> H;
                               true -> containsName(T,{N,S})
                             end;
containsName([],{N,S}) -> false.

containsEmail([H|T],Email) -> case lists:member(Email,H#person.email) of
                                true -> H;
                                _ -> containsEmail(T,Email)
                              end;
containsEmail([],Email) -> false.

containsPhoneNmbr([H|T],PhoneNmbr) -> case lists:member(PhoneNmbr,H#person.phoneNmbr) of
                                        true -> H;
                                        _ -> containsPhoneNmbr(T,PhoneNmbr)
                                      end;
containsPhoneNmbr([],PhoneNmbr) -> false.

containsDate([H|T],{D1,M1,Y1},{D2,M2,Y2},Stack) -> case isBetween(H,{D1,M1,Y1},{D2,M2,Y2}) of
                                                     true -> containsDate(T,{D1,M1,Y1},{D2,M2,Y2},Stack ++ [H]);
                                                     _ -> containsDate(T,{D1,M1,Y1},{D2,M2,Y2},Stack)
                                                   end;
containsDate([],{D1,M1,Y1},{D2,M2,Y2},Stack) -> Stack.

isBetween(H,{D1,M1,Y1},{D2,M2,Y2}) -> {HD,HM,HY} = H#person.birthDate,
  if
    (HY < Y2) and (HY > Y1)-> true;
    HY == Y2 -> if
                  HM < M2 -> true;
                  HM == M2 -> if
                                HD =< D2 -> true;
                                true -> false
                              end;
                  true -> false
                end;
    HY == Y1 -> if
                  HM > M1 -> true;
                  HM == M1 -> if
                                HD >= D1 -> true;
                                true -> false
                              end;
                  true -> false
                end;
    true -> false
  end.


%%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Dodawanie~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

addContact(AddressBook,Name,Surname) -> case  containsName(AddressBook,{Name,Surname}) of
                                          false -> AddressBook ++ [#person{name_surname = {Name,Surname},phoneNmbr =  [],email =  [], birthDate = {0,0,0}}];
                                          _ -> error({badarg,"Already exists! "})
                                        end.

addEmail(AddressBook,Name,Surname,Email) -> case containsEmail(AddressBook,Email) of
                                              false -> case containsName(AddressBook,{Name,Surname}) of
                                                         false -> AddressBook ++ [#person{name_surname = {Name,Surname}, phoneNmbr = [], email = [Email], birthDate = {0,0,0}}];
                                                         H -> lists:keyreplace({Name,Surname},2,AddressBook,H#person{email = H#person.email ++ [Email]})
                                                       end;
                                              _ -> error({badarg, "Address exists! "})
                                            end.

addPhone(AddressBook,Name,Surname,Phone) -> case containsPhoneNmbr(AddressBook,Phone) of
                                              false -> case containsName(AddressBook,{Name,Surname}) of
                                                         false -> AddressBook ++ [#person{name_surname = {Name,Surname}, phoneNmbr = [Phone], email = [], birthDate = {0,0,0}}];
                                                         H -> lists:keyreplace({Name,Surname},2,AddressBook,H#person{phoneNmbr = H#person.phoneNmbr ++ [Phone]})
                                                       end;
                                              _ -> error({badarg, "PhoneNmbr exists! "})
                                            end.

addBirthDate(AddressBook, Name, Surname,{Day, Month, Year}) -> case containsName(AddressBook, {Name,Surname}) of
                                                                 false ->  AddressBook ++ [#person{name_surname = {Name,Surname},phoneNmbr =  [],email =  [], birthDate = {Day,Month,Year}}];
                                                                 H -> lists:keyreplace({Name,Surname},2,AddressBook,H#person{birthDate = {Day,Month,Year}})
                                                               end.


%%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Usuwnie~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

removeContact(AddressBook,Name,Surname) -> lists:keydelete({Name,Surname},2,AddressBook).

removeEmail(AddressBook,Email) -> case containsEmail(AddressBook,Email) of
                                    false -> error({badarg,"No such Email!"});
                                    H -> lists:keyreplace(H#person.name_surname,2,AddressBook,H#person{email = H#person.email -- [Email]})
                                  end.

removePhone(AddressBook,PhoneNmbr) -> case containsPhoneNmbr(AddressBook,PhoneNmbr) of
                                        false -> error({badarg,"No such PhoneNmbr!"});
                                        H -> lists:keyreplace(H#person.name_surname,2,AddressBook,H#person{phoneNmbr = H#person.phoneNmbr -- [PhoneNmbr]})
                                      end.

removeBirthDate(AddressBook, Name, Surname) -> case containsName(AddressBook,{Name,Surname}) of
                                                 false -> error({badarg, "No such Name!"});
                                                 H -> lists:keyreplace({Name,Surname},2,AddressBook,H#person{birthDate = {0,0,0}})
                                               end.

%%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Wyszukiwanie~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

findByEmail(AddressBook,Email) -> case containsEmail(AddressBook,Email) of
                                    false -> error({badarg,"No such Email!"});
                                    H -> H#person.name_surname
                                  end.

findByPhone(AddressBook,PhoneNmbr) -> case containsPhoneNmbr(AddressBook,PhoneNmbr) of
                                        false -> error({badarg,"No such PhoneNmbr!"});
                                        H -> H#person.name_surname
                                      end.

findByDate(AddressBook,{D1,M1,Y1},{D2,M2,Y2}) -> if
                                                   Y1 > Y2 -> error({badarg, "Wrong input Dates! Expected: firts date < second date"});
                                                   Y1 == Y2 -> if
                                                                 M1>M2 -> error({badarg, "Wrong input Dates! Expected: firts date < second date"});
                                                                 M1 == M2 -> if
                                                                               D1 > D2 ->  error({badarg, "Wrong input Dates! Expected: firts date < second date"});
                                                                               true -> containsDate(AddressBook,{D1,M1,Y1},{D2,M2,Y2},[])
                                                                             end;
                                                                 true -> containsDate(AddressBook,{D1,M1,Y1},{D2,M2,Y2},[])
                                                               end;
                                                   true -> containsDate(AddressBook,{D1,M1,Y1},{D2,M2,Y2},[])
                                                 end.

%%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Pobieranie~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

getEmails(AddressBook,Name,Surname) -> case containsName(AddressBook,{Name,Surname}) of
                                         false -> error({badarg,"No such person"});
                                         H -> H#person.email
                                       end.

getPhones(AddressBook,Name,Surname) -> case containsName(AddressBook,{Name,Surname}) of
                                         false -> error({badarg,"No such person"});
                                         H -> H#person.phoneNmbr
                                       end.