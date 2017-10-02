g1([person(kara, [barry, clark]), person(bruce, [clark, oliver]), person(barry,[kara,oliver]),person(clark,[oliver,kara]),person(oliver,[kara])]).

person(kara, [barry, clark]).
person(bruce, [clark, oliver]).
person(barry,[kara,oliver]).
person(clark,[oliver,kara]).
person(oliver,[kara]).

isPerson(X):-
    person(X,_).

isMember(M,[M|_]).
isMember(M,[_|T]):- isMember(M,T).

%----------- UTILS----------------%

% Succeeds if X is a person in the graph G
% inGraph(X,G)
inGraph(X,[person(X,[_|_])|_]).
inGraph(X,[person(_,_)|T]):-
    inGraph(X,T).

%----------------------------------$


% Succeeds if X likes Y in graph
% likes(G,X,Y)
likes([person(X,[Y|_])|_],X,Y).
likes([person(X,[_|T])|T1],X,Y):-
    likes([person(X,T)|T1],X,Y).
likes([_|T],X,Y):-
    likes(T,X,Y).

% Succeeds if X and Y different members of the graph
% different(G,X,Y)
different([person(X,[_|_])|T],X,Y):-
    inGraph(Y,T).
different([person(Y,[_|_])|T],X,Y):-
    inGraph(X,T).
different([person(_,_)|T],X,Y):-
    different(T,X,Y).


% X dislikes Y if
%            Y likes X
%            X does not like Y
% dislikes1 helper recursive function
% dislikes1(G,G,X,Y)
% dislikes(G,X,Y)

dislikes1(_,[person(X,[])|_],X,_).
dislikes1(G,[person(X,[H|T])|T1],X,Y):-
    different(G,H,Y),
    dislikes1(G,[person(X,T)|T1],X,Y).
dislikes1(G,[_|T1],X,Y):-
    dislikes1(G,T1,X,Y).

dislikes(G,X,Y):-
    likes(G,Y,X),
    dislikes1(G,G,X,Y).

% X is popular if everyone whom X likes, likes him/her back.
% Predicate succeeds if X is popular in G
% popular(G, X)

popular1(_,[person(X,[])|_],X).
popular1(G,[person(X,[H|T])|T2],X):-
    likes(G,H,X),
    popular1(G,[person(X,T)|T2],X).
popular1(G,[_|T1],X):-
    popular1(G,T1,X).
popular(G, X):-
    popular1(G,G,X).

% X is an outcast if everybody whom X likes, dislikes
% outcast(G, X) that succeeds whenever X is an outcast in G.

outcast1(_,[person(X,[])|_],X).
outcast1(G,[person(X,[H|T])|T2],X):-
    dislikes(G,H,X),
    outcast1(G,[person(X,T)|T2],X).
outcast1(G,[_|T1],X):-
    outcast1(G,T1,X).
outcast(G, X):-
    outcast1(G,G,X).
% X is said to be friendly if X likes back everyone who likes him/her.
% friendly(G, X) that succeeds whenever X is friendly in G.

friendly2(_,[],_).
friendly2(G,[person(P,[X|_])|T2],X):-
    likes(G,X,P),
    friendly2(G,T2,X).
friendly2(G,[person(P,[_|T])|T2],X):-
    friendly2(G,[person(P,T)|T2],X).
friendly2(G,[person(_,[])|T2],X):-
    friendly2(G,T2,X).



friendly1(G,[person(P,[X|_])|T2],X):-
    likes(G,X,P),
    friendly2(G,T2,X).
friendly1(G,[person(P,[_|T])|T2],X):-
    friendly1(G,[person(P,T)|T2],X).
friendly1(G,[person(_,[])|T2],X):-
    friendly1(G,T2,X).

friendly(G,X):-friendly1(G,G,X).

