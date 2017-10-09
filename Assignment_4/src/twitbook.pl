
%--------------------- U T I L S --------------------------

g1([person(kara, [barry, clark]), person(bruce, [clark, oliver]), person(barry,[kara,oliver]),person(clark,[oliver,kara]),person(oliver,[kara])]).

% returns list of persons from a graph
% succeeds if LP is the list of persons from the graph
% listOfPersons(G,LP)
listOfPersons([],[]).
listOfPersons([person(X,_)|T],[X|R]):-
    listOfPersons(T,R).


%succeeds if L1 is included in L2
%incusion(L1,L2).
inclusion([],_).
inclusion([H|T],L):-
    isMember(H,L),
    inclusion(T,L).


% Succeeds if M is found/is a member of the list L
% isMember(M,L)
isMember(M,[M|_]).
isMember(M,[_|T]):- isMember(M,T).


% succeeds if all elements of the list L are different
% from the element E
% differentInListOne(G,L,E).

differentInListOne(_,[],_).
differentInListOne(G,[H|T],E):-
    different(G,E,H),
    differentInListOne(G,T,E).

% appends second to first list passed as arguments and returns result as the last one
append1([],L,L).
append1([H|T],L,[H|T1]):-
    append1(T,L,T1).

% succeeds if contains everyone from X in L
containsEveryone([],_).
containsEveryone([HX|HT],L):-
    isMember(HX,L),
    containsEveryone(HT,L).

% succeeds if all elements of the list L are different
% elements in the graph G
% differentInList(G,L).

differentInList(_,[_|[]]).
differentInList(G,[H|T]):-
    differentInListOne(G,T,H),
    differentInList(G,T).

% suceeds of 2 elements are equal
equals(X,X).

% Succeeds if X is a person in the graph G
% inGraph(X,G)
inGraph(X,[person(X,_)|_]).
inGraph(X,[_|T]):-
    inGraph(X,T).

% Succeeds if X is someone's friend in the graph G
% inList(X,G)

inList(X,[person(_,[X|_])|_]).
inList(X,[person(_,[_|T])|T1]):-
    inList(X,[person(_,T)|T1]).
inList(X,[person(_,[])|T1]):-
    inList(X,T1).


%-----------------------------------------------------------$


% Succeeds if X likes Y in graph
% likes(G,X,Y)
likes([person(X,[Y|_])|_],X,Y).
likes([person(X,[_|T])|T1],X,Y):-
    likes([person(X,T)|T1],X,Y).
likes([_|T],X,Y):-
    likes(T,X,Y).

% Succeeds if X and Y different members of the graph G
% different(G,X,Y)
different1([person(X,_)|T],X,Y,Acc):-
    append1(T,Acc,R),
    inGraph(Y,R).
different1([person(Y,_)|T],X,Y,Acc):-
    append1(T,Acc,R),
    inGraph(X,R).
different1([person(P,_)|T],X,Y,Acc):-
    different1(T,X,Y,[P|Acc]).

different(G,X,Y):-
    different1(G,X,Y,[]).


% succeeds if Y is not in the list of friends

notLikes1(_,[person(X,[])|_],X,_).
notLikes1(G,[person(X,[H|T])|T1],X,Y):-
    different(G,H,Y),
    notLikes1(G,[person(X,T)|T1],X,Y).
notLikes1(G,[_|T1],X,Y):-
    notLikes1(G,T1,X,Y).

notLikes(G,X,Y):- notLikes1(G,G,X,Y).

% X dislikes Y if
%            Y likes X
%            X does not like Y
% dislikes1 helper recursive function
% dislikes1(G,G,X,Y)
% dislikes(G,X,Y)
% [person(alice,[bob]),person(bob,[carol]),person(carol,[])]
%
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

friendly1(_,[],_,0).
friendly1(G,[person(P,[X|_])|T2],X,F):-
    likes(G,X,P),
    friendly1(G,T2,X,F).
friendly1(G,[person(_,[X|_])|T],X,_):-
    friendly1(G,T,X,1).
friendly1(G,[person(_,[])|T2],X,F):-
    friendly1(G,T2,X,F).
friendly1(G,[person(P,[H|T])|T2],X,F):-
    different(G,H,X),
    friendly1(G,[person(P,T)|T2],X,F).
friendly(G,X):-
    inGraph(X,G),
    friendly1(G,G,X,0).

% X is hostile if X dislikes everyone who dislikes him/her
% hostile(G,X) succeeds if X is hostile in G


hostile1(_,[],_,0).
hostile1(G,[person(P,[X|_])|T2],X,F):-
    dislikes(G,X,P),
    hostile1(G,T2,X,F).
hostile1(G,[person(_,[X|_])|T],X,_):-
    hostile1(G,T,X,1).
hostile1(G,[person(_,[])|T2],X,F):-
    hostile1(G,T2,X,F).
hostile1(G,[person(P,[H|T])|T2],X,F):-
    different(G,H,X),
    hostile1(G,[person(P,T)|T2],X,F).
hostile(G,X):-
    inGraph(X,G),
    hostile1(G,G,X,0).


% X admires Y if
%                X likes Y or
%                X like someone who admires Y - recursion here
% X admires Y if there is a chain of likes from X to Y
% admires(G,X,Y) succeeds if X admires Y in G
%


bfsA(G,[person(CurrentH,_)|_],_,[CurrentH|_],_,Y,_):-
    likes(G,CurrentH,Y).

bfsA(G,[person(CurrentH,L)|_],Visited,[CurrentH|_],X,Y,History):-
    differentInListOne(G,Visited,CurrentH),
    notLikes(G,CurrentH,Y),
    append1(History, L, History1),
    bfsA(G,G,[CurrentH|Visited],History1,X,Y,History1).

bfsA(G,[person(CurrentH,_)|_],Visited,[CurrentH|Current],X,Y,History):-
    bfsA(G,G,Visited,Current,X,Y,History).

bfsA(G,[_|T],Visited,[CurrentH|CurrentT],X,Y,History):-
    bfsA(G,T,Visited,[CurrentH|CurrentT],X,Y,History).

%G,G,Visited,Current,X,Y,History
admires(G,X,Y):-
    different(G,X,Y),
    bfsA(G,G,[],[X],X,Y,[X]).




% X is indifferent to some other person Y if X does not admire Y,
% That is there is no link of likes from X to Y
% Succeeds if X is indifferent to Y
% indifferent(G, X, Y)


bfs(_,_,[VisitedH|VisitedT],[],_,_,History):-
    containsEveryone(History,[VisitedH|VisitedT]).

bfs(G,[person(CurrentH,L)|_],Visited,[CurrentH|_],X,Y,History):-
    differentInListOne(G,Visited,CurrentH),
    notLikes(G,CurrentH, Y),
    append1(History, L, History1),
    bfs(G,G,[CurrentH|Visited],History1,X,Y,History1).

bfs(G,[person(CurrentH,_)|_],Visited,[CurrentH|Current],X,Y,History):-
    notLikes(G,CurrentH, Y),
    bfs(G,G,Visited,Current,X,Y,History).

bfs(G,[_|T],Visited,[CurrentH|CurrentT],X,Y,History):-
    bfs(G,T,Visited,[CurrentH|CurrentT],X,Y,History).

%G,G,Visited,Current,X,Y,History
indifferent(G,X,Y):-
    notLikes(G,X,Y),
    different(G,X,Y),
    bfs(G,G,[],[X],X,Y,[X]).





%!
% same world succeeds if G,H describe the same graph
% A returns a list of for each person in G who H is
% same_world(G, H, A)
%[(kara,supergirl),(bruce,batman),(barry,flash),(clark,superman),(oliver,green_arrow)]
%
place(E,L,[E|L]).
place(E,[H|T],[H|R]):-
	place(E,T,R).
perm([],[]).
perm([H|T],R):-
	perm(T,L),
	place(H,L,R).

makeTupleList([],[],[]).
makeTupleList([H1|T1],[H2|T2],[(H1,H2)|R]):-
    makeTupleList(T1,T2,R).

makeTuplePerm(L1,L2,R):-
    perm(L1,R1),
    perm(L2,R2),
    makeTupleList(R1,R2,R).


getEquiv(E,[(E,Q)|_],Q).
getEquiv(E,[(_,_)|T],R):-
    getEquiv(E,T,R).

transformList([],_,[]).
transformList([L1|T],TL,[L2|R]):-
    getEquiv(L1,TL,L2),
    transformList(T,TL,R).


transformGraph([],_,[]).
transformGraph([person(GH,GL)|GT],TL,[person(MH,HL)|Res]):-
               getEquiv(GH,TL,MH),
               transformList(GL,TL,HL),
               transformGraph(GT,TL,Res).


getLikesOfPerson([person(P,L)|_],P,L).
getLikesOfPerson([person(_,_)|T],P,R):-
    getLikesOfPerson(T,P,R).


checkWorld([],_).
checkWorld([person(X,GL1)|GT1],H):-
    getLikesOfPerson(H,X,L),
    inclusion(GL1,L),
    inclusion(L,GL1),
    checkWorld(GT1,H).


sameWorld1(G,H,GL,HL,A):-
    makeTuplePerm(GL,HL,A),
    transformGraph(G,A,GH),
    checkWorld(GH,H).
same_world(G,H,A):-
    listOfPersons(G,GL),
    listOfPersons(H,HL),
    sameWorld1(G,H,GL,HL,A).



