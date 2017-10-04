g1([person(kara, [barry, clark]),
 person(bruce, [clark, oliver]),
 person(barry, [kara, oliver]),
 person(clark, [oliver, kara]),
 person(oliver, [kara])]).


isMember(M, [M| _]).
isMember(M, [_|T]) :-
    isMember(M, T).

getFriends([person(Name, Friends)|_], Name, Friends).
getFriends([_|Tail], Name, Friends):-
    getFriends(Tail, Name, Friends).


