% For SICStus, uncomment line below: (needed for member/2)
use_module(library(lists)).
% Load model, initial state and formula from file.
verify(Input) :-
    see(Input), read(T), read(L), read(S), read(F), seen,
    check(T, L, S, [], F).

% check(T, L, S, U, F)
% T - The transitions in form of adjacency lists
% L - The labeling
% S - Current state
% U - Currently recorded states
% F - CTL Formula to check.
%
% Should evaluate to true iff the sequent below is valid. %
% (T,L), S |- F
%
%
:- discontiguous(check/5).

% Literals
check(_, L, S, [], F)      :-
  member([S,Neighbors], L),
  member(F,Neighbors).

check(T, L, S, [], neg(F)) :-
  \+ check(T,L,S,[],F).
%------------------------------------------------------------------------------
% And
check(T, L, S, [], and(F,G)) :-
  check(T,L,S,[],F),
  check(T,L,S,[],G).
%------------------------------------------------------------------------------
% Or
check(T,L,S,[],or(F,G)) :-
  check(T,L,S,[],F);
  check(T,L,S,[],G).
%------------------------------------------------------------------------------
% AX - Sant för alla S'
check(T,L,S,[],ax(F)) :-
  member([S, Neighbors], T),
  ax(T,L,Neighbors,[],F).

ax(T,L,[S0,S1|Sn],[],F) :-
  check(T,L,S0,[],F),
  ax(T,L,[S1|Sn],[],F).

ax(T,L,[S0|[]],[],F) :-
  check(T,L,S0,[],F).
%------------------------------------------------------------------------------
% EX - Sant för något S'
check(T, L, S, [], ex(F)) :-
  member([S,Neighbors],T),
  member(L0,Neighbors),
  check(T,L,L0,[],F).
%------------------------------------------------------------------------------
% AG
check(T,L,S,Visited,ag(F)) :-
  \+ member(S, Visited),
  member([S,Neighbors],T),
  check(T,L,S,[],F),
  ag(T,L,Neighbors,[S|Visited],F).

ag(T,L,[S0,S1|Sn],Visited,F) :-
  check(T,L,S0,Visited,ag(F)),
  ag(T,L,[S1|Sn],Visited,F).

ag(T,L,[S0|[]],Visited,F) :-
  check(T,L,S0,Visited,ag(F)).

check(_,_,S,Visited,ag(_)) :-
  member(S,Visited).
%------------------------------------------------------------------------------
%EF
check(T,L,S,U,ef(F)) :-
  \+ member(S,U),
  check(T,L,S,[],F).

check(T,L,S,U,ef(F)) :-
  \+ member(S,U),
  member([S,Neighbors],T),
  member(L0,Neighbors),
  check(T,L,L0,[S|U],ef(F)).
%------------------------------------------------------------------------------
% AF
check(T,L,S,Visited,af(F)) :-
  \+ member(S,Visited),
  check(T,L,S,[],F).

check(T,L,S,Visited,af(F)) :-
  \+ member(S,Visited),
  member([S,Neighbors],T),
  af(T,L,Neighbors,[S|Visited],F).

af(T,L,[S0,S1|Sn],Visited,F) :-
  check(T,L,S0,Visited,af(F)),
  af(T,L,[S1|Sn],Visited,F).

af(T,L,[S0|[]],Visited,F) :-
  check(T,L,S0,Visited,af(F)).
%------------------------------------------------------------------------------
% EG
check(_,_,S,U, eg(_)) :-
  member(S,U).

check(T,L,S,U,eg(F)) :-
  \+ member(S,U),
  check(T,L,S,[],F),
  member([S,Neighbors],T),
  member(L0,Neighbors),
  check(T,L,L0,[S|U],eg(F)).
%------------------------------------------------------------------------------
