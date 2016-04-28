:- module(games).

:- export games/5.
:- export alt_games/5.


% one_between(+X, +Y, ?Ζ).
% Z is a number between X and Y. X <= Z <= Y.
one_between(X, Y, X) :-
    X =< Y.
one_between(X, Y, Z) :-
    X < Y,
    X1 is X + 1,
    one_between(X1, Y, Z).

% games(Pleasures, Tokens, MaxTokens, Refill, Times, TotalPleasure).
% Given the pleasure from each game in Pleasures list, a number of Tokens, the
% number of MaxTokens and a number of Refill Tokens, Times is a list of the
% times each game is played and TotalPleasure is the obtained pleasure.
% All the different Times and pleasure can be obtained via backtracking.
games([], _, _, _, [], 0).
% Games with negative pleasure are played once as our overall goal is to
% maximize pleasure.
games([P|Ps], Tokens, MaxTokens, Refill, [1|Ts], TP) :-
    P < 0,
    RemTokens1 is Tokens - 1 + Refill,
    min(RemTokens1, MaxTokens, RemTokens), 
    games(Ps, RemTokens, MaxTokens, Refill, Ts, TP1),
    TP is TP1 + P.
% Games with zero pleasure are played 1 or Tokens times.
games([P|Ps], Tokens, MaxTokens, Refill, [T|Ts], TP) :-
    P =:= 0,
    one_between(1, Tokens, T),
    RemTokens1 is Tokens - T + Refill,
    min(RemTokens1, MaxTokens, RemTokens),
    games(Ps, RemTokens, MaxTokens, Refill, Ts, TP1),
    TP is TP1 + T * P.
% Games with positive pleasure are played 1 or more times at least and we make
% sure that no tokens are wasted by not playing them.
games([P|Ps], Tokens, MaxTokens, Refill, [T|Ts], TP) :-
    P > 0,
    Low is Refill + Tokens - MaxTokens,
    max(Low, 1, AtLeastPlayed),
    one_between(AtLeastPlayed, Tokens, T),
    RemTokens is Tokens - T + Refill,
    games(Ps, RemTokens, MaxTokens, Refill, Ts, TP1),
    TP is TP1 + T * P.

% alt_games(Pleasures, Tokens, Refill, Times, Pleasure).
% The Times each game is played and the Pleasure are obtained by finding a
% possible Times configuration and its pleasure and comparing it with every
% other configuration's pleasure to determine if it is the maximum.
alt_games(Ps, T, R, Ts, P) :-
    games(Ps, T, T, R, Ts, P),
    \+ (games(Ps, T, T, R, _, P1), P1 > P).


% maxlist(+L, ?Max).
% Max is the maximum number in list L.
maxlist([X|Xs], Max) :-
    maxlist(Xs, X, Max).

% maxlist(+L, +CurrentMax, ?Max).
% CurrentMax is the maximum number of the seen numbers of list L.
% When all the numbers of L have been seen CurrentMax equals Max.
maxlist([], Max, Max).
maxlist([X|Xs], CurrentMax, Max) :-
    X > CurrentMax,
    maxlist(Xs, X, Max).
maxlist([X|Xs], CurrentMax, Max) :-
    X =< CurrentMax,
    maxlist(Xs, CurrentMax, Max).

% find_max_pleasure(Pleasures, Tokens, Refill, MaxPleasure).
% MaxPleasure is obtained by finding all the possible pleasures and then
% selecting the maximum.
find_max_pleasure(Ps, T, R, Max) :-
    findall(P, games(Ps, T, T, R, _, P), Pleasures),
    maxlist(Pleasures, Max).

% games(Pleasures, Tokens, Refill, Times, Pleasure).
% The Times each game is played and the Pleasure are obtained by finding all the
% possible Times configurations and their pleasures and then selecting the ones
% that match the maximum pleasure.
games(Ps, T, R, Ts, P) :-
    find_max_pleasure(Ps, T, R, P),
    games(Ps, T, T, R, Ts, P).

