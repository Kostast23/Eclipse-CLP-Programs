:- module(games_csp).

:- export games_csp/5.

:- lib(ic).
:- lib(branch_and_bound).

% games_csp(+Pleasures, +Tokens, +Refill, ?Times, ?Pleasure).
% The Times each game is played and the Pleasure are obtained by using bb_min
% to find a configuration with the maximum Pleasure (minimum negative Pleasure).
% After using bb_min use search to populate the configuration variables.
% Every configuration that is found has the maximum Pleasure.
games_csp(Pleasures, Tokens, Refill, Times, Pleasure) :-
    length(Pleasures, N),
    length(Times, N),
    Times #:: 1..Tokens,
    constrain(Pleasures, Tokens, Refill, Times),
    pleasures(Pleasures, Times, ActualPleasures),
    NegativePleasure #= -sum(ActualPleasures),
    Pleasure #= -NegativePleasure,
    bb_min(search(Times, 0, input_order, indomain, complete, []),
           NegativePleasure, [], _, NegativePleasure, bb_options{}),
    search(Times, 0, input_order, indomain_middle, complete, []).

% constrain(+Pleasures, +Tokens, +Refill, +Times).
% Given the pleasure from each game in Pleasures list, a number of Tokens and a
% number of Refill tokens constain the Times each game is played.
constrain(Pleasures, Tokens, Refill, Times) :-
    constrain(Pleasures, Tokens, Tokens, Refill, Times).
 
constrain([], _, _, _, []).
% Games with negative pleasure are played once as our overall goal is to
% maximize pleasure.
constrain([P|Pleasures], MaxTokens, Tokens, Refill, [T|Times]) :-
    P < 0,
    T #= 1,
    NewTokens #= min([Tokens + Refill - 1, MaxTokens]),
    constrain(Pleasures, MaxTokens, NewTokens, Refill, Times).
% Games with zero pleasure are played 1 or Tokens times.
constrain([P|Pleasures], MaxTokens, Tokens, Refill, [T|Times]) :-
    P =:= 0,
    T #=< Tokens,
    NewTokens #= min([Tokens + Refill - T, MaxTokens]),
    constrain(Pleasures, MaxTokens, NewTokens, Refill, Times).
% Games with positive pleasure are played 1 or more times at least as we make
% sure that no tokens are wasted by not playing them.
constrain([P|Pleasures], MaxTokens, Tokens, Refill, [T|Times]) :-
    P > 0,
    MustPlayAtLeastTokens #= Refill + Tokens - MaxTokens,
    T #>= MustPlayAtLeastTokens,
    T #=< Tokens,
    NewTokens #= Tokens + Refill - T,
    constrain(Pleasures, MaxTokens, NewTokens, Refill, Times).

% pleasures(+Pleasures, +Times, -ActualPleasures).
% ActualPleasures list holds the pleasure obtained from each game of P pleasure
% (specified in Pleasures list) if played T times (specified in Times variables
% list).
pleasures([], [], []).
pleasures([P|Pleasures], [T|Times], [T*P|Actual]) :-
    pleasures(Pleasures, Times, Actual).

