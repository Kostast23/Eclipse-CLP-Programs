This problem is a slight revision of the first problem of the Prolog competition
[LP/CP Programming Contest 2015](http://picat-lang.org/lp_cp_pc/).

[Original problem description](http://picat-lang.org/lp_cp_pc/Games.html)

Alternative input format:
* Ps is the list of game obtained pleasures(its length is equal to the number of
  games).
* T is the number of tokens that our pocket can hold at most, and the pocket is
  full in the beginning.
* R is the number of tokens that we will be refilled after we finish one game Gi
  and before we start Gi+1.

Alternative output format:
* Ts is the list of times that each game must be played.
* P is the maximum pleasure that can be obtained.

We define the following predicates that solve the problem.
* Procedural solution: games(Ps, T, R, Ts, P).
* Declarative solution: alt_games(Ps, T, R, Ts, P).

Multiple maximum pleasure solutions can be found via backtracking if they exist.
