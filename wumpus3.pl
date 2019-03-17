% Position of the gold
gold(10, 10).

% Position of the Wumpus
wumpus(10, 10).

% Positions of the pits
pit(2, 1). pit(5, 1). pit(8, 1).
pit(5, 2). pit(8, 2).
pit(1, 3). pit(3, 3). pit(7, 3).
pit(1, 4). pit(5, 4). pit(7, 4). pit(10, 4).
pit(3, 5). pit(8, 5).
pit(2, 6). pit(4, 6). pit(5, 6). pit(8, 6).
pit(2, 7). pit(10, 7). pit(9, 7).
pit(1, 8). pit(4, 8). pit(6, 8). pit(7, 8). pit(8, 8). pit(9, 8). pit(10, 8).
pit(3, 9). pit(7, 9).
pit(1, 10). pit(2, 10). pit(3, 10). pit(4, 10). pit(5, 10).

% Checks if the gold is picked up
has_gold(G) :-
    G > 0.

% Checks if the Wumpus is killed
wumpus_killed(K) :-
    K > 0.

% Prints a reverse of the list
reverse_list([],Z) :-
    print_list(Z).
reverse_list([H|T],Acc) :-
    reverse_list(T,[H|Acc]).

% Prints a list
print_list([]).
print_list([H|T]) :-
    write(H), write(", "),
    print_list(T).

% Checks if H is in the list
member(H, [H | _]).
member(H, [_ | T]) :-
    member(H, T).

% Checks the direction of player sight
is_east(Dir) :-
    Dir = east.
is_west(Dir) :-
    Dir = west.
is_north(Dir) :-
    Dir = north.
is_south(Dir) :-
    Dir = south.

% When the player cannot go forward
bump(1, _, west).
bump(10, _, east). % 10 can be changed to any bound (within a reasonable)
bump(_, 1, south).
bump(_, 10, north).

% True if there is no wall in front
can_go_forward(X, Y, Dir) :-
    not(bump(X, Y, Dir)).

% Condition of the victory
is_victory(X, Y, K, G, J) :-
    X = 1, % Initial cell
    Y = 1,
    wumpus_killed(K),
    has_gold(G),
    J1 = [escape | J], % Add 'escape' to the end of the actions journal
    reverse_list(J1, []). % Print all the actions in case of victory

% Shoot an arrow in a given direction
shoot(X, Y, Dir) :-
    wumpus(X, Y);
    (not(bump(X, Y, Dir)), % Wall is reached
    (((is_east(Dir), X_east is X + 1) -> shoot(X_east, Y, Dir));
    ((is_west(Dir), X_west is X - 1) -> shoot(X_west, Y, Dir));
    ((is_north(Dir), Y_north is Y + 1) -> shoot(X, Y_north, Dir));
    ((is_south(Dir), Y_south is Y - 1) -> shoot(X, Y_south, Dir)))).

% Main query that explores the dungeon
% X and Y - current position
% Dir - current direction of sight
% G - is 0 when gold is not collected yet, 1 otherwise
% K - is 0 when Wumpus is not dead yet, 1 otherwise
% J - journal of actions performed. Initially empty
% H - history of visited cells. Initially empty
% Example of call:
% explore(1, 1, east, 0, 0, [], []).
explore(X, Y, Dir, G, K, J, H) :-
  not(pit(X, Y)), % Agent is not in the pit
  (
    not(wumpus(X, Y)); % Agent is not in the room with living Wumpus
    wumpus_killed(K)
  ),
  N is X + Y * 5,
  not(member(N, H)), % Agent has not been in this room before
  (
    is_victory(X, Y, K, G, J);
      (
        ( % Try to kill Wumpus
          ( % East
            K = 0,
            shoot(X, Y, east),
            (
              (
                is_east(Dir),
                J1 = [shoot | J]
              );
              (
                is_north(Dir),
                J1 = [shoot | [turn_left | J]]
              );
              (
                is_south(Dir),
                J1 = [shoot | [turn_right | J]]
              );
              (
                is_west(Dir),
                J1 = [shoot | [turn_right | [turn_right | J]]]
              )
            ),
            K1 is K + 1,
            explore(X, Y, east, G, K1, J1, [])
          );
          ( % North
            K = 0,
            shoot(X, Y, north),
            (
              (
                is_north(Dir),
                J1 = [shoot | J]
              );
              (
                is_east(Dir),
                J1 = [shoot | [turn_left | J]]
              );
              (
                is_west(Dir),
                J1 = [shoot | [turn_right | J]]
              );
              (
                is_south(Dir),
                J1 = [shoot | [turn_right | [turn_right | J]]]
              )
            ),
            K1 is K + 1,
            explore(X, Y, north, G, K1, J1, [])
          );
          ( % South
            K = 0,
            shoot(X, Y, south),
            (
              (
                is_south(Dir),
                J1 = [shoot | J]
              );
              (
                is_west(Dir),
                J1 = [shoot | [turn_left | J]]
              );
              (
                is_east(Dir),
                J1 = [shoot | [turn_right | J]]
              );
              (
                is_north(Dir),
                J1 = [shoot | [turn_right | [turn_right | J]]]
              )
            ),
            K1 is K + 1,
            explore(X, Y, south, G, K1, J1, [])
          );
          ( % West
            K = 0,
            shoot(X, Y, west),
            (
              (
                is_west(Dir),
                J1 = [shoot | J]
              );
              (
                is_north(Dir),
                J1 = [shoot | [turn_left | J]]
              );
              (
                is_south(Dir),
                J1 = [shoot | [turn_right | J]]
              );
              (
                is_east(Dir),
                J1 = [shoot | [turn_right | [turn_right | J]]]
              )
            ),
            K1 is K + 1,
            explore(X, Y, west, G, K1, J1, [])
          )
        );
        ( % Try to collect gold
            G = 0,
            gold(X, Y),
            G1 is G + 1,
            J1 = [collect_gold | J],
            explore(X, Y, Dir, G1, K, J1, [])
        );
        ( % Move east
          can_go_forward(X, Y, east),
          (
             (
                is_east(Dir),
                J1 = [move_east | J]
             );
             (
                is_south(Dir),
                J1 = [move_east | [turn_left | J]]
             );
             (
                is_north(Dir),
                J1 = [move_east | [turn_right | J]]
             );
             (
                is_west(Dir),
                J1 = [move_east | [turn_right | [turn_right | J]]]
             )
          ),
          X1 is X + 1,
          C is X + Y * 5,
          H1 = [C | H],
          explore(X1, Y, east, G, K, J1, H1)
        );
        ( % Move west
          can_go_forward(X, Y, west),
          (
             (
                is_west(Dir),
                J1 = [move_west | J]
             );
             (
                is_north(Dir),
                J1 = [move_west | [turn_left | J]]
             );
             (
                is_south(Dir),
                J1 = [move_west | [turn_right | J]]
             );
             (
                is_east(Dir),
                J1 = [move_west | [turn_right | [turn_right | J]]]
             )
          ),
          X1 is X - 1,
          C is X + Y * 5,
          H1 = [C | H],
          explore(X1, Y, west, G, K, J1, H1)
        );
        ( % Move north
          can_go_forward(X, Y, north),
          (
             (
                is_north(Dir),
                J1 = [move_north | J]
             );
             (
                is_east(Dir),
                J1 = [move_north | [turn_left | J]]
             );
             (
                is_west(Dir),
                J1 = [move_north | [turn_right | J]]
             );
             (
                is_south(Dir),
                J1 = [move_north | [turn_right | [turn_right | J]]]
             )
          ),
          Y1 is Y + 1,
          C is X + Y * 5,
          H1 = [C | H],
          explore(X, Y1, north, G, K, J1, H1)
        );
        ( % Move south
          can_go_forward(X, Y, south),
          (
             (
                is_south(Dir),
                J1 = [move_south | J]
             );
             (
                is_west(Dir),
                J1 = [move_south | [turn_left | J]]
             );
             (
                is_east(Dir),
                J1 = [move_south | [turn_right | J]]
             );
             (
                is_north(Dir),
                J1 = [move_south | [turn_right | [turn_right | J]]]
             )
          ),
          Y1 is Y - 1,
          C is X + Y * 5,
          H1 = [C | H],
          explore(X, Y1, south, G, K, J1, H1)
        )
      )
    ).
