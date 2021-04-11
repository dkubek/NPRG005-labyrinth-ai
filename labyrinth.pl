% +---------------------------------------------------------------------------+
% | NPRG008 - Non-procedural Programming                                      |
% | Labyrinth AI                                                              |
% |                                                                           |
% | Author: Dávid Kubek                                                       |
% +---------------------------------------------------------------------------+

:- use_module(library(random)).

% default_cards(?Cards:list) is det.
default_cards(Cards) :-
    Cards =
    [
      bat, sword, dragon, genie, book, owl, moth, keys, scarab, gold, princess,
      emerald, troll, skull, spider, crown, candelabrum, map, chest, lizard,
      helmet, ring, rat, ghost
    ].

default_goals(Goals) :-
    default_cards(Cards),
    append([red_home, yellow_home, blue_home, green_home, none], Cards, Goals).

default_free_tiles(FreeTiles) :-
    FreeTiles = [ tile(t, _, genie, _),
                  tile(t, _, ghost, _),
                  tile(t, _, dragon, _),
                  tile(t, _, troll, _),
                  tile(t, _, princess, _),
                  tile(t, _, bat, _),

                  tile(l, _, rat, _),
                  tile(l, _, none, _),
                  tile(l, _, scarab, _),
                  tile(l, _, lizard, _),
                  tile(l, _, spider, _),
                  tile(l, _, owl, _),
                  tile(l, _, moth, _),
                  tile(l, _, none, _),
                  tile(l, _, none, _),
                  tile(l, _, none, _),
                  tile(l, _, none, _),
                  tile(l, _, none, _),
                  tile(l, _, none, _),
                  tile(l, _, none, _),
                  tile(l, _, none, _),
                  tile(l, _, none, _),
                  tile(i, _, none, _),

                  tile(i, _, none, _),
                  tile(i, _, none, _),
                  tile(i, _, none, _),
                  tile(i, _, none, _),
                  tile(i, _, none, _),
                  tile(i, _, none, _),
                  tile(i, _, none, _),
                  tile(i, _, none, _),
                  tile(i, _, none, _),
                  tile(i, _, none, _)
                ].


% has_open_directions(Type, Directions:list)
has_open_directions(t, [w, n, e]).
has_open_directions(t, [n, e, s]).
has_open_directions(t, [w, e, s]).
has_open_directions(t, [w, n, s]).

has_open_directions(l, [n, e]).
has_open_directions(l, [e, s]).
has_open_directions(l, [w, s]).
has_open_directions(l, [w, s]).

has_open_directions(i, [n, s]).
has_open_directions(i, [w, e]).

% board
default_board_template(Board) :-
    Board =
    [ [ T11, _, T13, _, T15, _, T17]
    , [ _  , _, _  , _, _  , _, _  ]
    , [ T31, _, T33, _, T35, _, T37]
    , [ _  , _, _  , _, _  , _, _  ]
    , [ T51, _, T53, _, T55, _, T57]
    , [ _  , _, _  , _, _  , _, _  ]
    , [ T71, _, T73, _, T75, _, T77]
    ],

    T11 = tile(l,   [e, s],       red_home,      []),
    T13 = tile(t,   [w, e, s],    book,          []),
    T15 = tile(t,   [w, e, s],    gold,          []),
    T17 = tile(l,   [w, s],       yellow_home,   []),

    T31 = tile(t,   [n, e, s],    map,           []),
    T33 = tile(t,   [n, e, s],    crown,         []),
    T35 = tile(t,   [w, e, s],    keys,          []),
    T37 = tile(t,   [w, n, s],    skull,         []),

    T51 = tile(t,   [n, e, s],    ring,          []),
    T53 = tile(t,   [w, n, e],    chest,         []),
    T55 = tile(t,   [w, n, s],    emerald,       []),
    T57 = tile(t,   [w, n, s],    sword,         []),

    T71 = tile(l,   [n, e],       green_home,    []),
    T73 = tile(t,   [w, n, e],    candelabrum,   []),
    T75 = tile(t,   [w, n, e],    helmet,        []),
    T77 = tile(l,   [n, w],       blue_home,     []).

tile_type(tile(Type, _, _, _), Type).
tile_directions(tile(_, Directions, _, _), Directions).
tile_goal(tile(_, _, Goal, _), Goal).
tile_players(tile(_, _, _, Players), Players).

list_subtract([], _, []).
list_subtract([H | T], List2, Ans) :-
    (
        member(H, List2) -> Ans = Ans_ ; Ans = [H | Ans_]
    ),
    list_subtract(T, List2, Ans_).


collect_board_goals(Board, Goals) :-
    flatten(Board, Flattened),
    exclude(var, Flattened, NonVar),
    maplist(tile_goal, NonVar, Goals).

collect_free_board_template_positions(BoardTemplate, FreePositions) :-
    flatten(BoardTemplate, Flattened),
    include(var, Flattened, FreePositions).

random_board(RandomBoard) :-
    default_board_template(Board),
    collect_free_board_template_positions(Board, FreePositions),
    default_random_tiles(FreePositions),
    RandomBoard = Board,
    !.

default_random_tiles(FreePositions) :-
    default_free_tiles(FreeTiles),
    default_random_tiles(FreeTiles, FreePositions).

default_random_tiles(_, []).
default_random_tiles(FreeTiles, [T | Tiles]) :-
    T = tile(Type, Directions, _, []),
    random_select(T, FreeTiles, Rest),
    findall(Ds, has_open_directions(Type, Ds), Dss),
    random_member(RandomDirections, Dss),
    canonicalDirections(RandomDirections, Directions),
    default_random_tiles(Rest, Tiles).


% tile(Type, OpenDirections:list, Goal, Players:list)
is_valid_tile(Type, OpenDirections, Goal, _) :-
    member(Type, [t, l, i]),
    has_open_directions(Type, OpenDirections),
    default_goals(Goals),
    member(Goal, Goals).

is_insert_position(Position, Direction) :-
    member(Position, [2, 4, 6]),
    member(Direction, [n, e, s, w]).


random_deck(0, Cards, [], Cards) :- !.
random_deck(NCards, Cards, [Card | Deck], Rest) :-
    random_select(Card, Cards, WithoutCard),
    NewNCards is NCards - 1,
    random_deck(NewNCards, WithoutCard, Deck, Rest).


canonicalDirections(Directions, Canonical) :-
    once(canonicalDirections_(Directions, [w, n, e, s], Canonical)).

canonicalDirections_([], _, []).
canonicalDirections_(Directions, [ O | Order ], [ O | Cs ]) :-
    select(O, Directions, Rest),
    canonicalDirections_(Rest, Order, Cs).
canonicalDirections_(Directions, [ _ | Order ], Cs) :-
    canonicalDirections_(Directions, Order, Cs).


%%% GX

draw_tile(t, [w, n, s], '┌─┐  ┌─┐\n└─┘  │ │\n     │ │\n┌─┐  │ │\n└─┘  └─┘\n').
draw_tile(t, [w, n, e], '┌─┐  ┌─┐\n└─┘  └─┘\n        \n┌──────┐\n└──────┘\n').
draw_tile(t, [w, e, s], '┌──────┐\n└──────┘\n        \n┌─┐  ┌─┐\n└─┘  └─┘\n').
draw_tile(t, [n, e, s], '┌─┐  ┌─┐\n│ │  └─┘\n │ │    \n│ │  ┌─┐\n└─┘  └─┘\n').

draw_tile(l, [w, s],    '┌──────┐\n└────┐ │\n│ │     \n┌─┐  │ │\n└─┘  └─┘\n').
draw_tile(l, [w, n],    '┌─┐  ┌─┐\n└─┘  │ │\n│ │     \n┌────┘ │\n└──────┘\n').
draw_tile(l, [e, s],    '┌──────┐\n│ ┌────┘\n│ │     \n│ │  ┌─┐\n└─┘  └─┘\n').
draw_tile(l, [n, e],    '┌─┐  ┌─┐\n│ │  └─┘\n│ │     \n│ └────┐\n└──────┘\n').

draw_tile(i, [w, e],    '┌──────┐\n└──────┘\n        \n┌──────┐\n└──────┘\n').
draw_tile(i, [n, s],    '┌─┐  ┌─┐\n│ │  │ │\n│ │  │ │\n│ │  │ │\n└─┘  └─┘\n').


cls :- write('\33\[H\33\[2J').

main :-
    repeat,
    write('> '),
    read(X),
    call(X),
    cls,
    fail.
