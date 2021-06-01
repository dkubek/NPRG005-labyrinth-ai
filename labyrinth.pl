% +---------------------------------------------------------------------------+
% | NPRG008 - Non-procedural Programming                                      |
% | Labyrinth AI                                                              |
% |                                                                           |
% | Author: Dávid Kubek                                                       |
% +---------------------------------------------------------------------------+

% +---------------------------------------------------------------------------+
% | Game specific data
% +---------------------------------------------------------------------------+

% default_cards(?Cards:list) is det.
default_cards(Cards) :-
    Cards =
    [ bat, emerald, helmet, book, candelabrum, chest, crown, dragon, genie,
      ghost, gold, keys, lizard, map, moth, owl, princess, rat, ring, scarab,
      skull, spider, troll, sword
    ].

% List of default goald
default_goals(Goals) :-
    default_cards(Cards),
    append([red_home, yellow_home, blue_home, green_home, none], Cards, Goals).

% List of tiles as packaged in the game box
default_free_tiles(FreeTiles) :-
    FreeTiles = [ tile(t, _, genie,     _),
                  tile(t, _, ghost,     _),
                  tile(t, _, dragon,    _),
                  tile(t, _, troll,     _),
                  tile(t, _, princess,  _),
                  tile(t, _, bat,       _),

                  tile(l, _, rat,       _),
                  tile(l, _, scarab,    _),
                  tile(l, _, lizard,    _),
                  tile(l, _, spider,    _),
                  tile(l, _, owl,       _),
                  tile(l, _, moth,      _),

                  tile(l, _, none,      _),
                  tile(l, _, none,      _),
                  tile(l, _, none,      _),
                  tile(l, _, none,      _),
                  tile(l, _, none,      _),
                  tile(l, _, none,      _),
                  tile(l, _, none,      _),
                  tile(l, _, none,      _),
                  tile(l, _, none,      _),
                  tile(l, _, none,      _),
                  tile(l, _, none,      _),

                  tile(i, _, none,      _),
                  tile(i, _, none,      _),
                  tile(i, _, none,      _),
                  tile(i, _, none,      _),
                  tile(i, _, none,      _),
                  tile(i, _, none,      _),
                  tile(i, _, none,      _),
                  tile(i, _, none,      _),
                  tile(i, _, none,      _),
                  tile(i, _, none,      _),
                  tile(i, _, none,      _)
                ].


% has_open_directions(Type, Directions:list)
% true if one can get to the given directions with the specified file
has_open_directions(t, wne).
has_open_directions(t, nes).
has_open_directions(t, wes).
has_open_directions(t, wns).

has_open_directions(l, ne).
has_open_directions(l, es).
has_open_directions(l, ws).
has_open_directions(l, wn).

has_open_directions(i, ns).
has_open_directions(i, we).

opposite_direction(w, e).
opposite_direction(n, s).
opposite_direction(e, w).
opposite_direction(s, n).

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

    T11 = tile(l,   es,     red_home,      [r]),
    T13 = tile(t,   wes,    book,          []),
    T15 = tile(t,   wes,    gold,          []),
    T17 = tile(l,   ws,     yellow_home,   [y]),

    T31 = tile(t,   nes,    map,           []),
    T33 = tile(t,   nes,    crown,         []),
    T35 = tile(t,   nes,    keys,          []),
    T37 = tile(t,   wns,    skull,         []),

    T51 = tile(t,   nes,    ring,          []),
    T53 = tile(t,   wne,    chest,         []),
    T55 = tile(t,   wns,    emerald,       []),
    T57 = tile(t,   wns,    sword,         []),

    T71 = tile(l,   ne,     green_home,    [g]),
    T73 = tile(t,   wne,    candelabrum,   []),
    T75 = tile(t,   wne,    helmet,        []),
    T77 = tile(l,   wn,     blue_home,     [b]).

tile_goal(tile(_, _, Goal), Goal).

% Find all the goals on the board
collect_board_goals(Board, Goals) :-
    flatten(Board, Flattened),
    exclude(var, Flattened, NonVar),
    maplist(tile_goal, NonVar, Goals).

% Find all uninified board possitions. Used with board templates
collect_free_board_template_positions(BoardTemplate, FreePositions) :-
    flatten(BoardTemplate, Flattened),
    include(var, Flattened, FreePositions).

% +---------------------------------------------------------------------------+
% | Random generation
% +---------------------------------------------------------------------------+

% Generate a random board
random_board(RandomBoard, FreeTile) :-
    default_board_template(Board),
    collect_free_board_template_positions(Board, FreePositions),
    random_default_tiles(FreePositions, FreeTile),
    RandomBoard = Board,
    !.

% Generate a random ordering of default tiles and an additional free tile
random_default_tiles(FreePositions, FreeTile) :-
    default_free_tiles(FreeTiles),
    random_default_tiles(FreeTiles, FreePositions, FreeTile).

random_default_tiles([FreeTile], [], FreeTile).
random_default_tiles(FreeTiles, [T | Tiles], FreeTile) :-
    T = tile(Type, Directions, _, []),
    random_select(T, FreeTiles, Rest),
    findall(Ds, has_open_directions(Type, Ds), Dss),
    random_member(RandomDirections, Dss),
    canonical_directions(RandomDirections, Directions),
    random_default_tiles(Rest, Tiles, FreeTile).

% Generate a random deck
random_deck(0, Cards, [], Cards) :- !.
random_deck(NCards, Cards, [Card | Deck], Rest) :-
    random_select(Card, Cards, WithoutCard),
    NewNCards is NCards - 1,
    random_deck(NewNCards, WithoutCard, Deck, Rest).

% +---------------------------------------------------------------------------+
% | Validation
% +---------------------------------------------------------------------------+

% tile(Type, OpenDirections:list, Goal, Players:list)
is_valid_tile(tile(Type, OpenDirections, Goal, _)) :-
    member(Type, [t, l, i]),
    has_open_directions(Type, OpenDirections),
    default_goals(Goals),
    member(Goal, Goals).

% Cen we insert in this position?
is_insert_position(Position, Direction) :-
    member(Position, [2, 4, 6]),
    member(Direction, [n, e, s, w]).

% Convert directions to a cannonical form
canonical_directions(Directions, Canonical) :-
    atom_chars(Directions, Split),
    once(canonical_directions_(Split, [w, n, e, s], CanonicalList)),
    atom_chars(Canonical, CanonicalList).

canonical_directions_([], _, []).
canonical_directions_(Directions, [ O | Order ], [ O | Cs ]) :-
    select(O, Directions, Rest),
    canonical_directions_(Rest, Order, Cs).
canonical_directions_(Directions, [ _ | Order ], Cs) :-
    canonical_directions_(Directions, Order, Cs).


% state(Board, FreeTile, FreeTilePosition, PlayerOrder)
initialize_game(Board, FreeTile, PlayerOrder, State) :-
    is_valid_tile(FreeTile),
    forall(member(Player, PlayerOrder), member(Player, [r, g, b, y])),
    State = state(Board, FreeTile, none-none, PlayerOrder),
    !.

% +---------------------------------------------------------------------------+
% | Matrix and List Operations
% +---------------------------------------------------------------------------+

compose([H | T], H, T).
insert(X, Xs, [X | Xs]).
lift(X, [X]).

transpose([[]|_],[]) :- !.
transpose(Xss,[Hs|Zss]) :-
    maplist(compose,Xss, Hs,Ts),
    transpose(Ts,Zss).

% +---------------------------------------------------------------------------+
% | AI
% +---------------------------------------------------------------------------+

/*
next_state(OldState, NextState) :-
    OldState = state(Board, FreeTile, FreeTilePosition, PlayerOrder),
    PlayerOrder = [Player | Players],
    FreeTile = tile(Type, Directions, Goal, Players),

    findall(
            InserPosition,
            next_valid_position(FreeTilePosition, InserPosition),
            InserPositions
    ),
    find_best_new_state(OldState, InserPositions, none-none, NewState).

find_best_new_state(_, [], _-NewState, NewState).
find_best_new_state(OldState, [IP | IPs], AScore-AState, NewState) :-
    OldState = state(Board, FreeTile, FreeTilePosition, [Player | _]),

    insert_tile(OldState, IP, State),
    score_state(State, Player, Score),
    (
        Score > AScore
    ->
        find_best_new_state(OldState, IPs, Score-State, NewState)
    ;
        find_best_new_state(OldState, IPs, AScore-AState, NewState)
    )
    .

insert_tile(OldState, InserPosition-InsertDirection, NewState) :-
    OldState = state(Board, FreeTile, _, _),


next_valid_position(FTPosition-FTDirection, InserPosition-InsertDirection) :-
    is_insert_position(InserPosition, InsertDirection),
    InserPosition-InsertDirection \= FTPosition-FTDirection.
*/


% +---------------------------------------------------------------------------+
% | "Graphics"
% +---------------------------------------------------------------------------+

draw_tile(t, wns, [['┌','─','┐',' ',' ','┌','─','┐'],
                     ['└','─','┘',' ',' ','│',' ','│'],
                     [' ',' ',' ',' ',' ','│',' ','│'],
                     ['┌','─','┐',' ',' ','│',' ','│'],
                     ['└','─','┘',' ',' ','└','─','┘']]).
draw_tile(t, wne, [['┌','─','┐',' ',' ','┌','─','┐'],
                     ['└','─','┘',' ',' ','└','─','┘'],
                     [' ',' ',' ',' ',' ',' ',' ',' '],
                     ['┌','─','─','─','─','─','─','┐'],
                     ['└','─','─','─','─','─','─','┘']]).
draw_tile(t, wes, [['┌','─','─','─','─','─','─','┐'],
                     ['└','─','─','─','─','─','─','┘'],
                     [' ',' ',' ',' ',' ',' ',' ',' '],
                     ['┌','─','┐',' ',' ','┌','─','┐'],
                     ['└','─','┘',' ',' ','└','─','┘']]).
draw_tile(t, nes, [['┌','─','┐',' ',' ','┌','─','┐'],
                     ['│',' ','│',' ',' ','└','─','┘'],
                     ['│',' ','│',' ',' ',' ',' ',' '],
                     ['│',' ','│',' ',' ','┌','─','┐'],
                     ['└','─','┘',' ',' ','└','─','┘']]).

draw_tile(l, ws,    [['┌','─','─','─','─','─','─','┐'],
                     ['└','─','─','─','─','┐',' ','│'],
                     [' ',' ',' ',' ',' ','│',' ','│'],
                     ['┌','─','┐',' ',' ','│',' ','│'],
                     ['└','─','┘',' ',' ','└','─','┘']]).
draw_tile(l, wn,    [['┌','─','┐',' ',' ','┌','─','┐'],
                     ['└','─','┘',' ',' ','│',' ','│'],
                     [' ',' ',' ',' ',' ','│',' ','│'],
                     ['┌','─','─','─','─','┘',' ','│'],
                     ['└','─','─','─','─','─','─','┘']]).
draw_tile(l, es,    [['┌','─','─','─','─','─','─','┐'],
                     ['│',' ','┌','─','─','─','─','┘'],
                     ['│',' ','│',' ',' ',' ',' ',' '],
                     ['│',' ','│',' ',' ','┌','─','┐'],
                     ['└','─','┘',' ',' ','└','─','┘']]).
draw_tile(l, ne,    [['┌','─','┐',' ',' ','┌','─','┐'],
                     ['│',' ','│',' ',' ','└','─','┘'],
                     ['│',' ','│',' ',' ',' ',' ',' '],
                     ['│',' ','└','─','─','─','─','┐'],
                     ['└','─','─','─','─','─','─','┘']]).

draw_tile(i, we,    [['┌','─','─','─','─','─','─','┐'],
                     ['└','─','─','─','─','─','─','┘'],
                     [' ',' ',' ',' ',' ',' ',' ',' '],
                     ['┌','─','─','─','─','─','─','┐'],
                     ['└','─','─','─','─','─','─','┘']]).
draw_tile(i, ns,    [['┌','─','┐',' ',' ','┌','─','┐'],
                     ['│',' ','│',' ',' ','│',' ','│'],
                     ['│',' ','│',' ',' ','│',' ','│'],
                     ['│',' ','│',' ',' ','│',' ','│'],
                     ['└','─','┘',' ',' ','└','─','┘']]).

center(List, Length, Char, Out) :-
    length(List, L),
    ToPad is Length - L,
    (
        ToPad > 0
    ->
        Right is ToPad // 2,
        Left is ToPad - Right,
        length(LeftL, Left),
        length(RightL, Right),
        maplist(=(Char), LeftL),
        maplist(=(Char), RightL),
        append(List, RightL, Tmp),
        append(LeftL, Tmp, Out)
    ;
        Out = List
    ).

overlay(X, [], _, X).
overlay([H | T1], [C | T2], C, [H | O]) :-
    overlay(T1, T2, C, O),
    !.
overlay([_ | T1], [H | T2], C, [H | O]) :-
    overlay(T1, T2, C, O).

draw_tile(tile(Type, Directions, Goal, Players), S) :-
    draw_tile(Type, Directions, TileBase),
    TileBase = [R1, R2, R3, R4, R5],
    (
        Goal \= none
    ->
        Goals = [ '*' | Players ]
    ;
        Goals = Players
    ),
    center(Goals, 8, ' ', Centered),
    overlay(R3, Centered, ' ', Overlayed),
    S = [R1, R2, Overlayed, R4, R5],
    !.

empty_tile(Empty) :-
    length(Row, 8),
    maplist(=(' '), Row),
    length(Empty, 5),
    maplist(=(Row), Empty).

% pad_tile(number, direction, string)
pad_tile_base(Direction, Number, Tile) :-
    number_chars(Number, [Char]),
    Tile = [[' ',' ',' ', N ,' ',' ',' ',' '],
            [' ',' ',' ',' ',' ',' ',' ',' '],
            [ W ,' ',WA , MA,' ',EA ,' ', E ],
            [' ',' ',' ',' ',' ',' ',' ',' '],
            [' ',' ',' ', S ,' ',' ',' ',' ']],
    (
        Direction = w
    ->
        W = Char, N = ' ', E = ' ', S = ' ',
        (
            0 is Number mod 2
        ->
            WA = '<', MA = ' ',  EA = ' '
        ;
            WA = ' ', MA = ' ',  EA = ' '
        )
    ;
        Direction = n
    ->
        W = ' ', N = Char, E = ' ', S = ' ',
        (
            0 is Number mod 2
        ->
            WA = ' ', MA = '^',  EA = ' '
        ;
            WA = ' ', MA = ' ',  EA = ' '
        )
    ;
        Direction = e
    ->
        W = ' ', N = ' ', E = Char, S = ' ',
        (
            0 is Number mod 2
        ->
            WA = ' ', MA = ' ',  EA = '>'
        ;
            WA = ' ', MA = ' ',  EA = ' '
        )
    ;
        Direction = s
    ->
        W = ' ', N = ' ', E = ' ', S = Char,
        (
            0 is Number mod 2
        ->
            WA = ' ', MA = 'v',  EA = ' '
        ;
            WA = ' ', MA = ' ',  EA = ' '
        )
    ).

draw_board(Board) :-
    maplist(maplist(draw_tile), Board, Sprites),
    pad_board(Sprites, Padded),
    draw_board_rows(Padded).

draw_board_rows([]).
draw_board_rows([Row | Rows]) :-
    draw_lines(Row),
    draw_board_rows(Rows),
    !.

pad_board(Board, Padded) :-
    numlist(1, 7, Ys),
    empty_tile(Empty),

    maplist(pad_tile_base(s), Ys, MidF),
    append([ Empty | MidF ], [Empty], First),
    lift(First, FirstL),

    maplist(pad_tile_base(e), Ys, Front),
    maplist(insert, Front, Board, FrontInserted),

    maplist(pad_tile_base(w), Ys, Back),
    maplist(lift, Back, BackL),
    maplist(append, FrontInserted, BackL, BackAppended),

    maplist(pad_tile_base(n), Ys, MidL),
    append([ Empty | MidL ], [Empty], Last),
    lift(Last, LastL),

    append(FirstL, BackAppended, Tmp),
    append(Tmp, LastL, Padded).

draw_lines([[] | _]).
draw_lines(Sprites) :-
    maplist(compose, Sprites, Row, Rest),
    draw_line(Row),
    draw_lines(Rest).

draw_line(Row) :-
    maplist(text_to_string, Row, Strings),
    atomics_to_string(Strings, ' ', TextRow),
    writeln(TextRow).

center_goal(Length, Goal, Out) :-
    atom_chars(Goal, Chars),
    center(Chars, Length, ' ', Centered),
    atomics_to_string(Centered, '', Out).

draw_table(Board) :-
    string_table(Board, Table),
    draw_table_lines(Table).

draw_table_lines([]).
draw_table_lines([Row | Rows]) :-
    writeln(Row),
    draw_table_lines(Rows).

string_table(Board, Table) :-
    FieldWidth = 13,

    % Create a separator
    length(Lines, FieldWidth),
    maplist(=('-'), Lines),
    atomics_to_string(Lines, '', SepPart),
    length(Separators, 7),
    maplist(=(SepPart), Separators),

    atomics_to_string(Separators, '+', SeparatorRow),
    atomics_to_string(['+' , SeparatorRow, '+'], '', SeparatorBordered),

    string_table_(Board, FieldWidth, SeparatorBordered, Table).


string_table_([], _, Separator, [Separator]).
string_table_([Row | Rows], FieldWidth, Separator, [Separator, TextRow | TextRows]) :-
    % Create a row of centered goals
    maplist(tile_goal, Row, Goals),
    maplist(center_goal(FieldWidth), Goals, Padded),

    atomics_to_string(Padded, '|', Fields),
    atomics_to_string(['|', Fields, '|'], '', TextRow),

    string_table_(Rows, FieldWidth, Separator, TextRows).

cls :- write('\33\[H\33\[2J').

main :-
    repeat,
    write('> '),
    read(X),
    call(X),
    cls,
    fail.
