# NPRG005-labyrinth-ai

https://www.ultraboardgames.com/labyrinth/game-rules.php

## Design

Board : 7x7 (fixed)
Positions : {1, ..., 7} rows, {A, ..., G} (columns)
Valid insert positions: 2,4,6,B,C,D,F
Direction: N - North, S - South, E - East, W - West
Move: (tile, insert_position, direction)
Tile = (tile type, card, players)
Tile types:
    T1, T2, T3, T4
    L1, L2, L3, L4
    I1, I2
Tile open paths: dict - tile -> [directions]

Treasyre Cards: 24
bat
sword
dragon
genie
book
owl
moth
keys
scarab
gold
princess
emerald
troll
skull
spider
crown
candelabrum
map
chest
lizard
helmet
ring
rat
ghost

Players: R - red, Y - yellow, G - green, B - blue

## TODO

- [ ] Model states
- [ ] Read board
- [ ] Randomize board
- [ ] Generate new state
- [ ] Support multiple players
- [ ] All cards are shown
