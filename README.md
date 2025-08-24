# conways-game-of-life

An implementation of [Conway's Game of Life](https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life) in Haskell.

[conways-game-of-life.webm](https://github.com/user-attachments/assets/e4a9bf58-7feb-4e0a-93ad-2ab3bd372c38)

## Setup

Before you begin, ensure that you have GHC and Cabal installed.

1. Clone the repository and install dependencies.

```bash
git clone https://github.com/abmu/conways-game-of-life.git
cd conways-game-of-life/
cabal update
cabal install --lib gloss
```

2. Compile the source file.

```bash
ghc -threaded Life.hs
```

3. Run the simulation game.

```bash
./Life
```

## Controls

- Spacebar -- Pause/Resume simulation.
- Left Mouse Button (while paused) -- Toggle a cell between Alive and Dead.
