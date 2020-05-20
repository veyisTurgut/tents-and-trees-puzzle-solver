# tents-and-trees-puzzle-solver
a solver to the puzzle in the following link written in Scheme language: https://www.chiark.greenend.org.uk/~sgtatham/puzzles/js/tents.html

Place tents in the empty squares in such a way that:
1. No two tents are adjacent, even diagonally.
2. The number of tents in each row and column matches the numbers
around the edge of the grid.
3. It is possible to match tents to trees so that each tree is horizontally or
vertically adjacent to its own tent (but may also be adjacent to other
tents that are matched with other trees).

To work the program open the file in Scheme interpreter and run it with followin command:
(TENTS-SOLUTION '( (vertical edges) (horizantal edges) (tree locations)))
ex: (TENTS-SOLUTION '(( 1 0 1) (0 2) ((2 2) (3 1)) ) )
