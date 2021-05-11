# Ant STM Demo

In an effort to explore [Software Transactional Memory](https://en.wikipedia.org/wiki/Software_transactional_memory) more I came up with a small challenge involving many shared transactional variables.

## Challenge Statement

1. Randomly place N ants on a 2d grid of WxH dimensions
1. No more than one ant may occupy a cell at a time
1. Randomly assign each ant a unique target cell
1. Ants may move from their current cell to any of the 8 adjacent cells (or stay where they are)
1. Simulate the ants' movements and then generate a report showing their travel log

### Variation 1: Synchronous

For each simulated clock tick, each ant may move no more than once. Until the ants reach their respective destinations the simulation must repeatedly and atomically advance the clock.

### Variation 2: Asynchronous

As long as the challenge statement invariants are preserved each ant may move at its own pace without coordinating with a shared simulated clock.

Consider how to adapt the simulation reports to convince yourself that no two ants ever occupy the same cell at the same time.
