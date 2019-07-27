# flat-tree

Map a binary tree to a list. Common Lisp implementation, adapted from
[mafintosh/flat-tree](https://github.com/mafintosh/flat-tree).

## API

- [Documentation](./api.html)

## Usage
```lisp
> (flat-tree:parent 0)
1
```

## Why?
You can represent a binary tree in a simple flat list using the following
structure:

```text
      3
  1       5
0   2   4   6  ...
```

Each number represents an **index** in a flat list. So a tree:

```text
      A
  B       C
D   E   F   G  ...
```

would be represented as a list: `[D B E A F C G]`

Furthermore, indexes `0`, `2`, `4`, `6` are on **depth** `0`. `1`, `5`, `9` on depth `1`. And so forth.

```text
depth = 2  ^        3
depth = 1  |    1       5
depth = 0  |  0   2   4   6  ...
```

In some cases it is also useful to calculate an **offset**. Indexes `0`, `1`, `3`, `7` have an offset `0`:

```text
                (7)
       (3)
  (1)       5
(0)   2   4   6      ...
```

`2`, `5`, `11`, `23` offset `1`:

```text
                 7
       3                  (11)
  1        (5)        9          13
0   (2)   4   6    10   12    14    15
```

This module exposes a series of functions to help you build and maintain
this data structure.

## License
[MIT](./LICENSE-MIT)
