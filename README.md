# ConwayLife

An implementation of Conway's game of life in Haskell. Uses a set of coordinates to simulate an
infinite grid rather than using a finite array of cell flags.

### Usage:

Projecct is built with `stack` the haskell tool stack, so can be built/run after cloning:

```sh
$ git clone https://github.com/Luminiscental/ConwayLife
$ cd ConwayLife
$ stack run -- <options>
```

Command-line options can be seen by passing the `--help` option.
For an example game you can try passing `--board=".**,*...******.*..*.*..*,*****"`.

