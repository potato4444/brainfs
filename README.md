# brainfs

brainfs currently includes interpreters for the following languages:

 - Alphuck
 - AutoSpoon
 - Binaryfuck
 - Bitfuck
 - BitZ
 - Boolfuck
 - Braincrash
 - brainfuck
 - Brainfunct
 - Brainhype
 - Headsecks
 - ReverseFuck
 - ReversibleBF
 - RISBF
 - Spoon


## Usage

`usage: brainfs LANG [-h] (-e expr | file) [OPTIONS]`

Most languages allow you to specify the type of the tape and what kind of
integers to use, please refer to the corresponding help output - for example:

```
~ $ brainfs bitz -h

 usage: brainfs bitz [-h] (-e expr | file) [OPTIONS]

  -e       --expr         supply source via command
  -f file  --file=file    read inputs from file
  -d       --debug        use ? to dump the state
  -v       --verbose      dump state at every tick
           --bf2b         transpile brainfuck toBitZ
           --b2bf         transpile BitZ to brainfuck
  -t TYPE  --type=TYPE    set integer type
  -l[N]    --left[=N]     tape in left direction
  -r[N]    --right[=N]    tape in right direction
  -c N     --circular=N   finite circular tape
  -i       --infinite     infinite tape in both directions
           --eof=(N|nop)  what to do on eof (set N or nop)
  -b       --bytes        read source as bytestring
  -h       --help         print this help

```

Using the `-l` flag tells it to use an infinite tape to the left, though you
could restrict its size by supplying a positive number `N`. Or you could tell
it to use a different type with the `-t` flag - the `TYPE` can be one of the
following:

| `TYPE` | description       |
|:------:|-------------------|
|  `b`   |   1bit            |
|  `u8`  |   8bit unsigned   |
|  `s8`  |   8bit signed     |
|  `u16` |  16bit unsigned   |
|  `s16` |  16bit signed     |
|  `u32` |  32bit unsigned   |
|  `s32` |  32bit signed     |
|  `u64` |  64bit unsigned   |
|  `s64` |  64bit signed     |
|  `u`   |  unbounded integer|


## Installation


Just use `cabal` to build and install:

```
~/brainfs $ cabal install
```

or `stack:

```
~/brainfs $ stack setup
~/brainfs $ stack install
```
