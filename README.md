# timeclock

A time clock implemented in Haskell.

## Usage

```
timeclock
```

C-c when done.

Will output `~/timesheet.tsv` with format `startDate timeStart-timeEnd hours`.

## Development

Build/install:

```
$ nix build
$ nix-env -i ./result
```

Get a development shell:

```
$ nix-shell
# then
$ cabal build
$ cabal repl
$ cabal run
# etc
```

