# Silly commandline weather app

Just to play around with Haskell and the Brick commandline app lib.

## Development

### System requirements

* Nix

Everything happens inside a nix shell as defined in `shell.nix`.

After you've got a working nix installation run this from the project root:

```
$ nix-shell
```

This ensures that you have a working environment which incldues: direnv, ghc, cabal, along with all required packages.

### Environment

You'll need a Dark Sky API key. When you've got one just pop it in an `.envrc` file as follows:

```
export DARK_SKY_API_KEY=<YOUR KEY>
```

Then `direnv allow`.

### Building

```
$ cabal build
```

### Running

```
$ cabal run
```
