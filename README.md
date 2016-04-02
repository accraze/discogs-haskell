# Discogs-Haskell [![travis build](https://img.shields.io/travis/accraze/discogs-haskell.svg)](https://travis-ci.org/accraze/discogs-haskell)

Haskell Client for Discogs REST API. Based on a Monad Transformer Stack.

This does not the wrap complete API yet. Only [database actions](https://www.discogs.com/developers/#page:database) (with the exception of searching) are implemented at this time. If you are interested in hacking on this, checkout the [CONTRIBUTING](https://github.com/accraze/discogs-haskell/blob/master/CONTRIBUTING.md) doc for more info.

## Build Locally

To install and build locally, clone the repo:

```
$ git clone http://github.com/accraze/discogs-haskell.git
```

This project is built using [Stack](http://docs.haskellstack.org/en/stable/README.html). To install all deps and build an executable:

```
$ stack setup
...
$ stack build

```

Then you can use the `ghci` REPL to use the client.

```
$ stack ghci
.....
ghci> runDiscogsAnon $ Discogs.Actions.getRelease $ ReleaseID "249504"
```

## License:

[MIT](https://github.com/accraze/discogs-haskell/blob/master/LICENSE) License 2016 © Andy Craze