# Discogs-Haskell
Haskell Client for Discogs REST API.

Work in Progress

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