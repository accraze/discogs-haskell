# Discogs-Haskell
Haskell Client for Discogs REST API.

Work in Progress

## Examples

#### Artists
##### GET /artists/:artistId
```
ghci> runDiscogsAnon $ Discogs.Actions.getArtist $ ArtistID "108713"
```
200 returns:
```
Right (Artist {profile = Just "Alternative Rock / Modern Rock band from Hanna, Alberta formed in 1995. Nickelback's music is classed as hard rock and alternative metal. 
Nickelback is one of the most commercially successful Canadian groups, having sold almost 50 million albums worldwide, ranking as the 11th best selling music act of the 2000s, 
and is the 2nd best selling foreign act in the U.S. behind The Beatles for the 2000's.", 
releases_url = "https://api.discogs.com/artists/108713/releases", resource_url = Just "https://api.discogs.com/artists/108713", 
uri = Just "https://www.discogs.com/artist/108713-Nickelback", data_quality = "Needs Vote", 
namevariations = Just ["Nickleback","\12491\12483\12465\12523\12496\12483\12463"], 
urls = ["http://www.nickelback.com/","http://en.wikipedia.org/wiki/Nickelback"]})
```

##### GET /artists/:artistId/releases
```
ghci> runDiscogsAnon $ Discogs.Actions.getArtistReleases $ ArtistID "108713"
```
200 returns:
```
Right (ReleaseList {releases = [Object (fromList [("status",String "Accepted"),
("format",String "CD, EP"),("resource_url",String "https://api.discogs.com/releases/4299404"),
("thumb",String ""),("role",String "Main"),("year",Number 1996.0),("id",Number 4299404.0),
("title",String "Hesher"),("type",String "release"),("label",String "Not On Label (Nickelback Self-released)"),
("artist",String "Nickelback")]),Object (fromList [("main_release",Number 1078373.0),
("resource_url",String "https://api.discogs.com/masters/173765"),("thumb",String ""),
("role",String "Main"),("year",Number 1996.0),("id",Number 173765.0),("title",String "Curb"),
("type",String "master"),("artist",String "Nickelback")]).....
```


#### Releases
##### GET /releases/:releaseId
```
ghci> runDiscogsAnon $ Discogs.Actions.getRelease $ ReleaseID "249504"
```
200 returns:
```
Right (Release {title = "Never Gonna Give You Up", data_quality = "Correct", 
thumb = Just "", country = Just "UK", master_url = "https://api.discogs.com/masters/96559", 
uri = "https://www.discogs.com/Rick-Astley-Never-Gonna-Give-You-Up/release/249504", 
notes = "UK Release has a black label with the text \"Manufactured In England\" printed on it.\n\n
Sleeve:\n\8471 1987 \8226 BMG Records (UK) Ltd. \169 1987 \8226 BMG Records (UK) Ltd.\nDistributed 
in the UK by BMG Records \8226  Distribu\233 en Europe par BMG/Ariola \8226 Vertrieb en Europa
 d\252rch BMG/Ariola.\n\nCenter labels:\n\8471 1987 Pete Waterman Ltd.\nOriginal Sound Recording made by PWL.\
 nBMG Records (UK) Ltd. are the exclusive licensees for the world.\n\nDurations do not appear on the release.\n"})
```

#### Master Releases
##### GET /masters/:masterId
```
ghci> runDiscogsAnon $ Discogs.Actions.getMaster $ MasterID "1000"
```
200 returns:
```
Right (Master {title = "Stardiver", id = 1000, artists = [Object (fromList 
[("tracks",String ""),("join",String ""),("resource_url",
String "https://api.discogs.com/artists/21849"),("role",String ""),
("name",String "Electric Universe"),("id",Number 21849.0),("anv",String "")])], 
data_quality = "Correct", genres = ["Electronic"], images = Just [Object 
(fromList [("height",Number 569.0)......
...
```

##### GET /masters/:masterId/versions
```
ghci> runDiscogsAnon $ Discogs.Actions.getMasterVersions $ MasterID "1000"
```
200 returns:
```
Right (MasterVersionsList {pagination = Pagination {per_page = 50, items = 4, 
page = 1, pages = 1, urls = Urls {last = Nothing, next = Nothing}}, 
versions = [Object (fromList [("status",String "Accepted"),
("released",String "1997-03-14"),("country",String "Germany"),("format",String
 "CD, Album, Mixed"),("resource_url",String "https://api.discogs.com/releases/66785"),
 ("thumb",String ""),("catno",String "SPIRIT ZONE 027"),("id",Number 66785.0),
 ("title",String "Stardiver"),("label",String "Spirit Zone Recordings")]),
...
```
#### Labels
##### GET /labels/:labelId
```
ghci> runDiscogsAnon $ Discogs.Actions.getLabel $ LabelID "1"
```
200 returns:
```
Right (Label {id = 1, profile = "[a=Carl Craig]'s classic techno label founded in 1991.\r
\n\r\nOn at least 1 release, Planet E is listed as publisher.", releases_url =
 "https://api.discogs.com/labels/1/releases", name = "Planet E", 
 contact_info = "Planet E Communications\r\nP.O. Box 27218\r\nDetroit, Michigan, MI 48227
 \r\nUSA\r\n\r\nPhone: +1 313 874 8729\r\nFax: +1 313 874 8732\r\nEmail: info@Planet-e.net", 
...
```

##### GET /labels/:labelId/releases
```
ghci> runDiscogsAnon $ Discogs.Actions.getLabelReleases $ LabelID "1"
```
200 returns:
```
Right (LabelReleaseList {pagination = ReleasePagination {per_page = 50, page = 1, 
pages = 8, p_urls = Urls {last = "https://api.discogs.com/labels/1/releases?per_page=50&page=8", 
next = "https://api.discogs.com/labels/1/releases?per_page=50&page=2"}, items = 374}, 
releases = [Object (fromList [("status",String "Accepted"),("format",String "CD, Mixed"),
("resource_url",String "https://api.discogs.com/releases/2801"),
...
```


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
```

### Run Tests
Run test suite using this command
```
$ stack test
```

## License:

[BSD3](https://github.com/accraze/discogs-haskell/blob/master/LICENSE) License 2016 © Andy Craze