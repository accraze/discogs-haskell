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