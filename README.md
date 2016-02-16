# Discogs-Haskell
Haskell Client for Discogs REST API.

Work in Progress

## Examples

#### Artists
##### GET /artists/:artistId
```
ghci> runDiscogsAnon $ Discogs.Actions.Artist.getArtist $ ArtistID "108713"
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
