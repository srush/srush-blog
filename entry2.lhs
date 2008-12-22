Overheard in Palo Alto -

Man in suit talking on phone "All my friends are getting married to people they meet on craigslist. I don't get it. I never have any luck, all I ever get are sexual deviants."

I don't think I'll ever use Craigslist for
dating. But I'm always impressed that people are willing to put up such
entertaining and often embarrassing dating entries for the whole world
to see. I guess it is a lot cheaper than match.com and certainly a
whole lot more interesting.

What I'm really curious about is whether these personals are actually
working. I really want to know if "please u must have a car!! :) - 25 -
(inner sunset / UCSF)," has found her man/car, or if "Restore My Faith
in Men - 22 - (belmont)" has her faith back. Maybe "You Me Foreva
Maybe Baby? - 32 - (mendocino county)" is totally happy now and foreva.

Ok, so maybe I'll never know. Instead I thought I'd try to do the next best
thing. I'm going to try my hand at playing Craigslist cupid and match
up the boys and girls of the craigslist in eternal bless, or at least
get that first girl a car.

You might ask - Sasha, how are you at all qualified for this. You
don't know the first thing about relationships! Ah true, but I've got a trick, I've got the book of love. Well namely,
I have High Fidelity. And High Fidelity has the enlightning thesis
that really isn't it what you like, and not what you're like that is
important. And if we can't trust John Cusack and Jack Black for our
romantic advice. Really, where can we turn.

So let's get to work. Here's the plan. We'll take all the personals
from craigslist then write code to go through the entries and match up the
people who should be together. Then we'll write fake love emails to them
as each other and make them fall in love. It will be like the Matrix
meets Much Ado About Nothing, except with way less Keanu Reeves.

Warning: Here on down is incredibly geeky (read math+code), so if
these things don't turn you on skip to the bottom for results. (If they
do turn you on, you may want to check out "Math or Physics major? - 35 - (South Bay)" really, it sounds like a solid match,).So the rest of this entry is a fully runnable haskell program. Just save the entry as dating.lhs and compile you should be good to go.

first let's get some libs data. Ignore this stuff, just need it to make the entry compile.

> import qualified Network.Curl as C
> import Text.Feed.Import
> import Text.Feed.Types
> import Text.RSS1.Syntax
> import qualified Data.Map as M
> import Data.Char
> import Control.Monad (liftM, msum)
> import Data.Map ((!))
> import Data.List (intersect, group, nub, sortBy, sort)
> import Control.Arrow
> import Data.Maybe

We're going to read the Personals from the craigslist RSS feed. So we need to right a simple function to fetch the

> fetchEntries :: String -> IO [String]
> fetchEntries u = do
>   (_, result) <- C.curlGetString u []
>   print u
>   case parseFeedString result of
>       Just (RSS1Feed e)  ->
>           return [fromJust des |
>                    item <- feedItems e
>                  , let des = itemDesc item
>                  , isJust des]
>       Just _ -> return []
>       Nothing -> return []


Craiglist only lets me get 100 personals at a time from each server, so we'll cheat a little and get the entries from all the cities on the front page.

> places = ["sfbay", "newyork", "losangeles", "washingtondc", "philadelphia",
>           "miami", "lasvegas", "chicago", "atlanta", "austin", "portland",
>           "dallas", "denver", "houston", "minneapolis", "phoenix",
>           "raleigh", "sacrmento", "sandiego", "seattle", "boston",
>           "baltimore"]

Fun, fun code for pulling down and combining all the posts.

> fetchAllInCat cat =
>     liftM concat $ sequence $ map fetchEntries $
>     map (\p-> "http://"++p++".craigslist.org/"++cat++"/index.rss") places
>
> m4w = fetchAllInCat "m4w"
> w4m = fetchAllInCat "w4m"


The data has a bunch of random characters in it. Let's just eliminate that all and stick with the basics.

> sanitize = filter (\l -> isLetter l || isSpace l) . map toLower


Now onto the matching.

Casually, we want to find two people who like the same thing. That's a
bit fuzzy so let's
restate the problem more formally. We have a bunch of people who have
described themselves and more importantly their interests using
unstructured language. What'd we'd really like is for this data to be
given in a more structured form. You can kind of imagine what that would
look like.

Original: please u must have a car!!!
New: How important is it that he has a car?

Original: Kinky Guy Seeks Unapologetic Kinky Gal
New: How much experimentation are you comfortable with?

Original: I'm a Hottie
New: Are you a hottie?

If we had all these it would be easy to match people up. We just map
each person to a n-dimensional vector where each dimension represents
the answer to a particular question. People who should go out are just
the ones with the smallest euclidean distance. We can call this the
Newlywed Game metric for compatibility.

But we don't have this. And frankly, I'm glad. If craigslist posts
were just people checking boxes, they would be horrendously boring and
pretty much useless. Freeform has a lot of advantages. For one, it
lets you choose which questions you want to ask and answer. The car
girl doesn't talk anything about kinkiness, but the kinky guy does
talk about cars. More importantly, it also lets you choose which words
you want to use to express yourself and your personality.

My favorite example of this is the cliche "I like long walks on the beach" (which
gets 20+ hits on craigslist dating). People put it on personals
because it is something everyone likes, so you might as well have
it. But in practice, because everyone likes it, it is totally
meaningless since it doesn't make you stand out at all. Or to put it
another way, "Does your wive like Long walks on the beach?" would be a
really easy question on the Newlywed game. That is unless she <a
href="http://xkcd.com/120/"> really, really likes long walks on the beach</a>

So let's get down to business. 1) We want some way to map personals down
to vectors where each dimension represents some fact about the
person. 2) We also want to factor in the style and choices on the
writer. 3) We want to weight based on the information content of the
facts (skydiver > beachwalker, Breeders > Beach Boys).

The popular way to do this (from Foundations of Statistical NLP) is
known as tf.idf, (Term Factor, Inverse Document Factor). It's a pretty
simple idea. We have an RSS feed (''Collection'') of
personals (''Documents''). Each personal is made up of a bunch of
interesting words and phrases (''Terms''). The tf part handles the
mapping of personals to vectors. We'll use a pretty simple set of
questions at first - "Do you want someone who uses the term
blah?". The idf part is for weighting the terms based on how much
information they have. In code this breaks down to -

> type Term       = String
> type Document   = [Term]
> type Collection = [Document]

We'll throw away excessively short or long personals for cleanliness.

> toCollection :: [String] -> Collection
> toCollection  = filter ((< 500) . length) .
>                 filter ((> 100) . length) .
>                 map (words . sanitize)


The formula for combining these is pretty intuitive. It takes three arguments. Term Frequency, Document Frequency, and n the amount of documents in the collection.

The first part weights the occurence of the Term. It is a log scale so the first one counts a lot but after that more terms don't effect the score that much. The second part weights how rare this word is in the full collection

> tfidf :: Int -> Int -> Int -> Float
> tfidf 0  df n = 0
> tfidf tf df n = (1 + log(f tf)) * sqrt(f n / f df)
>     where f = fromIntegral


The rest is just record keeping. We need to count the words in all the documents and also within each personal. We keep track on everything using Haskell's Map (roughly equivalent to python dictionaries). Since we are just mapping terms to counts we can be super click,

Counts the number each item in a list. Love this function. group.sort takes a list like [1,2,3,4,2,1] and makes it [[1,1], [2,2], 3, 4]. The rest just counts it up.

> toFreqMap :: (Ord k) => [k] -> M.Map k Int
> toFreqMap = M.fromList . map (head &&& length) . group . sort

Counts the number of documents each term appears in. nub is like unix unique.

> documentFrequency :: Collection -> M.Map Term Int
> documentFrequency = toFreqMap . concat. map nub

Counts the number of times a term appears in a document

> termFrequency :: Document -> M.Map Term Int
> termFrequency = toFreqMap . filter ((> 4).length)


Now we compare the vectors. A little gross cause I'm tired. Basically for each document, we convert it from a frequency vector to a tfidf vector representation.

> collectionInfo coll = (documentFrequency coll, length coll)

> compareDocuments (dfMap,n) doc1 doc2 =
>     dotDist (makeVector doc1) (makeVector doc2)
>     where makeVector d =
>               M.mapWithKey (\t -> tfidf n (dfMap ! t)) $
>               termFrequency d

The last thing we need to do is define a way to compare the vector representations. The obvious way is to use euclidean distance between the vectors . I tried that at first, but it has the stupid problem of failing when one of the entries is really long. As a simple examples take the entries ["the"] and ["the", "the", "the"]. They're really similar but have a high euclidean distance becase one vecotr is just longer. One way to deal with this is to normalize all the vectors. The easier way is just to use cosine distance and compare the angle between the vectors. Apparently these give the same ordering, angle versus distance along unit sphere.

Here's the cosine distance. Got to use the sweet infix <*> for dot product

> type Vector =  M.Map Term Float
> cosineDist :: Vector -> Vector -> Float
> cosineDist p q = (p <*> q) / (sqrt (p <*> p) * sqrt (q <*> q))
>     where  v1 <*> v2  = sum $ M.elems $ M.intersectionWith (*) v1 v2


So I tried cosine dist for a bit, and altough it is the "right" metric for vector closeness. It didn't give me exactly what I was looking for. The couplings we often very conservative, matching people who put the same things and didn't do anything out of the ordinary. If you have just one really rare word it make the denominator huge. So even if you have a couple matches, those rare words dominate. Instead I wanted couples who go out on a limb and happen to match up. So I decided to just use the dot product. This probably is no longer a distance metric, but it does tell me exactly how much two people overlap, and discounts the quirks of one of the members.

> dotDist :: Vector -> Vector -> Float
> dotDist p q = (p <*> q)
>     where  v1 <*> v2  = sum $ M.elems $ M.intersectionWith (*) v1 v2



Finally, lets do the matching. For simplicity I just compared all men to all womnen and took the top 200 matches.

> makeCollectionInfo = do
>     m <- toCollection `liftM` m4w
>     w <- toCollection `liftM` w4m
>     return $ collectionInfo (m ++ w)


> main = do
>     m <- toCollection `liftM` m4w
>     w <- toCollection `liftM` w4m
>     mci <- makeCollectionInfo
>     let results = take 200 $ sortBy (\ b a-> compare (fst a) (fst b)) $
>                   [(comp, (man, woman)) | man <- m, woman <- w,
>                    let comp = compareDocuments mci man woman]
>     print $ map (\(c,(m,w)) -> (c, filter ((>4) . length) $ intersect (nub m) (nub w), unwords m, unwords w) ) results


Ok, no more code. I promise, you can come back now!

Here are some of the matches. I'm actually pretty impressed. I kind of want to get these guys together.


Couple Number 1 - Cute pseudo-hipster puppy love.

<i>so im looking for someone to go see nick and norahs infinite playlist with tonight or some other night yea the movie looks a bit cheesy and as cliched as a quirky movie can get but im a sucker for movies like that br br and since im posting this in mw and not strictly platonic it would be nice if you were looking for something potentially long term so maybe this could be a first date that leads to many more br br the only other thing i ask is that you be somewhere relatively close to bucks county so we dont have to drive an hour to meet</i>

<i>cute clever and creative girl wants to go see nick norahs infinite playlist tonight like a high school date if we get along maybe we can hold hands in the dark if we really get along ill give you a kiss at the end of the night and who knows maybe well see each other again i have a bag of red vines and a fifth of citrus rum we can sneak into the theater br br im looking for a cute and nerdy type indiehipster style smart and snarky send me your picture and maybe other moviesdirectors you love be adorable i am</i>

ohhhhh. a bag of red vines and a fifth of citrus rum. how adorable!


