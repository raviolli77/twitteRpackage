# Sample Code of Playing around with twitteR package. By Raul Eulogio
# twitteR authorization
# First step is create a dev for yourself going to dev.twitter.com. Fill out all information necessary.
# Secondly go to apps.twitter.com. Here you want to create an app. Call it what you like and you will have important information on there that you will need when running the package twitteR
# Once you created your app go to settings and on Calback URL use: http://127.0.0.1:1410/
# Callback url needs to be: http://127.0.0.1:1410/ (Recommended on GitHub)
# on apps.twitter.com which you need to activate to use twitteR

# Next open Rstudio and install these packages; httpuv, twitteR, and ROAuth
setwd("/home/rxe/myProjects/dataScience/twitterProject")
# Install necessary packages
# install.packages("httpuv")
# install.packages("twiiteR")
# install.packages("ROAuth")
# install.packages("devtools")
# install.packages("bit64")
# install.packages("wordcloud")
# install.packages("tm")
require(ROAuth)
require(twitteR)
require(devtools)
require(bit64)
require(wordcloud)
require(tm)
require(data.table)
# Set up variables with your consumer key (API key), consumer secret (API Secret) both of which are located under the "Keys and Access Tokens" tab in apps.twitter.com
consumer_key = "420bLA$iTTybG" # Consumer key provided by twitter 
consumer_secret = "19SmVreP805cAIle13" # Conumer secret provided by twitter
#  These links are accessible on apps.twitter.com in the "details" tab
reqURL <- "https://api.twitter.com/oauth/request_token" 
accessURL <- "http://api.twitter.com/oauth/access_token"

setup_twitter_oauth(consumer_key, consumer_secret)


# Messing around with twitteR functions as a whole. 
homeTimeline(n = 500, retryOnRateLimit = 600) # This gives my twitter timeline. So recent posts of people I follow on twitter (the most recent 500)
availableTrendLocations() # This will give you the name of the City, the Country it comes from, and the woeid!
world.trends <- availableTrendLocations()
head(world.trends) # See the first six which in my case are all locations in Cananda 
woeid = 2442047 # woeif is a numerical id describing a location (Yahoo! Where on Earth ID)
losAng <- getTrends(woeid) # Here we create a data frame with the location 2442047 (Los Angeles)
View(losAng) # See the actual data frame which includes the trending content, the url, the query, and the woeid.
woeid1 = 2972 # This is for Winnipeg, Canada
closestTrendLocations(lat = 34.435829, long = -119.827639) # We can find the closest trends to the specified longitude and latitude (I used Goleta!)


# Searching for specific items and content
suiSq <- searchTwitter("#suicidesquad + #joker", n=500) 
suiSq.df <- twListToDF(suiSq)
attributes(suiSq.df)
suiSq.df$screenName # You get the handles of the people whose tweet we kept in our data frame!
write.csv(suiSq.df, file = "suicideSquadtweets.csv") # Here we create a csv file with suicide squad tweets related to joker!

userTimeline("h3h3productions") # This grabs a specific handle and you get to view the tweets of of that specific handle (I used this Youtube channel I follow!)
h3h3.df <- twListToDF(userTimeline("h3h3productions", n=500)) # Here I'm creating a data frame with h3h3's 500 most recent twitter posts!
names(h3h3.df) # You can see all the variables twitteR collects and names that are assigned to said variables 
View(h3h3.df) # View the actual data frame
plot(h3h3.df$retweetCount) # You can get information on the number of retweets of each respective tweet!
Rtweets(n=25) # If you want to find tweets related to R this funciton does it for you!!!


searchTwitter("#prince + #purplerain") # Here I search when these two hashtags are posted on the same tweet! They would have to make sense to be together (Prince wrote purple rain and since he dies recently more tweets will come out!)

# Direct messaging and other cool stuff with twitteR!
dmSend('Pussy', user = "lo___9") # Here I called my roommate a pussy through a twiiter DM in R! 

# Word cloud 
purpRain <- searchTwitter("#prince + #purplerain", n = 1000, lang = "en") # RIP Prince
purpText <- sapply(purpRain, function(x) x$getText())
purpText
# purpClean <- clean.text(purpText)
s <- iconv(purpText, to="utf-8") # I kept getting error codes before adding this line, basically converts if you have mac this might be better "utf-8-mac"
purpCorpus <- Corpus(VectorSource(s))

tdm <- TermDocumentMatrix(purpCorpus, control = list(removePunctuation=TRUE, stopwords = c('joker', 'suicidesquad', stopwords('en')),
                                                       removeNumbers = TRUE, tolower = TRUE))
f <- as.matrix(tdm)
f
word_freqs <- sort(rowSums(f), decreasing = TRUE)
dn <- data.frame(word =  names(word_freqs), freq = word_freqs)

wordcloud(dn$word, dn$freq, random.order = FALSE, colors = brewer.pal(8, "Dark2"))
# Creating a word cloud
joker_tweets <- searchTwitter("joker suicidesquad", n=500, lan="en")

# save text
joker_text = sapply(joker_tweets, function(x) x$getText())
joker_text
# joker_clean = clean.text(joker_text)

t <- iconv(joker_text,to="utf-8") # The biggest issue was that the character encoding was not the right one so I had to convert to utf-8 and thus everything ran smoothly :)
t
# Create a corpus
removeURL <- function(x) gsub("http[[:alnum:]]*", "", x)
joker_corpus <- Corpus(VectorSource(t))
joker_corpus <- tm_map(joker_corpus, removeURL, lazy = TRUE)


# create document term matrix applying some transformations
deb <- TermDocumentMatrix(joker_corpus, control = list(removePunctuation=TRUE, stopwords = c('joker', 'suicidesquad', stopwords('english')),
                        removeNumbers = TRUE, tolower = TRUE))
p <- as.matrix(deb)
p
word_freqs <- sort(rowSums(p), decreasing = TRUE)
dm <- data.frame(word =  names(word_freqs), freq = word_freqs)

wordcloud(dm$word, dm$freq, random.order = FALSE, colors = brewer.pal(8, "Dark2"))

# Conclusions: Its really flexible but because of finals and other shit won't be able to tinker around with this until after (like many of you)
# So I will add more after the next two weeks!


purpRain <- searchTwitter("harambe", n = 500, lang = "en") # RIP Prince
purpText <- sapply(purpRain, function(x) x$getText())
purpText
# purpClean <- clean.text(purpText)
s <- iconv(purpText, to="utf-8") # I kept getting error codes before adding this line, basically converts if you have mac this might be better "utf-8-mac"
purpCorpus <- Corpus(VectorSource(s))

tdm <- TermDocumentMatrix(purpCorpus, control = list(removePunctuation=TRUE, stopwords = c('joker', 'suicidesquad', stopwords('en')),
                                                     removeNumbers = TRUE, tolower = TRUE))
f <- as.matrix(tdm)
f
word_freqs <- sort(rowSums(f), decreasing = TRUE)
dn <- data.frame(word =  names(word_freqs), freq = word_freqs)
View(dn)
wordcloud(dn$word, dn$freq, random.order = FALSE, colors = brewer.pal(8, "Dark2"))
