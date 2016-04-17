## Data Science Capstone

This is the final course in the Johns Hopkins/Coursera Data Science program. The purpose of the capstone project is to predict the next word in a phrase or sentence.

To access the word prediction Shiny app, go to: [https://joyflowers.shinyapps.io/MatchGame/](https://joyflowers.shinyapps.io/MatchGame/)

To access the presentation slides, go to: [http://joyflowers.github.io/Capstone/DSCAPPres.html#/](http://joyflowers.github.io/Capstone/DSCAPPres.html#/)

To provide a twist to word prediction, the app created is a spin off of a former television show called Match Game. 

<p><b>MATCH GAME</b> was a popular game show in the USA in the 1980s where at the final round, contestants attempted to match the top three answers from a prior studio audience poll. Contestants had the help of three celebrities who were most often Richard Dawson, Brett Summers, and Charles Nelson Reilly. The goal of the final round of the Match Game show was to see if your guess matched the top 3 answers on the board. The app is similar. By entering a phrase and pressing  <b><i> Get Top 3 Matches NOW </i></b> you can play the Match Game too.</p>'


## Input
A collection of words from blogs, news and twitter (called a corpus) was made available at [https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip](https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip). This app uses a random sample of 20% from each of the English-language sources.

## Method
After the corpus was read, it was cleaned of profanity and punctuation. Ngrams were created from the corpus. After trying many methods, quanteda was used. 4-grams, 3-grams, 2-grams, and uni-grams were created. The initial strategy was to use smoothing and interpolation combining the 4-,3-,2- and 1 grams, but that proved to yield a lenghty response time, so instead the simple linear interpolation is only used for bi- and unigrams. With the second strategy, if  match(es) are found with the 4 gram, they are reported right away; and same with the tri-gram. To speed up the response time, each of the ngram files was separated into 26 alphabetic files based on the starting character of the phrase. 

## Output
The app outputs the top three choices based on their frequency of use. These frequencies are adjusted for each ngram.

