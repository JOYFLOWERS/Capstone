

MATCH GAME 2016
=======
author: Joy Flowers
date: April, 2016
font-import: http://fonts.googleapis.com/css?family=Helvitica
font-family: 'Helvitica'



<p><b>MATCH GAME</b> was a popular game show in the USA in the 1980s where at the final round, contestants attempted to match the top three answers from a prior studio audience poll. Contestants had
the help of three celebrities who were most often Richard Dawson, Brett Summers, and Charles Nelson
Reilly. See if your guess matches the top 3 answers on the board by entering a phrase and pressing  <b><i> Get Top 3 Matches NOW </i></b></p>'


How to Use
========================================================
<p style="font-size:32px"><span style="font-weight:normal; color:#000000">Playing games on a computer or mobile phone is a common past time. This app is a game that is easy and fun to play. It is a spin off of the popular US TV show, <b><span style="font-weight:normal; color:#004d80">Match Game </span></b>from the 1980s. How it works: Think of a phrase and the possible words that could finish that phrase. See if your guess matches one of the top 3 answers. If you match the top answer you earn 100 points, for matching the second answer you earn 50 point, and for matching the third answer, you earn 25 points. The first person to 250 points wins!</span></p>

Method
========================================================

<p style="font-size:32px"><span style="font-weight:bold; color:#004d80;">The prediction algorithm was created in the following way:</span> </p>

 - <p style="font-size:26px">Used about 20% of the news, blogs, and twitter corpus.</p>

 - <p style="font-size:26px">Removed profanity and punctuation. Used quanteda for ngrams</p>

 - <p style="font-size:26px">Used a simple linear interpolation with 4-,3-,2-, and 1- grams with self-assigned weights. </p>

 - <p style="font-size:26px">Separated ngram results alphabetically into files for all ngrams.</p>




Features and Drawbacks
===================
left: 50%
<p style="font-size:40px"><span style="font-weight:normal; color:#004d80;">Features</span></p>
><li><p style="font-size:15px">Easy to Use</p></li>
<li><p style="font-size:15px">A fun distraction</p></li>
<li><p style="font-size:15px">Predicts with accuracy</p></li>
<li><p style="font-size:15px">It's Free</p></li>

***
<p style="font-size:40px">Drawbacks</p>
<li><p style="font-size:15px">Available in English only</p></li>
<li><p style="font-size:15px">Corpus is limited</p></li>
<li><p style="font-size:15px">Not available yet on mobile phone</p></li>
<li><p style="font-size:15px">Limited Functionality</p></li>

Future Enhancements
===================



<p style="font-size:20px">The app is fun to use, but there is room for enhancement. For example, the game could be made in other languages. Another enhancement would be improving the word prediction if no matches are found. The current algorithm uses unigrams for this, but a better algorithm would use skip grams so that the grammar stays in tack. The GUI could have more bling to it. Lastly, implementing automatic scorekeeping would be useful.</p>

<p style="font-size:26px"><span style="font-weight:bold; color:#004d80">To play the game right now, go to <a  href="https://joyflowers.shinyapps.io/SalInfo">https://joyflowers.shinyapps.io/MatchGame/</a> to try it now.</span></p> 



