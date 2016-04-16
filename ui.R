##Data Science Capstone Project
library("shiny")
shinyUI(fluidPage( theme = "bootstrap.css",
        headerPanel("Match Game 2016"),
        sidebarPanel(
        textInput("text",label="Enter Your Phrase Here",value="These three remain: Faith, Hope and "),
        submitButton(text="Get Top 3 Matches NOW")),
        mainPanel(
                HTML('<p><b>MATCH GAME</b> was a popular game show in the USA in the 1980s where at the final round, 
                     contestants attempted to match the top three answers from a prior studio audience poll. They had
                     the help of three celebrities who were most often Richard Dawson, Brett Summers, and Charles Nelson
                     Reilly. See if your guess matches the top 3 answers on the board by entering a phrase and pressing 
                     <b><i> Get Top 3 Matches NOW </i></b></p>')
        ),
        h4('Top Three Answers are: '),
        fluidRow((verbatimTextOutput("inp"))),
        tags$head(tags$style("#inp{color: red;
                                 font-size: 20px;
                                 font-style: italic;
                                 }"
        )
        )
)
##Files to upload
##FN4a-z.rds, FN3a-z.rds, FN2a-z.rds
##bootstrap.css (in www folder)
##ngram_lst1dt.rds
##ui.R
##server.R
##DSCapstone.R
##DSCAPPres.Rpres
)
