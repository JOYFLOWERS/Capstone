##Data Science Capstone
##Joy Flowers
##April, 2016
##This is a word prediction algorithm. The corpus was gathered from news, 
##blogs, and twitter. The corpus was preprocessed (see DSCaptone.R) into small 
##subfiles which the shiny app reads in order to make predictions.
library("shiny")
library("ngram")
library("stringr")

shinyServer(function(input,output) {
      datasetInput <- reactive({  
                predict_word(input$current_sentence)
        })
        
        predict_word <- function(input_phrase) {
                options(scipen=999)
                ##input_phrase <- tolower("heres one for the gipper  ")
                top3_4 <- c(0)
                input_phrase <- tolower(gsub("(?!')[[:punct:]]", " ", input_phrase, perl=TRUE))
                input_phrase <- gsub(pattern='\\s+'," ",input_phrase)
                if (nchar(gsub("(^[[:space:]]+|[[:space:]]+$)", "",input_phrase))==0) 
                        return("Please enter a phrase now")
                in_cnt4 <- length(strsplit(input_phrase,' ')[[1]])
                ##adjust input phrase to last 3 words
                adj_input_phrase <- concatenate(tail(strsplit(input_phrase,split=" ")[[1]],3))
                adj_input_phrase <- concatenate(adj_input_phrase,"")
                ##if in_cnt < 2, skip to bigram or unigram
                if (in_cnt4 >= 3) {
                        indx4 <- strtrim(adj_input_phrase,1)
                        ng4_file <- readRDS(paste("FN4", indx4, ".rds",sep = ""))
                        matches4 <- grep(paste0('\\<^',adj_input_phrase),ng4_file$ngram_phrase4)
                        frq4 <- ng4_file$ngram_freq4[matches4]
                        if (length(matches4) > 1) {
                                frq4 <- as.numeric(frq4)
                                prb4 <- frq4/sum(frq4) #probability for matches     prb <- as.numeric(frq)/sum(as.numeric(frq)) 
                                # get 4th word (note phrase got love these when searching for love the... not good.
                                pred4 <- word(ng4_file$ngram_phrase4[matches4[1:length(prb4)]], 4)   
                                pred4; prb4
                                qd4 <- cbind(pred4,prb4)
                                lst4 <- as.data.frame(cbind(pred4,prob4=as.numeric(prb4)))
                                srt_lst4 <- lst4[order(lst4$prob4,decreasing=TRUE), ]
                                top3_4 <- head(srt_lst4,3)
                                top3_4
                                if (nrow(top3_4) > 2) {
                                        return(toupper(top3_4[1:3,1]))
                                }
                        } else {
                                prb4 <- c(0)
                                pred4 <- c(0)
                        }
                } else {
                        prb4 <- c(0)
                        pred4 <- c(0)
                }      
                
                ##trigrams
                options(scipen=999)
                top3 <- c(0)
                tri_input_phrase <- concatenate(tail(strsplit(adj_input_phrase,split=" ")[[1]],2))
                tri_input_phrase <- concatenate(tri_input_phrase,"")
                ##input_phrase <- tolower("take a ")
                in_cnt <- length(strsplit(tri_input_phrase,' ')[[1]])
                ##if in_cnt < 2, skip to bigram or unigram
                if (in_cnt >= 2) {
                        ##adjust input phrase to last 2 words
                        adj_input_phrase <- concatenate(tail(strsplit(input_phrase,split=" ")[[1]],2))
                        adj_input_phrase <- concatenate(adj_input_phrase,"")
                        indx <- strtrim(adj_input_phrase,1)
                        ng3_file <- readRDS(paste("FN3", indx, ".rds",sep = ""))
                        matches <- grep(paste0('\\<^',adj_input_phrase),ng3_file$ngram_phrase3)
                        frq <- ng3_file$ngram_freq3[matches]
                        if (length(matches) > 1) {
                                frq <- as.numeric(frq)
                                prb <- frq/sum(frq) #probability for matches     prb <- as.numeric(frq)/sum(as.numeric(frq)) 
                                pred <- word(ng3_file$ngram_phrase3[matches[1:length(prb)]], 3)   
                                pred; prb
                                lst <- as.data.frame(cbind(pred,prob=as.numeric(prb)))
                                comb <- c(0)
                                if ((top3_4[1] != 0)) { 
                                        comb <- rbind(top3_4, setNames(lst,names(top3_4)))
                                        comb$pred4 <- as.character(comb$pred4)
                                        comb$prob4 <- as.numeric(as.character(comb$prob4))
                                        cmb <- aggregate(prob4 ~ pred4, data = comb, FUN = sum)
                                        srt_lst <- cmb[order(cmb$prob,decreasing=TRUE), ]
                                } else srt_lst <- lst[order(lst$prob,decreasing=TRUE), ]
                                top3 <- head(srt_lst,3)              
                                top3
                                if ((top3_4[1] != 0)) { 
                                        if (nrow(comb) > 2) {
                                                return(toupper(top3[1:3,1]))
                                        }
                                }
                        }  else {
                                prb <- c(0)
                                pred <- c(0)
                        }
                } else {
                        prb <- c(0)
                        pred <- c(0)
                } ##end of calc trigrams
                
                ##bigrams
                options(scipen=999)
                bi_input_phrase <- concatenate(tail(strsplit(adj_input_phrase,split=" ")[[1]],1))
                bi_input_phrase <- concatenate(bi_input_phrase,"")
                indx2 <- strtrim(bi_input_phrase,1)
                ng2_file <- readRDS(paste("FN2", indx2, ".rds",sep = ""))
                matches2 <- grep(paste0('\\<^',bi_input_phrase),ng2_file$ngram_phrase2)
                pred_wd <- TRUE
                if (length(matches2) == 0) {
                        pred_wd <- FALSE 
                } else {
                        frq2 <- ng2_file$ngram_freq2[matches2]
                        frq2 <- as.numeric(frq2)
                        prb2 <- frq2/sum(frq2) #probability for matches
                        pred2 <- word(ng2_file$ngram_phrase2[matches2[1:length(prb2)]], 2)   
                        pred2; prb2
                        bi <- cbind(pred2,prb2)
                        lst2 <- as.data.frame(cbind(pred2,prob2=as.numeric(prb2)))
                        srt_lst2 <- lst2[order(lst2$prob2,decreasing=TRUE), ]
                        top3_2 <- head(srt_lst2,3)
                        top3_2
                }        
                if (!pred_wd) {
                       ##executed the code below to find most freq unigrams, but now just print them, no calcs
                       ##ngram_lst1 <- readRDS("ngram_lst1.rds")
                       ##ngram_lst1$ngram_freq1 <- as.numeric(as.character(ngram_lst1$ngram_freq1))
                       ##srt_lst_all <- ngram_lst1[order(ngram_lst1$ngram_freq1,decreasing=TRUE), ]
                       ##srt_lst_all$ngram_freq1 <- srt_lst_all$ngram_freq1/sum(srt_lst_all$ngram_freq1)
                       ##top <- head(srt_lst_all)
                        top <- as.data.frame(c("the","to","and"))
                } else {        
                        pred1 <- pred2
                        pred3 <- pred2
                        #######indx3 <- strtrim(pred1,1)
                        #######ng1_file <- readRDS(paste("FN1", indx, ".rds",sep = "")) 
                        ##prb3 <- 0
                        ngram_lst1dt <- readRDS("ngram_lst1dt.rds")
                        p1_index <- match(pred1, ngram_lst1dt$ngram_phrase1)
                        ngram_lst1dt$ngram_freq1 <- as.numeric(ngram_lst1dt$ngram_freq1)
                        prb1 <- ngram_lst1dt$ngram_freq1[p1_index]/sum(ngram_lst1dt$ngram_freq1[p1_index])
                        uni <- cbind(pred1,prb1)
                        ##prb3[1:length(pred3)] <- 0
                        ##prb4a[1:length(pred2)] <- 0
                        pred4a <- pred2
                        p4_index <- match(pred4a,pred4)
                        p3_index <- match(pred3,pred)
                        prb4a <- prb4[p4_index]
                        #####prb4a[p4_index] <- prb4[1:length(pred2)]
                        prb3 <- prb[p3_index]  
                        prb4a[is.na(prb4a)] <- 0
                        prb3[is.na(prb3)] <- 0
                        ##extra_wt <- colSums(all_results != 0)[1]/colSums(all_results != 0)[4]
                        if (sum(prb3 != 0) == 0) {
                                extra_wt <- 1 
                        } else  {
                                extra_wt <- length(pred3)/sum(prb3 != 0) 
                        }
                        ##w1 <- 1/3; w2 <- 1/3; w3 <- 1/3
                        ##w1 <- .20; w2 <- .35; w3 <- 0.45
                        w1 <- .10; w2 <- .20; w3 <- .30; w4 <- .40
                        ##This is the main probability table
                        ##result is w1*prb1 + w2*prb2 + w3*prb3
                        all_results <- as.data.frame(cbind(pred3,prb1,prb2,prb3,prb4a,wt_prb=extra_wt*w1*prb1+extra_wt*w2*prb2+w3*prb3+w4*prb4a),stringsAsFactors = FALSE)
                        srt_lst_all <- all_results[order(all_results$wt_prb,decreasing=TRUE), ]
                        top <- head(srt_lst_all)
                        if (nrow(srt_lst_all) == 1) {
                                top <- rbind(top,c("the",0,0,0,0,0))
                                top <- rbind(top,c("to",0,0,0,0,0))
                        } else if (nrow(srt_lst_all) == 2) {
                                top <- rbind(top,c("the",0,0,0,0,0))
                        }

                        prb_all <- cbind(w1*prb1,w2*prb2,w3*prb3,w4*prb4a)
                }
                return(toupper(top[1:3,1]))  
        }
        output$inp <- renderText({paste('*',predict_word(tolower(input$text)) )}) 
}
)

    