rm(list=ls())

require("tm")||install.packages("tm")
require("wordcloud")||install.packages("wordcloud")
require("textir")||install.packages("textir")

require("rJava")||install.packages("rJava")
require("RWeka")||install.packages("RWeka")
require("qdap")||install.packages("qdap")
require("maptpx")||install.packages("maptpx")

library("rJava")
library("RWeka")
library("qdap")
library("maptpx")

library("tm")
library("wordcloud")
library("textir")

#############################################################
#                userdefined functions                      #
#############################################################

text.clean = function(x)                          # text data
{
  x  =  gsub("<.*?>", "", x)                  # gsub == global substitution; regex for removing HTML tags
  x  =  gsub("[^[:alnum:]///' ]", " ", x)     # keep only alpha numeric 
  x  =  iconv(x, "latin1", "ASCII", sub="")   # Convert latin to ASCII characters (American Standard Code for Information Interchange)
  x  =  tolower(x)                          # convert to lower case characters
  x  =  removePunctuation(x)                # removing punctuation marks
  x  =  removeNumbers(x)                    # removing numbers
  x  =  stripWhitespace(x)                  # removing white space
  x  =  gsub("^\\s+|\\s+$", "", x)          # remove leading and trailing white space
  return(x)
}

source(file.choose())  #    Select text_functions.R

############################################################
#           Read the Text Data in R                        # 
############################################################

text  = readLines (file.choose())     # Select txt file you want to analyse
head(text)

Doc.id=seq(1:length(text))            # Assign Document no for each Document 
calib=data.frame(Doc.id,text)         # Create a dataframe for text documents with document ID

dim(calib)

stpw = readLines(file.choose())      # Select stopwords.txt file
stpw1 = stopwords('english')         # tm package stop word list
comn  = unique(c(stpw, stpw1))       # Union of two list
stopwords = unique(c(gsub("'","",comn),comn)) # final stop word lsit after removing punctuation
head (stopwords)
length(stopwords)

#############################################################
#                        Text Cleaning                      #
#############################################################

test = text.clean(text)                         # basic HTML Cleaning etc
test  =  removeWords(test,stopwords)            # removing stopwords created above
head(test)                                      # print top documents

#test[test != ""]                                # remove any blank document
clean_text = test

########################################################
#             Create Document Term Matrix              #
########################################################

x1 = Corpus(VectorSource(test))          # Create the corpus

dtm = DocumentTermMatrix(x1)               # Document Term Frequency 
dim(dtm)

#dtm1 = custom.dtm(x1,"tf")               # Document Term Frequency 
dtm2 = custom.dtm(x1,"tfidf")            # Term Frequency Inverse Document Frequency Scheme

######################################################
#         Basic Analysis                             #

#   1- Using Term frequency(tf)             
a1 = apply(dtm,  # matrix object on which we will operate
           2,   # 2 means column, 1 means row
           sum)  # sum 

freq1 = (sort(a1, decreasing =T)) # Calcualte term frequency
freq1[1:50]                                     # View top 50 terms 

windows()  # New plot window
wordcloud(names(freq1), freq1, scale=c(4,0.5),1, max.words=200,colors=brewer.pal(8, "Dark2")) # Plot results in a word cloud 
title(sub = "Term Frequency (Kindle) - Wordcloud")


#   2- UsingTerm Frequency Inverse Document Frequency (tfidf)             
freq2 = (sort(apply(dtm2,2,sum), decreasing =T)) # Calcualte term frequency
freq2[1:50]                                     # View top 50 terms 

windows()  # New plot window
wordcloud(names(freq2), freq2, scale=c(4,0.5),1, max.words=200,colors=brewer.pal(8, "Dark2")) # Plot results in a word cloud 
title(sub = "Term Frequency Inverse Document Frequency (Kindle) - Wordcloud")


#url = "http://www.amazon.in/product-reviews/B00K81HIDS/ref=cm_cr_pr_top_link_2?ie=UTF8&pageNumber=2&showViewpoints=0&sortBy=byRankDescending"print(dtm2[1,3])

###########################################################
#         Sentiment Analysis                              #
###########################################################

dtm = as.matrix(dtm)

pos.words.wt=read.csv(file.choose(),header=T,sep="\t")  # read-in positive_words.txt  # Source http://www.cs.uic.edu/~liub/FBS/sentiment-analysis.html #(LEN(A2)-MIN(LEN(A2),3.9))*0.1
head(pos.words.wt)

neg.words.wt=read.csv(file.choose(),header=T,sep="\t")   # read-in negative_words.txt  # Source http://www.cs.uic.edu/~liub/FBS/sentiment-analysis.html
head(neg.words.wt)

pos.words=unique(c(as.character(pos.words.wt$word),"wow", "kudos", "hurray")) 			# including our own positive words to the existing list

neg.words = unique(c(as.character(neg.words.wt$word)))

pos.matches = match(colnames(dtm), pos.words) 		# match() returns the position of the matched term or NA

pos.matches = !is.na(pos.matches)                 # Convert to logical variable
b1 = colSums(dtm)[pos.matches]
b1 = as.data.frame(b1)

colnames(b1) = c("freq")

windows()
wordcloud(rownames(b1), b1[,1], scale=c(5, 1), colors=1:10)  	# wordcloud of positive words
title(sub = "Possitive Words (Kindle) - Wordcloud")


neg.matches = match(colnames(dtm), neg.words)

neg.matches = !is.na(neg.matches)

b2 = colSums(dtm)[neg.matches]
b2 = as.data.frame(b2)
colnames(b2) = c("freq")
windows()
wordcloud(rownames(b2), b2[,1], scale=c(5, 1), colors=1:10)  	 # wordcloud of negative words
title(sub = "Negetive Words (Kindle) - Wordcloud")

######### Sentiment polarities  ######

doc_polarity = function(dtm,
                        pos.words.wt,
                        neg.words.wt)
  
{     pos.words=c(as.character(pos.words.wt$word))   		# 
      neg.words = c(as.character(neg.words.wt$word))
      
      pos.matches = match(colnames(dtm), pos.words)   	# match() returns the position of the matched term or NA
      pos.matches = !is.na(pos.matches)                 # Convert to logical variable
      pos.dtm = dtm[,pos.matches]
      pm = pos.words.wt[match(colnames(pos.dtm),pos.words),]
      pos.dtm.wt = t(t(pos.dtm)*pm[,2])
      #           pm[1:5,]
      #           pos.dtm[c(687, 410, 371, 279, 221, 118, 25),1:5]
      #           pos.dtm.wt[c(687, 410, 371, 279, 221, 118, 25),1:5]
      neg.matches = match(colnames(dtm), neg.words)
      neg.matches = !is.na(neg.matches)
      neg.dtm = dtm[,neg.matches]
      nm = neg.words.wt[match(colnames(neg.dtm),neg.words),]
      neg.dtm.wt = t(t(neg.dtm)*nm[,2])         
      
      neutral.dtm = dtm[,setdiff(colnames(dtm),c(unique(colnames(neg.dtm.wt),colnames(pos.dtm.wt))))]
      polarity = NULL
      for(i in 1:nrow(dtm)) {
        p = sum(pos.dtm.wt[i,])
        ng = sum(neg.dtm.wt[i,])
        n = sum(dtm[i,])+1
        pol = (p+ng)/n
        polarity = c(polarity,pol)
      }
      
      polarity = as.data.frame(polarity)
      rownames(polarity) = rownames(dtm)
      return(polarity)
      
}

polarity = doc_polarity(dtm, pos.words.wt, neg.words.wt)

head(polarity)
head(clean_text)
head(text)

maxVal = max(polarity)
minVal = min(polarity)
maxPostion = 0;
minPosition = 0;

#x <- 1:12 ; dim(polarity) <- c(3,4)
x <- data.matrix(polarity, rownames.force = NA)
#x[153]
#length(x)

for (i in 1:length(x) ) {
  if(x[i] == maxVal){
    maxPostion = i;
  }
  
 if(x[i] == minVal){
   minPosition = i;
 }
}
maxPostion
minPosition

#max(polarity)
#x[maxPostion]

#min(polarity)
#x[minPosition]
#text[minPosition] 
#text[maxPostion]
#x[287]
#c <-  data.matrix(x[order(x)], rownames.force = NA)
#c[0]



#combined = cat ('Most Negative : ',text[minPosition],'\n','Most Positive: ',text[maxPostion]) 
#cat ('Most Negative : ',text[minPosition],'\n','Most Positive: ',text[maxPostion], file = file.choose())
#write.table(cat ('Most Negative : ',text[minPosition],'\n','Most Positive: ',text[maxPostion]), file.choose(), row.names=F, col.names=F)


c <- sort(x)
#c[554][1]

minPosition1 = 0;
minPosition2 = 0;
minPosition3 = 0;

maxPosition1 = 0;
maxPosition2 = 0;
maxPosition3 = 0;


#c[length(c) - 2][1]
#x[1]
for (i in 1:length(x) ) {
  if(x[i] == c[1][1] && minPosition1 == 0){
    minPosition1 = i;
  }else if(x[i] == c[2][1] && minPosition2 == 0){
    minPosition2 = i;
  }else if(x[i] == c[3][1]){
    minPosition3 = i;
  }
  
  if(x[i] == c[length(c)][1] && maxPosition1 == 0){
    maxPosition1 = i;
  }else if(x[i] == c[length(c) - 1][1] && maxPosition2 == 0){
    maxPosition2 = i;
  }else if(x[i] == c[length(c) - 2][1]){
    maxPosition3 = i;
  }
}
#x[maxPosition2]
#x[minPosition2]
#text[minPosition] 
#text[minPosition1] 

#x[minPosition] 
#x[minPosition1]

#text[maxPostion]
#text[maxPosition1]

cat ('Top 3 Negative Comments: ','\n',text[minPosition1],'\n',text[minPosition2],'\n',text[minPosition3],'\n','Top 3 Positive Comments: ','\n',text[maxPosition1],'\n',text[maxPosition2],'\n',text[maxPosition3], file = file.choose())
