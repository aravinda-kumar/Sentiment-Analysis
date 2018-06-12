###################################################################
#               User defined Functions                            #
###################################################################

# 1. Text cleaning

text.clean = function(x)                          # text data
{
  x  =  gsub("<.*?>", "", x)                  # regex for removing HTML tags
  x  =  gsub("[^[:alnum:]///' ]", " ", x)     # keep only alpha numeric 
  x  =  iconv(x, "latin1", "ASCII", sub="")   # Keep only ASCII characters
  x  =  tolower(x)                          # convert to lower case characters
  x  =  removePunctuation(x)                # removing punctuation marks
  x  =  removeNumbers(x)                    # removing numbers
  x  =  stripWhitespace(x)                  # removing white space
  x  =  gsub("^\\s+|\\s+$", "", x)          # remove leading and trailing white space
  return(x)
}


############################################################################
# Encoding bi/tri gram as unigram

n.gram = function(x1,                       # Text Corpus
                  ngram,                    # encoding scheme "bi" or "tri"
                  f )                       # Mininmum frequency for bi/tri gram pattern matching
  
{ if ( ngram =="bi")
  
  { 
  
  ##################################################################
  # Weka_control(min = 2, max = 2)
  ##################################################################
  
  ngram <- function(x1) NGramTokenizer(x1, Weka_control(min = 2, max = 2))  
  
  tdm0 <- TermDocumentMatrix(x1, control = list(tokenize = ngram,
                                                tolower = TRUE, 
                                                removePunctuation = TRUE,
                                                removeNumbers = TRUE,
                                                stopwords = TRUE         ))     # patience. Takes a minute.
  
  tdm = tdm0; rm('tdm0')
  
  a1 = apply(tdm, 1, sum)  
  a2 = ((a1 >= f))
  tdm.new = tdm[a2, ]
  rm('a1','a2','tdm')
  
  # remove blank documents (i.e. columns with zero sums)
  a0 = NULL; 
  for (i1 in 1:ncol(tdm.new)){ if (sum(tdm.new[, i1]) == 0) {a0 = c(a0, i1)} }
  length(a0)    # no. of empty docs in the corpus
  if (length(a0) >0) { tdm.new1 = tdm.new[, -a0]} else {tdm.new1 = tdm.new};
  
  rm('a0','tdm.new')
  dim(tdm.new1)    # reduced tdm
  x1mat = t(tdm.new1)    # don't do tfidf, not mentioned anywhere for topic modeling.
  dim(x1mat);    # store[i1, 5] = ncol(x2mat);
  
  
  test = colnames(x1mat); 
  test1 = gsub(" ",".", test);  # replace spaces with dots
  colnames(x1mat) = test1
  
  a11 = apply(x1mat, 2, sum)
  a12 = order(a11, decreasing = T)
  a13 = as.matrix(a11[a12])
  
  x1 = unlist(lapply(x1, content))
  x1 = paste("",x1,"")
  
  
  for (i in 1:nrow(a13)){    
    
    focal.term = gsub("\\.", " ", rownames(a13)[i])
    replacement.term = gsub(" ", "-", focal.term)
    replacement.term=paste("",replacement.term,"")
    x1 = gsub(paste("",focal.term,""), replacement.term, x1)  
    
  }  # now, our x corpus has the top 400 bigrams encoded as unigrams
  
  x1 = Corpus(VectorSource(x1))    # Constructs a source for a vector as input
  
  return(x1)
}

if ( ngram =="tri") 
  
  { 
  
  ##################################################################
  # Weka_control(min = 3, max = 3)
  ##################################################################
  
  ngram <- function(x1) NGramTokenizer(x1, Weka_control(min = 3, max = 3))  
  
  tdm0 <- TermDocumentMatrix(x1, control = list(tokenize = ngram,
                                                tolower = TRUE, 
                                                removePunctuation = TRUE,
                                                removeNumbers = TRUE,
                                                stopwords = TRUE  ))     # patience. Takes a minute.
  
  tdm = tdm0; rm('tdm0')
  
  a1 = apply(tdm, 1, sum)  
  a2 = ((a1 >= f))
  tdm.new = tdm[a2, ]
  rm('a1','a2','tdm')
  
  # remove blank documents (i.e. columns with zero sums)
  a0 = NULL; 
  for (i1 in 1:ncol(tdm.new)){ if (sum(tdm.new[, i1]) == 0) {a0 = c(a0, i1)} }
  length(a0)    # no. of empty docs in the corpus
  if (length(a0) >0) { tdm.new1 = tdm.new[, -a0]} else {tdm.new1 = tdm.new};
  
  rm('a0','tdm.new')
  dim(tdm.new1)    # reduced tdm
  x1mat = t(tdm.new1)    # don't do tfidf, not mentioned anywhere for topic modeling.
  dim(x1mat);    # store[i1, 5] = ncol(x2mat);
  
  
  test = colnames(x1mat); 
  test1 = gsub(" ",".", test);  # replace spaces with dots
  colnames(x1mat) = test1
  
  a11 = apply(x1mat, 2, sum)
  a12 = order(a11, decreasing = T)
  a13 = as.matrix(a11[a12])
  
  x1 = unlist(lapply(x1, content))
  x1 = paste("",x1,"")
    
  for (i in 1:nrow(a13)){    
    
    focal.term = gsub("\\.", " ", rownames(a13)[i])
    replacement.term = gsub(" ", "-", focal.term)
    replacement.term=paste("",replacement.term,"")
    x1 = gsub(paste("",focal.term,""), replacement.term, x1)   
    
  }  # now, our x corpus has the top 400 bigrams encoded as unigrams
  
  x1 = Corpus(VectorSource(x1))    # Constructs a source for a vector as input
  
  
  ##################################################################
  # Weka_control(min = 2, max = 2)
  ##################################################################
  
  ngram <- function(x1) NGramTokenizer(x1, Weka_control(min = 2, max = 2))  
  
  tdm0 <- TermDocumentMatrix(x1, control = list(tokenize = ngram,
                                                tolower = TRUE, 
                                                removePunctuation = TRUE,
                                                removeNumbers = TRUE,
                                                stopwords = TRUE))     # patience. Takes a minute.
  
  tdm = tdm0; rm('tdm0')
  
  a1 = apply(tdm, 1, sum)  
  a2 = ((a1 >= f))
  tdm.new = tdm[a2, ]
  rm('a1','a2','tdm')
  
  # remove blank documents (i.e. columns with zero sums)
  a0 = NULL; 
  for (i1 in 1:ncol(tdm.new)){ if (sum(tdm.new[, i1]) == 0) {a0 = c(a0, i1)} }
  length(a0)    # no. of empty docs in the corpus
  if (length(a0) >0) { tdm.new1 = tdm.new[, -a0]} else {tdm.new1 = tdm.new};
  
  rm('a0','tdm.new')
  dim(tdm.new1)    # reduced tdm
  x1mat = t(tdm.new1)    # don't do tfidf, not mentioned anywhere for topic modeling.
  dim(x1mat);    # store[i1, 5] = ncol(x2mat);
  
  
  test = colnames(x1mat); 
  test1 = gsub(" ",".", test);  # replace spaces with dots
  colnames(x1mat) = test1
  
  a11 = apply(x1mat, 2, sum)
  a12 = order(a11, decreasing = T)
  a13 = as.matrix(a11[a12])
  
  x1 = unlist(lapply(x1, content))
  x1 = paste("",x1,"")
  
  
  for (i in 1:nrow(a13)){    
    
    focal.term = gsub("\\.", " ", rownames(a13)[i])
    replacement.term = gsub(" ", "-", focal.term)
    replacement.term=paste("",replacement.term,"")
    x1 = gsub(paste("",focal.term,""), replacement.term, x1)  
    
  }  # now, our x corpus has the top 400 bigrams encoded as unigrams
  
  x1 = Corpus(VectorSource(x1))    # Constructs a source for a vector as input
  return(x1)
}

}

###########################################################
# Customize Document Term Matrix

custom.dtm  = function(x1,                               # Text Corpus
                scheme)                           # tf or tfidf
{
  
tdm = TermDocumentMatrix(x1)

a1 = apply(tdm, 1, sum)
a2 =((a1 >= 2))
tdm.new = tdm[a2, ]

# remove blank documents (i.e. columns with zero sums)
a0 = NULL; 
for (i1 in 1:ncol(tdm.new)){ if (sum(tdm.new[, i1]) == 0) {a0 = c(a0, i1)} }
length(a0)    # no. of empty docs in the corpus
if (length(a0) >0) { tdm.new1 = tdm.new[, -a0]} else {tdm.new1 = tdm.new};

dim(tdm.new1)    # reduced tdm
if (scheme == "tfidf") {
x2mat = t(tfidf(tdm.new1))
}
else {x2mat = t((tdm.new1))}
return(x2mat)
}




