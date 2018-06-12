rm(list=ls())

require(tm)||install.packages("tm"); library(tm)  
require(tm.plugin.webmining)||install.packages("tm.plugin.webmining"); library(tm.plugin.webmining)


# Extract Google news for Moto Turbo

mt <- WebCorpus(GoogleNewsSource("KINDLE"))

x1 = mt # save the corpus in a local file

x1 = unlist(lapply(x1, content)) # strip relevant content from x1

x1 = gsub("\n", "", x1) # remove newline chars

x1[50:50] # view content

write.table(x1, file.choose(), row.names=F, col.names=F) # save file as 'mt_news.txt'