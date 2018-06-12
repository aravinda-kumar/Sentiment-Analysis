require(RCurl)
require(XML)

#thepage = readLines('http://mmb.moneycontrol.com/stock-message-forum/vipindustries/comments/1542')

thepage = readLines('http://mmb.moneycontrol.com/stock-message-forum/vipindustries/comments/1542')

#c <- grep('<div class=\"CL\">',thepage)
c <- grep('<div class=\"info\">',thepage)
#<div class=\"info\">
thepage[c]

write.table(thepage[c], 'C:/Apps/Temp Data/VIPIND.txt', row.names=F, col.names=F)

for (i in 1:length(c)) {
  temp = thepage[c[i]]
  #temp = substr(temp, 136,1000000L)
  #temp = sapply(strsplit(temp, ":"), "", 2)
  temp = gsub(">","", temp)
  print(temp)
}

temp = thepage[c[1]]
#temp = substr(temp, 136,1000000L)
#temp = sapply(strsplit(temp, ":"), "", 2)
temp = gsub("[A-Z][1-9]:", "", temp)
print(temp)


MyData <- read.csv(file="C:/Apps/BSE/Raw Data/Daily Data/500002.csv", header=F, sep="," ,nrows=10)
View(MyData)
count(MyData)


#thepage[1465:1520]
#<div class=\"FL PL15 col2\">
