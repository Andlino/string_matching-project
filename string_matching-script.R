#string matching project

## Fuzzy logic -> Classify: Char vs x+numeric
## stepwise match: fuzzy string
## total similarity
## These should be coded bw 0 and 1 -> Author, Title, Journal, Strings + Year, Volum, Issue, Paper, Page1, Page2 <- These should be coded 0/1/NA

# Loading dependencies
require(stringr)
require(tidyverse)
require(stringi)
require(RSQLite)
require(reshape2)
require(gdata)
require(stringdist)
require(data.table)
require("R.utils")


# Loading in data
cleandata <- read.delim("E:/R/string_matching_project/clean_refs.txt")
dirtydata <- read.delim("E:/R/string_matching_project/dirty_refs.txt")
dirtiestdata <- read.delim("C:/Users/au615270/Documents/Matching project/data/dirtier_refs.txt")
cleandata <- read.delim("C:/Users/au615270/Documents/Matching project/data/clean_refs.txt")

#Creating SQLite database
db.name <- "testdb"
c <- dbConnect(RSQLite::SQLite(), paste(db.name,"db",sep='.'))

r <- dbSendStatement(c,"create table if not exists num_token (id integer primary key autoincrement,token varchar(255))")
dbClearResult(r)
r <- dbSendStatement(c,"create index if not exists num_token_idx on num_token(token)")
dbClearResult(r)
r <- dbSendStatement(c,"create table if not exists str_token (id integer primary key autoincrement,token varchar(255))")
dbClearResult(r)
r <- dbSendQuery(c,"create index if not exists str_token_idx on str_token(token)")
dbClearResult(r)
r <- dbSendStatement(c,"create table if not exists dirty_ref (id integer primary key)")
dbClearResult(r)
r <- dbSendStatement(c,"create table if not exists dirty_num (id integer primary key, ref_id integer, num_token_id integer)")
dbClearResult(r)
r <- dbSendStatement(c,"create table if not exists dirty_str (id integer primary key, ref_id integer, str_token_id integer)")
dbClearResult(r)
r <- dbSendQuery(c,"create index if not exists dirty_num_ref_idx on dirty_num(ref_id); create index if not exists dirty_num_token_idx on dirty_num(num_token_id); ")
dbClearResult(r)
r <- dbSendQuery(c,"create index if not exists dirty_str_ref_idx on dirty_str(ref_id); create index if not exists dirty_str_token_idx on dirty_str(str_token_id); ")
dbClearResult(r)

# Loading data

#Tim
cleandata <- read_tsv("E:/R/string_matching_project/clean_refs.txt")
dirtydata <- read_tsv("E:/R/string_matching_project/dirty_refs.txt")
dirtiestdata <- read_tsv("C:/Users/au615270/Documents/Matching project/data/dirtier_refs.txt")
cleandata <- read_tsv("C:/Users/au615270/Documents/Matching project/data/clean_refs.txt")
#JP
dirtydata <- read_tsv("D:/syncs/dirty_refs.txt")
dirtiestdata <- read_tsv("D:/syncs/dirtier_refs.txt")
cleandata <- read_tsv("D:/syncs/clean_refs.txt")
#Both

dirtiestdata <- dirtiestdata %>% mutate(id = row_number())

numerics <- str_extract_all(dirtydata$ref, "\\b[a-zA-Z]{1,5}\\b\\s([\\d]{1,10})")
possiblenumerics <- str_extract_all(dirtiestdata$ref, "\\b[a-zA-Z]+?\\b(\\s|\\.|\\,|\\-|)([\\d]+)")
doi <- str_extract_all(dirtiestdata$ref, "\\bdoi\\b\\:\\d+\\.\\d+\\/[\\s\\S]+?\\s")
lotmorenumbers <- str_extract_all(dirtiestdata$ref, "\\w*?[\\s\\S].?\\d+")

character <- str_replace_all(dirtydata$ref, "\\b[a-zA-Z]{1,5}\\b\\s([\\d]{1,10})", "")
character <- str_remove_all(character, "[^\\w \\xC0-\\xFF]") # non-character words 
character <- str_remove_all(character, "\\d")
urls <- str_extract_all(dirtydata$ref, "(<script(\\s|\\S)*?<\\/script>)|(<style(\\s|\\S)*?<\\/style>)|(<!--(\\s|\\S)*?-->)|(<\\/?(\\s|\\S)*?>)")

maybehelpful <- str_extract_all(dirtiestdata$ref, "[a-zA-Z][a-zA-Z0-9-_]{3,32}") #Must start with an alphabetic character. Can contain the following characters: a-z A-Z 0-9 - and _

characters <- as.character(dirtydata$ref)
characters <- str_replace_all(characters, "  ", " ")
characters <- as.list(characters)
authors <- str_extract_all(dirtiestdata$ref, "\\w*[\\s\\S]\\w{1}.?[\\s\\S]\\w{1}.?[\\s\\S]\\bet al\\b|\\w*[\\s\\S]\\w{1}.?[\\s\\S]\\bet al\\b|(\\w*[\\s\\S]\\bet al\\b)")

dirtydata$numerics <- numerics
dirtydata$characters <- character

#######################################################################################################
#######################################################################################################
################################### CLEANING DIRTY DATA ###############################################

lotmorenumbers <- str_extract_all(dirtiestdata$ref, "\\w*?[\\s\\S].?\\d+")
authors <- str_extract_all(dirtiestdata$ref, "\\w*[\\s\\S]\\w{1}.?[\\s\\S]\\w{1}.?[\\s\\S]\\bet al\\b|\\w*[\\s\\S]\\w{1}.?[\\s\\S]\\bet al\\b|(\\w*[\\s\\S]\\bet al\\b)")
characters <- str_extract_all(dirtiestdata$ref, "[a-zA-Z]+")
#mdata <- do.call("rbind", lapply(lotmorenumbers, data.frame, stringsAsFactors = FALSE)) 
mdata <- melt(lotmorenumbers)
#mdata <- tibble::rowid_to_column(mdata, "ID")
colnames(mdata)[colnames(mdata)=="value"] <- "token"
colnames(mdata)[colnames(mdata)=="L1"] <- "ID"
#temp note: changed $numeric to $num_token as numeric is a keyword
mdata$num_token <- regmatches(mdata$token, gregexpr("[[:digit:]]+", mdata$token))
mdata$words <- (str_extract(mdata$token, "[aA-zZ]+"))

#clean DB before executing
r <- dbSendStatement(c,"delete from dirty_ref")
dbClearResult(r)
r <- dbSendStatement(c,"delete from num_token")
dbClearResult(r)
r <- dbSendStatement(c,"delete from str_token")
dbClearResult(r)
r <- dbSendStatement(c,"delete from dirty_num")
dbClearResult(r)
r <- dbSendStatement(c,"delete from dirty_str")
dbClearResult(r)

pb <- ProgressBar(max=100, ticks=10, stepLength=100/nrow(mdata), newlineWhenDone=TRUE)
reset(pb)
#insert tokens into DB
for (i in 1:nrow(mdata)) {
  x <- mdata[i,]
  # Check and insert reference
  r <- dbSendQuery(c,"select id from dirty_ref where id = ?",param=c(x$ID))
  rs <- dbFetch(r)
  dbClearResult(r)
  if (nrow(rs) == 0) {
    r <- dbSendStatement(c,"insert into dirty_ref (id) values (?)",param=c(x$ID))
    dbClearResult(r)
  }
  
  # Check and insert numerical token
  r <- dbSendQuery(c,"select id from num_token where token = ?",param=c(x$num_token))
  rs <- dbFetch(r)
  dbClearResult(r)
  if (nrow(rs) == 0) {
    r <- dbSendStatement(c,"insert into num_token (token) values (?)",param=c(x$num_token))
    dbClearResult(r)
    r <- dbSendStatement(c,"select last_insert_rowid()")
    rs <- dbFetch(r)
    num_id <- rs[1,]
    dbClearResult(r)
  } else {
    num_id <- rs$id
  }
  
  # Check and insert numerical relation
  r <- dbSendQuery(c,"select id from dirty_num where ref_id = ? and num_token_id = ?",param=c(x$ID,num_id))
  rs <- dbFetch(r)
  dbClearResult(r)
  if (nrow(rs) == 0) {
    r <- dbSendStatement(c,"insert into dirty_num (ref_id,num_token_id) values (?,?)",param=c(x$ID,num_id))
    dbClearResult(r)
  }
  
  # Check and insert string token
  if (!is.na(x$words)) {
    r <- dbSendQuery(c,"select id from str_token where token = ?",param=c(x$words))
    rs <- dbFetch(r)
    dbClearResult(r)
    if (nrow(rs) == 0) {
      r <- dbSendStatement(c,"insert into str_token (token) values (?)",param=c(x$words))
      dbClearResult(r)
      r <- dbSendStatement(c,"select last_insert_rowid()")
      rs <- dbFetch(r)
      str_id <- rs[1,]
      dbClearResult(r)
    } else {
      str_id <- rs$id
    }
    
    # Check and insert string relation
    r <- dbSendQuery(c,"select id from dirty_str where ref_id = ? and str_token_id = ?",param=c(x$ID,str_id))
    rs <- dbFetch(r)
    dbClearResult(r)
    if (nrow(rs) == 0) {
      r <- dbSendStatement(c,"insert into dirty_str (ref_id,str_token_id) values (?,?)",param=c(x$ID,str_id))
      dbClearResult(r)
    }
  }
  increase(pb)
}


#######################################################################################################
#######################################################################################################

##### Fuzzy matching project #####


options(stringsAsFactors = FALSE)


matchingdata <- read.delim("E:/R/string_matching_project/clean_refs.txt")
matchingdata2 <- read.delim("E:/R/string_matching_project/dirty_refs.txt")

source1.devices <- as.data.frame(test$words)
colnames(source1.devices)[1] <- "name"
source1.devices$name <-as.character(source1.devices$name)

source2.devices <- as.data.frame(dirtiestdata$ref)
colnames(source2.devices)[1] <-"name"
source2.devices$name<- as.character(source2.devices$name)


distance.methods<-c('lv')
dist.methods<-list()
system.time(for(m in 1:length(distance.methods)) {
  dist.name.enh<-matrix(NA, ncol = length(source2.devices$name),nrow = length(source1.devices$name))
  for(i in 1:length(source2.devices$name)) {
    for(j in 1:length(source1.devices$name)) { 
      dist.name.enh[j,i]<-stringdist(tolower(source2.devices[i,]), tolower(source1.devices[j,]), method = distance.methods[m])      
      #adist.enhance(source2.devices[i,]$name,source1.devices[j,]$name)
    }  
  }
  dist.methods[[distance.methods[m]]]<-dist.name.enh
})

match.s1.s2.enh<-NULL
for(m in 1:length(dist.methods)) {
  
  dist.matrix<-as.matrix(dist.methods[[distance.methods[m]]])
  min.name.enh<-apply(dist.matrix, 1, base::min)
  for(i in 1:nrow(dist.matrix))
  {
    s2.i<-match(min.name.enh[i],dist.matrix[i,])
    s1.i<-i
    match.s1.s2.enh<-rbind(data.frame(s2.i=s2.i,s1.i=s1.i,s2name=source2.devices[s2.i,], s1name=source1.devices[s1.i,], adist=min.name.enh[i],method=distance.methods[m]),match.s1.s2.enh)
  }
}
# Let's have a look at the results


matched.names.matrix<-dcast(match.s1.s2.enh,s2.i+s1.i+s2name+s1name~method, value.var = "adist")
View(matched.names.matrix)


lotmorenumbers <- str_extract_all(dirtiestdata$ref, "\\w*?[\\s\\S].?\\d+")
test <- melt(lotmorenumbers)
colnames(test)[colnames(test)=="value"] <- "token"
colnames(test)[colnames(test)=="L1"] <- "ID"
test$numeric <- regmatches(test$token, gregexpr("[[:digit:]]+", test$token))
test$words <- (str_extract(test$token, "[aA-zZ]+"))
View(test)