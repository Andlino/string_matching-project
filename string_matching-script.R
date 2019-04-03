#string matching project

## Fuzzy logic -> Classify: Char vs x+numeric
## stepwise match: fuzzy string
## total similarity
## These should be coded bw 0 and 1 -> Author, Title, Journal, Strings + Year, Volum, Issue, Paper, Page1, Page2 <- These should be coded 0/1/NA

# Loading in data
install.packages("stringr")
library(stringr)


cleandata <- read.delim("E:/R/string_matching_project/clean_refs.txt")
dirtydata <- read.delim("E:/R/string_matching_project/dirty_refs.txt")
teststring <- dirtydata$ref[5]

numerics <- str_extract_all(dirtydata$ref, "\\b[a-zA-Z]{1,5}\\b\\s([\\d]{1,10})")
character <- str_replace_all(dirtydata$ref, "\\b[a-zA-Z]{1,5}\\b\\s([\\d]{1,10})", "")
character <- str_remove_all(character, "\\d")
character <- as.list(character)
