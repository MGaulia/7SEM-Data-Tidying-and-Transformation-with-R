library(stringr)
library(tidyverse)

testStr <- c("a: skyryba , . ? ! ... ; - ,- : ()",
             "b?: text?",
             "b.: text.",
             "c: text$$$text",
             "momo")

#1.a)
atest <- " , . ? ! ; - : ()"
aPattern <- "[[:punct:]]"
writeLines(aPattern)
str_view_all(string = atest, pattern = aPattern)
 
#1.b)
btest <- c("text?", "text.")
bPattern <- "(\\?|\\.)$"
writeLines(bPattern)
str_view_all(string = btest, pattern = bPattern)

#1.c)
ctest <-  c("test$$$test","$$$test","test$$$","$$$")
cPattern <- "\\$\\$\\$"
writeLines(cPattern)
str_view_all(string = ctest, pattern = cPattern)

#1.d)
dtest <- c("momo","mom")
dPattern <- "([\\w]).*\\1.{1}"
writeLines(dPattern)
str_view(string = dtest, pattern = dPattern)

#1.e)
etest <- c("Test", "Testagain", "test")
ePattern <- "([A-Z]{1}+[a-z]{3,})"
writeLines(ePattern)
str_view(string = etest, pattern = ePattern)

#2
papers <- read.csv(file = "papers.csv",header = FALSE)[[1]]

#2.a)
sum(str_count(string = papers, pattern = "(?<![\\w\\d])(Katauskis|Pileckas|Skakauskas)(?![\\w\\d])"))
# 17

#2.b1)
max(nchar(papers))
# 1448

#2.b2)
max(str_count(papers, "\\S+"))
# 190

#2.c)
authorcounts <- str_count(word(papers,1, sep = fixed(".")), ";") + 1
max(authorcounts)
papers[which.max(authorcounts)]

#2.d)
# Pastebejau kad Clarivate JCR SCIE kvartilis visada eina pirmas
twodres <- str_match(string = papers, pattern = "Q.")
as.data.frame(twodres) %>% filter(V1 =="Q1") %>% nrow()
# 73

#2.e)
sum(gsub("[^A-Z]","",sapply(str_split(papers, ":"), tail, 1)) %>% nchar() > 1)
# 54

#2.f)
twof <- as.data.frame(str_match(papers, "IF: (.*?); AIF")[,2])
colnames(twof) <- "IF_char"
twof <- twof %>% mutate(IF_num = as.numeric(str_replace(IF_char, ",", ".")))
mean(twof$IF_num)
# 1.800296
