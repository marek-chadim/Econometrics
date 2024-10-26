library(magrittr)
library(rvest)
library(stringr)

larecherche <- read_html("https://www.abebooks.com/servlet/SearchResults?pt=book&sortby=17&tn=a+la+recherche+du+temps+perdu&an=proust&cm_sp=pan-_-srp-_-ptbook")

title <- larecherche %>%
        html_nodes(".col-xs-8 a span") %>%
        html_text()

#Then discard the "ISBN" code entries and keep the titles only.
# You can achieve this with str_detect()

title <- title[!str_detect(title, "ISBN")]


pricehtml<-html_elements(larecherche,".col-xs-4 .item-price")
pricetext<-html_text(pricehtml)

# The possible mismatch between the length of 'title' and 'pricetext' comes from 
# the fact that some books are offered at a discounted price and said 
# price is also displayed in the page (above the discounted one). 
