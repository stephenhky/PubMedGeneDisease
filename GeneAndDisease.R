

library(RCurl)
library(XML)

#a function is written to return a string when put in a gene name and a disease name

pubmed.engine.prefix = 'http://www.ncbi.nlm.nih.gov/pubmed/?term='

retrieve.pubmed.searchpage<-function(gene, disease) {
  link.string<-paste(pubmed.engine.prefix, gene, '+AND+', gsub(" ", "+", disease), sep='')
  htmlParse(link.string)
}

mine.pubmed.content<-function(parsedHTML) {
  category<-xpathSApply(parsedHTML, '//title', xmlValue)
  if (grepl("No items found",category)==TRUE){
    resultstr<-"None"
  } else if (grepl("[.*]",category)==TRUE){
    resultstr<-as.numeric(1)
  } else {
    alternative<-xpathSApply(parsedHTML, '//ul//li//span//i', xmlValue)
    if (length(alternative)>0){
      resultstr<-"None"
    } else {
      resultstr<-xpathSApply(parsedHTML, '//div//h2[@class = "result_count"]', xmlValue)
    }
  }
  resultstr
}

mine.pubmed.num<-function(gene, disease) {
  resultstr<-mine.pubmed.content(retrieve.pubmed.searchpage(gene, disease))
  result<-0
  if (grepl("of",resultstr)==TRUE){
    count<-regexpr("of",resultstr)
    result<-as.numeric(substr(resultstr,count+3,count+10))
  }else if (grepl("Results:",resultstr)==TRUE){
    count<-regexpr(":",resultstr)
    result<-as.numeric(substr(resultstr,count+1,count+10))
  }
  if (resultstr=='None' | is.na(resultstr)) {
    result<-0
  }
  result
}

