require(RCurl)
require(XML)

pubmed.engine.prefix = 'http://www.ncbi.nlm.nih.gov/pubmed/?term='

retrieve.pubmed.searchpage<-function(gene, disease) {
  link.string<-paste(pubmed.engine.prefix, gene, '+AND+', gsub(" ", "+", disease), sep='')
  #webpage<-getURL(link.string)
  #readLines(tc<-textConnection(webpage))
  htmlParse(link.string)
}

mine.pubmed.num<-function(parsedHTML) {
  resultstr<-xpathSApply(parsedHTML, '//div//h2[@class = "result_count"]', xmlValue)
  resultstr
}
