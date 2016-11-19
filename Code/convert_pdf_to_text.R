library(pdftools)

library(stringr)
library(tidyverse)
#nohup R --no-save <win/project/bogachiel/baker/BakerPDFS/bakerpdfcode2.R >& win/project/bogachiel/baker/BakerPDFS/bakerpdfcode2.Rout &
library(tm)
library(SnowballC)
library(stringr)
library(RCurl)

processURI2 <-function(uri) {
  uri <- as.character(uri)
  if (identical(substr(uri, 1, 7), "file://"))
    uri <- substr(uri, 8, nchar(uri))
  uri
}



readPDF2<-function (engine = c("xpdf", "Rpoppler", "ghostscript", "Rcampdf","custom"), control = list(info = NULL, text = NULL)) {
  stopifnot(is.character(engine), is.list(control))
  engine <- match.arg(engine)
  pdf_info <- switch(engine, xpdf = function(x) tm:::pdf_info_via_xpdf(x,control$info), Rpoppler = Rpoppler::PDF_info, ghostscript = tm:::pdf_info_via_gs,Rcampdf = Rcampdf::pdf_info, custom = control$info)
  pdf_text <- switch(engine, xpdf = function(x) system2("pdftotext", c(control$text, shQuote(x), "-"), stdout = TRUE), Rpoppler = Rpoppler::PDF_text, ghostscript = pdf_text_via_gs, Rcampdf = Rcampdf::pdf_text, custom = control$text)
  if (!is.function(pdf_info) || !is.function(pdf_text)) 
    stop("invalid function for PDF extraction")
  function(elem, language, id) {
    uri <- processURI2(elem$uri)
    #meta <- pdf_info(uri)
    meta<-list()
    content <- pdf_text(uri)
    content<-iconv(enc2utf8(content), sub = "byte")
    tm::PlainTextDocument(content, meta$Author, meta$CreationDate, 
                          meta$Subject, meta$Title, basename(elem$uri), language, 
                          meta$Creator)
  }
}


file_list = grep('\\.pdf$',list.files('Input/pdfs/clean/'),value=T)
prefix = 'Input/pdfs/clean/'
save_loc = 'Input/pdfs/text/'

# lapply(file_list,function(i) 
#   write.table(
#     x=stringr::str_replace_all(paste0(pdf_text(paste0(prefix,i)),
#                                       sep=' ',collapse=''),
#                                "[\\s]+", " "),
#     file=paste0(save_loc,gsub('\\.pdf$','',i),'.txt'),
#     quote = FALSE, row.names = FALSE, col.names = FALSE, eol = " "))
# 

tt <- readPDF2(engine=c("xpdf"),control = list(info = '-f'))

for(i in file_list){
  print(i)
  dfile <-paste0(prefix,i)
  rr <- tt(elem = list(uri = dfile), language = "en") 
  rr <- stringr::str_replace_all(unlist(rr),"[\\s]+", " ")
  write.table(rr, file=paste(save_loc,gsub('\\.pdf','',i), ".txt",sep=""),
              quote = FALSE, row.names = FALSE, col.names = FALSE, eol = " ")
  rm(rr)
}

