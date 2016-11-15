library(pdftools)

library(stringr)
library(tidyverse)
#nohup R --no-save <win/project/bogachiel/baker/BakerPDFS/bakerpdfcode2.R >& win/project/bogachiel/baker/BakerPDFS/bakerpdfcode2.Rout &
library(tm)
library(SnowballC)
library(stringr)
library(RCurl)

tt <- readPDF(engine=c("xpdf"))

file_list = grep('\\.pdf$',list.files('Input/pdfs/clean/'),value=T)
prefix = 'Input/pdfs/clean/'
save_loc = 'Input/pdfs/text/'


#lapply(file_list[1:10],function(i)
for(i in 1:length(file_list)){
  dfile <-paste0(prefix,file_list[i])}
  rr <- tt(elem=list(uri=dfile), language="en")
  write.table(unlist(rr), file=paste(save_loc,gsub('\\.pdf','',file_list[i]), ".txt",sep=""),
              quote = FALSE, row.names = FALSE, col.names = FALSE, eol = " ")
  rm(rr)
}



lapply(file_list,function(i) 
write.table(
x=stringr::str_replace_all(paste0(pdf_text(paste0(prefix,i)),
                                  sep=' ',collapse=''),
                           "[\\s]+", " "),
file=paste0(save_loc,gsub('\\.pdf$','',i),'.txt'),
quote = FALSE, row.names = FALSE, col.names = FALSE, eol = " "))

