
library(dtplyr)
library(data.table)
tdf = fread('Input/scraped_data/temp_cleaned_data.csv',header=T)



file_list = c(list.files('nlp_special/BakerPDFS/pdfs2/'),list.files('nlp_special/BakerPDFS/pdfs/'),list.files('nlp_special/BakerPDFS/pdfs3/'))

meet_files =file_list %>% .[!grepl('Lake Shannon|Justification|Baker Lake|summary draft|Agenda|agenda|Decision|decision|Propos|propos|Update|update|bulletin|ProjEval|ImPlan|Committee Report for July|refresher|request|Request|Designated|designated|Aquatic Resources Group_FINAL',.)] %>% gsub('\\.pdf$','',.)

meet_files[!meet_files %in% sort(unique(tdf$Meeting))]


meeting_df = tdf %>% filter(!duplicated(Meeting))

knitr::kable(table(meeting_df$Topic,meeting_df$Year))




meeting_df %>% filter(Topic == 'admin',Year==2010) %>% arrange(Year)

sort(meet_files[meet_files %in% sort(unique(temp$Meeting))])

meet_files %in% sort(unique(temp$Meeting))
meet_files
temp$Meeting

for (i in )