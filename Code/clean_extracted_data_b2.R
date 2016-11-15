library(plyr);library(dplyr);library(tidyr)
library(lubridate)
temp = read.csv('Input/wa_longitudinal/scraped_data/meeting_attendance_extraction_batch2.csv',
                stringsAsFactors = F)
temp = temp %>% select(-X)
library(stringr)

# temp$Date = NA
# temp$Date = ymd(str_extract(temp$Meeting,'[0-9]{4}-[0-9]{1,2}-[0-9]{1,2}'))

temp = temp[!grepl('^[^A-Z]',temp$Name),]
temp$Org = as.character(temp$Org)
temp$Name = as.character(temp$Name)
temp$Org[grepl('^[a-z]',temp$Org)] = 99999
## Drop name obs where "upper baker" or "lower baker" is recorded as person
temp = temp[temp$Name %in% c('Upper Baker','Lower Baker')==F,]


# Drop ASAP (and extra things like "WDFW" that shouldn't be part of names)
temp$Name = gsub(' [A-Z]{4}','',temp$Name)
temp$Name = gsub('[A-Z]{4}','',temp$Name)
#### Drop All 1-name only observations (i.e., "Kevin") ###
temp = temp[grep(' ',temp$Name,invert=F),]

#### Drop "names" which are note to email someone
temp = temp[grep('Email',temp$Name,invert=T),]

temp$Org[grep('Perkins',temp$Org)] = 'Perkins Coie'
temp$Org[grep('R2',temp$Org)] = 'R2 Resource Consultants'
temp$Org[grep('Nature Conservancy',temp$Org)] = 'TNC'
temp$Org[grep('TNC',temp$Org)] = 'TNC'

###make all pse consistent
temp$Org = gsub('Puget Sound Energy','PSE',temp$Org)
temp$Org[grep('PSE',temp$Org)] = 'PSE'


### make all usfs consistent
temp$Org[grep('Forest',temp$Org)] = 'USFS'
temp$Org[grep('USDA-FS',temp$Org)] = 'USFS'
temp$Org[grep('USFS',temp$Org)] = 'USFS'

temp$Org[grep('Indian Fisheries|Indian Fish Com|NWIF',temp$Org)] = 'NWIFC'

temp$Org[grep('Upper Skagit',temp$Org)] = 'Upper Skagit Indian Tribe'
temp$Org[grep('Sauk',temp$Org)] = 'Sauk-Suiattle Indian Tribe'
temp$Org[grep('Swinomish Tr|Swinomish Ind',temp$Org)] = 'Swinomish Indian Tribe'

temp$Org[grep('Jessie',temp$Name)] = 'PSE'
temp$Org = gsub('consultant from ','',temp$Org)
temp = temp[grep('Pied Piper',temp$Name,invert=T),]
temp$Org[grep('Charles Howard',temp$Name)] = 'Charles Howard & Associates'
temp = temp[temp$Org != 'Salvelinus malma',]
temp = temp[temp$Name != 'Platanthera orbiculata',]

temp$Org = gsub('^the ','',temp$Org)
temp$Org = gsub('^with ','',temp$Org)
temp$Org = gsub('^of ','',temp$Org)

temp$Org[grep('U.S. Fish',temp$Org)] = 'USFWS'
temp$Org[grep('USFWS',temp$Org)] = 'USFWS'
temp$Org[grep('US Fish and Wildlife',temp$Org)] = 'USFWS'
temp$Org[grep('Natural Resources',temp$Org)] = 'WDNR'
temp$Org[grep('DNR',temp$Org)] = 'WDNR'
temp$Org[grep('Ecology',temp$Org)] = 'WDOE'
temp$Org[grep('WA DOE',temp$Org)] = 'WDOE'
temp$Org[grep('DOE',temp$Org)] = 'WDOE'
temp$Org[intersect(grep('Arch',temp$Org),grep('WA',temp$Org))] = 'WAHP'
temp$Org[intersect(grep('Arch',temp$Org),grep('Hist',temp$Org))] = 'WAHP'
temp$Org[grep('DAHP',temp$Org)] = 'WAHP'
temp$Org[intersect(intersect(grep('WA|Wash',temp$Org),grep('Fish|fish',temp$Org)),grep('Wild|wild',temp$Org))] = 'WDFW'
temp$Org[intersect(intersect(grep('US|U.S.',temp$Org),grep('Fish|fish',temp$Org)),grep('Wild|wilde',temp$Org))] = 'WDFW'
temp$Org[grep('WDFW',temp$Org)] = 'WDFW'
temp$Org[temp$Org == 'FS'] = 'USFS'
temp$Org[grep('Berger Group',temp$Org)] = 'Louis Berger Group'
temp$Org[grep('Trout',temp$Org)] = 'Trout Unlimited'
temp$Org[intersect(intersect(grep('Corp|Corps',temp$Org),grep('Engineer',temp$Org)),grep('US|U.S.|Army',temp$Org))] = 'USACE'
temp$Org[grep('USACE',temp$Org)] = 'USACE'
temp$Org[grep('N. Park',temp$Org)] = 'NPS'
temp$Org[grep('National Park',temp$Org)] = 'NPS'
temp$Org[temp$Name == 'Gary Davis'] = 'WDOT'
temp$Org[temp$Name == 'Ted Smith'] = 'WA State Parks'
temp$Org[temp$Name == 'Steve Flude'] = 'Skagit Co. Public Works'
temp$Org[grep('U.S.F.S',temp$Org)] = 'USFS'
temp$Org = gsub('&','and',temp$Org)
temp$Org = gsub('Weeds','Weed',temp$Org)
temp$Org = gsub("Attorney’s",'Attorney',temp$Org)
temp$Org[grep('citizen|Citizen',temp$Org)] = 'Citizen'
temp$Org[grep(' resident',temp$Org)] = 'Citizen'
temp$Org[grep('Hamer',temp$Org)] = 'Hamer Environmental'
temp$Org[grep('Hall Assoc.',temp$Org)] = 'Hall and Associates'
temp$Org = gsub('President, ','',temp$Org)
temp$Org[grep('Powel',temp$Org)] = 'Powel Ltd.'
temp$Name[grep('Tung',temp$Name)] = 'Tung Van Do'
temp = temp[temp$Name != 'Arnie Kurt Bearsdlee Don Schluter',]
temp$Org[temp$Org=='Senior Consultant and former president of CHAL'] = 'CHAL'
temp$Org[grep('Charles Howard',temp$Org)] = 'CHAL'
temp$Name[temp$Name=='Chuck Howard'] = 'Charles Howard'
temp$Org[grep('guest|Guest',temp$Org)] = 'Guest'
temp$Org[intersect(grep('Nox',temp$Org),grep('Whatcom',temp$Org))] = 'Whatcom County Noxious Weed Control Board'
temp$Org[intersect(grep('Nox',temp$Org),grep('Skagit',temp$Org))] = 'Skagit County Noxious Weed Control Board'
temp$Org[intersect(grep('Skagit',temp$Org),grep('Fish',temp$Org))] = 'SFEG'
temp$Org[intersect(intersect(grep('Skagit',temp$Org),grep('Sys',temp$Org)),grep('Coop',temp$Org))] = 'Skagit River System Coop'
temp$Org[intersect(grep('Skagit',temp$Org),grep('Parks',temp$Org))] = 'Skagit County Parks and Recreation'
temp$Org[intersect(grep('Skagit',temp$Org),grep('Emerg',temp$Org))] = 'Skagit County Emergency Management'
temp$Org[grep('SRSC|SSC',temp$Org)] = 'Skagit River System Coop'
temp$Org[intersect(intersect(grep('Skagit',temp$Org),grep('Public',temp$Org)),grep('Works',temp$Org))] = 'Skagit County Public Works'
temp$Org[intersect(intersect(grep('Rocky',temp$Org),grep('Elk',temp$Org)),grep('Mt|Mountain',temp$Org))] = 'RMEF'
temp$Org[grep('CHAL',temp$Org)] = 'CHAL'
temp$Org[grep('NPS',temp$Org)] = 'NPS'
temp$Org[grep('Smayda',temp$Org)] = 'Smayda Environmental Associates'
temp = temp[temp$Name!='Charles Howard Model|Charles Howard Operation',]
temp$Name = gsub('.Facilitator','',temp$Name)
temp$Org = gsub('Co\\.','County',temp$Org)
temp$Org[grep('Wildcat',temp$Org)] = 'Wildcat Steelhead Club'
temp$Org[grep('Huckell',temp$Org)] = 'Huckell/Weinman Associates'
temp$Org[grep('Vanter',temp$Org)] = 'Van De Vanter Group'
temp$Org[grep('PDSA',temp$Org)] = 'PDSA Consulting'

temp$Org[intersect(grep('Arch',temp$Org),grep('North',temp$Org))] = 'Northwest Archaeological Associates'



## Loop through names; if there is a close match, and close match is more common than i, replace i with close match
for (i in 1:nrow(temp))
{
  count_sim = table(agrep(temp$Name[i],as.character(temp$Name),max.distance =list(all=0.1,insertions=1,deletions=1,substitutions=1),value=T))
  if(length(count_sim)>1)
  {
    if(count_sim[which.max(count_sim)]>1 & names(count_sim[temp$Name[i]]) != names(which.max(count_sim)))
    {temp$Name[i] = names(which.max(count_sim))}
  }
}




temp = temp[grep('rep\\.',temp$Name,invert=T),]
temp = temp[nchar(temp$Name)<24,]
temp = temp[grep(' re',temp$Name,invert=T),]
temp = temp[grep('^[a-z]',temp$Name,invert=T),]
temp = temp[grep(' [a-z]',temp$Name,invert=T),]
temp = temp[grep('[0-9]',temp$Name,invert=T),]


temp$Org[temp$Name == 'Leland Stilson/DNR'] = 'WDNR'
temp$Name[temp$Name == 'Arnold Aspelund'] = 'Arnie Aspelund'
temp$Name[temp$Name == 'Leland Stilson/DNR'] = 'Lee Stilson'
temp$Org[temp$Name == 'Bill Ryan/EPA Hydro'] = 'EPA Hydro'
temp$Name[temp$Name == 'Bill Ryan/EPA Hydro'] = 'Bill Ryan'
temp$Name = gsub('\\/$','',temp$Name)

temp$Org[temp$Name == 'Barb Gassler-PSE'] = 'PSE'
temp$Name[temp$Name == 'Barb Gassler-PSE'] = 'Barb Gassler'
temp$Name[temp$Name == "Mark DownenAssessments" ] = "Mark Downen" 

temp$Name[temp$Name == "Dave Seiler-"] = 'Dave Seiler'
temp$Name[temp$Name == "Gary Sprague –"] = 'Gary Sprague'

temp = temp[grep('[A-Z]{2}',temp$Name,invert=T),]
temp = temp[grep(' [A-Z]',temp$Name),]
temp = temp[grep(' [^a-z] ',temp$Name,invert=T),]
temp = temp[grep('^[^a-z] ',temp$Name,invert=T),]

temp$Name = gsub('^Mr\\. ','',temp$Name)
temp$Name = gsub('^Dr\\. ','',temp$Name)
temp$Name = gsub('^Ms\\. ','',temp$Name)

temp$Org[temp$Name=='Bob Wright-Ecology'] = 'WDOE'
temp$Name[temp$Name=="Peter H. Dykstra"] = "Peter Dykstra"
temp$Name[temp$Name=="Chal A. Martin"] = "Chal Martin"
temp$Name[temp$Name=="Sgt. Rich Phillip"] = "Sgt. Richard Phillip"

temp$Name[temp$Name=='Bob Wright-Ecology'] = 'Bob Wright'
temp$Name[grep('Jerry Stedinger',temp$Name)] = 'Jerry Stedinger'
temp$Name[grep("David R. Montgomery",temp$Name)] = "David R. Montgomery"
temp$Name[grep("Omroa Bhagwandin",temp$Name)] = "Omrau Bhagwandin" 
temp$Name[temp$Name=='Kathleen W. Smayda'] = 'Kathy Smayda'
temp$Name[temp$Name=='Washington Scott Heller'] = 'Scott Heller'

temp = temp[grep('\\.$',temp$Name,invert=T),]
temp = temp[grep('Wiltse ',temp$Name,invert=T),]
temp = temp[!(temp$Name %in% c("Reed Canarygrass"    ,   "Tony Forward Kathy" ,"Rick Call Daryl Hamburg","Smayda Envrionmental","Tony Contact Susan Hada",
                             "Tony Tony Fuchs","Tony Tony Tony Tony"  ,"Tony Work"   ,"Mike Tony/Ray"     ,"Tony Research","Tony Forward Kathy",
                             "Salmonid Ruth", "La Connor","Fidalgo Flyfishers","Ryan Booth Kelly Bush","Tony Tony Fuchs","Sedro Woolley" ,
                             "Shannon Cr." ,"Samish Jessie" ,"Rob W. What","Peregrine Falcon" ,"Point Elliott","President Bush","Baker R." ,
                             "Chum Salmon" ,"Well Dones", "Representative Larson","Preschedule Baker"  ,"Meeting Dee"  ,"Marty Draft"  ,"Andy Aesthetics" ,
                             "Andy Need"         ,      "Andy—Tell Berger"  , "Burger King"," Cary"   ,"Deputy Nelson"    ,"G. Reopeners" ,
                             "Equinox Campbell" ,"Elk Herd"   ,"Grizzly Bear"  , "Jerry Anacortes","J. A." ,"Kathleen Update",
                             "Don Connect" ,"Design Eldridge"  ,"Teamlets Dee"  , "Vernon Hydrographs","Baker Relicense" ,
                             "Baker Nale.","Moore/Kate Chaney",    "Susan What","Alison Studley",'Dave Work','Kuzler','Scott Status',
                             "Shannon Lakes","Mother Hen","Mark Dailyhere","Blue Tarp","Dave Talk",'Dave Vet','Dave Dave','June Mtgs',
                             "Greg- Deputy","Greg Lind Craig Gannett","Miss Saul",'Chris Elk',
                             "Montgomery Watson","Montgomery Watson Harza","Elk Teamlet","Jeff McGowan Lyn Wiltse","Colonel Graves",
                             "Congressman Larson","Charles D. D. Howard","J. Largeplantsare",
                             'Vernon Joint',"Blue Tarp","Bob W.—Regarding" ,'Bob Helton Nick','Ira Marty',
                             "Bob W—Draft","Bob N. Actions","Bob Need","Kathleen Test","Carl Report"     ,        "Chris What"  ,
                             "Captain Hebner" ,"Charles Howard Model","Janis Bouma Ardis Bynum" ,
                             "Patrick Dylan Parks","Kim Work",'Lorna Check','Nick Coordinate',
                             'Tony Coordinate',"Elizabeth Investigate",
                             "Louis Berger Andy","Louis Berger/Meridian",
                             "Desmond Dr. S.E. Lacy","Don Gay/Fred Seavey",
                             "Baker Ranger" ,"Brian What"  ,"Krispy Kreme","Salmonid Fry",
                             "Tony Marty" ,  "Tony From"  ,  "Tony Chris –", "Tony Status" ,
                             "Tony Connect", "Tony Contact Susan Hada","Tony- I",'Tony Work','Tony Sent', 'Tony What','Tony Research',
                             'Tony Check','Tony Kathy',"Tony Coordinate Joetta",
                             "Hayes","Stan Cc", "Carol Check" , "Jeff- Cake" )),] 
temp$Name = gsub('Officer ','',temp$Name)

most.common.org = temp %>% group_by(Name,Org)  %>% summarise(count.obs=n()) %>% filter(Org != 99999,nchar(Org)!=1)
v1 = most.common.org %>% filter(any(grepl('[A-Z]{2}',Org))) %>% filter(grepl('[A-Z]{2}',Org)) %>% filter(row_number(desc(count.obs))==1)
v2 = most.common.org %>% filter(!any(grepl('[A-Z]{2}',Org))) %>% filter(row_number(desc(count.obs))==1)
common.org = full_join(v1,v2)

temp$Org = common.org$Org[match(temp$Name,common.org$Name)]

temp = temp[!duplicated(temp),]

attended = temp %>% group_by(Name) %>% summarise(meetings.attended = n())

temp$meetings.attended = attended$meetings.attended[match(temp$Name,attended$Name)]


temp = temp[!(grepl('Tony',temp$Name)&!grepl('Tony Fuchs',temp$Name)),]

temp = temp[!grepl('•',temp$Name),]
temp = temp[!grepl(' Work',temp$Name),]
temp = temp[!grepl('Talk',temp$Name),]
temp = temp[!grepl('Char ',temp$Name),]
temp = temp[!grepl(' Meet',temp$Name),]
temp = temp[!grepl(' Meet',temp$Name),]
temp = temp[!grepl('Cc',temp$Name),]
temp = temp[!grepl('Jeff- Cake',temp$Name),]
temp = temp[!grepl('Pam Pam',temp$Name),]
temp = temp[!grepl('Hayes',temp$Name),]
temp = temp[!grepl('Haley Re',temp$Name),]
temp = temp[!grepl('Check',temp$Name),]
temp = temp[!grepl('Oversee',temp$Name),]
temp = temp[!grepl('Schedule',temp$Name),]
temp = temp[!grepl('Gifford Pinchot',temp$Name),]
temp = temp[!grepl('G. Bernard Shaw',temp$Name),]
temp = temp[!grepl('Olympia Stephen',temp$Name),]
temp = temp[!grepl('Pam Bill Pam Kim',temp$Name),]
temp = temp[!grepl("Burlington Kara",temp$Name),]
temp = temp[!duplicated(paste(temp$Name,temp$Meeting)),]


library(lubridate)
temp$Date[temp$Date == 'january, 2009'] = 'january 8, 2009'

temp = temp[!temp$Meeting %in% c('2010-10-27_BRCC_Final_Notes.doc.pdf',
                                 "Baker River Coordinating Committee update for May and directions for June 8 meeting"),]

temp$Date = (gsub(',$','',gsub(' [0-9]{1,2}:[0-9]{2}$','',gsub(' am| pm','',temp$Date))))
temp$Date[temp$Date == 'sept. 5, 2013'] = 'september 5, 2013'
temp$Date = mdy(temp$Date)

## Things that aren't meetings
#https://pse.com/aboutpse/HydroLicensing/Documents/2010_Resource_Group_Documents/Baker%20River%20Coordinating%20Committee/2010-10-27_BRCC_Final_Notes.doc.pdf
#https://pse.com/aboutpse/HydroLicensing/Documents/2010_Resource_Group_Documents/Baker%20River%20Coordinating%20Committee/Baker%20River%20Coordinating%20Committee%20update%20for%20May%20and%20directions%20for%20June%208%20meeting.pdf

write.csv(temp,'Input/wa_longitudinal/scraped_data/temp_cleaned_data_batch2.csv')

