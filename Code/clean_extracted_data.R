library(tidyverse)
library(lubridate)
temp = read_csv('Input/scraped_data/meeting_attendance_extraction.csv') %>% 
  dplyr::select(-X1) %>% mutate(Meeting = gsub('\\.txt$','',Meeting))

library(tm)
library(pdftools)

library(stringr)
library(tidyverse)
#nohup R --no-save <win/project/bogachiel/baker/BakerPDFS/bakerpdfcode2.R >& win/project/bogachiel/baker/BakerPDFS/bakerpdfcode2.Rout &
library(tm)
library(SnowballC)
library(stringr)
library(RCurl)

file_list = list.files('Input/pdfs/text/')

temp = temp %>% filter(paste0(Meeting,'.txt') %in% file_list)


# for (meet in unique(temp$Meeting))
# {
#   meet_file = paste0(base,meet,suffix)
#   temp_file = readChar(meet_file, file.info(meet_file)$size)
#   in_meet_text = filter(temp,Meeting==meet)
#   #temp_file = gsub('.*PRESENT:','',temp_file)
#   #print(grepl('.*PRESENT:',temp_file))
#   #after_present = sapply(in_meet_text$Name,function(x) grepl(x,temp_file),simplify=T)
#   #temp = temp[((temp$Name %in% names(after_present)[after_present]) & meet == temp$Meeting)|meet!=temp$Meeting,]
# }

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

temp$Org[grep('U.S. Fish|US Fish and Wildlife|USFWS',temp$Org)] = 'USFWS'
temp$Org[grep('Natural Resources',temp$Org)] = 'WDNR'
temp$Org[grep('DNR',temp$Org)] = 'WDNR'
temp$Org[grep('Ecology|WA DOE',temp$Org)] = 'WDOE'
temp$Org[grep('DOE',temp$Org)] = 'WDOE'
temp$Org[intersect(grep('Arch',temp$Org),grep('WA',temp$Org))] = 'WAHP'
temp$Org[intersect(grep('Arch',temp$Org),grep('Hist',temp$Org))] = 'WAHP'
temp$Org[grep('DAHP',temp$Org)] = 'WAHP'
temp$Org[intersect(intersect(grep('WA|Wash',temp$Org),grep('Fish|fish',temp$Org)),grep('Wild|wild',temp$Org))] = 'WDFW'
temp$Org[intersect(intersect(grep('US|U.S.',temp$Org),grep('Fish|fish',temp$Org)),grep('Wild|wilde',temp$Org))] = 'WDFW'
temp$Org[grep('WDFW',temp$Org)] = 'WDFW'
temp$Org[temp$Org == 'FS'] = 'USFS'
temp$Org[grep('Berger Group',temp$Org)] = 'Louis Berger Group'
temp$Org[grep('USIT',temp$Org)] = 'Upper Skagit Indian Tribe'
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






temp = temp[grep('rep\\.',temp$Name,invert=T),]
temp = temp[nchar(temp$Name)<24,]
temp = temp[grep(' re',temp$Name,invert=T),]
temp = temp[grep('^[a-z]',temp$Name,invert=T),]
temp = temp[grep(' [a-z]',temp$Name,invert=T),]
temp = temp[grep('[0-9]',temp$Name,invert=T),]


temp$Org[temp$Name == 'Leland Stilson/DNR'] = 'WDNR'
temp$Org[temp$Name == 'Haley Edwards'] = 'PSE'
temp$Name[temp$Name == 'Arnold Aspelund'] = 'Arnie Aspelund'
temp$Name[grepl('Shanahan',temp$Name)] = 'Jon-Paul Shanahan'
temp$Name[temp$Name == 'Leland Stilson/DNR'] = 'Lee Stilson'
temp$Org[temp$Name == 'Bill Ryan/EPA Hydro'] = 'EPA Hydro'
temp$Name[temp$Name == 'Bill Ryan/EPA Hydro'] = 'Bill Ryan'
temp$Name[temp$Name == "Arnie Aspelund Arnie" ] = "Arnie Aspelund" 


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


temp$Name[temp$Name=='Bob Wright-Ecology'] = 'Bob Wright'
temp$Name[grep('Jerry Stedinger',temp$Name)] = 'Jerry Stedinger'
temp$Name[grep("David R. Montgomery",temp$Name)] = "David R. Montgomery"
temp$Name[grep("Omroa Bhagwandin",temp$Name)] = "Omrau Bhagwandin" 
temp$Name[temp$Name=='Kathleen W. Smayda'] = 'Kathy Smayda'
temp$Name[grep("Bill Hebner",temp$Name)] = "Bill Hebner" 
temp$Name[grep("Marylouise Keefe",temp$Name)] = "Mary Lou Keefe" 


temp$Name[grep("Chuck Ebel",temp$Name)] = "Chuck Ebel" 
temp$Name[grep("Warren.*Coughlin",temp$Name)]  = 'Warren Coughlin'
temp$Name[grep("Ed Meyer Wayne Porter",temp$Name)] = "Wayne Porter" 

temp$Name[grep("Charles Howard",temp$Name)] = "Charles Howard"
temp$Name[grep("Scott Heller",temp$Name)] = "Scott Heller"
temp$Name[grep("Roger Nicols",temp$Name)] = "Roger Nicholls"
temp$Name[grep("Charlie O'Hara",temp$Name)] = "Charles O'Hara"
temp$Name[temp$Name=='Jamie DeVanter'|temp$Name=='Jamie Van De Vanter'] = 'Jamie Van DeVanter'
temp$Name[temp$Name=='H. Beecher'] = 'Hal Beecher'
temp$Name[temp$Name=="Mary Jean What"] = "Mary Jean Wiltse"

temp$Name[grepl('Mary Lou|Marylouise Keefe',temp$Name)] = 'Mary Lou Keefe'
temp$Name[grepl('Killgore',temp$Name)] = 'Mark Killgore'
temp$Name[grepl('Potash|Laura Martin',temp$Name)] = 'Laura Potash Martin'
temp$Name[grepl('Jamie',temp$Name)&grepl(' Van',temp$Name)] = 'Jamie Van De Vanter'

temp$Name[temp$Name=="Jon Vanderheyden"] = "Jon VanderHayden"
temp$Name[temp$Name=="Burton Reanier"] = "Burt Reanier"

temp$Name[grep("Mierendorf",temp$Name)] = 'Bob Mierendorf'

temp = temp[!grepl("Status|Discussion|Phone|Potential|Draft|Dolly Varden",temp$Name),]
temp = temp %>% filter(!Name %in% c('W.T. Sexton',"E. Rydin",'P. Hyenstrand','R.C. Szaro','A.J. Malk','N.C. Johnson'))

temp[temp$Meeting=='2009rrg20090224',]

temp = temp[grep('\\.$',temp$Name,invert=T),]
temp$Name = gsub('Officer |Colonel ','',temp$Name)
temp$Name = gsub('Capt\\. |Captain |Sgt\\. ','',temp$Name)


#temp = temp[grep('Wiltse ',temp$Name,invert=T),]
temp = temp %>% filter(!Name %in% c("Reed Canarygrass" ,'Haley Dave' ,"Roy Hamilton","Burpee Hill" ,"Warner Wayne" , "Tom Facilitation", "Montgomery Watson/Harza", "Tony Forward Kathy" ,"Rick Call Daryl Hamburg","Smayda Envrionmental","Tony Contact Susan Hada",
                             "Tony Tony Fuchs","Tony Tony Tony Tony" ,"Baker Sockeye" ,"Tony Work"   ,"Mike Tony/Ray"     ,"Tony Research","Tony Forward Kathy",
                             "Salmonid Ruth", "La Connor","Fidalgo Flyfishers","Ryan Booth Kelly Bush","Tony Tony Fuchs","Sedro Woolley" ,
                             "Shannon Cr." ,"Samish Jessie" ,"Rob W. What","Peregrine Falcon" ,"Point Elliott","President Bush","Baker R." ,
                             "Chum Salmon" ,"Well Dones", "M. Vaughn" ,"Representative Larson","Preschedule Baker"  ,"Meeting Dee"  ,"Marty Draft"  ,"Andy Aesthetics" ,
                             "Andy Need"     ,"Elizabeth Elizabeth"  ,"Steve All" , "Chris Chris"  , "Kim Lane-Comments", "Jay Web-Ex", "Chris Use",   "Andy—Tell Berger"  , "Burger King"," Cary"   ,"Deputy Nelson"    ,"G. Reopeners" ,
                             "Equinox Campbell" ,"Elk Herd"  ,"D. Drake" ,"Grizzly Bear"  , "Jerry Anacortes","J. A." ,"Kathleen Update",
                             "Don Connect" ,"Design Eldridge"  ,"Teamlets Dee"  , "Vernon Hydrographs","Baker Relicense" ,"O'connell",
                             "Baker Nale.","Moore/Kate Chaney","General Stockton", "Marty Tony", "Herb Robert",  "Susan What","Alison Studley",'Dave Work','Kuzler','Scott Status',
                             "Shannon Lakes", "Pam Connect"  ,"Heather Meadows"  ,"Mother Hen","Mark Dailyhere","Blue Tarp","Census Scott" ,
                             "Dave Talk",'Dave Vet','Dave Dave','June Mtgs',"Emily Chris","Arnie Have Jacob","Scott Jan","K. Howard",
                             "Greg- Deputy","Greg Lind Craig Gannett","Miss Saul",'Chris Elk',"Tony Fuchs Pam Garland","M.L. Rosenau",
                             "Montgomery Watson","Montgomery Watson Harza","Elk Teamlet","Jeff McGowan Lyn Wiltse","Colonel Graves",
                             "Congressman Larson","Charles D. D. Howard","J. Largeplantsare", "Ann Ann Carol Carol Pam","J. Shaw",
                             "Paul Assessment"  ,"Laura Laurel","Jim Passage","J. Shaw" ,"Haley Don","Doug Marty Jacob"   ,"Dave Coordinate",
                             'Vernon Joint',"Blue Tarp","Bob W.—Regarding","Robin Wendy" ,'Bob Helton Nick','Ira Marty',
                             "Bob W—Draft","Bob N. Actions","Bob Need","Kathleen Test","Carl Report"     ,    "Elizabeth Jan"  ,    "Chris What"  ,
                             "Captain Hebner" ,"Charles Howard Model","Janis Bouma Ardis Bynum" ,"Nick Develop",
                             "Patrick Dylan Parks","Kim Work",'Lorna Check','Nick Coordinate',"Ira Ira"   ,"Haley Draft" ,"Heidi Get",
                             'Tony Coordinate',"Elizabeth Investigate","Vogler Brett","Bill Bill Rich",
                             "Louis Berger Andy","Louis Berger/Meridian","Tony Get" , "Tony Queue",
                             "Desmond Dr. S.E. Lacy","Don Gay/Fred Seavey","Tony Reserve" , "Dave Cary Jacob",
                             "Baker Ranger" ,"Brian What",'T. Brock'  ,"Krispy Kreme","Salmonid Fry","Ann Ann Carol Carol Pam",
                             "Tony Tony","History Hydrops" ,"Dave Pam Lyn" ,"Tony Kathy All Marty","Lyn Ira Ray" ,
                             "Stan Coordinate" ,"Oren Dame","D.B. Thompson","Connie Charlie Black","Bob Don",
                             "Tony Add"     ,"Dan See" , "Saint Valentine",'Chris Drechsel Lloyd'  ,"Cary Cary Feldmann"  ,  "Haley Connect",    "Tony Baker"        ,   
                             "Tony Chris"      ,        "Tony Discussion" ,"Ira Mail","Mike See",
                             "Tony Next"  , "Ray Tony", "Vernon Chris", "Tony Fuchs Pam Garland",  "Todd Wilbur Don Gay"  , "Tony Post"   ,'Ira Mail Stan'     ,  
                             "Tony Reed"  ,  "Tony Report"   ,"Sedro Wooley" ,"Bob K. Cary",
                             "Jim Neiland","Jim Kneeland",
                             "Tony Marty" ,  "Tony From" ,  "Ann Share","Tony Chris –", "Tony Status" ,"C.S. Sodhi" ,
                             "Tony Connect", "Tony Contact Susan Hada","Tony- I",'Tony Work','Tony Sent', 'Tony What','Tony Research',
                             'Tony Check','Tony Kathy',"Tony Coordinate Joetta","Virginia Mason","Elizabeth Jessie Piper",
                             "Hayes","Stan Cc", "Cary Feldman Ron","Carol Check" , "Jeff- Cake" ,"C.J. Perrin",
                             "Kathy Next","Kathy Kathy","Elizabeth Set" ,"Chris March" ,"Howard Hansen","Howard Hanson","Eric Markell",
                             "Lyn Wiltse Ron Campbell","Ron Mcbride","Louis Berger","Curtis Spalding"))

temp = temp[!grepl('Send$',temp$Name),]
temp = temp[!grepl('Share$',temp$Name),]
temp = temp[!grepl('Stay$',temp$Name),]
temp = temp[!grepl('Head$',temp$Name),]
temp = temp[!grepl('Clean$',temp$Name),]
temp = temp[!grepl('Rempel',temp$Name),]
temp = temp[!grepl('•',temp$Name),]
temp = temp[!grepl(' Work',temp$Name),]
temp = temp[!grepl('Talk',temp$Name),]
temp = temp[!grepl('Char ',temp$Name),]
temp = temp[!grepl(' Meet',temp$Name),]
temp = temp[!grepl(' Meet',temp$Name),]
temp = temp[!grepl('Review|Forward|Research|Distribute|Clarify|Send |La Conner|Cape Horn| Call ',temp$Name),]
temp = temp[!grepl('Cc',temp$Name),]
temp = temp[!grepl('Jeff- Cake',temp$Name),]
temp = temp[!grepl('Pam Pam',temp$Name),]
temp = temp[!grepl('Hayes',temp$Name),]
temp = temp[!grepl('Haley Re',temp$Name),]
temp = temp[!grepl('Check',temp$Name),]
temp = temp[!grepl('Oversee',temp$Name),]
temp = temp[!grepl('Schedule',temp$Name),]
temp = temp[!grepl('Carex',temp$Name),]
temp = temp[!grepl('Gifford Pinchot',temp$Name),]
temp = temp[!grepl('Teamlet',temp$Name),]
temp = temp[!grepl('Little Baker',temp$Name),]
temp = temp[!grepl('G. Bernard Shaw',temp$Name),]
temp = temp[!grepl('Olympia Stephen',temp$Name),]
temp = temp[!grepl('Vogler',temp$Name),]
temp = temp[!grepl('Pam Bill Pam Kim',temp$Name),]
temp = temp[!grepl("Burlington Kara",temp$Name),]
temp = temp[!grepl("Joe Leary",temp$Name),]
temp = temp[!grepl("Shannon Smolt",temp$Name),]
temp = temp[!grepl("Let Martha",temp$Name),]
temp = temp[!grepl("Sawyer Hall Lacey",temp$Name),]
temp = temp[!grepl("Phil Bob",temp$Name),]
temp = temp[!grepl("Elwa Mort" ,temp$Name),]

temp$Name[temp$Name =="John Shultz"] = 'John Schultz'
temp$Name[grep("Bill Heinck",temp$Name)] = "Bill Heinck"
temp$Name[grep("De Coteau",temp$Name)] = "Ernie DeCoteau"
temp$Name[grep("Dayna Matthews",temp$Name)] = "Dayna Matthews"
temp$Name[grep("Rob Whitlam",temp$Name)] = "Robert Whitlam"

temp$Name[grep("Chris Hansen",temp$Name)] = "Chris Hansen-Murray"
temp$Name[grep("Gordy Iverson",temp$Name)] = "Gordon Iverson"
temp$Name[grep("Brenda Werden",temp$Name)] = "Brenda Warden"
temp$Name[grep("Charlie O’Hara",temp$Name)] = "Charles O’Hara"
temp$Name[grep("VanderVeen",temp$Name)] = "Jackie Vanderveen"
temp$Name[grep("Paula Ogden",temp$Name)] = "Paula Ogden-Muse"
temp$Name[grep("Ronald Kent",temp$Name)] = "Ron Kent"
temp$Name[grep("Russell Holter",temp$Name)] = "Russ Holter"
temp$Name[grep("Rich Phillips|Richard Phillips",temp$Name)] = "Richard Phillips"

temp = temp[temp$Name!='A.',]

temp$Topic[grepl('Coordinating Committee',temp$Meeting)] = 'brcc'
temp$Topic[grepl('BRCC',temp$Meeting)] = 'brcc'
temp$Topic[grepl('CRAG',temp$Meeting)] = 'crag'
temp$Topic[grepl('ARG',temp$Meeting)&grepl('TRIG',temp$Meeting)] = 'arg_trig'
temp$Topic[grepl('ARG',temp$Meeting)] = 'arg'
temp$Topic[grepl('RRG',temp$Meeting)] = 'rrg'
temp$Topic[grepl('TRIG',temp$Meeting)] = 'trig'
temp$Topic[grepl('LEP',temp$Meeting)] = 'lep'
temp$Topic[grepl('botanical',temp$Meeting)] = 'trig_bot'
temp$Topic[grepl(' char',temp$Meeting)] = 'fish'
temp$Topic[grepl('Alder Creek|Burpee Hill',temp$Meeting)] = 'trig'
temp$Topic[grepl('_ Final Notes',temp$Meeting)] = 'rrg'


temp$Topic[temp$Topic %in% c('rrg','recreational','rrg_lep','lep')] = 'recreational'
temp$Topic[temp$Topic %in% c('trig','trig_elk','trig_bot','trig_loon','terrestrial','wildlife')] = 'terrestrial'
temp$Topic[temp$Topic %in% c('crag','cultural')] = 'cultural'
temp$Topic[temp$Topic %in% c('aquatic','aquatictech','arg','instream','fish','water')] = 'aquatic/fish'
temp$Topic[temp$Topic %in% c('economic','economics','floodcontrol')] = 'economic'
temp$Topic[temp$Topic %in% c('tst','solution')] = 'solution'
temp$Topic[temp$Topic %in% c('public','workshop')] = 'public/general'
temp$Topic[temp$Topic %in% c('bricc','solution','process','brcc')] = 'admin'

temp = temp[!grepl('^ ',temp$Name),]

library(pbapply)
temp$Name = str_to_title(temp$Name)
name_table = pblapply(temp$Name,function(x) table(agrep(x,temp$Name,max.distance =list(all=0.1,insertions=1,deletions=1,substitutions=1),value=T)))
modal_name = sapply(name_table,function(x) names(x)[x==max(x)][1],simplify=T)
temp$Name = unlist(modal_name)

temp$Org[temp$Org=='NCCC'] = 'North Cascades Conservation Council'
temp$Org[grepl('NOAA',temp$Org)] = 'NOAA'
temp$Org[grepl('DOT',temp$Org)] = 'WDOT'
temp$Org[grepl('HRA',temp$Org)] = 'HRA'
temp$Org[grepl('Biota',temp$Org)] = 'Biota Pacific'
temp$Org[grepl("D\\.C\\.|D\\.C|Portland OR",temp$Org)] = "FERC"
temp$Org[grepl("Louis Berger",temp$Org)] = "Louis Berger Group"
temp$Org[grepl('Sherif',temp$Org)&grepl('Whatcom',temp$Org)] = "Whatcom County Sheriff's Office"
temp$Org[temp$Org=='Univ'] = 'UW'
temp$Org[temp$Org=='Real'] = 'PSE'
temp$Org[temp$Name=='Donald S. Dixon'] = 'Skagit County Public Works'
temp$Org[temp$Name=='Tom Spicher'] = 'Hydro Y.E.S.'

temp$Org[temp$Name=='James Roberts'] = 'Sauk-Suiattle Indian Tribe'
temp$Org[grep('Corps|UCACE',temp$Org)] = 'USACE'


prob_not_org = unlist(lapply(temp$Org,function(x) any(grepl(x,temp$Name))))
temp$Org[prob_not_org] = '99999'
temp$Org[temp$Org=='Team'] = '99999'
temp$Org[temp$Org=='Phone'] = '99999'
temp$Org[temp$Org=='Director'] = '99999'
temp$Org[temp$Org=='Note'] = '99999'
temp$Org[temp$Org=='Project'] = '99999'
temp$Org[temp$Org=='PM for this project'] = '99999'
temp$Org[grepl('425|360|[0-9]{4}',temp$Org)] = '99999'

org_table_byname = pblapply(temp$Name,function(x) as.data.frame(table(temp$Org[temp$Name==x])) %>% arrange(-Freq) %>% mutate(Var1 = as.character(Var1)))
modal_org = lapply(org_table_byname,function(x) ifelse(x$Var1[1]!='99999',x$Var1[1],x$Var1[2]))

temp$Org = unlist(modal_org)
temp = temp %>% arrange(Meeting,Name,Relevance) %>% filter(!duplicated(paste0(Meeting,Name)))
write.csv(temp,'Input/scraped_data/temp_cleaned_data.csv')

library(tidyverse)
library(stringr)
library(statnet)
library(btergm)
rm(list=ls())

talkers = read_csv('Input/pdfs/extraction/verbs_categorized_multi_row.csv') %>%
  filter(!is.na(Verb)) %>%
  dplyr::select(-X1) %>%
  filter(!grepl('[A-Z]{4}',Subject),!grepl('^[a-z]',Subject)) %>%
  mutate(Subject = gsub('\\.$','',Subject)) %>%
  mutate(Meeting = gsub('\\.txt$','',Meeting)) %>%
  filter(!grepl('Upper Baker',Subject)) %>% mutate(Year = str_extract(Meeting,'[0-9]{4}'))

#verb_categories = c('all','communication')
#talkers = read_csv('Input/pdfs/extraction/named_entities.csv')

attendance = read_csv('Input/scraped_data/temp_cleaned_data.csv') %>% 
  dplyr::select(-X1) %>% mutate(Year = str_extract(Meeting,'[0-9]{4}'))
library(lubridate)

temp_dates = ymd(str_extract(attendance$Meeting,'[0-9]{8}$'))
temp_dates[is.na(temp_dates)] = 
  ymd(str_extract(attendance$Meeting[is.na(temp_dates)],'[0-9]{4}.[0-9]{1,2}.[0-9]{1,2}'))

attendance$Date = temp_dates
attendance$Dec_Date = decimal_date(attendance$Date)

talkers$Date = attendance$Date[match(talkers$Meeting,attendance$Meeting)]
talkers$Dec_Date = attendance$Dec_Date[match(talkers$Meeting,attendance$Meeting)]

phase_break_date = c(mdy('5/8/2003'),mdy('11/24/2004'),mdy('10/17/2008'))#,mdy('01/01/2015'))
phase_name = c('planning/scoping','application/settlement development',
               'agency review','license implementation')
phase_break_ddate  = decimal_date(phase_break_date)
period_break_ddates = seq(decimal_date(mdy('05/01/2000')),decimal_date(mdy('11/01/2014')),0.5)

meeting_master = attendance %>% filter(!duplicated(Meeting)) %>% 
  dplyr::select(-Name,-Org,-Relevance,-Topic,-Count)
meeting_master$Interval = findInterval(meeting_master$Dec_Date,period_break_ddates)
meeting_master = meeting_master %>% 
  mutate(Phase = ifelse(Interval<=7,1,ifelse(Interval<=10,2,ifelse(Interval<=18,3,4))))




# 
# 
# 
# meeting_master$Phase = ifelse(meeting_master$Dec_Date < phase_break_ddate[1],1,
#                               ifelse((meeting_master$Dec_Date >= phase_break_ddate[1] &
#                                         meeting_master$Dec_Date < phase_break_ddate[2]), 2,
#                                      ifelse((meeting_master$Dec_Date >= phase_break_ddate[2] &
#                                                meeting_master$Dec_Date < phase_break_ddate[3]),3,4)))
# 


# library(gridExtra)
# g1 = ggplot(meeting_master,aes(x=Dec_Date)) + geom_histogram(breaks=period_breaks$pbreaks) + 
#   scale_x_continuous(name = '6 month intervals (May to November')+ scale_y_continuous(name = '# observed meetings')
# g2 = ggplot(meeting_master,aes(x=as.factor(phase))) + geom_bar() + scale_y_continuous(name = '# observed meetings') +
#   scale_x_discrete(name = 'Phase',labels=as.character(phase_breaks$phase_name))
# grid.arrange(g1,g2)

attendance = left_join(attendance,meeting_master)
talkers = left_join(talkers,meeting_master)





match_as_present = lapply(1:length(talkers$Subject),
                          function(x) ifelse(length(grep(talkers$Subject[x],attendance$Name[attendance$Meeting==talkers$Meeting[x]]))==0,NA,
                                             grep(talkers$Subject[x],attendance$Name[attendance$Meeting==talkers$Meeting[x]],value=T)))
talkers = talkers %>% mutate(Subject_Match = unlist(match_as_present)) %>%
  filter(!is.na(Subject_Match))

talkers_summary = talkers %>%
  filter(!is.na(Subject_Match)) %>% 
  filter(!duplicated(paste(Subject_Match,Meeting,Verb))) %>%
  group_by(Subject_Match,Meeting,Year,Date,Dec_Date,Interval,Phase) %>% 
  summarise(part_count = n())


#save summary of talking
write.csv(talkers,'Input/scraped_data/participation_detail.csv')
write.csv(talkers_summary,'Input/scraped_data/participation_summary.csv')
write.csv(attendance,'Input/scraped_data/attendance_summary.csv')
write.csv(meeting_master,'Input/scraped_data/meeting_master.csv')

# for (uq in unique(temp$Name))
# {
#   tt = unique(agrep(uq,temp$Name,ignore.case = T,max.distance =list(insertions=7,deletions=0,substitutions=0),value=T))
#   if(length(tt)>1){print(tt)}
# }

# 
# for (name in unique(temp$Name))
# {
# t = agrep(name,unique(temp$Name),max.distance =list(all=0.2,insertions=1,deletions=1,substitutions=1),value=T)
# if (length(t)>1){print(t)}
# }
# 

