## LOAD LIBRARIES ##
library(streamR)

# Extracts tweets from Twitter Stream and saves them in JSON format.
# Words list taken from http://usefulenglish.ru/vocabulary/jobs-professions-occupations
load("my_oauth_twitter.Rdata")
filterStream(file.name = "job_offers_EN.json", # Save tweets in a json file
             track = c("job","jobs","job offer","jobs offer","vacant position","vacant positions",
                       "hiring","hire","work",
                       "director","deputy director","managing director","financial director","marketing director","general manager"
                       ,"assistant manager","manager","personnel manager","production manager","marketing manager","sales manager"
                       ,"project manager","supervisor","inspector","Office","office clerk","receptionist","secretary","typist"
                       ,"stenographer","banking","banker","bank officer","accountant","bookkeeper","economist","teller","cashier"
                       ,"auditor","doctor","physician","family doctor","general practitioner","eye specialist","ear specialist"
                       ,"throat specialist","heart specialist","cardiologist","surgeon","pediatrician","psychiatrist","dentist"
                       ,"dietician","pharmacist","veterinarian","nurse","paramedic","chef","head cook","cook","maitreheadwaiter"
                       ,"waiter","waitress","bartender","barman","sales representative","sales manager","salesperson","salesman"
                       ,"saleswoman","salesgirl","salesclerk","cashier","seller","buyer","wholesale buyer","wholesaler","retailer"
                       ,"distributor","advertising agent","journalist","reporter","correspondent","photographer","designer"
                       ,"fashion designer","dress designer","interior designer","furniture designer","graphic designer"
                       ,"principal","dean","professor","teacher","student","pupil","schoolteacher","college teacher"
                       ,"university teacher","head teacher","senior teacher","English teacher","history teacher"
                       ,"maths teachermath teachermusic teacher","engineer","technician","mechanic","builder"
                       ,"construction worker","repairer","welder","bricklayer","mason","carpenter","plumber"
                       ,"scientist","scholar","researcher","explorer","mathematician","physicist"
                       ,"chemist","biologist","astronomer","historian","archeologist","traffic officer","detective"
                       ,"guard","bodyguard","expert","specialist","analyst","consultant","adviser","computer programmer"
                       ,"computer operator","systems analyst","software specialist","web developer","web programmer","webmaster"
                       ,"web designer","pilot","flight engineer","flight navigator","flight attendant","stewardess","driver","taxi driver"
                       ,"bus driver","truck driver","car mechanic","firefighter","librarian","farmer","tailor","models","politic sciences"
                       ,"travel agent","hairdresser","hairstylist","barber","beautician","cosmetologist","cleaning lady"
                       ,"cleaning woman","janitor","full-timepart-time","permanent job","temporary job","temporary worker"
                       ,"freelance worker","freelancer","season worker","caretaker"
             ), 
             language = c("en")
             ,timeout = 60*60*24, #(4*60*60), # Keep connection alive for 60 seconds
             oauth = my_oauth) # Use my_oauth file as the OAuth credentials
