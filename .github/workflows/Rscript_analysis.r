
library(httr)
library(gargle)
library(googleAuthR)
library(jsonlite)
library(knitr)
library(dplyr)
library(huxtable)
library(httr2)
library(xts)
library(calendar)
library(DescTools)
library(splitstackshape)
library(pdftools)
library(rvest)
library(lubridate)
library(stringr)
library(rlang)
library(purrr)
library(httpuv)
library(curl)

google_client_id <- Sys.getenv("GOOGLE_CLIENT_ID")
google_client_secret <- Sys.getenv("GOOGLE_CLIENT_SECRET")

#Eurostat Calendar Below
url <- "https://ec.europa.eu/eurostat/cache/RELEASE_CALENDAR/calendar_EN.ics"
response <- GET(url)

EUROSTAT_cal<-calendar::ic_read(textConnection(rawToChar(response$content))) %>% data.frame()

#EUROSTAT_cal <- ic_read("C:\Users\flanagam\Documents\calendar_EN.ics") %>% data.frame()

EUROSTAT_cal$DTSTART.TZID.Europe.Luxembourg<-EUROSTAT_cal$DTSTART.TZID.Europe.Luxembourg %>% str_sub(1,8) %>% strptime("%Y%m%d", tz = "GMT")
EUROSTAT_cal$DTSTART<-as.POSIXct(paste0("",EUROSTAT_cal$DTSTART.TZID.Europe.Luxembourg," 10:00:00"))

EUROSTAT_cal$DTEND.TZID.Europe.Luxembourg<-EUROSTAT_cal$DTEND.TZID.Europe.Luxembourg %>% str_sub(1,8) %>% strptime("%Y%m%d", tz = "GMT")
EUROSTAT_cal$DTEND<-as.POSIXct(paste0("",EUROSTAT_cal$DTEND.TZID.Europe.Luxembourg," 10:00:00"))


EUROSTAT_cal = subset(EUROSTAT_cal, (SUMMARY %like any% c("%Inflation%","%inflation%","%Flash%","%GDP%","%Unemployment%")))
EUROSTAT_cal$LOCATION<-c("Eurostat")
EUROSTAT_cal$transparent<-c("TRUE")

EUROSTAT_cal_short<-data.frame(EUROSTAT_cal$DTSTART,EUROSTAT_cal$DTEND,EUROSTAT_cal$SUMMARY,EUROSTAT_cal$LOCATION,EUROSTAT_cal$UID,EUROSTAT_cal$transparent)
colnames(EUROSTAT_cal_short)<-c("DTSTART","DTEND","SUMMARY","LOCATION","UID","transparent")
EUROSTAT_calendar<-ical(EUROSTAT_cal_short)



PMI_URL_download<-pdf_text("PMI_2024.pdf") %>% .[[1]] %>% stringr::str_split("\n")  %>%  unlist() %>% matrix()  
PMI_URL_download_PG2<-pdf_text("PMI_2024.pdf") %>% .[[2]] %>% stringr::str_split("\n")  %>%  unlist() %>% matrix()  

PMI_URL_download_PG1<-PMI_URL_download[3:nrow(PMI_URL_download)] %>% data.frame() 
PMI_URL_download_PG2<-PMI_URL_download_PG2[3:nrow(PMI_URL_download)] %>% data.frame() 



PMI_URL_download_ALL<-rbind(PMI_URL_download_PG1,PMI_URL_download_PG2)






Date_PMI<-substr(PMI_URL_download_ALL[[1]],1,23) %>% as.Date(format = "%d %b") %>% as.data.frame()
Name_PMI<-substr(PMI_URL_download_ALL[[1]],100,130) %>% as.data.frame()

PMI_DF<-data.frame(Date_PMI,Name_PMI) %>% na.omit() 
colnames(PMI_DF)<-c("Date", "Release")
PMI_XTS<-as.xts(PMI_DF,PMI_DF$Date)
PMI_XTS$Location<-"S&P Global"

RELEASE_PMI<-data.frame(matrix(NA,nrow=nrow(PMI_XTS),ncol=3))

for(i in 1:nrow(PMI_XTS)){
  if(PMI_XTS$Release[i] %like any% c("%Ireland Manufacturing PMI*%","%Ireland Manufacturing PMI%","%Ireland Services PMI*%","%Ireland Services PMI%","%Ireland Construction PMI%")){
    RELEASE_PMI[i,]<-PMI_XTS[i,]
  } else {
    RELEASE_PMI[i,] <- NA
  }
}


RELEASE_PMI<-na.omit(RELEASE_PMI)
colnames(RELEASE_PMI)<-c("Date","Release","Location")


RELEASE_PMI_DATE<-as.POSIXct(strptime(paste0("",as.POSIXct.Date(as.Date(RELEASE_PMI$Date,"%Y-%m-%d")), " 10:00:00"),format= "%Y-%m-%d %H:%M:%S"),tz = c("GMT"))

PMI_RELEASE = data.frame(DTSTART = RELEASE_PMI_DATE,
                         DTEND = RELEASE_PMI_DATE+1,
                        SUMMARY =RELEASE_PMI$Release ,
                        LOCATION = RELEASE_PMI$Location,
                         transparent=TRUE)



PMI_RELEASE<-PMI_RELEASE %>%
  mutate(UID = replicate(nrow(PMI_RELEASE), ic_guid()))





#Central Bank Calendar Below

CB_URL<-"https://www.centralbank.ie/news-media/schedule"
CB_URL_download<-read_html(CB_URL)
CB_Schedule_table<-CB_URL_download %>% html_nodes("article")
CB_Schedule_table3<-(CB_Schedule_table[[1]])   %>% html_text() %>% stringr::str_split("\n") %>% .[[1]] %>% data.frame()

Date_CB<-data.frame(matrix(NA,nrow=nrow(CB_Schedule_table3),ncol=1))
RELEASE_CB<-data.frame(matrix(NA,nrow=nrow(CB_Schedule_table3),ncol=2))


for(i in 1:nrow(CB_Schedule_table3)){
  
  Date_CB[i,1]<-paste(format(parse_date_time2(paste0(CB_Schedule_table3[i,]," 2024"),orders="%d-%m-%Y"),"%d/%m/%Y"))
  
}

CB_Schedule_table3<-cbind(CB_Schedule_table3,Date_CB)
colnames(CB_Schedule_table3)<-c("Releases","Date")

for (i in 1:nrow(CB_Schedule_table3)){
  
  if(CB_Schedule_table3$Date[i]!="NA"){
    CB_Schedule_table3$Date[i]<-CB_Schedule_table3$Date[i]
  } else if (!is_empty(CB_Schedule_table3$Date[i-1])) {
    CB_Schedule_table3$Date[i]<-CB_Schedule_table3$Date[i-1]
  } else {
    CB_Schedule_table3$Date[i]<-NA
  }
  
  if(CB_Schedule_table3$Releases[i] %like any% c("Activity: Statistics: %","%Publication: Quarterly Bulletin%")){
    RELEASE_CB[i,1]<-CB_Schedule_table3$Date[i]
    RELEASE_CB[i,2]<-CB_Schedule_table3$Releases[i]
    
  } else {
    RELEASE_CB[i,1]<-NA
    RELEASE_CB[i,2]<-NA
  }
  
  if(RELEASE_CB[i,2] %like any% c("%Monthly Card Payment Statistics%" ,"%Activity: Statistics: Private Household Credit and Deposits Statistics%","%Publication: Quarterly Bulletin%","%Interest Rates%","%Private Household Credit and Deposits%","%Mortgage Arrears%","%Credit and Debit Card Statistics%","%Private Household Credit and Deposits Statistics%")){
    RELEASE_CB[i,1]<-RELEASE_CB[i,1]
    RELEASE_CB[i,2]<-RELEASE_CB[i,2]
  } else {
    RELEASE_CB[i,1]<-NA
    RELEASE_CB[i,2]<-NA
  }
}

RELEASE_CB<-na.omit(RELEASE_CB)
colnames(RELEASE_CB)<-(c("Date","Release"))


Date_CB<-as.POSIXct(strptime(paste0("",as.POSIXct.Date(as.Date(RELEASE_CB$Date,"%d/%m/%Y")), " 10:00:00"),format= "%Y-%m-%d %H:%M:%S"),tz = c("GMT"))
RELEASE_CB$Release<-str_replace_all(RELEASE_CB$Release, "Activity: Statistics:", " ")

if(is.na(Date_CB[1])){
  event_CB = data.frame(DTSTART = c("2024-12-01 10:00:00 GMT"),
                        DTEND = c("2024-12-01 10:00:00 GMT"),
                        SUMMARY = "NA",
                        LOCATION = c("Central Bank of Ireland"),
                        transparent=TRUE)

  
} else {
  
  event_CB = data.frame(DTSTART = Date_CB,
                        DTEND = Date_CB+1,
                        SUMMARY = paste(RELEASE_CB$Release),
                        LOCATION = c("Central Bank of Ireland"),
                        transparent=TRUE)
  
  
  
}

event_CB <- event_CB %>%
  mutate(UID = replicate(nrow(event_CB), ic_guid()))


#OECD Calendar Below


#OECD Calendar Below
#OECD_RELEASE<-"https://www.oecd.org/newsroom"
#tmp <- tempfile()
#OECD_RELEASE_v1<-curl::curl_download(OECD_RELEASE, #tmp)


#OECD_URL_download<-read_html(OECD_RELEASE_v1)
#OECD_Schedule_table<-OECD_URL_download %>% #html_nodes(xpath='/html/body/div[2]/div[2]/div[2]/#div[3]/div[1]')   %>% html_text()
#OECD_Schedule_table_2<-OECD_Schedule_table[[1]] #%>% stringr::str_split("\n") %>% data.frame()


#RELEASE_OECD<-data.frame(matrix(NA,nrow=nrow(OECD_#Schedule_table_2),ncol=2))


#for(i in 1:nrow(OECD_Schedule_table_2)){
#if(OECD_Schedule_table_2[i,1]==""){
#  OECD_Schedule_table_2[i,1]<-NA 
#} else {
  #OECD_Schedule_table_2[i,1]<-OECD_Schedule_table_2[#i,1]
#}
#}


#for(i in 1:nrow(OECD_Schedule_table_2)){
#  if(OECD_Schedule_table_2[i,1] %like any% c("OECD #Interim Economic Outlook")){
#    RELEASE_OECD[i,1]<-OECD_Schedule_table_2[i,1]
#    #RELEASE_OECD[i,2]<-OECD_Schedule_table_2[i-1,1]
#      } 
#}

#RELEASE_OECD<-na.omit(RELEASE_OECD)
#colnames(RELEASE_OECD)<-c("Release", "Date")
#RELEASE_OECD$Date<-parse_date_time2(paste0(RELEASE#_OECD$Date),orders="%d %B %Y")
#
#
#if(is.na(RELEASE_OECD)){
#  EVENTS_OECD = data.frame(DTSTART = c("2024-12-01 10:00:00 GMT"),
#                        DTEND = c("2024-12-01 #10:00:00 GMT"),
#                        SUMMARY = "NA",
 #                       LOCATION = c("OECD"),
#                        transparent=TRUE)
  
  
  
  
#} else {
  
#  EVENTS_OECD = data.frame(DTSTART = #RELEASE_OECD$Date,
#                        DTEND = #RELEASE_OECD$Date+1,
#                        SUMMARY = #paste(RELEASE_OECD$Release),
#                        LOCATION = c("OECD"),
#                        transparent=TRUE)
#  
  
#  }

cat("OECD EVENT CREATED")

EVENTS_OECD = data.frame(DTSTART = c("2024-02-05 10:00:00 GMT"),
                     DTEND = c("2024-02-05 10:00:01 GMT"),
                        SUMMARY = c("OECD Economic Outlook, Interim Report"),
                       LOCATION = c("OECD"),
                        transparent=TRUE)

EVENTS_OECD <- EVENTS_OECD %>%
 mutate(UID = replicate(nrow(EVENTS_OECD), ic_guid()))




########


#IMF Calendar Below

cat("IMF begins")

IMF_URL_download<-read_html(
  c("https://www.imf.org/en/News/Seminars")
)


IMF_Schedule_table<-IMF_URL_download %>% html_nodes(xpath='/html/body/div[3]/main/article/div/div[2]/div[5]')   %>% html_text()
IMF_Schedule_table_2<-IMF_Schedule_table[[1]] %>% stringr::str_split("[\r\n]")   %>% data.frame()


RELEASE_IMF<-data.frame(matrix(NA,nrow=nrow(IMF_Schedule_table_2),ncol=2))


cat("IMF begins FIRST FOR LOOP BLANKS")

for(i in 1:nrow(IMF_Schedule_table_2)){
  if(IMF_Schedule_table_2[i,1]==""){
    IMF_Schedule_table_2[i,1]<-NA 
  } else {
    IMF_Schedule_table_2[i,1]<-IMF_Schedule_table_2[i,1]
  }
}


for(i in 1:nrow(IMF_Schedule_table_2)){
  if(IMF_Schedule_table_2[i,1] %like any% c("%World Economic Outlook Update%")){
    RELEASE_IMF[i,1]<-IMF_Schedule_table_2[i,1]
    RELEASE_IMF[i,2]<-IMF_Schedule_table_2[i-2,1]
  } 
}

colnames(RELEASE_IMF)<-c("Release", "Date")

cat("IMF BLANK event created")


if(is.na(RELEASE_IMF$Release[1])){
  EVENTS_IMF = data.frame(DTSTART = c("2024-12-01 10:00:00 GMT"),
                          DTEND = c("2024-12-01 10:00:00 GMT"),
                          SUMMARY = c("NA"),
                          LOCATION = c("IMF"),
                          transparent=TRUE)
  
  
  
  
} else {
  RELEASE_IMF$Date<-parse_date_time2(RELEASE_IMF$Date, orders="%B %d, %Y") 
  
  EVENTS_IMF = data.frame(DTSTART = RELEASE_IMF$Date,
                          DTEND = RELEASE_IMF$Date+1,
                          SUMMARY = paste(trimws(RELEASE_IMF$Release,"l")),
                          LOCATION = c("IMF"),
                          transparent=TRUE)
  
}


EVENTS_IMF <- EVENTS_IMF %>%
  mutate(UID = replicate(nrow(EVENTS_IMF), ic_guid()))


########
cat("EC EVENT CREATED")

EVENTS_EC = data.frame(DTSTART = c("2024-02-15 10:00:00 GMT"),
                     DTEND = c("2024-02-15 10:00:01 GMT"),
                        SUMMARY = c("EC Economic Forecast, Winter 2024"),
                       LOCATION = c("European Commission"),
                        transparent=TRUE)

EVENTS_EC <- EVENTS_EC %>%
 mutate(UID = replicate(nrow(EVENTS_EC), ic_guid()))





#######################







CENTRAL_BANK_SCHEDULE<-read_html("https://www.dailyfx.com/central-bank-calendar")
CENTRAL_BANK_SCHEDULE_table<-CENTRAL_BANK_SCHEDULE %>% html_nodes("aside")
FED_CENTRAL_BANK_SCHEDULE_table2<-(CENTRAL_BANK_SCHEDULE_table[[1]])   %>% html_text() %>% stringr::str_split("\n") %>% .[[1]]   %>% as.data.frame()
ECB_CENTRAL_BANK_SCHEDULE_table2<-(CENTRAL_BANK_SCHEDULE_table[[2]])   %>% html_text() %>% stringr::str_split("\n") %>% .[[1]] %>% data.frame()
BOE_CENTRAL_BANK_SCHEDULE_table2<-(CENTRAL_BANK_SCHEDULE_table[[3]])   %>% html_text() %>% stringr::str_split("\n") %>% .[[1]] %>% data.frame()

FED_CENTRAL_BANK_SCHEDULE_table3<-data.frame(matrix(NA,nrow=nrow(FED_CENTRAL_BANK_SCHEDULE_table2),ncol=1))
ECB_CENTRAL_BANK_SCHEDULE_table3<-data.frame(matrix(NA,nrow=nrow(ECB_CENTRAL_BANK_SCHEDULE_table2),ncol=1))
BOE_CENTRAL_BANK_SCHEDULE_table3<-data.frame(matrix(NA,nrow=nrow(BOE_CENTRAL_BANK_SCHEDULE_table2),ncol=1))



for(i in 1:length(FED_CENTRAL_BANK_SCHEDULE_table2[[1]])){
  if(FED_CENTRAL_BANK_SCHEDULE_table2[[1]][i]!=""){
    FED_CENTRAL_BANK_SCHEDULE_table3[i,1]<-paste(FED_CENTRAL_BANK_SCHEDULE_table2[[1]][i])
  }
  
  if(ECB_CENTRAL_BANK_SCHEDULE_table2[[1]][i]!=""){
    ECB_CENTRAL_BANK_SCHEDULE_table3[i,1]<-paste(ECB_CENTRAL_BANK_SCHEDULE_table2[[1]][i])
  }
  
  if(BOE_CENTRAL_BANK_SCHEDULE_table2[[1]][i]!=""){
    BOE_CENTRAL_BANK_SCHEDULE_table3[i,1]<-paste(BOE_CENTRAL_BANK_SCHEDULE_table2[[1]][i])
  }
}

Central_Banks_Schedules<-data.frame(na.omit(FED_CENTRAL_BANK_SCHEDULE_table3),na.omit(ECB_CENTRAL_BANK_SCHEDULE_table3),na.omit(BOE_CENTRAL_BANK_SCHEDULE_table3))
colnames(Central_Banks_Schedules)<-c("FED","ECB","Bank of England")

for (i in 1:nrow(Central_Banks_Schedules)+1){
  
  Central_Banks_Schedules[i,1]<-paste(parse_date_time(paste0(Central_Banks_Schedules[i,1] ," ",Central_Banks_Schedules[i+1,1]," 2024"),orders="%d %m %Y"))
  Central_Banks_Schedules[i,2]<-paste(parse_date_time(paste0(Central_Banks_Schedules[i,2] ," ",Central_Banks_Schedules[i+1,2]," 2024"),orders="%d %m %Y"))
  Central_Banks_Schedules[i,3]<-paste(parse_date_time(paste0(Central_Banks_Schedules[i,3] ," ",Central_Banks_Schedules[i+1,3]," 2024"),orders="%d %m %Y"))
  
}

Central_Banks_Schedules$FED<- as.Date(Central_Banks_Schedules$FED,"%Y-%m-%d")
Central_Banks_Schedules$ECB<- as.Date(Central_Banks_Schedules$ECB,"%Y-%m-%d")
Central_Banks_Schedules$`Bank of England`<- as.Date(Central_Banks_Schedules$`Bank of England`,"%Y-%m-%d")

Central_Banks_Schedules<-data.frame(na.omit(Central_Banks_Schedules))
colnames(Central_Banks_Schedules)<-c("FED Monetary Decision (FOMC)","ECB Monetary Decision","BOE Monetary Decision")

FED_DECISIONS<-as.POSIXct(strptime(paste0("",as.POSIXct.Date(as.Date(Central_Banks_Schedules$`FED Monetary Decision (FOMC)`,"%d/%m/%Y")), " 10:00:00"),format= "%Y-%m-%d %H:%M:%S"),tz = c("GMT"))
ECB_DECISIONS<-as.POSIXct(strptime(paste0("",as.POSIXct.Date(as.Date(Central_Banks_Schedules$`ECB Monetary Decision`,"%d/%m/%Y")), " 10:00:00"),format= "%Y-%m-%d %H:%M:%S"),tz = c("GMT"))
BOE_DECISIONS<-as.POSIXct(strptime(paste0("",as.POSIXct.Date(as.Date(Central_Banks_Schedules$`BOE Monetary Decision`,"%d/%m/%Y")), " 10:00:00"),format= "%Y-%m-%d %H:%M:%S"),tz = c("GMT"))


FED_Monetary_Decision = data.frame(DTSTARTv = FED_DECISIONS,
                                   DTEND = FED_DECISIONS+1,
                                   SUMMARY = c("FED Monetary Policy Decision (FOMC)"),
                                   LOCATION = c("US Federal Reserve"),
                                   transparent=TRUE)


FED_Monetary_Decision <- FED_Monetary_Decision %>%
  mutate(UID = replicate(nrow(FED_Monetary_Decision), ic_guid()))



ECB_Monetary_Decision = data.frame(DTSTARTv = ECB_DECISIONS,
                                   DTEND = ECB_DECISIONS+1,
                                   SUMMARY = c("ECB Monetary Policy Decision"),
                                   LOCATION = c("European Central Bank"),
                                   transparent=TRUE)


ECB_Monetary_Decision <- ECB_Monetary_Decision %>%
  mutate(UID = replicate(nrow(ECB_Monetary_Decision), ic_guid()))





BOE_Monetary_Decision = data.frame(DTSTARTv = BOE_DECISIONS,
                                   DTEND = BOE_DECISIONS+1,
                                   SUMMARY = c("Bank of England Monetary Policy Decision"),
                                   LOCATION = c("Bank of England "),
                                   transparent=TRUE)


BOE_Monetary_Decision <- BOE_Monetary_Decision %>%
  mutate(UID = replicate(nrow(BOE_Monetary_Decision), ic_guid()))



Monetary_Policy_Decisions<-rbind(BOE_Monetary_Decision,FED_Monetary_Decision,ECB_Monetary_Decision)
colnames(Monetary_Policy_Decisions)<-c("DTSTART",     "DTEND" ,      "SUMMARY"  ,   "LOCATION"    ,"transparent" ,"UID")



#CSO Calendar Below
CSO_cal<-"https://cdn.cso.ie/static/data/ReleaseCalendar.json" %>%
  request() %>%
  req_perform() %>%
  resp_body_json(simplifyVector = TRUE) %>%
  pluck("releases") %>%
  as.data.frame()


CSO_cal_XTS<-as.xts(CSO_cal, order.by=as.Date(CSO_cal$releasedate,format=c("%d/%m/%Y")))

CSO_Date<-as.POSIXct(strptime(paste0("",as.POSIXct.default(CSO_cal_XTS$releasedate), " 10:00:00"),format= "%Y-%m-%d %H:%M:%S"),tz = c("GMT"))


CSO_event = data.frame(DTSTART = CSO_Date,
                       DTEND = CSO_Date+1,
                       SUMMARY = paste(CSO_cal_XTS$title, "- ",CSO_cal_XTS$refperiod),
                       LOCATION = c("CSO"),
                       transparent=TRUE)
CSO_event <- CSO_event %>%
  mutate(UID = replicate(nrow(CSO_event), ic_guid()))

event_all<-rbind(Monetary_Policy_Decisions,CSO_event,event_CB,PMI_RELEASE,EVENTS_IMF,EVENTS_OECD,EVENTS_EC)

event_all = subset(event_all, !(SUMMARY %like any% c("%Material Flow Accounts%","%Waste Water%" ,"%Fertiliser%","%Adult%","%Agricultural%","%HSE%","%Men Hub%","%Full Irish Breakfast%","%Census 1911%","%Stories from Census 1911%","%Further Education Outcomes%","%Futher Education%","%Higher Education%","%PxStat%","%Pension%","%Inbound Tourism%","%Freight%","%Babie%","%Livestock Survey%","%Pancakes%" ,"%County Incomes and Regional%" ,"%Metered Electricity Generation%","%Networked Gas Daily Supply and Demand%","%Wood and Paper Exports and Imports%","%Circumstances of People Linked to Justice Sanctions%","%Register of Public Sector Bodies in Ireland%","%Wood Input Purchases by Industry%","%Fish%","%Fossil Fuel Subsidies%","%Survey Response Index%","%Meat Supply Balance%","%Foreign Portfolio Securities%" ,"%Crops and Livestock Survey%" ,"%Environmental%","%Industrial Disputes%","%Ecosystem%","%Rivers and Lakes%","%Building Energy Ratings%","%Forest%","%agriculture%","%Agriculture%","%Children%","%Transport Bulletin%","%Prison%","%Marriages%","%Crime%","%Violence%","%Sexual%","%Vital Statistics%","%Vital%","%Decoupling Emissions from Economic Activity%","%Measuring Ireland's Progress%","%UN%","%SDGs%","%Vaccination%","%COVID-19 Vaccination Statistics%","%Milk Statistics%","%Fuel Excise Clearances%","%Agricultural Price Indices%","%Aviation Statistics%","%Statistics of Port Traffic%","%Livestock Slaughterings%","%Area, Yield and Production of Crops%","%Household Travel Survey%","%Household Survey Response Burden Index%")))

exch_days<-seq(from=as.Date(c("2024-01-01")),to=as.Date(c("2024-12-31")), by="day")
exch_weekdays<-exch_days[format(exch_days,"%a") %like any% c("Mon", "Tue", "Wed", "Thu", "Fri")]





for(i in seq_along(exch_weekdays)){
  if (format(exch_weekdays[i],"%Y-%m-%d") %like any% c("2024-01-01","2024-02-05","2024-03-18","2024-04-01","2024-05-06","2024-06-03","2024-08-05","2024-10-28","2024-12-25","2024-12-26")){
    exch_weekdays[i]<-NA 
  }
}


January<-NA
February<-NA
March<-NA
April<-NA
May<-NA
June<-NA
July<-NA
August<-NA
September<-NA
October<-NA
November<-NA
December<-NA

exch_weekdays<-na.omit(exch_weekdays)
for(i in seq_along(exch_weekdays)){
if(format(exch_weekdays[i],"%m")=="01"){
  January[i]<-exch_weekdays[i]
} else if(format(exch_weekdays[i],"%m")=="02"){
  February[i]<-exch_weekdays[i]
} else if(format(exch_weekdays[i],"%m")=="03"){
  March[i]<-exch_weekdays[i]
} else if(format(exch_weekdays[i],"%m")=="04"){
  April[i]<-exch_weekdays[i] 
} else if(format(exch_weekdays[i],"%m")=="05"){
  May[i]<-exch_weekdays[i]
} else if(format(exch_weekdays[i],"%m")=="06"){
  June[i]<-exch_weekdays[i]
} else if(format(exch_weekdays[i],"%m")=="07"){
  July[i]<-exch_weekdays[i]
} else if(format(exch_weekdays[i],"%m")=="08"){
  August[i]<-exch_weekdays[i]
} else if(format(exch_weekdays[i],"%m")=="09"){
  September[i]<-exch_weekdays[i]
} else if(format(exch_weekdays[i],"%m")=="10"){
  October[i]<-exch_weekdays[i]
} else if(format(exch_weekdays[i],"%m")=="11"){
  November[i]<-exch_weekdays[i]
} else if(format(exch_weekdays[i],"%m")=="12"){
  December[i]<-exch_weekdays[i] 
}
}

Exch_Release<-as.Date(c(
  na.omit(January)[3],
  na.omit(February)[3],
  na.omit(March)[3],
  na.omit(April)[3],
  na.omit(May)[3],
  na.omit(June)[3],
  na.omit(July)[3],
  na.omit(August)[3],
  na.omit(September)[3],
  na.omit(October)[3],
  na.omit(November)[3],
  na.omit(December)[3]
))


DOF_DATE<-as.POSIXct(strptime(paste0("",as.POSIXct.default(paste(Exch_Release[1:12]
)), " 16:00:00"),format= "%Y-%m-%d %H:%M:%S"),tz = c("GMT"))







DOF_EVENTS<-data.frame(DTSTART = DOF_DATE,
                       DTEND = DOF_DATE+1,
                       SUMMARY = paste("Exchequer Returns"),
                       LOCATION = c("Department of Finance"),
                       transparent=TRUE)
DOF_EVENTS_2024 <- DOF_EVENTS %>%
  mutate(UID = replicate(nrow(DOF_EVENTS), ic_guid()))




CALENDAR_ALL<-rbind(EUROSTAT_calendar,event_all,DOF_EVENTS_2024)
CALENDAR_ALL_XTS<-as.xts(CALENDAR_ALL,order.by=as.Date(CALENDAR_ALL$DTSTART))
CALENDAR_ALL_short<-CALENDAR_ALL_XTS[seq(from=Sys.Date(),length.out=50, by='days')] %>% data.frame() %>% ical()
CALENDAR_ALL_short[nrow(CALENDAR_ALL_short),3]<-paste(Sys.Date())

write.csv(CALENDAR_ALL_short, file="ECON_CAL.CSV")

google_app <- httr::oauth_app(
  "THIS_IS_A_NAME",
  key = "500583945095-psku68b6caon0uug08f3iuihm3umrnqe.apps.googleusercontent.com",
  secret = "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"
  
)


G_ENDPOINT<-oauth_endpoint(authorize="https://accounts.google.com/o/oauth2/auth",
                           access="https://accounts.google.com/o/oauth2/token")


#oauth_2<-oauth2.0_token(G_ENDPOINT,google_app, scope=c(
# "https://www.googleapis.com/auth/calendar",
# "https://www.googleapis.com/auth/calendar.events"
#))


cat("authentication begins")


oauth_2 <- readRDS("credentials_file_1.json")
oauth_2$refresh()

cat("authentication ends")

calendar_url <- "https://www.googleapis.com/calendar/v3/calendars/9b1e48819517c85b915328ee7dfb7f8ef4c08ddf55af4c22a9e5688fc50adff8@group.calendar.google.com/events"

time_min<-"2023-01-01T00:00:00Z"

events <- GET(
  calendar_url,
  query=list(singleEvents=TRUE),
  add_headers(Authorization = paste("Bearer", oauth_2$credentials$access_token)
  ))


events_data <- content(events)

if(is.data.frame(data.frame(events_data$items))){
  
  for (i in seq_along(events_data$items)){
    
    calendar_url_del <- "https://www.googleapis.com/calendar/v3/calendars/9b1e48819517c85b915328ee7dfb7f8ef4c08ddf55af4c22a9e5688fc50adff8@group.calendar.google.com/events/"
    
    delete_url <- paste0(calendar_url_del,  events_data$items[[i]]$id)
    DELETE(
      delete_url,
      add_headers(
        Authorization = paste("Bearer", oauth_2$credentials$access_token))
    )
  }
  
  
  
  
  
  
  for (i in 1: length(CALENDAR_ALL_short$SUMMARY)){
    new_event<- list(
      summary = paste(CALENDAR_ALL_short$SUMMARY[i]),
      start = list(dateTime= paste0(format(as.Date(CALENDAR_ALL_short$DTSTART[i]),"%Y-%m-%d"),"T",format(as.Date(CALENDAR_ALL_short$DTSTART[i]),"%H:%M:%S")), timeZone = "GMT"),
      end = list(dateTime= paste0(format(as.Date(CALENDAR_ALL_short$DTEND[i]),"%Y-%m-%d"),"T",format(as.Date(CALENDAR_ALL_short$DTEND[i]),"%H:%M:%S")), timeZone = "GMT"),
      status = "confirmed",
      location =CALENDAR_ALL_short$LOCATION[i])
    
    POST(calendar_url, body = toJSON(new_event), add_headers(Authorization = paste("Bearer", oauth_2$credentials$access_token)))
    
    
  }
  
} else {
  print(events_data$error$message)
  
}


NEXT_WEEK_release<-NA
for(i in 1:nrow(CALENDAR_ALL_XTS)){
  if(index(CALENDAR_ALL_XTS$DTSTART[i]) %like any% (seq(from=Sys.Date()+1,to=Sys.Date()+7, by="day"))){
    NEXT_WEEK_release[i]<-paste0("* ",format(index(CALENDAR_ALL_XTS$DTSTART[i]),"%A"),": ",CALENDAR_ALL_XTS$LOCATION[i]," - " ,CALENDAR_ALL_XTS$SUMMARY[i]) 
  }
}


NEXT_WEEK_release<-na.omit(NEXT_WEEK_release)

NEXT_WEEK_release_text_1<-if(paste(NEXT_WEEK_release[1])== "NA"){
  paste0("")
} else {
  paste0(na.omit(NEXT_WEEK_release[1]))
}

NEXT_WEEK_release_text_2<-if(paste(NEXT_WEEK_release[2])== "NA"){
  paste0("")
} else {
  paste0(na.omit(NEXT_WEEK_release[2]))
}


NEXT_WEEK_release_text_3<-if(paste(NEXT_WEEK_release[3])== "NA"){
  paste0("")
} else {
  paste0(na.omit(NEXT_WEEK_release[3]))
}


NEXT_WEEK_release_text_4<-if(paste(NEXT_WEEK_release[4])== "NA"){
  paste0("")
} else {
  paste0(na.omit(NEXT_WEEK_release[4]))
}



NEXT_WEEK_release_text_5<-if(paste(NEXT_WEEK_release[5])== "NA"){
  paste0("")
} else {
  paste0(na.omit(NEXT_WEEK_release[5]))
}



NEXT_WEEK_release_text_6<-if(paste(NEXT_WEEK_release[6])== "NA"){
  paste0("")
} else {
  paste0(na.omit(NEXT_WEEK_release[6]))
}



NEXT_WEEK_release_text_7<-if(paste(NEXT_WEEK_release[7])== "NA"){
  paste0("")
} else {
  paste0(na.omit(NEXT_WEEK_release[7]))
}


NEXT_WEEK_release_text_8<-if(paste(NEXT_WEEK_release[8])== "NA"){
  paste0("")
} else {
  paste0(na.omit(NEXT_WEEK_release[8]))
}


NEXT_WEEK_release_text_9<-if(paste(NEXT_WEEK_release[9])== "NA"){
  paste0("")
} else {
  paste0(na.omit(NEXT_WEEK_release[9]))
}



NEXT_WEEK_release_text_10<-if(paste(NEXT_WEEK_release[10])== "NA"){
  paste0("")
} else {
  paste0(na.omit(NEXT_WEEK_release[10]))
}



NEXT_WEEK_release_text_11<-if(paste(NEXT_WEEK_release[11])== "NA"){
  paste0("")
} else {
  paste0(na.omit(NEXT_WEEK_release[11]))
}

NEXT_WEEK_release_text_12<-if(paste(NEXT_WEEK_release[12])== "NA"){
  paste0("")
} else {
  paste0(na.omit(NEXT_WEEK_release[12]))
}


NEXT_WEEK_release_text_13<-if(paste(NEXT_WEEK_release[13])== "NA"){
  paste0("")
} else {
  paste0(na.omit(NEXT_WEEK_release[13]))
}


NEXT_WEEK_release_text_14<-if(paste(NEXT_WEEK_release[14])== "NA"){
  paste0("")
} else {
  paste0(na.omit(NEXT_WEEK_release[14]))
}


NEXT_WEEK_release_text_15<-if(paste(NEXT_WEEK_release[15])== "NA"){
  paste0("")
} else {
  paste0(na.omit(NEXT_WEEK_release[15]))
}



smtp <- emayili::server(host = "smtp.gmail.com",
               port = 587,
               username = "mflanagan201@gmail.com",
               password = "ddauvuifpknvsobo")



Body_weekly_email<-emayili::envelope(
  to=c("michael.flanagan@finance.gov.ie"
  ),bcc=c("Eamonn.Sweeney@finance.gov.ie","colm.roche@finance.gov.ie","michael.flanagan@finance.gov.ie","Oisin.Tarrant@finance.gov.ie","Joanne.Mulholland@finance.gov.ie","Brendan.O'Connor@finance.gov.ie","Hannah.Cousins@finance.gov.ie","Fionn.Roche@finance.gov.ie","Luke.Rehill@finance.gov.ie","Eimear.Flynn@finance.gov.ie", "Sorcha.O'Connor@finance.gov.ie","harry.morris@finance.gov.ie","David.Hughes@finance.gov.ie","Pascal.McMenamy@finance.gov.ie","Patrick.OBrien@finance.gov.ie"),
  
  from="mflanagan201@gmail.com",
  subject = "Weekly Economic Calendar"
) %>%
  emayili::render(' <span class="text-center" style="color:#A3915E"> <left> <font size="4"> *Hi, The following economic releases will be released next week.* </font> </left> </span>


                
                       {{NEXT_WEEK_release_text_1}}   
                       </br>
                       </br>
                       </br> 
                       
                       {{NEXT_WEEK_release_text_2}}
                       </br>
                       </br>
                       </br> 
                       
                       {{NEXT_WEEK_release_text_3}}
                       </br>
                       </br>
                       </br> 
                       
                       {{NEXT_WEEK_release_text_4}}
                       </br>
                       </br>
                       </br> 
                       
                       {{NEXT_WEEK_release_text_5}}
                       </br>
                       </br>
                       </br> 
                      
                       {{NEXT_WEEK_release_text_6}}
                       </br>
                       </br>
                       </br> 
                      
                       {{NEXT_WEEK_release_text_7}}
                       </br>
                       </br>
                       </br>        
                       
                       {{NEXT_WEEK_release_text_8}}
                       </br>
                       </br>
                       </br> 
                       
                       {{NEXT_WEEK_release_text_9}}
                       </br>
                       </br>
                       </br> 
                       
                       {{NEXT_WEEK_release_text_10}}
                       </br>
                       </br>
                       </br> 
                       
                       {{NEXT_WEEK_release_text_11}}
                       </br>
                       </br>
                       </br> 
                       
                       {{NEXT_WEEK_release_text_12}}
                       </br>
                       </br>
                       </br> 
                       
                       {{NEXT_WEEK_release_text_13}}
                       </br>
                       </br>
                       </br> 
                      
                       {{NEXT_WEEK_release_text_14}}
                       </br>
                       </br>
                       </br> 
                      
                       {{NEXT_WEEK_release_text_15}}
                       </br>
                       </br>
                       </br> 

                       
')


if((NEXT_WEEK_release_text_1!="") &&  (format(Sys.Date(),"%a")==c("Fri"))){
  smtp(Body_weekly_email)  
}




#"Oisin.Tarrant@finance.gov.ie","Joanne.Mulholland@finance.gov.ie","Brendan.O'Connor@finance.gov.ie","Shane.Dunne@finance.gov.ie","Hannah.Cousins@finance.gov.ie","Fionn.Roche@finance.gov.ie","Luke.Rehill@finance.gov.ie", "Sarah.Hogan@finance.gov.ie","Eimear.Flynn@finance.gov.ie", "Sorcha.O'Connor@finance.gov.ie","harry.morris@finance.gov.ie"


  
