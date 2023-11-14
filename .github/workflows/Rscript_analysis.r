library(gargle)
library(googleAuthR)
library(jsonlite)
library(knitr)
library(dplyr)
library(blastula)
library(Microsoft365R)
library(huxtable)
library(tidyverse)
library(httr2)
library(emayili)
library(rmarkdown)
library(csodata)
library(xts)
library(calendar)
library(DescTools)
library(calendar)
library(huxtable)
library(tidyverse)
library(httr2)
library(splitstackshape)
library(pdftools)
library(rvest)
library(lubridate)

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


PMI_CALENDAR<-"https://www.pmi.spglobal.com/Public/Release/ReleaseDates"


PMI_URL_download<-pdf_text("C:/Users/flanagam/Downloads/UK_Rel_Dates_2.pdf") %>% .[[1]] %>% stringr::str_split("\n")  %>%  unlist() %>% matrix()  
PMI_URL_download_PG2<-pdf_text("C:/Users/flanagam/Downloads/UK_Rel_Dates_2.pdf") %>% .[[2]] %>% stringr::str_split("\n")  %>%  unlist() %>% matrix()  

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
  
  Date_CB[i,1]<-paste(format(parse_date_time2(paste0(CB_Schedule_table3[i,]," 2023"),orders="%d-%m-%Y"),"%d/%m/%Y"))
  
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
  
  if(RELEASE_CB[i,2] %like any% c("%Activity: Statistics: Private Household Credit and Deposits Statistics%","%Publication: Quarterly Bulletin%","%Interest Rates%","%Private Household Credit and Deposits%","%Mortgage Arrears%","%Credit and Debit Card Statistics%","%Holders of Irish Government Bonds%")){
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

if(is.na(Date_CB)){
  event_CB = data.frame(DTSTART = "NA",
                        DTEND = "NA",
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
  
  Central_Banks_Schedules[i,1]<-paste(parse_date_time(paste0(Central_Banks_Schedules[i,1] ," ",Central_Banks_Schedules[i+1,1]," 2023"),orders="%d %m %Y"))
  Central_Banks_Schedules[i,2]<-paste(parse_date_time(paste0(Central_Banks_Schedules[i,2] ," ",Central_Banks_Schedules[i+1,2]," 2023"),orders="%d %m %Y"))
  Central_Banks_Schedules[i,3]<-paste(parse_date_time(paste0(Central_Banks_Schedules[i,3] ," ",Central_Banks_Schedules[i+1,3]," 2023"),orders="%d %m %Y"))
  
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
cso_clear_cache()
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


if(event_CB$SUMMARY!="NA"){
  event_all<-rbind(Monetary_Policy_Decisions,event_CB,CSO_event,PMI_RELEASE)
} else {
  event_all<-rbind(Monetary_Policy_Decisions,CSO_event,PMI_RELEASE)
}

event_all = subset(event_all, !(SUMMARY %like any% c("%Circumstances of People Linked to Justice Sanctions%","%Register of Public Sector Bodies in Ireland%","%Wood Input Purchases by Industry%","%Fish%","%Fossil Fuel Subsidies%","%Survey Response Index%","%Meat Supply Balance%","%Foreign Portfolio Securities%" ,"%Crops and Livestock Survey%" ,"%Environmental%","%Industrial Disputes%","%Ecosystem%","%Rivers and Lakes%","%Building Energy Ratings%","%Forest%","%agriculture%","%Agriculture%","%Children%","%Transport Bulletin%","%Prison%","%Marriages%","%Crime%","%Violence%","%Sexual%","%Vital Statistics%","%Vital%","%Decoupling Emissions from Economic Activity%","%Measuring Ireland's Progress%"
                                                     ,"%UN%","%SDGs%","%Vaccination%","%COVID-19 Vaccination Statistics%","%Milk Statistics%","%Fuel Excise Clearances%","%Agricultural Price Indices%","%Aviation Statistics%","%Statistics of Port Traffic%","%Livestock Slaughterings%","%Area, Yield and Production of Crops%","%Household Travel Survey%","%Household Survey Response Burden Index%")))


DOF_DATE<-as.POSIXct(strptime(paste0("",as.POSIXct.default("2023/11/03","2023/12/04"), " 16:00:00"),format= "%Y-%m-%d %H:%M:%S"),tz = c("GMT"))


DOF_EVENTS<-data.frame(DTSTART = DOF_DATE,
           DTEND = DOF_DATE+1,
           SUMMARY = paste("Exchequer Returns"),
           LOCATION = c("Department of Finance"),
           transparent=TRUE)
DOF_EVENTS_2023 <- DOF_EVENTS %>%
  mutate(UID = replicate(nrow(DOF_EVENTS), ic_guid()))




CALENDAR_ALL<-rbind(EUROSTAT_calendar,event_all,DOF_EVENTS_2023)
CALENDAR_ALL_XTS<-as.xts(CALENDAR_ALL,order.by=as.Date(CALENDAR_ALL$DTSTART))
CALENDAR_ALL_short<-CALENDAR_ALL_XTS[seq(from=Sys.Date(),length.out=50, by='days')] %>% data.frame() %>% ical()
CALENDAR_ALL_short[nrow(CALENDAR_ALL_short),3]<-paste(Sys.Date())

google_app <- httr::oauth_app(
  "THIS_IS_A_NAME",
  key = google_client_id,
  secret = google_client_secret
)



google_app <- httr::oauth_app(
  "THIS_IS_A_NAME",
  key = "500583945095-psku68b6caon0uug08f3iuihm3umrnqe.apps.googleusercontent.com",
  secret = "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"
  
)



G_ENDPOINT<-oauth_endpoint(authorize="https://accounts.google.com/o/oauth2/auth",
                           access="https://accounts.google.com/o/oauth2/token")


oauth_2<-oauth2.0_token(G_ENDPOINT,google_app, scope=c(
  "https://www.googleapis.com/auth/calendar",
  "https://www.googleapis.com/auth/calendar.events"
))

oauth_2$refresh()

calendar_url <- "https://www.googleapis.com/calendar/v3/calendars/9b1e48819517c85b915328ee7dfb7f8ef4c08ddf55af4c22a9e5688fc50adff8@group.calendar.google.com/events"

time_min<-"2023-01-01T00:00:00Z"

events <- GET(
  calendar_url,
  query=list(timeMin=time_min,showDeleted=TRUE,showHiddenInvitations=FALSE),
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



# Change events status
#  for (i in seq_along(events_data$items)) {

#   events_data$items[[i]]$status<-c("confirmed")

#  httr::PUT(paste0(calendar_url, "/", events_data$items[[i]]$id),body=events_data$items[[i]], encode="json",    add_headers(Authorization = paste("Bearer", oauth_2$credentials$access_token)),
#                    "Content-Type"="application/json",    query=list(showDeleted=FALSE,showHiddenInvitations=FALSE))
#  }







#  oauth_2$refresh()

# Delete events



#test_pdf <- pdftools::pdf_text(pdf = "https://www.centralbank.ie/docs/default-source/statistics/statistical-tables-publication-calendar.pdf")
#by_row_pdf <- stringr::str_split(test_pdf,"\n")


#CB_Retail_Interest_Rates<-(by_row_pdf[[1]])[7] %>% t() %>% as.data.frame() %>%
#  cSplit( 'V1', sep=" ") %>% t() %>% as.Date(format="%d/%m/%Y") %>% na.omit()

#CB_Retail_Interest_Rates_df<-data.frame("Retail Interest Rates", (CB_Retail_Interest_Rates))
#colnames(CB_Retail_Interest_Rates_df)<-c("Title","Date")

#CB_Credit_Debit_Card<-(by_row_pdf[[1]])[10] %>% t() %>% as.data.frame() %>%
#  cSplit( 'V1', sep=" ") %>% t() %>% as.Date(format="%d/%m/%Y") %>% na.omit()

#CB_Credit_Debit_Card_df<-data.frame("Credit and Debit Card Statistics", (CB_Credit_Debit_Card))
#colnames(CB_Credit_Debit_Card_df)<-c("Title","Date")



#Private_Household_Credit_Deposits<-seq(from=as.Date("30/03/2023",format="%d/%m/%Y"), to=as.Date("31/12/2023",format="%d/%m/%Y"), by="quarter")
#Private_Household_Credit_Deposits_df<-data.frame("Private Household Credit and Deposits", (Private_Household_Credit_Deposits))
#colnames(Private_Household_Credit_Deposits_df)<-c("Title","Date")



#Mortgage_Arrears<-seq(from=as.Date("15/03/2023",format="%d/%m/%Y"), to=as.Date("15/12/2023",format="%d/%m/%Y"), by="quarter")
#Mortgage_Arrears_df<-data.frame("Mortgage Arrears", (Mortgage_Arrears))
#colnames(Mortgage_Arrears_df)<-c("Title","Date")

