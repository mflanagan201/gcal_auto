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
library(csodata)
library(gmailr)
library(emayili)
library(eurostat)

CALENDAR_ALL_short<-read.csv("ECON_CAL.CSV")





smtp <- server(host = "smtp.gmail.com",
               port = 465,
               username = "mflanagan201@gmail.com",
               password = "ddauvuifpknvsobo")


for(i in 1:nrow(CALENDAR_ALL_short)){
if(Sys.Date()==as.Date(CALENDAR_ALL_short$DTSTART[i],format="%Y-%m-%d") && CALENDAR_ALL_short$SUMMARY[i] %like any% c("%Consumer Price Index%")){

    Body_INFLATION<-emayili::envelope(
    from = "mflanagan201@gmail.com",
    to=c("michael.flanagan@finance.gov.ie"),
bcc=c("ian.power@finance.gov.ie","harry.morris@finance.gov.ie"),
    subject = "[Auto-Email] Inflation Release"
  ) %>%
    emayili::render(".github/workflows/2024_March_Inflation_efficent_v3.Rmd")  

    smtp(Body_INFLATION)
  
    } else { 
    
    print("Don't run code")
  }
}

for(i in 1:nrow(CALENDAR_ALL_short)){
if(Sys.Date()+1==as.Date(CALENDAR_ALL_short$DTSTART[i],format="%Y-%m-%d") && CALENDAR_ALL_short$SUMMARY[i] %like any% c("%Consumer Price Index%")){

    Body_INFLATION<-emayili::envelope(
    from = "mflanagan201@gmail.com",
    to=c("michael.flanagan@finance.gov.ie"),
    subject = "[Auto-Email] Inflation Release"
  ) %>%
    emayili::render(".github/workflows/2024_March_Inflation_efficent_v3.Rmd")  

    smtp(Body_INFLATION)
  
    } else { 
    
    print("Don't run code")
  }
}



print("running code from now")
        Body_EXTERNAL_TRADE<-emayili::envelope(
          from = "mflanagan201@gmail.com",
         to=c("michael.flanagan@finance.gov.ie"),
subject = "External Trade Release!"
      ) %>%
        # Render R Markdown from a file.
        emayili::render(".github/workflows/Monthly_External_Trade_Beta_5.Rmd")
    
     # smtp(Body_EXTERNAL_TRADE)  
    
    



for(i in 1:nrow(CALENDAR_ALL_short)){
  if(Sys.Date()==as.Date(CALENDAR_ALL_short$DTSTART[i],format="%Y-%m-%d") && CALENDAR_ALL_short$SUMMARY[i] %like any% c("%Goods Exports and Imports%")){
    
        Body_EXTERNAL_TRADE<-emayili::envelope(
          from = "mflanagan201@gmail.com",
        to=c("michael.flanagan@finance.gov.ie"),
bcc=c("Ian.Power@finance.gov.ie","Oisin.Tarrant@finance.gov.ie","Daire.DeHora@finance.gov.ie"), subject = "External Trade Release!"
    ) %>%
       # Render R Markdown from a file.
      emayili::render(".github/workflows/Monthly_External_Trade_Beta_5.Rmd")
    
     #smtp(Body_EXTERNAL_TRADE)  
    
    
 } else { 
    
   print("Don't run code")
  }
}




WEEK<-seq(from=as.Date("02/12/2024", format=c('%d/%m/%Y')),to=as.Date("31/12/2025", format=c('%d/%m/%Y')), by="week")
if(Sys.Date() %like any% WEEK){

  MACRO_TABLE<-emayili::envelope(
          from = "mflanagan201@gmail.com",
        to=c("michael.flanagan@finance.gov.ie"),
    bcc=c("Ian.Power@finance.gov.ie","David.Hughes@finance.gov.ie","Joanne.Mulholland@finance.gov.ie","Pascal.McMenamy@finance.gov.ie","Patrick.OBrien@finance.gov.ie","Patrick.Wolohan@finance.gov.ie","mark.hennessy@finance.gov.ie"), 
      subject = "[Auto-Email] Macroeconomic Tables"
    ) %>%
       # Render R Markdown from a file.
      emayili::render(".github/workflows/macro_tables.Rmd")
    
     smtp(MACRO_TABLE)  

} else {
  print("nothing to send")
}




