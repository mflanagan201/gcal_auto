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
library(tframe)
library(timeSeries)

as.quarterly <- function (x, FUN=sum, na.rm=FALSE, ...){
  if (4 == frequency(x)) return(if(na.rm) trimNA(x) else x) 
  if (12 != frequency(x)) stop("only monthly conversion supported for now.")
  tf <- tframe(x)
  nm <- seriesNames(x)
  x <- tfExpand(x, add.start=(tfstart(tf)[2] %% 3)-1,
                add.end  =(3 - tfend(tf)[2]) %% 3)
  r <- aggregate(x, nfrequency=4, FUN=FUN, 
                 ndeltat=1, ts.eps=getOption("ts.eps"), ...) 
  if(na.rm) trimNA(r) else r
}

CALENDAR_ALL_short<-read.csv("ECON_CAL.CSV")

smtp <- server(host = "smtp.gmail.com",
               port = 465,
               username = "mflanagan201@gmail.com",
               password = "ddauvuifpknvsobo")




for(i in 1:nrow(CALENDAR_ALL_short)){
if(Sys.Date()==as.Date(CALENDAR_ALL_short$DTSTART[i],format="%Y-%m-%d") && CALENDAR_ALL_short$SUMMARY[i] %like any% c("%Consumer Price Index%")){
    Body_INFLATION<-emayili::envelope(
    from = "mflanagan201@gmail.com",
    to=c("mflanagan201@gmail.com"),
    subject = "Inflation Release"
  ) %>%
    emayili::render(".github/workflows/2024_March_Inflation_efficent_v3.Rmd")  

    smtp(Body_INFLATION)
  
    } else { 
    
    print("Don't run code")
  }
}




for(i in 1:nrow(CALENDAR_ALL_short)){
  if(Sys.Date()==as.Date(CALENDAR_ALL_short$DTSTART[i],format="%Y-%m-%d") && CALENDAR_ALL_short$SUMMARY[i] %like any% c("%Goods Exports and Imports%")){
    
        Body_EXTERNAL_TRADE<-emayili::envelope(
          from = "mflanagan201@gmail.com",
        to=c("mflanagan201@gmail.com"), subject = "External Trade Release!"
    ) %>%
       # Render R Markdown from a file.
      emayili::render(".github/workflows/Monthly_External_Trade_Beta_5.Rmd")
    
     smtp(Body_EXTERNAL_TRADE)  

  } else { 
    
   print("Don't run code")
  }
}


  






    Body_MACRO_TABLE<-emayili::envelope(
    from = "mflanagan201@gmail.com",
    to=c("michael.flanagan@ec.europa.eu"),
    subject = "Macroeconomic Tables"
  ) %>%
    emayili::render(".github/workflows/macro_tables.Rmd")  

if(((format(Sys.Date(),"%a")==c("Fri")))){ 
smtp(Body_MACRO_TABLE)  
}

     

bcc=c("Ian.Power@finance.gov.ie",
"David.Hughes@finance.gov.ie",
"Joanne.Mulholland@finance.gov.ie",
"Pascal.McMenamy@finance.gov.ie",
"Patrick.OBrien@finance.gov.ie",
"Patrick.Wolohan@finance.gov.ie",
"mark.hennessy@finance.gov.ie",
"Harry.Williamson@finance.gov.ie",
"Eamonn.Sweeney@finance.gov.ie",
"Rachel.Sheehan@finance.gov.ie",
"Eoin.OFlaithearta@finance.gov.ie",
"Louise.Heenan@finance.gov.ie","Leo.Redmond@finance.gov.ie","Paul.Lambert@finance.gov.ie","Daniel.Logan@finance.gov.ie")
