library(csodata)
library(gmailr)
library(emayili)
library(dplyr)


smtp <- server(host = "smtp.gmail.com",
               port = 465,
               username = "mflanagan201@gmail.com",
               password = "ddauvuifpknvsobo")


for(i in 1:length(CALENDAR_ALL_short$DTSTART)){
  if(Sys.Date()==as.Date(CALENDAR_ALL_short$DTSTART[i],format="%Y-%m-%d") && CALENDAR_ALL_short$SUMMARY[i] %like any% c("%Labour Force Survey%")){
    
    Body_INFLATION<-emayili::envelope(
      from = "mflanagan201@gmail.com",
      to=c("michael.flanagan@finance.gov.ie"),
      subject = "[Auto-Email] Inflation Chartpack"
    ) %>%
      # Render R Markdown from a file.
      emayili::render("C:/Users/flanagam/Documents/Inflation_Breakdown_v4.Rmd")
    
    smtp(Body_INFLATION)  
    
    
  } else if (Sys.Date()==as.Date(CALENDAR_ALL_short$DTSTART[i],format="%Y-%m-%d") && CALENDAR_ALL_short$SUMMARY[i] %like any% c("%Goods Exports and Imports%")){
      
    Body_EXTERNAL_TRADE<-emayili::envelope(
      from = "mflanagan201@gmail.com",
      to=c("michael.flanagan@finance.gov.ie"),
      subject = "External Trade Release!"
    ) %>%
      # Render R Markdown from a file.
      emayili::render("C:/Users/flanagam/Documents/Monthly_External_Trade_Beta_4.Rmd")
    
    smtp(Body_EXTERNAL_TRADE)  
    
    
  } else { 
    
    print("Don't run code")
  }
}



