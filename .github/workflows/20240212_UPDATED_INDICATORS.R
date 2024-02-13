
library(rvest)
library(readxl)
library(emayili)

URL_COMMENCEMENTS<-"https://www.gov.ie/en/publication/a5cb1-construction-activity-starts/"
HREF<-read_html(URL_COMMENCEMENTS)  %>% html_nodes(xpath='//*[@id="download_link_245151"]')  %>% html_attr('href')

temp_xls <- tempfile(fileext = ".xlsx")
download.file(HREF, destfile = temp_xls, mode = "wb")
NEW_DATA <- read_excel(temp_xls)       


Current_Comm<-read_excel("Commencements_Current.xlsx")




UPDATED_INDEX<-NA



if(na.omit(Current_Comm[ncol(Current_Comm)])[1,1] != na.omit(NEW_DATA[ncol(NEW_DATA)])[1,1]){
  UPDATED_INDEX<-paste0("* ",print("Housing Commencements, Availble:", ))

}



UPDATED_release<-na.omit(UPDATED_INDEX)

UPDATED_release_1<-if(paste(UPDATED_release[1])== "NA"){
  paste0("")
} else {
  paste0(UPDATED_release[1])
}


smtp <- server(host = "smtp.gmail.com",
               port = 587,
               username = "mflanagan201@gmail.com",
               password = "ddauvuifpknvsobo")



UPDATED_EMAIL<-emayili::envelope(
  to=c("michael.flanagan@finance.gov.ie"
  ),bcc=c("michael.flanagan@finance.gov.ie"),
  from="mflanagan201@gmail.com",
  subject = "Updated Indicator!"
) %>%
  emayili::render(' <span class="text-center" style="color:#A3915E"> <left> <font size="4"> <font face="Arial"> *Hi, It seems like the following indicator has just been updated!* </font> </left> </span>

                         
                       {{UPDATED_release_1}}   
                       </br>
                       </br>
                       </br> 
                       
')


if(UPDATED_release_1!=""){
  smtp(UPDATED_EMAIL)  
}
