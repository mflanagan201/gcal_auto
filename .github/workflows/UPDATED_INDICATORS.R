library(DescTools)
library(rvest)
library(readxl)
library(emayili)
library(openxlsx)


smtp <- server(host = "smtp.gmail.com",
               port = 587,
               username = "mflanagan201@gmail.com",
               password = "ddauvuifpknvsobo")


URL_COMMENCEMENTS<-read_html("https://www.gov.ie/en/publication/a5cb1-construction-activity-starts/")
COMMENCEMENT_DATE_TEXT<-URL_COMMENCEMENTS %>% html_nodes("div.col-lg-8.col-md-8.col-sm-12.col-xs-12") %>% .[[1]]  %>% html_text() %>% data.frame() %>% stringr::str_split("[\r\n]")
 
COMMENCEMENT_DATE<-data.frame(matrix(NA,nrow=length(COMMENCEMENT_DATE_TEXT[[1]]),ncol=1))

for(i in 1: length(COMMENCEMENT_DATE_TEXT[[1]])){
  if(COMMENCEMENT_DATE_TEXT[[1]][i] %like any% c("%Last updated%")){
COMMENCEMENT_DATE[i,]<-(COMMENCEMENT_DATE_TEXT[[1]][i])    %>% substr(.,17,200)
}
}
COMMENCEMENT_DATE<-substr(na.omit(COMMENCEMENT_DATE),17,200) %>% as.Date("%d %B %Y")



Commencements_df<-data.frame("Monthly dwelling commencement notices", as.character(COMMENCEMENT_DATE),"https://www.gov.ie/en/publication/a5cb1-construction-activity-starts/",c("mflanagan201@gmail.com mflanagan202@gmail.com"),Sys.Date()) 
colnames(Commencements_df)<-c("varaible", "Date","URL","TO","TIME_STAMP")


URL_DAFT<-read_html("https://ww1.daft.ie/report?d_rd=1")
URL_DAFT_TEXT<-URL_DAFT %>% html_nodes("p#date_published") %>% .[[1]] %>% html_text() %>% stringr::str_split("[\r\n]") %>% data.frame()

DAFT_DATE<-data.frame(matrix(NA,nrow=1,ncol=1))

for(i in 1: nrow(URL_DAFT_TEXT)){
  if(trimws(gsub("th|nd|st|rd", "", URL_DAFT_TEXT[i,])) %>% as.Date(format=c('%d %B %Y')) %>% IsDate()){
    DAFT_DATE[i]<-trimws(gsub("th|nd|st|rd", "", URL_DAFT_TEXT[i,])) %>% as.Date(format=c('%d %B %Y')) 
    }
}


DAFT_DATE<-data.frame(na.omit(t(DAFT_DATE)))[1,] %>% as.Date()
DAFT_df<-data.frame("Daft research report", as.character(DAFT_DATE),"https://ww1.daft.ie/report?d_rd=1",c("mflanagan201@gmail.com mflanagan202@gmail.com"),Sys.Date())
colnames(DAFT_df)<-c("varaible", "Date","URL","TO","TIME_STAMP")

Sentiment<-read_html("https://www.creditunion.ie/news/consumer-sentiment-index/consumer-sentiment-index-roi/") 
sentiment_text<-Sentiment %>% html_nodes("li.active-item.parent-active") %>% html_children() %>% .[2] %>%.[[1]] %>% html_children() %>% html_text() %>% data.frame()
SENTIMENT_DATE<-sentiment_text[1,1]

SENTIMENT_df<-data.frame("Credit union consumer sentiment", SENTIMENT_DATE,"https://www.creditunion.ie/news/consumer-sentiment-index/consumer-sentiment-index-roi/",c("mflanagan201@gmail.com mflanagan202@gmail.com"),Sys.Date())
colnames(SENTIMENT_df)<-c("varaible", "Date","URL","TO","TIME_STAMP")


mortgage_approval<-read_html("https://bpfi.ie/search-resources/?_sft_category=bpfi-mortgage-approvals-report&post_types=publications")
mortgage_approval_text<-mortgage_approval %>% html_nodes("div.elementor-posts-container.elementor-posts.elementor-posts--skin-cards.elementor-grid") %>% html_text() %>% stringr::str_split("[\r\n\t]")
mortgage_approval_TEXT_DF<-(data.frame(mortgage_approval_text)) 
colnames(mortgage_approval_TEXT_DF)<-c("Date")

mortgage_approval_df_DATE<-as.Date(mortgage_approval_TEXT_DF$Date, format=c('%d %B, %Y')) %>% na.omit() %>% .[1]  

mortgage_approval_df<-data.frame("BPFI mortgage approvals report", as.character(mortgage_approval_df_DATE),"https://bpfi.ie/search-resources/?_sft_category=bpfi-mortgage-approvals-report&post_types=publications",c("mflanagan201@gmail.com niamhmmcd@gmail.com"),Sys.Date()) 
colnames(mortgage_approval_df)<-c("varaible", "Date","URL","TO","TIME_STAMP")



ALL_INDICATORS<-rbind(Commencements_df,DAFT_df,SENTIMENT_df,mortgage_approval_df)

ALL_INDICATORS_EXISTING<-read.csv("ALL_INDICATORS.csv")


UPDATED_release<-NA
for(i in 1:length(ALL_INDICATORS$varaible)){
  if(ALL_INDICATORS_EXISTING$Date[i]!=ALL_INDICATORS$Date[i]){
      UPDATED_release<-paste0("* ",ALL_INDICATORS$varaible[i], " availble:  ",ALL_INDICATORS$URL[i])
      UPDATED_EMAIL<-emayili::envelope(
        to="michael.flanagan@finance.gov.ie",
        from="mflanagan201@gmail.com",
        subject = "Updated Indicator!"
      ) %>%
        emayili::render(' <span class="text-center" style="color:#075792"> <left> <font size="4"> <font face="Arial"> *Hi, It seems like the following indicator has just been updated!* </font> </left> </span>

                       </br>  
                       {{UPDATED_release}}   
                       </br>
                       </br>
                       </br> 
                       
')
      
      
      if(UPDATED_release!=""){
              {Sys.sleep(60)}
        smtp(UPDATED_EMAIL)  
      }      
      
      
      
      
      
  } else {
  print("No Update")
}
}



  
  
write.csv(ALL_INDICATORS, file="ALL_INDICATORS.csv")




