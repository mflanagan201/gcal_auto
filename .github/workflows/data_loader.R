
  library(dplyr)
  library(xts)
  library(rdbnomics)
  library(eurostat)
  library(data.table)
  library(lubridate)
  library(openxlsx)
  
  # ---- helper functions ----
  growth <- function(x, freq){(x/lag.xts(x, k = freq)-1)*100}
  growth_pp <- function(x, freq){(x-lag.xts(x, k = freq))}
  safe_col <- function(x, cc) {
    if (cc %in% colnames(x)) x[, cc] else xts(rep(NA, nrow(x)), order.by = index(x))
  }
  
  growth <- function(x, freq){(x/lag.xts(x, k = freq)-1)*100}
  
  monthStart <- function(x) {
    x <- as.POSIXlt(x)
    x$mday <- 1
    as.Date(x)
  }
  
  CARRYOVER <- function(x){
    CURRENTYEAR<-x[paste0("",index((last(x))) %>% year(),"")]
    LASTYEAR<-x[paste0("",index((last(x))) %>% year()-1,"")]
    FY_CURRENTYEAR<-CURRENTYEAR*(1+(length(LASTYEAR)-length(CURRENTYEAR)))
    FY_LASTYEAR<- sum(LASTYEAR)
    ((FY_CURRENTYEAR/FY_LASTYEAR)-1)*100
  }
  
  YTD <- function(x){
    CURRENTYEAR_YTD<-x[paste0("",index((last(x))) %>% year(),"")]
    LASTYEAR<-x[paste0("",(index(CURRENTYEAR_YTD)-years(1)),"")] %>% data.frame()
    CURRENTYEAR_YTD<- CURRENTYEAR_YTD %>% data.frame()
    YTD_CURRENTYEAR <- CURRENTYEAR_YTD
    for(i in 1:nrow(CURRENTYEAR_YTD)){
      YTD_CURRENTYEAR[i,]<-((as.numeric(sum(as.numeric(CURRENTYEAR_YTD[1:i,])))/as.numeric(sum(as.numeric(LASTYEAR[1:i,]))))-1)*100
    } 
    return(YTD_CURRENTYEAR)
  }
  
  growth_pp<- function(x, freq){(x-lag.xts(x, k = freq))}
  growth_pp_precovid<-function(x){x-x[2020-02-01]}
  
  MS<-c("AUT","BEL","BGR","CYP","CZE","DEU","DNK","ESP","EST","FIN","FRA","GRC","HRV","HUN","IRL","ITA","LTU","LUX","LVA","MLT","NLD","POL","PRT","ROM","SVK","SVN","SWE")
  
  GGB <- data.frame(matrix(NA,nrow=length(rdb(ids = paste0("AMECO/UBLGE/",MS[1],".1.0.0.0.UBLGE")) %>% data.frame() %>% .$value),ncol =length(MS)))
  
  for(i in 1: length(MS)){
    GGB[,i]<-rdb(ids = paste0("AMECO/UBLGE/",MS[i],".1.0.0.0.UBLGE")) %>% data.frame() %>% .$value  
  }
  colnames(GGB)<-c(MS)
  GGB_XTS<-as.xts(GGB,order.by =as.Date(paste0("01-01-", year(as.Date(paste(rdb(ids = paste0("AMECO/UBLGE/",MS[1],".1.0.0.0.UBLGE")) %>% data.frame() %>% .$original_period),format=c('%Y')))), format='%d-%m-%Y'))
  
  GGD <- data.frame(matrix(NA,nrow=length(rdb(ids = paste0("AMECO/UDGG/",MS[1],".1.0.0.0.UDGG")) %>% data.frame() %>% .$value),ncol =length(MS)))
  
  for(i in 1: length(MS)){
    GGD[,i]<-rdb(ids = paste0("AMECO/UDGG/",MS[i],".1.0.0.0.UDGG")) %>% data.frame() %>% .$value  
  }
  colnames(GGD)<-c(MS)
  GGD_XTS<-as.xts(GGD,order.by =as.Date(paste0("01-01-", year(as.Date(paste(rdb(ids = paste0("AMECO/UDGG/",MS[1],".1.0.0.0.UDGG")) %>% data.frame() %>% .$original_period),format=c('%Y')))), format='%d-%m-%Y'))
  
  GDP <- data.frame(matrix(NA,nrow=length(rdb(ids = paste0("AMECO/UVGD/",MS[1],".1.0.0.0.UVGD")) %>% data.frame() %>% .$value),ncol =length(MS)))
  
  for(i in 1: length(MS)){
    GDP[,i]<-rdb(ids = paste0("AMECO/UVGD/",MS[i],".1.0.0.0.UVGD")) %>% data.frame() %>% .$value  
  }
  colnames(GDP)<-c(MS)
  GDP_XTS<-as.xts(GDP,order.by =as.Date(paste0("01-01-", year(as.Date(paste(rdb(ids = paste0("AMECO/UVGD/",MS[1],".1.0.0.0.UVGD")) %>% data.frame() %>% .$original_period),format=c('%Y')))), format='%d-%m-%Y'))
  
  GGB_RATIO_XTS<-(GGB_XTS/GDP_XTS)*100
  GGD_RATIO_XTS<-(GGD_XTS/GDP_XTS)*100
  
  MS_DEFENCE<-c("AUT", "BEL", "BGR", "CZE", "DEU", "DNK", "ESP" ,"EST" ,"FIN", "FRA" ,"GRC" ,"HRV", "HUN", "IRL","LTU" ,"LUX","LVA" ,"MLT", "POL" ,"PRT", "ROM", "SVK" ,"SVN" ,"SWE")
  
  DEFENCE <- data.frame(matrix(NA,nrow=length(rdb(ids = paste0("AMECO/UUTG02H/",MS_DEFENCE[1],".1.0.0.0.UUTG02H")) %>% data.frame() %>% .$value),ncol =length(MS)))
  
  for(i in 1: length(MS_DEFENCE)){
    DEFENCE[,i]<-rdb(ids = paste0("AMECO/UUTG02H/",MS_DEFENCE[i],".1.0.0.0.UUTG02H")) %>% data.frame() %>% .$value  
  }
  colnames(DEFENCE)<-c(MS_DEFENCE)
  DEFENCE_XTS<-as.xts(DEFENCE,order.by =as.Date(paste0("01-01-", year(as.Date(paste(rdb(ids = paste0("AMECO/UUTG02H/",MS[1],".1.0.0.0.UUTG02H")) %>% data.frame() %>% .$original_period),format=c('%Y')))), format='%d-%m-%Y'))
  DEFENCE_XTS["2026/", "ROM"]<-DEFENCE_XTS["2026/", "ROM"]*1000
  DEFENCE_XTS["2026/", "LVA"]<-DEFENCE_XTS["2026/", "LVA"]*1000
  DEFENCE_XTS["2026/", "LTU"]<-DEFENCE_XTS["2026/", "LTU"]*1000
  DEFENCE_XTS["2026/", "MLT"]<-DEFENCE_XTS["2026/", "MLT"]*1000
  DEFENCE_XTS["2026/", "IRL"]<-DEFENCE_XTS["2026/", "IRL"]*1000
  
  
  DEFENCE_GDP_XTS<-(DEFENCE_XTS[,paste(MS_DEFENCE)]/GDP_XTS[,paste(MS_DEFENCE)])*100
  
  
  
  
  
  
  
  DEFENCE_GDP_2021BASE <- data.frame(matrix(NA,nrow=length(DEFENCE_XTS["2021/",MS_DEFENCE[1]]),ncol =length(MS_DEFENCE)))
  
  for(i in 1: length(MS_DEFENCE)){
    if(MS_DEFENCE[i] == c("GRC") || MS_DEFENCE[i] == c("BGR")){
      DEFENCE_GDP_2021BASE[,i]<- as.numeric(DEFENCE_GDP_XTS["2021/",MS_DEFENCE[i]])-as.numeric(DEFENCE_GDP_XTS["2024",MS_DEFENCE[i]]) 
    } else {
      DEFENCE_GDP_2021BASE[,i]<- as.numeric(DEFENCE_GDP_XTS["2021/",MS_DEFENCE[i]])-as.numeric(DEFENCE_GDP_XTS["2021",MS_DEFENCE[i]]) 
    }
  }
  
  colnames(DEFENCE_GDP_2021BASE)<-c(MS_DEFENCE)
  
  DEFENCE_GDP_2021BASE_XTS<-as.xts(DEFENCE_GDP_2021BASE,order.by=index(DEFENCE_GDP_XTS["2021/"]))
  
  DEFENCE_GDP_2024BASE <- data.frame(matrix(NA,nrow=length(DEFENCE_XTS["2024/",MS_DEFENCE[1]]),ncol =length(MS_DEFENCE)))
  
  for(i in 1: length(MS_DEFENCE)){
    DEFENCE_GDP_2024BASE[,i]<- as.numeric(DEFENCE_GDP_XTS["2024/",MS_DEFENCE[i]])-as.numeric(DEFENCE_GDP_XTS["2024",MS_DEFENCE[i]]) 
  }
  
  colnames(DEFENCE_GDP_2024BASE)<-c(MS_DEFENCE)
  DEFENCE_GDP_2024BASE_XTS<-as.xts(DEFENCE_GDP_2024BASE,order.by=index(DEFENCE_GDP_XTS["2024/"]))
  
  TOTAL_EXP <- data.frame(matrix(NA,nrow=length(rdb(ids = paste0("AMECO/UUTGE/",MS[1],".1.0.0.0.UUTGE")) %>% data.frame() %>% .$value),ncol =length(MS)))
  
  for(i in 1: length(MS)){
    TOTAL_EXP[,i]<-rdb(ids = paste0("AMECO/UUTGE/",MS[i],".1.0.0.0.UUTGE")) %>% data.frame() %>% .$value  
  }
  colnames(TOTAL_EXP)<-c(MS)
  TOTAL_EXP_XTS<-as.xts(TOTAL_EXP,order.by =as.Date(paste0("01-01-", year(as.Date(paste(rdb(ids = paste0("AMECO/UUTGE/",MS[1],".1.0.0.0.UUTGE")) %>% data.frame() %>% .$original_period),format=c('%Y')))), format='%d-%m-%Y'))
  
  TOTAL_EXP_GDP_XTS<-(TOTAL_EXP_XTS/GDP_XTS)*100
  
  INTEREST_EXP <- data.frame(matrix(NA,nrow=length(rdb(ids = paste0("AMECO/UYIGE/",MS[1],".1.0.0.0.UYIGE")) %>% data.frame() %>% .$value),ncol =length(MS)))
  
  for(i in 1: length(MS)){
    INTEREST_EXP[,i]<-rdb(ids = paste0("AMECO/UYIGE/",MS[i],".1.0.0.0.UYIGE")) %>% data.frame() %>% .$value  
  }
  colnames(INTEREST_EXP)<-c(MS)
  INTEREST_EXP_XTS<-as.xts(INTEREST_EXP,order.by =as.Date(paste0("01-01-", year(as.Date(paste(rdb(ids = paste0("AMECO/UYIGE/",MS[1],".1.0.0.0.UYIGE")) %>% data.frame() %>% .$original_period),format=c('%Y')))), format='%d-%m-%Y'))
  
  INTEREST_EXP_GDP_XTS<-(INTEREST_EXP_XTS/GDP_XTS)*100
  
  ONCE_OFF <- data.frame(matrix(NA,nrow=length(rdb(ids = paste0("AMECO/UOOMS/",MS[1],".1.0.0.0.UOOMS")) %>% data.frame() %>% .$value),ncol =length(MS)))
  
  for(i in 1: length(MS)){
    ONCE_OFF[,i]<-rdb(ids = paste0("AMECO/UOOMS/",MS[i],".1.0.0.0.UOOMS")) %>% data.frame() %>% .$value  
  }
  colnames(ONCE_OFF)<-c(MS)
  ONCE_OFF_XTS<-as.xts(ONCE_OFF,order.by =as.Date(paste0("01-01-", year(as.Date(paste(rdb(ids = paste0("AMECO/UOOMS/",MS[1],".1.0.0.0.UOOMS")) %>% data.frame() %>% .$original_period),format=c('%Y')))), format='%d-%m-%Y'))
  
  ONCE_OFF_GDP_XTS<-(ONCE_OFF_XTS/GDP_XTS)*100
  
  DRM_CURRENT_EXP <- data.frame(matrix(NA,nrow=length(rdb(ids = paste0("AMECO/UDMGCE/",MS[1],".1.0.0.0.UDMGCE")) %>% data.frame() %>% .$value),ncol =length(MS)))
  
  for(i in 1: length(MS)){
    DRM_CURRENT_EXP[,i]<-rdb(ids = paste0("AMECO/UDMGCE/",MS[i],".1.0.0.0.UDMGCE")) %>% data.frame() %>% .$value  
  }
  colnames(DRM_CURRENT_EXP)<-c(MS)
  DRM_CURRENT_EXP_XTS<-as.xts(DRM_CURRENT_EXP,order.by =as.Date(paste(rdb(ids = paste0("AMECO/UDMGCE/",MS[i],".1.0.0.0.UDMGCE")) %>% data.frame() %>% .$original_period),format=c('%Y')))
  
  DRM_CURRENT_REV <- data.frame(matrix(NA,nrow=length(rdb(ids = paste0("AMECO/UDMGCR/",MS[1],".1.0.0.0.UDMGCR")) %>% data.frame() %>% .$value),ncol =length(MS)))
  
  for(i in 1: length(MS)){
    DRM_CURRENT_REV[,i]<-rdb(ids = paste0("AMECO/UDMGCR/",MS[i],".1.0.0.0.UDMGCR")) %>% data.frame() %>% .$value  
  }
  colnames(DRM_CURRENT_REV)<-c(MS)
  DRM_CURRENT_REV_XTS<-as.xts(DRM_CURRENT_REV,order.by =as.Date(paste0("01-01-", year(as.Date(paste(rdb(ids = paste0("AMECO/UDMGCR/",MS[1],".1.0.0.0.UDMGCR")) %>% data.frame() %>% .$original_period),format=c('%Y')))), format='%d-%m-%Y'))
  
  DRM_CAPITAL_EXP <- data.frame(matrix(NA,nrow=length(rdb(ids = paste0("AMECO/UDMGKE/",MS[1],".1.0.0.0.UDMGKE")) %>% data.frame() %>% .$value),ncol =length(MS)))
  
  for(i in 1: length(MS)){
    DRM_CAPITAL_EXP[,i]<-rdb(ids = paste0("AMECO/UDMGKE/",MS[i],".1.0.0.0.UDMGKE")) %>% data.frame() %>% .$value  
  }
  colnames(DRM_CAPITAL_EXP)<-c(MS)
  DRM_CAPITAL_EXP_XTS<-as.xts(DRM_CAPITAL_EXP,order.by =as.Date(paste(rdb(ids = paste0("AMECO/UDMGKE/",MS[i],".1.0.0.0.UDMGKE")) %>% data.frame() %>% .$original_period),format=c('%Y')))
  
  DRM_CAPITAL_REV <- data.frame(matrix(NA,nrow=length(rdb(ids = paste0("AMECO/UDMGKTR/",MS[1],".1.0.0.0.UDMGKTR")) %>% data.frame() %>% .$value),ncol =length(MS)))
  
  for(i in 1: length(MS)){
    DRM_CAPITAL_REV[,i]<-rdb(ids = paste0("AMECO/UDMGKTR/",MS[i],".1.0.0.0.UDMGKTR")) %>% data.frame() %>% .$value  
  }
  colnames(DRM_CAPITAL_REV)<-c(MS)
  DRM_CAPITAL_REV_XTS<-as.xts(DRM_CAPITAL_REV,order.by =as.Date(paste0("01-01-", year(as.Date(paste(rdb(ids = paste0("AMECO/UDMGKTR/",MS[1],".1.0.0.0.UDMGKTR")) %>% data.frame() %>% .$original_period),format=c('%Y')))), format='%d-%m-%Y'))
  
  EU_TRANSFERS_EXP <- data.frame(matrix(NA,nrow=length(rdb(ids = paste0("AMECO/URTGEU/",MS[1],".1.0.0.0.URTGEU")) %>% data.frame() %>% .$value),ncol =length(MS)))
  
  for(i in 1: length(MS)){
    EU_TRANSFERS_EXP[,i]<-rdb(ids = paste0("AMECO/URTGEU/",MS[i],".1.0.0.0.URTGEU")) %>% data.frame() %>% .$value  
  }
  colnames(EU_TRANSFERS_EXP)<-c(MS)
  EU_TRANSFERS_EXP_XTS<-as.xts(EU_TRANSFERS_EXP,order.by =as.Date(paste0("01-01-", year(as.Date(paste(rdb(ids = paste0("AMECO/URTGEU/",MS[1],".1.0.0.0.URTGEU")) %>% data.frame() %>% .$original_period),format=c('%Y')))), format='%d-%m-%Y'))
  
  EU_TRANSFERS_EXP_GDP_XTS<-(EU_TRANSFERS_EXP_XTS/GDP_XTS)*100
  
  CYCLICAL_U_EXP <- data.frame(matrix(NA,nrow=length(rdb(ids = paste0("AMECO/UUTZ105/",MS[1],".1.0.0.0.UUTZ105")) %>% data.frame() %>% .$value),ncol =length(MS)))
  
  for(i in 1: length(MS)){
    CYCLICAL_U_EXP[,i]<-rdb(ids = paste0("AMECO/UUTZ105/",MS[i],".1.0.0.0.UUTZ105")) %>% data.frame() %>% .$value  
  }
  colnames(CYCLICAL_U_EXP)<-c(MS)
  CYCLICAL_U_EXP_XTS<-as.xts(CYCLICAL_U_EXP,order.by =as.Date(paste0("01-01-", year(as.Date(paste(rdb(ids = paste0("AMECO/UUTZ105/",MS[1],".1.0.0.0.UUTZ105")) %>% data.frame() %>% .$original_period),format=c('%Y')))), format='%d-%m-%Y'))
  
  CYCLICAL_U_EXP_GDP_XTS<-(CYCLICAL_U_EXP_XTS/GDP_XTS)*100
  
  NET_EXP_BEF_DRM <- data.frame(matrix(NA,nrow=length(rdb(ids = paste0("AMECO/UUTGIN/",MS[1],".1.0.0.0.UUTGIN")) %>% data.frame() %>% .$value),ncol =length(MS)))
  
  for(i in 1: length(MS)){
    NET_EXP_BEF_DRM[,i]<-rdb(ids = paste0("AMECO/UUTGIN/",MS[i],".1.0.0.0.UUTGIN")) %>% data.frame() %>% .$value  
  }
  colnames(NET_EXP_BEF_DRM)<-c(MS)
  NET_EXP_BEF_DRM_XTS<-as.xts(NET_EXP_BEF_DRM,order.by =as.Date(paste0("01-01-", year(as.Date(paste(rdb(ids = paste0("AMECO/UUTGIN/",MS[1],".1.0.0.0.UUTGIN")) %>% data.frame() %>% .$original_period),format=c('%Y')))), format='%d-%m-%Y'))
  
  NET_EXP_BEF_DRM_GDP_XTS<-(NET_EXP_BEF_DRM_XTS/GDP_XTS)*100
  
  ONCE_OFF_REV <- data.frame(matrix(NA,nrow=length(rdb(ids = paste0("AMECO/UOOMSR/",MS[1],".1.0.0.0.UOOMSR")) %>% data.frame() %>% .$value),ncol =length(MS)))
  
  for(i in 1: length(MS)){
    ONCE_OFF_REV[,i]<-rdb(ids = paste0("AMECO/UOOMSR/",MS[i],".1.0.0.0.UOOMSR")) %>% data.frame() %>% .$value  
  }
  colnames(ONCE_OFF_REV)<-c(MS)
  ONCE_OFF_REV_XTS<-as.xts(ONCE_OFF_REV,order.by =as.Date(paste0("01-01-", year(as.Date(paste(rdb(ids = paste0("AMECO/UOOMSR/",MS[1],".1.0.0.0.UOOMSR")) %>% data.frame() %>% .$original_period),format=c('%Y')))), format='%d-%m-%Y'))
  
  ONCE_OFF_REV_GDP_XTS<-(ONCE_OFF_REV_XTS/GDP_XTS)*100
  
  
  
  
  
  
  
  NET_EXP_BEFORE_DRMS_EXCL_COFINANCING_EU<-TOTAL_EXP_XTS-INTEREST_EXP_XTS-CYCLICAL_U_EXP_XTS-EU_TRANSFERS_EXP_XTS
  NET_EXP_BEFORE_DRMS_EXCL_COFINANCING_EU_GDP_XTS<-(NET_EXP_BEFORE_DRMS_EXCL_COFINANCING_EU/GDP_XTS)*100
  
  COFINANCING_EU<-NET_EXP_BEFORE_DRMS_EXCL_COFINANCING_EU-NET_EXP_BEF_DRM_XTS
  COFINANCING_EU_GDP_XTS<-(COFINANCING_EU/GDP_XTS)*100
  
  
  NET_DRM<-DRM_CURRENT_REV_XTS+DRM_CAPITAL_REV_XTS-(ONCE_OFF_REV_XTS-lag(ONCE_OFF_REV_XTS,1)) 
  NET_DRM_GDP_XTS<-(NET_DRM/GDP_XTS)*100
  
  NET_EXP_AFTER_DRMS<-NET_EXP_BEF_DRM_XTS-NET_DRM
  NET_EXP_AFTER_DRMS_GDP_XTS<-(NET_EXP_AFTER_DRMS/GDP_XTS)*100
  
  change_NET_EXP_BEF_DRM_XTS<-NET_EXP_BEF_DRM_XTS-lag(NET_EXP_BEF_DRM_XTS,1)
  change_NET_EXP_AFTER_DRM_XTS<-change_NET_EXP_BEF_DRM_XTS-NET_DRM
  
  NEG_RATE<-(change_NET_EXP_AFTER_DRM_XTS/lag(NET_EXP_BEF_DRM_XTS,1))*100
  index(NEG_RATE)<-as.Date(paste0("01-01-", year(index(NEG_RATE))), format='%d-%m-%Y')
  
  
  
  
  safe_col <- function(x, cc) {
    if (cc %in% colnames(x)) {
      return(x[, cc])
    } else {
      return(xts(rep(NA, nrow(x)), order.by = index(x)))
    }
  }
  
  
  
  NEG_RATE<-data.frame(index(NEG_RATE),NEG_RATE)
  colnames(NEG_RATE)<-c("DATE",MS)
  NEG_RATE<-as.xts(NEG_RATE)
  
  
  
  NEC<-data.frame(c("TRUE","TRUE","TRUE","TRUE","TRUE","FALSE","FALSE","TRUE","FALSE","FALSE","TRUE","FALSE","FALSE","TRUE","TRUE","FALSE","TRUE","FALSE","FALSE","TRUE","TRUE","TRUE","FALSE","TRUE","TRUE","TRUE","FALSE")) %>% t()
  colnames(NEC)<-  c("BEL","BGR","CZE","DNK","DEU","EST","IRL","GRC","ESP","FRA","HRV","ITA","CYP","LVA","LTU","LUX","HUN","MLT","NLD","AUT","POL","PRT","ROM","SVN","SVK","FIN","SWE")
  
  
  ####High frenquency indicators 
  MS<-c("AUT" ,"BEL", "BGR", "CYP", "CZE", "DEU" ,"DNK","EST","GRC", "ESP",  "FIN", "FRA", "HRV", "HUN", "IRL", "ITA", "LTU", "LUX", "LVA", "MLT", "NLD","POL", "PRT", "ROM", "SWE","SVN","SVK")
  MS_2L<-c("AT", "BE", "BG", "CY", "CZ", "DE" ,"DK", "EE", "EL", "ES", "FI", "FR", "HR", "HU", "IE", "IT", "LT", "LU", "LV", "MT", "NL", "PL", "PT", "RO", "SE","SI", "SK")  
  MEMBER_STATES<-data.frame(MS,MS_2L)
  
  
  UR <- get_eurostat("une_rt_m",filters = list(sex="T",age="TOTAL", s_adj="SA",unit="PC_ACT"),cache = FALSE) 
  UR_xts<-matrix(NA,nrow=length(unique(UR$time)),ncol=length(MS_2L)) %>% as.xts(order.by=as.Date(unique(UR$time)))
  
  for(i in 1:length(MS_2L)){
    UR_xts[,i]<-  as.xts(subset(UR,geo==MS_2L[i], select=c("values")), order.by=index(UR_xts))
  }
  
  colnames(UR_xts)<-c(MS_2L)
  
  REAL_GDP <- get_eurostat("namq_10_gdp",filters = list(s_adj="NSA",unit="CLV_PCH_SM",na_item="B1GQ"),cache = FALSE) 
  REAL_GDP_xts<-matrix(NA,nrow=length(unique(REAL_GDP$time)),ncol=length(MS_2L)) %>% as.xts(order.by=as.Date(unique(REAL_GDP$time)))
  
  for(i in 1:length(MS_2L)){
    REAL_GDP_xts[,i]<-  as.xts(subset(REAL_GDP,geo==MS_2L[i], select=c("values")), order.by=as.Date(unique(REAL_GDP$time)))
  }
  
  colnames(REAL_GDP_xts)<-c(MS_2L)
  
  
  HICP <- get_eurostat("prc_hicp_minr",filters = list(coicop18="TOTAL",unit="RCH_A",freq="M"),cache = FALSE) 
  HICP_xts<-matrix(NA,nrow=length(unique(HICP$time)),ncol=length(MS_2L)) %>% as.xts(order.by=as.Date(unique(HICP$time)))
  
  for(i in 1:length(MS_2L)){
    HICP_xts[,i]<-  as.xts(subset(HICP,geo==MS_2L[i], select=c("values")), order.by=as.Date(unique(HICP$time)))
  }
  
  colnames(HICP_xts)<-c(MS_2L)
  
  
  DEFICIT_Q <- get_eurostat("gov_10q_ggnfa",filters = list(na_item="B9",s_adj="SCA",sector="S13",unit="PC_GDP"),cache = FALSE) 
  DEFICIT_Q_xts<-matrix(NA,nrow=length(unique(DEFICIT_Q$time)),ncol=length(MS_2L)) %>% as.xts(order.by=as.Date(unique(DEFICIT_Q$time)))
  
  for(i in 1:length(MS_2L)){
    DEFICIT_Q_xts[,i]<-  as.xts(subset(DEFICIT_Q,geo==MS_2L[i], select=c("values")), order.by=as.Date(unique(DEFICIT_Q$time)))
  }
  
  colnames(DEFICIT_Q_xts)<-c(MS_2L)
  
  
  yield <- get_eurostat("irt_lt_mcby_m",filters = list(),cache = FALSE) 
  yield_xts<-matrix(NA,nrow=length(unique(yield$time)),ncol=length(MS_2L)) %>% as.xts(order.by=as.Date(unique(yield$time)))
  
  for(i in 1:length(MS_2L)){
    yield_xts[,i]<-  as.xts(subset(yield,geo==MS_2L[i], select=c("values")), order.by=as.Date(unique(yield$time)))
  }
  
  colnames(yield_xts)<-c(MS_2L)
  
  
  
  data<-list(
    'GGB_RATIO_XTS' = data.frame(index(GGB_RATIO_XTS),GGB_RATIO_XTS),
    'GGD_RATIO_XTS' = data.frame(index(GGD_RATIO_XTS),GGD_RATIO_XTS),
    'GDP_XTS' = data.frame(index(GDP_XTS),GDP_XTS),
    'TOTAL_EXP_GDP_XTS' = data.frame(index(TOTAL_EXP_GDP_XTS),TOTAL_EXP_GDP_XTS),
    'INTEREST_EXP_GDP_XTS' = data.frame(index(INTEREST_EXP_GDP_XTS),INTEREST_EXP_GDP_XTS),
    'CYCLICAL_U_EXP_GDP_XTS' = data.frame(index(CYCLICAL_U_EXP_GDP_XTS),CYCLICAL_U_EXP_GDP_XTS),
    'EU_TRANSFERS_EXP_GDP_XTS' = data.frame(index(EU_TRANSFERS_EXP_GDP_XTS),EU_TRANSFERS_EXP_GDP_XTS),
    'ONCE_OFF_GDP_XTS' = data.frame(index(ONCE_OFF_GDP_XTS),ONCE_OFF_GDP_XTS),
    'TOTAL_EXP_XTS' = data.frame(index(TOTAL_EXP_XTS),TOTAL_EXP_XTS),
    'INTEREST_EXP_XTS' = data.frame(index(INTEREST_EXP_XTS),INTEREST_EXP_XTS),
    'CYCLICAL_U_EXP_XTS' = data.frame(index(CYCLICAL_U_EXP_XTS),CYCLICAL_U_EXP_XTS),
    'EU_TRANSFERS_EXP_XTS' = data.frame(index(EU_TRANSFERS_EXP_XTS),EU_TRANSFERS_EXP_XTS),
    'ONCE_OFF_XTS' = data.frame(index(ONCE_OFF_XTS),ONCE_OFF_XTS),
    'NET_EXP_BEF_DRM_XTS'=data.frame(index(NET_EXP_BEF_DRM_XTS),NET_EXP_BEF_DRM_XTS), 
    'NET_EXP_BEF_DRM_GDP_XTS' = data.frame(index(NET_EXP_BEF_DRM_GDP_XTS),NET_EXP_BEF_DRM_GDP_XTS),
    'NET_EXP_AFTER_DRMS_GDP_XTS' = data.frame(index(NET_EXP_AFTER_DRMS_GDP_XTS),NET_EXP_AFTER_DRMS_GDP_XTS),
    'NET_DRM'= data.frame(index(NET_DRM),NET_DRM),
    'NET_DRM_GDP_XTS' = data.frame(index(NET_DRM_GDP_XTS),NET_DRM_GDP_XTS),
    'NEG_RATE' = data.frame(index(NEG_RATE),NEG_RATE),
    'NEC' = data.frame(NEC),
    'DEFENCE_GDP_XTS'= data.frame(index(DEFENCE_GDP_XTS),DEFENCE_GDP_XTS),
    'DEFENCE_XTS'= data.frame(index(DEFENCE_XTS),DEFENCE_XTS),
    'DEFENCE_GDP_2021BASE_XTS' = data.frame(index(DEFENCE_GDP_2021BASE_XTS),DEFENCE_GDP_2021BASE_XTS),
    'UR_xts' = data.frame(index(UR_xts),UR_xts),
    'REAL_GDP_xts' = data.frame(index(REAL_GDP_xts),REAL_GDP_xts),
    'HICP_xts' = data.frame(index(HICP_xts),HICP_xts),
    'DEFICIT_Q_xts' = data.frame(index(DEFICIT_Q_xts),DEFICIT_Q_xts),
    'yield_xts' = data.frame(index(yield_xts),yield_xts),
    'MEMBER_STATES' = data.frame(MEMBER_STATES),
    'safe_col' = data.frame(safe_col)
  )
  
  
  
  
  
  names(data$GGB_RATIO_XTS)[1]<-c("Date")  
  names(data$GGD_RATIO_XTS)[1]<-c("Date")  
  names(data$GDP_XTS)[1]<-c("Date")  
  names(data$TOTAL_EXP_GDP_XTS)[1]<-c("Date")
  names(data$INTEREST_EXP_GDP_XTS)[1]<-c("Date")
  names(data$CYCLICAL_U_EXP_GDP_XTS)[1]<-c("Date")
  names(data$EU_TRANSFERS_EXP_GDP_XTS)[1]<-c("Date")
  names(data$ONCE_OFF_GDP_XTS)[1]<-c("Date")
  names(data$TOTAL_EXP_XTS)[1]<-c("Date")
  names(data$INTEREST_EXP_XTS)[1]<-c("Date")
  names(data$CYCLICAL_U_EXP_XTS)[1]<-c("Date")
  names(data$EU_TRANSFERS_EXP_XTS)[1]<-c("Date")
  names(data$ONCE_OFF_XTS)[1]<-c("Date")
  names(data$NET_EXP_BEF_DRM_XTS)[1]<-c("Date")
  names(data$NET_EXP_BEF_DRM_GDP_XTS)[1]<-c("Date")
  names(data$NET_EXP_AFTER_DRMS_GDP_XTS)[1]<-c("Date")
  names(data$NET_DRM_GDP_XTS)[1]<-c("Date")
  names(data$NEG_RATE)[1]<-c("Date")

  names(data$DEFENCE_GDP_2021BASE_XTS)[1]<-c("Date")
  names(data$UR_xts)[1]<-c("Date")
  names(data$REAL_GDP_xts)[1]<-c("Date")
  names(data$HICP_xts)[1]<-c("Date")
  names(data$DEFICIT_Q_xts)[1]<-c("Date")
  names(data$DEFENCE_GDP_XTS)[1]<-c("Date")
  names(data$DEFENCE_XTS)[1]<-c("Date")
  names(data$NET_DRM)[1]<-c("Date")
  names(data$yield_xts)[1]<-c("Date")
  
  wb <- createWorkbook()
  
  addWorksheet(wb,'GGB_RATIO_XTS')
  addWorksheet(wb,'GGD_RATIO_XTS')
  addWorksheet(wb,'GDP_XTS') 
  addWorksheet(wb,'TOTAL_EXP_GDP_XTS')
  addWorksheet(wb,'INTEREST_EXP_GDP_XTS')
  addWorksheet(wb,'CYCLICAL_U_EXP_GDP_XTS')
  addWorksheet(wb,'EU_TRANSFERS_EXP_GDP_XTS')
  addWorksheet(wb,'ONCE_OFF_XTS')
  addWorksheet(wb,'ONCE_OFF_GDP_XTS')
  addWorksheet(wb,'TOTAL_EXP_XTS')
  addWorksheet(wb,'INTEREST_EXP_XTS')
  addWorksheet(wb,'CYCLICAL_U_EXP_XTS')
  addWorksheet(wb,'EU_TRANSFERS_EXP_XTS')
  addWorksheet(wb,'NET_EXP_BEF_DRM_XTS') 
  addWorksheet(wb,'NET_EXP_BEF_DRM_GDP_XTS')
  addWorksheet(wb,'NET_EXP_AFTER_DRMS_GDP_XTS')
  addWorksheet(wb,'NET_DRM_GDP_XTS')
  addWorksheet(wb,'NEG_RATE')
  addWorksheet(wb,'NEC')
  addWorksheet(wb,'DEFENCE_GDP_2021BASE_XTS')
  addWorksheet(wb,'DEFENCE_GDP_XTS')
  addWorksheet(wb,'DEFENCE_XTS')
  addWorksheet(wb,'UR_xts')
  addWorksheet(wb,'REAL_GDP_xts')
  addWorksheet(wb,'HICP_xts')
  addWorksheet(wb,'DEFICIT_Q_xts')
  addWorksheet(wb,'yield_xts')
  addWorksheet(wb,'MEMBER_STATES')
  addWorksheet(wb,'safe_col')
  addWorksheet(wb,'NET_DRM')
  

  
  
  writeData(wb,'GGB_RATIO_XTS',data$GGB_RATIO_XTS)
  writeData(wb,'GGD_RATIO_XTS',data$GGD_RATIO_XTS)
  writeData(wb,'GDP_XTS',data$GDP_XTS) 
  writeData(wb,'TOTAL_EXP_GDP_XTS',data$TOTAL_EXP_GDP_XTS)
  writeData(wb,'INTEREST_EXP_GDP_XTS',data$INTEREST_EXP_GDP_XTS)
  writeData(wb,'CYCLICAL_U_EXP_GDP_XTS',data$CYCLICAL_U_EXP_GDP_XTS)
  writeData(wb,'EU_TRANSFERS_EXP_GDP_XTS',data$EU_TRANSFERS_EXP_GDP_XTS)
  writeData(wb,'ONCE_OFF_GDP_XTS',data$ONCE_OFF_GDP_XTS)
  writeData(wb,'TOTAL_EXP_XTS',data$TOTAL_EXP_XTS)
  writeData(wb,'INTEREST_EXP_XTS',data$INTEREST_EXP_XTS)
  writeData(wb,'CYCLICAL_U_EXP_XTS',data$CYCLICAL_U_EXP_XTS)
  writeData(wb,'EU_TRANSFERS_EXP_XTS',data$EU_TRANSFERS_EXP_XTS)
  writeData(wb,'ONCE_OFF_XTS',data$ONCE_OFF_XTS)
  writeData(wb,'NET_EXP_BEF_DRM_XTS',data$NET_EXP_BEF_DRM_XTS) 
  writeData(wb,'NET_EXP_BEF_DRM_GDP_XTS',data$NET_EXP_BEF_DRM_GDP_XTS)
  writeData(wb,'NET_EXP_AFTER_DRMS_GDP_XTS',data$NET_EXP_AFTER_DRMS_GDP_XTS)
  writeData(wb,'NET_DRM_GDP_XTS',data$NET_DRM_GDP_XTS)
  writeData(wb,'NEG_RATE',data$NEG_RATE)
  
  writeData(wb,'NEC',data$NEC)
  writeData(wb,'DEFENCE_GDP_2021BASE_XTS',data$DEFENCE_GDP_2021BASE_XTS)
  writeData(wb,'DEFENCE_GDP_XTS',data$DEFENCE_GDP_XTS)
  writeData(wb,'DEFENCE_XTS',data$DEFENCE_XTS)
  writeData(wb,'UR_xts',data$UR_xts)
  writeData(wb,'REAL_GDP_xts',data$REAL_GDP_xts)
  writeData(wb,'HICP_xts',data$HICP_xts)
  writeData(wb,'DEFICIT_Q_xts',data$DEFICIT_Q_xts)
  writeData(wb,'yield_xts',data$yield_xts)
  writeData(wb,'MEMBER_STATES',data$MEMBER_STATES)
  writeData(wb,'NET_DRM',data$NET_DRM)
  
  saveWorkbook((wb), "fiscaldata.xlsx", overwrite = TRUE)
  
  
