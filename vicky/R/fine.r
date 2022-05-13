
scissor<-function(fp,RefkeyIDthread1="Reference",RefkeyIDthread2="Key",
                  IDvars=list(Lab=c("LIS"),Med=c("Disp","Pres"),Dx=c("diag"),
                              Px=c("proc"),Demo=c("birth","death","sex")),out_fp=getwd(),
                  milliseconds=6,sav=TRUE,as.parquet=FALSE,csv.sep="\t"){
  options(digits.secs=milliseconds)
  dat<-if(is.character(fp)) {opener(fp)%>%mutate_all(as.character)} else if (is.data.frame(fp)) fp else NULL
  if(!grepl(r"((/$)|(\\$))",out_fp)) {out_fp<-paste0(out_fp,r"(/)",collapse="|")}
  Refkeypos<-grep(paste0(RefkeyIDthread1,".*",RefkeyIDthread2),names(dat),ignore.case=TRUE)%>%{if(length(.)>0) . else 0}
  map(names(IDvars),~dir.create(paste0(out_fp,.x),showWarnings=FALSE))
  map2(IDvars,names(IDvars),~grep(paste0(.x,collapse="|"),names(dat),ignore.case=TRUE)%>%
         {if(length(.)>0) {tmp<-select(dat,all_of(Refkeypos),min(.):ncol(dat))
         if (dim(tmp)[1]>0&as.parquet==TRUE&save==TRUE) tmp%>%write_parquet(paste0(out_fp,.y,"/",format(Sys.time(), "%Y%m%d_%H%M%OS"),".parquet"))
         if (dim(tmp)[1]>0&as.parquet==FALSE&save==TRUE) tmp%>%fwrite(paste0(out_fp,.y,"/",format(Sys.time(), "%Y%m%d_%H%M%OS"),".csv"),sep=csv.sep="\t")
         if (dim(tmp)[1]>0&save==FALSE) tmp
         }
         } 
  )
}


cutter<-function(dat,RefkeyIDthread1 = "Reference", RefkeyIDthread2 = "Key", 
                 IDvars = list(Lab = c("LIS"), Med = c("Disp","Pres"), 
                               Dx = c("diag"), Px = c("proc"), 
                               Demo = c("birth", "death", "sex"))){
    Refkeypos <- grep(paste0(RefkeyIDthread1, ".*", RefkeyIDthread2),names(dat), ignore.case = TRUE)%>%
      { if (length(.) > 0) . else 0}
    map2(IDvars, names(IDvars), 
         ~grep(paste0(.x, collapse = "|"),names(dat), ignore.case = TRUE) %>% 
           {if (length(.) > 0) {
             tmp <- select(dat, all_of(Refkeypos), min(.):ncol(dat))
           }})
}

  

chop<-function(fp,RefkeyIDthread1="Reference",RefkeyIDthread2="Key",IDvarfilterIDthread1="LIS",IDvarfilterIDthread2="Description",
                  IDvars=IDvars2,out_fp=getwd(),milliseconds=6,as.parquet=FALSE,csv.sep="\t"){
  options(digits.secs=milliseconds)
  dat<-if(is.character(fp)) {opener(fp)%>%mutate_all(as.character)} else if (is.data.frame(fp)) fp else NULL
  if(!grepl(r"((/$)|(\\$))",out_fp)) {out_fp<-paste0(out_fp,r"(/)",collapse="|")}
  Refkeypos<-grep(paste0(RefkeyIDthread1,".*",RefkeyIDthread2),names(dat),ignore.case=TRUE)%>%{if(length(.)>0) . else 0}
  IDvarfilterIDpos<-grep(paste0(IDvarfilterIDthread1,".*",IDvarfilterIDthread2),names(dat),ignore.case=TRUE)%>%{if(length(.)>0) . else 0}
  if(length(IDvarfilterIDpos)>1) {
    dat<-sticker(dat,names(dat)[IDvarfilterIDpos],outvar=paste("chop :",IDvarfilterIDthread1,IDvarfilterIDthread2),is.numeric=FALSE)
    IDvarfilterID<-paste("chop :",IDvarfilterIDthread1,IDvarfilterIDthread2)
  }else{IDvarfilterID<-names(dat)[IDvarfilterIDpos][1]}
  IDvarfilterID<-ensym(IDvarfilterID)
  map(names(IDvars2),~dir.create(paste0(out_fp,.x),showWarnings=FALSE))
  map2(IDvars2,names(IDvars2),~dat%>%filter(grepl(paste(.x,collapse="|"),!!IDvarfilterID,ignore.case=TRUE))%>%
         {
           tmp<-.
           if (dim(tmp)[1]>0&as.parquet==TRUE) tmp%>%write_parquet(paste0(out_fp,.y,"/",format(Sys.time(), "%Y%m%d_%H%M%OS"),".parquet"))
           if (dim(tmp)[1]>0&as.parquet==FALSE) tmp%>%fwrite(paste0(out_fp,.y,"/",format(Sys.time(), "%Y%m%d_%H%M%OS"),".csv"),sep=csv.sep)
         } 
  )
}

