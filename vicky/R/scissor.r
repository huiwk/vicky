
scissor<-function(fp,RefkeyIDthread1="Reference",RefkeyIDthread2="Key",IDvars=list(Lab=c("LIS"),Med=c("Disp","Pres"),Dx=c("diag"),
  Px=c("proc"),Demo=c("birth","death","sex")),out_fp=getwd(),milliseconds=6){
  options(digits.secs=milliseconds)
  dat<-read_parquet(fp)%>%mutate_all(as.character)
  if(!grepl(r"((/$)|(\\$))",out_fp)) {out_fp<-paste0(out_fp,r"(/)",collapse="|")}
  Refkeypos<-grep(paste0(RefkeyIDthread1,".*",RefkeyIDthread2),names(dat),ignore.case=TRUE)%>%{if(length(.)>0) . else 0}
  map(names(IDvars),~dir.create(paste0(out_fp,.x),showWarnings=FALSE))
  map2(IDvars,names(IDvars),~grep(paste0(.x,collapse="|"),names(dat),ignore.case=TRUE)%>%
         {if(length(.)>0) select(dat,all_of(Refkeypos),min(.):ncol(dat))%>%
            {if (dim(.)[1]>0) .%>%write_parquet(paste0(out_fp,.y,"/",format(Sys.time(), "%Y%m%d_%H%M%OS"),".parquet"))} 
         })
}
