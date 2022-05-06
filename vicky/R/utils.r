`%clip%`<-function(){
  x <- gsub(r"(\\)", "/", readClipboard(raw = F))
  rstudioapi::insertText(paste0(x,"\n"))
 }
 saveCohort<-function(cohort,fp){
  cohort%>%fwrite(paste0(fp,"\\cohort_",format(Sys.time(),"%d%m%Y"),'.csv'))
}
readCohort<-function(path)
{
  details<-list.files(path,full.names=TRUE)%>%file.info
  details<-details[with(details,order(as.POSIXct(mtime),decreasing=TRUE)),]
  files=rownames(details)
  cohort<<-fread(files[1])%>%tibble%>%mutate_all(as.character)
}
DefineX<-function(name){assign(name,NULL,envir=.GlobalEnv)}
