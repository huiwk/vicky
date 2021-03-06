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
DefineX<-function(name){map(name,~assign(.x,NULL,envir=.GlobalEnv))}
WipeX<-function(namelist){rm(list=namelist,envir=.GlobalEnv)}
sticker<-function(dat,var,outvar="Paste",is.numeric=TRUE,invert=FALSE){
  dat%>%{
    tmp<-.
    if(is.numeric) {tmp%>%{if(invert) mutate(!!outvar:=coalesce(!!!select(tmp,-var))) else mutate(!!outvar:=coalesce(!!!select(tmp,var)))}}
    else {{if(invert) unite(tmp,!!outvar,-var,remove=FALSE,sep=" ") else unite(tmp,!!outvar,var,remove=FALSE,sep=" ")}}
  }
}
IDvarEncoder<-function(fp,groupvar,outvar="Paste"){
  groupvar<-ensym(groupvar)
  outvar<-ensym(outvar)
  dat<-if(is.character(fp)) {opener(fp)%>%mutate_all(as.character)} else if (is.data.frame(fp)) fp else NULL
  dat<-sticker(dat,groupvar,outvar,is.numeric=FALSE,invert=TRUE)
  dat%>%by(.,dat[[groupvar]],identity)%>%map(~.x%>%select(outvar)%>%unlist(use.names=FALSE)%>%unique%>%
                                               gsub(pattern = "([[:punct:]])",replacement="\\\\\\1",.))
}
