find_re<-function(df,re="[[:alpha:]]"){
  df%>%map(grepl,pattern=re,ignore.case=TRUE)%>%{
    t<-.
    name<-t%>%names%>%replace_na("NA")
    t%>%set_names(name)
  }%>%bind_rows%>%mutate_all(replace_na,replace=FALSE)%>%as.matrix%>%which(.,arr.ind=TRUE)
}
subset_df<-function(df,row=0,col=0,rev=FALSE){if(!rev) df[row:dim(df)[1],col:dim(df)[2]] else df[1:row,col:dim(df)[2]]}
clean_xlsheet<-function(dfpath,sheet,keywords){
  df<-read_xlsx(dfpath,col_names=FALSE,sheet=sheet)%>%data.table
  if(df%>%find_re(keywords)%>%length%>%{.==0}) return(NULL)
  df%>%find_re(keywords)%>%{
    minrow<-min(.[,"row"])
    mincol<-min(.[,"col"])
    subset_df(df,minrow,mincol)
  }%>%janitor::row_to_names(row_number=1)%>%
    subset(.,select=which(!duplicated(names(.))))%>%
    {
      {.->tmp}%>%
        find_re()%>%
        as.data.frame()%>%
        filter(col==1)%>%
        select(row)%>%
        {if(dim(.)[1]>0) min(.)}%>%
        {if(length(.)>0) subset_df(tmp,.,0,rev=TRUE) else tmp}
    }
}
name_cleaner<-function(dat){
  colname<-names(dat)
  dat%>%set_names(gsub("[^[:alnum:]]","",colname))
}
clean_xlfile<-function(dfpath,keywords){
  sheets<-readxl::excel_sheets(dfpath)
  map(sheets,~clean_xlsheet(dfpath,sheet=.x,keywords=keywords)%>%{if((dim(.)[1]>0|!is.null(.))%>%any) mutate(.,sheet=.x)})%>%
    rbindlist(fill=TRUE)
}
opener<-function(fp,keywords="Reference Key",as.character=TRUE){
  if(tools::file_ext(fp)=="sav") {read_sav(fp)%>%name_cleaner%>%{if (as.character=TRUE) mutate_all(.,as.character) else .}}
  else if(tools::file_ext(fp)=="xlsx") {clean_xlfile(fp,keywords)%>%{if (as.character=TRUE) mutate_all(.,as.character) else .}}
  else NULL
}
