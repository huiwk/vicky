dts_align<-function(dat,type,refid=`Reference Key`,startid=`Prescription Start Date`,endid=`Prescription End Date`){
  type<-if(hasArg(type)) ensym(type)
  refid<-ensym(refid)
  startid<-ensym(startid)
  endid<-ensym(endid)
  refidname<-expr(!!refid)%>%as.character
  startidname<-expr(!!startid)%>%as.character
  endidname<-expr(!!endid)%>%as.character
  dat%>%distinct(.keep_all=T)%>%arrange(across(contains(refidname)|contains(startidname)|contains(endidname)))%>%{if(!is.null(type)) group_by(.,!!refid,!!type) else group_by(.,!!refid)}%>%
    mutate(`Last End Date`=lag(!!endid)%>%as.Date(),
           `Min Start Date`=min(!!startid)%>%as.Date(),
           `Accumulated Last End Date`=cummax((`Last End Date`%>%as.numeric%>%ifelse(is.na(.),-999,.)))%>%ifelse(.==-999,min(!!endid),.)%>%as.Date,
           `Aligned Prescription Start Date`=ifelse((startid<`Accumulated Last End Date`)&(startid>`Min Start Date`),`Accumulated Last End Date`,!!startid)%>%as.Date,
           `Drug Duration`=time_length(interval(`Aligned Prescription Start Date`,!!endid),"day"),
           `Accumulated Drug Duration`=cumsum(ifelse(`Drug Duration`<0,0,`Drug Duration`)))
}
