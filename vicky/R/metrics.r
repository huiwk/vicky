Scale<-function(fps){
  fileInfo<-file.info(fps)
  fileNames<-fileInfo %>% rownames
  fileSizes<-mutate(file.info(filepath),size=case_when(
    size<(1024^1)*8 ~ sprintf("%5.2f B",size/((1024^0)*8)),
    size>=(1024^1)*8 & size<(1024^2)*8 ~ sprintf("%5.2f KB",size/((1024^1)*8)),
    size>=(1024^2)*8 & size<(1024^3)*8 ~ sprintf("%5.2f MB",size/((1024^2)*8)),
    size>=(1024^3)*8 & size<(1024^4)*8 ~ sprintf("%5.2f GB",size/((1024^3)*8)),
    size>=(1024^4)*8 & size<(1024^5)*8 ~ sprintf("%5.2f TB",size/((1024^4)*8)),
    size>=(1024^5)*8 & size<(1024^6)*8 ~ sprintf("%5.2f PB",size/((1024^5)*8)),
    size>=(1024^6)*8 & size<(1024^7)*8 ~ sprintf("%5.2f EB",size/((1024^6)*8)),
    TRUE ~ "NA"
  ) %>% trimws)%>% .$size
  fileSizeSummary<-tibble(fileName=fileNames,fileSize=fileSizes)
  LargestFileSize<-fileSizeSummary %>% filter(fileName == fileNames[fileInfo$size %>% which.max])
  list(`Largest File`=LargestFileSize,`File Size Summary`=list(`All files`=fileSizeSummary,`Total Size`=case_when(
    sum(fileInfo$size)<(1024^1)*8 ~ paste0(sum(fileInfo$size)/((1024^0)*8)," B"),
    sum(fileInfo$size)>=(1024^1)*8 & sum(fileInfo$size)<(1024^2)*8 ~ sprintf("%5.2f KB",sum(fileInfo$size)/((1024^1)*8)),
    sum(fileInfo$size)>=(1024^2)*8 & sum(fileInfo$size)<(1024^3)*8 ~ sprintf("%5.2f MB",sum(fileInfo$size)/((1024^2)*8)),
    sum(fileInfo$size)>=(1024^3)*8 & sum(fileInfo$size)<(1024^4)*8 ~ sprintf("%5.2f GB",sum(fileInfo$size)/((1024^3)*8)),
    sum(fileInfo$size)>=(1024^4)*8 & sum(fileInfo$size)<(1024^5)*8 ~ sprintf("%5.2f TB",sum(fileInfo$size)/((1024^4)*8)),
    sum(fileInfo$size)>=(1024^5)*8 & sum(fileInfo$size)<(1024^6)*8 ~ sprintf("%5.2f PB",sum(fileInfo$size)/((1024^5)*8)),
    sum(fileInfo$size)>=(1024^6)*8 & sum(fileInfo$size)<(1024^7)*8 ~ sprintf("%5.2f EB",sum(fileInfo$size)/((1024^6)*8)),
    TRUE ~ "NA"
  )
  ))
}



