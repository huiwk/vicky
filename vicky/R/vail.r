
fishhook<-function(fp,format="parquet"){
	fps<-list.files(fp,full.names=TRUE,pattern=format)
	fnames<-map(fps,~open_dataset(.x,format=format)%>%names)%>%unlist%>%unique
	schema(map(fnames,~Field$create(name=.x,type=utf8())))
}
fishnet<-function(fp,format="parquet"){
	subfolders<-list.dirs(fp,full.names=TRUE)
	map(subfolders,~fishhook(.x,format))%>%set_names(subfolders)
}
