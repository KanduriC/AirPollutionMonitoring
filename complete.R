###### Function that reads a directory full of files and reports the number of completely observed cases in each data file. The function returns a data frame where the first column is the name of the file and the second column is the number of complete cases. ###

complete <- function(directory, id=1:332) {
	file_list<-list.files(directory, full.names=T)
	req_files<-file_list[id]
	myfiles<-lapply(req_files,read.csv)
	megafile<-vector()
	for (i in 1:length(myfiles)){
		megafile<-rbind(megafile,myfiles[[i]])
		}
	mega<-na.omit(megafile)
	nob<-cbind(table(mega[,4]))
	nob<-cbind(rownames(nob),nob[,1])
	colnames(nob)=c('id','nobs')
	rownames(nob)=NULL
	nob<-as.data.frame(nob)
	nob$id<-as.numeric(levels(nob$id))[nob$id]
	nob$nobs<-as.numeric(levels(nob$nobs))[nob$nobs]
	orig_id<-vector()
	for(i in 1:length(id)){
		if(length(which(is.element(id[i],nob$id)))>0){
			orig_id[i]=nob[which(is.element(nob$id,id[i])),2]
		}
		else if(length(which(is.element(nob$id,id[i])))==0){
		orig_id[i]=0
		}
	}
	nobs<-orig_id
	nob<-cbind(id,nobs)
	nob<-as.data.frame(nob)
	return(nob)
	}
	