######## Function that calculates the mean of a pollutant (sulfate or nitrate) across a specified list of monitors. The function 'pollutantmean' takes three arguments: 'directory', 'pollutant', and 'id'. Given a vector monitor ID numbers, 'pollutantmean' reads that monitors' particulate matter data from the directory specified in the 'directory' argument and returns the mean of the pollutant across all of the monitors, ignoring any missing values coded as NA. ###

pollutantmean <- function(directory, pollutant, id=1:332) {
	setwd(directory)
	file_list<-list.files()
	req_files<-file_list[id]
	myfiles<-lapply(req_files,read.csv)
	megafile<-vector()
	for (i in 1:length(myfiles)){
		megafile<-rbind(megafile,myfiles[[i]])
		}
	if(pollutant=='sulfate'){
		pollutant_mean<-mean(megafile[,2],na.rm=T)
		}
	else if(pollutant=='nitrate') {
		pollutant_mean<-mean(megafile[,3],na.rm=T)
		}
	return(pollutant_mean)
}