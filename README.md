## R functions to interact with the air pollution monitoring data

## Background
The specdata directory contains 332 comma-separated-value (CSV) files containing pollution monitoring data for fine particulate matter (PM) air pollution at 332 locations in the United States. Each file contains data from a single monitor and the ID number for each monitor is contained in the file name. For example, data for monitor 200 is contained in the file "200.csv". Each file contains three variables:  
Date: the date of the observation in YYYY-MM-DD format (year-month-day)  
sulfate: the level of sulfate PM in the air on that date (measured in micrograms per cubic meter)  
nitrate: the level of nitrate PM in the air on that date (measured in micrograms per cubic meter)  
In each file there are many days where either sulfate or nitrate (or both) are missing (coded as NA). This is common with air pollution monitoring data in the United States.

## R functions 

Here are three functions (pollutantmean, complete and corr) that interact with the dataset.

The first function 'pollutantmean' calculates the mean of a pollutant (sulfate or nitrate) across a specified list of monitors. The function takes three arguments: 'directory', 'pollutant', and 'id'. Given a vector monitor ID numbers, 'pollutantmean' reads that monitors' particulate matter data from the directory specified in the 'directory' argument and returns the mean of the pollutant across all of the monitors, ignoring any missing values coded as NA.

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

The second function 'complete' reads a directory full of files and reports the number of completely observed cases in each data file. The function returns a data frame where the first column is the name of the file and the second column is the number of complete cases.

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

The third function 'corr' takes a directory of data files and a threshold for complete cases and calculates the correlation between sulfate and nitrate for monitor locations where the number of completely observed cases (on all variables) is greater than the threshold. The function returns a vector of correlations for the monitors that meet the threshold requirement. If no monitors meet the threshold requirement, then the function returns a numeric vector of length 0. 

	corr <- function(directory, threshold=0) {
		file_list<-list.files(directory,full.names=T)
		cases<-complete(directory,1:length(file_list))
		req_indices<-which(cases$nobs>threshold)
		if (length(req_indices)<2){
			corr<-0
		}
		else {
			myfiles<-lapply(file_list[req_indices],read.csv)
			cor_comp<-function(dataframe){
				dataframe<-dataframe[,2:3]
				corred<-cor(dataframe[,1],dataframe[,2],use='na.or.complete')
				return(corred)
			}
			corr<-lapply(myfiles,cor_comp)
			corr<-unlist(corr)
		}
		return(corr)
	}

