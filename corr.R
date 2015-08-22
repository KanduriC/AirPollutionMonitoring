### Function that takes a directory of data files and a threshold for complete cases and calculates the correlation between sulfate and nitrate for monitor locations where the number of completely observed cases (on all variables) is greater than the threshold. The function returns a vector of correlations for the monitors that meet the threshold requirement. If no monitors meet the threshold requirement, then the function returns a numeric vector of length 0. ###

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

