# Project 2 Part B Alec Hon -----------------------------------------------
# UCLAx Data Science Intensive
library(purrr)
library(dplyr)

#write function to calculate CHCI
calculate_CHCI <- function(percent){
 chci <- (50 * percent[1]/100 + 100 * percent[2]/100 + 120 * percent[3]/100 + 130 * percent[4]/100 + 140 * percent[5]/100 + 190 * percent[6]/100 + 230 *percent[7]/100)
 return(chci)
}

#write function that reads data and adds a CHCI column
data_read_chci<- function(file_name){
  data <- data.frame(read.csv(file_name), stringsAsFactors  = FALSE, skip = 1)
  data <- data[,c(2,3,232,238,242,246,250,254,258,262)]
  data[,c(3:10)] <- sapply(data[,c(3:10) ], as.character)
  data[,c(3:10)] <- sapply(data[,c(3:10) ], as.numeric)
  data$chci <- apply(data[,c(4:10) ], 1, calculate_CHCI)
  data <- data[, c(1:3, 11)]
  return(data)
}

#import all data
data_09 <- data_read_chci('ACS_09_5YR_DP02_with_ann.csv')
colnames(data_09) <- c('ID', 'County', 'pop09', 'chci09')
data_10 <- data_read_chci('ACS_10_5YR_DP02_with_ann.csv')
colnames(data_10) <- c('ID', 'County', 'pop10', 'chci10')
data_11 <- data_read_chci('ACS_11_5YR_DP02_with_ann.csv')
colnames(data_11) <- c('ID', 'County', 'pop11', 'chci11')
data_12 <- data_read_chci('ACS_12_5YR_DP02_with_ann.csv')
colnames(data_12) <- c('ID', 'County', 'pop12', 'chci12')
data_13 <- data_read_chci('ACS_13_5YR_DP02_with_ann.csv')
colnames(data_13) <- c('ID', 'County', 'pop13', 'chci13')
data_14 <- data_read_chci('ACS_14_5YR_DP02_with_ann.csv')
colnames(data_14) <- c('ID', 'County', 'pop14', 'chci14')
data_15 <- data_read_chci('ACS_15_5YR_DP02_with_ann.csv')
colnames(data_15) <- c('ID', 'County', 'pop15', 'chci15')
data_16 <- data_read_chci('ACS_16_5YR_DP02_with_ann.csv')
colnames(data_16) <- c('ID', 'County', 'pop16', 'chci16')

#combine all dataframes into one dataframe, reorder
comb_data <- Reduce(function(x, y) merge(x, y, all=T, by=c("ID", "County")), list(data_09, data_10, data_11, data_12, data_13, data_14, data_15, data_16), accumulate=F)
comb_data <- comb_data[c('ID', 'County', 'chci09', 'chci10', 'chci11', 'chci12', 'chci13', 'chci14', 'chci15', 'chci16', 'pop09', 'pop10', 'pop11', 'pop12', 'pop13', 'pop14', 'pop15','pop16')]  

#add chcig and chcigr columns
comb_data$chcig <- comb_data$chci16 - comb_data$chci09
comb_data$chcigr <- comb_data$chcig/comb_data$chci09

#add popg and popgr columns
comb_data$popg <- comb_data$pop16 - comb_data$pop09
comb_data$popgr <- comb_data$popg/comb_data$pop09

#write csv file
write.csv(comb_data, 'chci_Alec_Hon.csv')

#get top and bottom 20 data
chci2016_top20 <- arrange(comb_data, desc(chci16))[c(1:20),]
chcig_top20 <- arrange(comb_data, desc(chcig))[c(1:20),]
pop2016_top20 <- arrange(comb_data, desc(pop16))[c(1:20),]
popg_top20 <-arrange(comb_data, desc(popg))[c(1:20),]
popgr_top20 <- arrange(comb_data, desc(popgr))[c(1:20),]

chci2016_bottom20 <- arrange(comb_data, chci16)[c(1:20),]
chcig_bottom20 <- arrange(comb_data, chcig)[c(1:20),]
popg_bottom20 <-arrange(comb_data, popg)[c(1:20),]
popgr_bottom20 <- arrange(comb_data, popgr)[c(1:20),]

#arrange final csv
final_output <- rbind(chci2016_top20,NA,chcig_top20,NA,chcig_bottom20,NA,chci2016_bottom20,NA, pop2016_top20,NA,popg_top20,NA,popgr_top20,NA,popg_bottom20,NA,popgr_bottom20)
#write csv
write.csv(final_output, 'chci_top20_Alec_Hon.csv')


