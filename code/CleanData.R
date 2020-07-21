library(readxl)

# Import data
#data <- read_excel("data/dataedited.xlsx")
data <- read_excel("data/data.xlsx", sheet = "DataChecked")

# Get the names of ratios
ratios <- as.character(data[1,9:ncol(data)])

# Drop the first 4 rows as  they do not contain useful information
data <- data[4:nrow(data),]

# Drop the next 54  rows, which corresponds to company that stoped listing as the time dimension is limited
data <- data[-c(1:54),]

# Separate companies that are bankrupted and companies that are not
library(dplyr)
firm_bankrupted <- data %>% filter(Status == 1)
br_name <- unique(firm_bankrupted$Ticker)
firm_normal <- data %>% filter(Status == 0)
nm_name <- unique(firm_normal$Ticker)
firm_names <- unique(data$Ticker)

# Drop the columns that are not necessary
data <- data[,-c(1,4,5,6,8)]

# ReOrganise data so that for each ratios in each firm, we have three observations, one for current year and two for last two years
library(foreach)
data_organised <- foreach(i = 1:length(firm_names),.combine = "rbind")%do%{
  dat <- data %>% filter(Ticker == firm_names[i])
  ans <- foreach(ii = 1:3,.combine = "cbind")%do%{
    ans = dat[ii,4:ncol(dat)]
    if(ii == 2){
      colnames(ans) = paste(colnames(ans),"lag1",sep = "_")
    } else if(ii == 3){
      colnames(ans) = paste(colnames(ans),"lag2",sep = "_")
    }
    ans
  }
  ans = cbind(unique(dat$Status),ans)
  colnames(ans) = c("Status",colnames(ans)[-1])
  ans
}

# Change the ratios columns to numeric
data_organised <- as.data.frame(apply(data_organised,2,as.numeric))

# Replace NAs with median
for(i in 3:ncol(data)){
  naLot <- which(is.na(data[,i]))
  if(length(naLot)>0){
    data[naLot,i] = median(data[,i],na.rm = TRUE)
  }
}

# Save data
saveRDS(data,"data/data_raw_cleaned.rds")
saveRDS(data_organised,"data/data_Organised.rds")
rm(list = setdiff(ls(),c("data","data_organised","firm_names","br_name","nm_name","ratios")))
save.image("data/data_cleaned.RData")