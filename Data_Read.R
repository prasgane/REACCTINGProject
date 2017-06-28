#Program that complies lab generated reference CO to measure CO. Complication generated from #inconsistent file format and file name. Also account for time difference between Ghana and Boulder, #CO.
setwd("~LocalDir")


weird_file_format_flag = 1

time_diff_flag = 6

myfiles = list.files(pattern = '.txt')
myfiles = myfiles[substr(myfiles, nchar(myfiles)-2, nchar(myfiles)) == 'txt']
myfiles = myfiles[substr(myfiles, 1, 6) == 'Lascar']
lascar_number = substr(myfiles,10,11)
file_names = paste("dataset",lascar_number,sep = '')
name_dataset = "dataset"


for (file in myfiles)
{
  for (i in 1:length(file_names)) 
  {
    if (exists(file_names[i]) && substr(file,10,11) == substr(file_names[i],8,9) )
    {
      if (weird_file_format_flag == 0)
      {
        temp_dataset = read.table(file, header = TRUE,sep=',')
        
      }
      if(weird_file_format_flag == 1)
      {
        header = read.table(file, nrows = 1, header = FALSE, sep =',', stringsAsFactors = FALSE)
        header$V1 = substr(file,1,11)
        header$V4 = NULL
        header$V5 = NULL
        header$V6 = NULL
        header$V7 = NULL
        temp_dataset <- read.table(file, skip = 3, header = FALSE, sep =',')
        colnames( temp_dataset ) <- unlist(header)
        
      }
      colnames(temp_dataset)[1] = substr(file,1,11)
      colnames(temp_dataset)[3] = "CO.ppm"
      if(ncol(temp_dataset) > 3){
        temp_dataset[,4:ncol(temp_dataset)] = NULL
      }
      temp_dataset$seconds = 0
      temp = rbind(eval(parse(text = file_names[i])),temp_dataset)
      assign(file_names[i],temp)
    }
    
    
    if (!exists(file_names[i]) && substr(file,10,11) == substr(file_names[i],8,9))
    {
      if (weird_file_format_flag == 0)
        {
          temp = read.table(file, header = TRUE,sep=',')
        }
      if(weird_file_format_flag == 1)
      {
        header = read.table(file, nrows = 1, header = FALSE, sep =',', stringsAsFactors = FALSE)
        header$V4 = NULL
        header$V5 = NULL
        header$V6 = NULL
        header$V7 = NULL
        temp <- read.table(file, skip = 3, header = FALSE, sep =',')
        colnames( temp ) <- unlist(header)
        
      }
      colnames(temp)[1] = substr(file,1,11)
      colnames(temp)[3] = "CO.ppm"
      if(ncol(temp) > 3){
        temp[,4:ncol(temp)] = NULL
      }
      temp$seconds = 0
      assign(file_names[i],temp)
    }
    
  }
  
}

data_names = ls()
data_names = data_names[substr(data_names,1,7)=="dataset"]

for (i in 1:length(file_names)){
  x = unique(eval(parse(text = file_names[i])))
  x$seconds = as.numeric(as.POSIXct(x$Time))
  assign(file_names[i],x)
}
rm(temp,temp_dataset,x,header)


csv_files = list.files(pattern="*.csv")
csv_files = csv_files[substr(csv_files, nchar(csv_files)-2, nchar(csv_files)) == 'csv']

if(!is.null(csv_files))
{
  for (file in csv_files)
    {
  
      if (exists("ref_data"))
        {
          temp_data <-read.table(file, header=TRUE, sep=",")
          if (ncol(temp_data)>2)
            {
              temp_data[,3:ncol(temp_data)] = NULL
            }
          temp_data$seconds = as.numeric(as.POSIXct(strptime(temp_data$Datetime,format = "%m/%d/%Y %H:%M")))
          ref_data<-rbind(ref_data, temp_data)
    
        }
  
  # if the merged dataset doesn't exist, create it
      if (!exists("ref_data"))
        {
          ref_data <- read.table(file, header=TRUE, sep=",")
          if (ncol(ref_data)>2)
            {
              ref_data[,3:ncol(ref_data)] = list(NULL)
            }
          ref_data$seconds = as.numeric(as.POSIXct(strptime(ref_data$Date,format = "%m/%d/%Y %H:%M")))
    
        }
  # if the merged dataset does exist, append to it
  }
}
ref_data = unique(ref_data)
row.names(ref_data) <- 1:nrow(ref_data)
rm(temp_data)

ref_data = ref_data[!is.na(ref_data$CO_ppm),]
row.names(ref_data) <- 1:nrow(ref_data)

bad.time.index = which(is.na(ref_data$seconds))
ref_data$seconds[bad.time.index] = as.numeric(as.POSIXct(strptime(ref_data$Datetime[bad.time.index],format = "%m-%d-%y %H:%M")))

#as.POSIXct(1403552033,origin = "1970-01-01") ## Converts seconds to time again

for(i in 1:length(data_names))
{
  d = eval(parse(text = data_names[i]))
  bad.time.d = which(d$seconds<0)
  d$seconds[bad.time.d] = as.numeric(as.POSIXct(strptime(d$Time,format = "%d/%m/%Y %H:%M")))
  d$seconds = d$seconds - time_diff_flag*60*60 #
  assign(data_names[i],d)
}

df <- data.frame(lascar_number=integer(),
                 COLascar=integer(),
                 lascartime=double(),
                 refCO=integer(),
                 reftime=double(),
                 time.diff = integer(),
                 CO.diff = integer()
                )



min_val_ref = min(ref_data$seconds,na.rm = TRUE) 
max_val_ref = max(ref_data$seconds,na.rm = TRUE) 


l = 1
for (i in 1:length(data_names))
{
  d = eval(parse(text = data_names[i]))
  min_val_d = min(d$seconds,na.rm = TRUE)
  max_val_d = max(d$seconds,na.rm = TRUE)
  min_val = max(c(min_val_d, min_val_ref)) 
  max_val = min(c(max_val_d,max_val_ref))
  new_val_d = d[which(d$seconds <= max_val & d$seconds>=min_val),]
  new_val_ref = ref_data[which(ref_data$seconds <= max_val & ref_data$seconds>=min_val),]
  row.names(new_val_d) = 1:nrow(new_val_d)
  row.names(new_val_ref) = 1:nrow(new_val_ref)
  for (j in 1:nrow(new_val_d))
  {
      diffs = abs(new_val_d$seconds[j]-new_val_ref$seconds)
      index = which.min(diffs)
      if(diffs[index] <= 60){
        df[l,1] = substr(data_names[i],8,9)
        df[l,2] = new_val_d$CO.ppm[j]
        df[l,3] = new_val_d$seconds[j]
        df[l,4] = mean(new_val_ref$CO_ppm[new_val_ref$seconds[index]-30 <= new_val_ref$seconds & new_val_ref$seconds <= new_val_ref$seconds[index]+30]) 
        df[l,5] = new_val_ref$seconds[index]
        df[l,6] = (df[l,3]-df[l,5])
        df[l,7] = (df[l,2] - df[l,4])
        l=l+1
    }
  }
}
df = unique(df)
df$DateTime = as.POSIXct(df$lascartime,origin = "1970-01-01")
if (!exists("compiled_data"))
{
  compiled_data = data.frame(lascar_number=integer(),
                             COLascar=integer(),
                             refCO=integer(),
                             reftime=double(),
                             lascartime=double(),
                             lascarDate = double(),
                             refDate = double()
  )
  compiled_data = rbind(compiled_data,df)
  
} else
{
  compiled_data = rbind(compiled_data,df)
}

#compiled_data$DateTime = as.POSIXct(compiled_data$lascartime,origin = "1970-01-01")
rm(list=setdiff(ls(), "compiled_data"))

split_data = split(compiled_data,compiled_data$lascar_number)

lapply(names(split_data), function(x){write.table(split_data[[x]], file = paste("output", x,".txt" ,sep = " "))})

write.csv(compiled_data, file = "Compiled_data.csv")
