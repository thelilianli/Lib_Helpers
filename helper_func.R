create_hrmap <- function(hr_dat, id_lst, col_lst = setdiff(names(hr_dat),id_lst)){
  # this function takes in the hr masterfile loops through each id and appends final list for full mapping
  # function assumes latest record is dedupped 
  # hr_dat is hr masterfile
  # id_lst - list of system id cols in hr masterfile
  # col_lst - list of non-id columns to be kept in the final dataset, defaults to all non id cols
  # the join keys must be named differently
  hr_dat <- as.data.frame(hr_dat)
  
  if(length(col_lst)<1){
    col_lst <- setdiff(names(hr_dat),id_lst)
    #print(col_lst)
  }
  
  datalist = list()
  i <- 1
  
  for (id_col in id_lst){
    #print(id_col)
    hr_dat[[id_col]]<- toupper(hr_dat[[id_col]])
    hr_dat1 <- hr_dat[!is.na(hr_dat[[id_col]]) 
                      & !(hr_dat[[id_col]] %in% c(NA,"")),
                      unique(c(id_col,col_lst))]
    names(hr_dat1) <- unique(c("ID",col_lst))
    datalist[[i]] <- hr_dat1
    # print(nrow(hr_dat1))
    i <- i+1
  }
  
  hr_dat <-  do.call(rbind, datalist)
  return(hr_dat)
}

#Merging different columns  
condensed_merge <- function(t1,t2,t1key,t2key){
  #this function helps to full left join two tables with the columns condensed
  #duplicated columns from the secondary tables will be removed before the join
  #the join keys must be named differently
  t1 <- as.data.frame(t1)
  t2 <- as.data.frame(t2)
  Cols <- setdiff(names(t2),names(t1))
  Cols <- match(Cols,colnames(t2))
  t2 <- t2[,Cols]
  
  tf <-merge.data.frame(t1,t2,by.x = t1key,by.y = t2key,all.x = TRUE)#joining data with sales and commissions thresholds for a role and product group
  return(tf)
}

#month period that should load from Database
prev_ym_date <- function(t,n){
  #t is starting yyyymm 
  #n is number of months prior
  #output previous yyyymm
  t <- format(seq(t,by="-1 month",length.out = n)-1,"%Y%m")[n]
  return(t)
  
}

# # Test
# t <- Sys.Date()
# for (n in c(1:10)){
#   print(n)
#   nt <- prev_ym_date(t,n)
#   print(nt)
# }

#calculating previous yyyymm
prev_ym <- function(t,n){
  #t is starting yyyymm 
  #n is number of months prior
  #output previous yyyymm
  #print(t)
  t1 <- as.numeric(as.character(substr(t,1,4)))
  t2 <- as.numeric(as.character(substr(t,5,6)))
  # print (paste0(n,"-previous month"))
  if(t2>11 & ((t2-n)%%12) == 0){
    t2 = 12
    t1 <- t1-((n%/%12))
    t <- paste0(t1,str_pad(as.character(t2),2,pad = "0"))
  }else if(t2<=11 & ((t2-n)%%12) == 0){
    t2 = 12
    t1 <- t1-((n%/%12))-1
    t <- paste0(t1,str_pad(as.character(t2),2,pad = "0"))
  } else if ((t2-n) < 0){
    t1 <- t1-((t2-n)%/%(-12)+1)
    t2 <-(t2-n)%%12
    t <- paste0(t1,str_pad(as.character(t2),2,pad = "0"))}
  else{
    t2 <-(t2-n)%%12
    t <- paste0(t1,str_pad(as.character(t2),2,pad = "0"))}
  #print(t)
  return(t)
}

# # Test
# t <- "202105"
# for (n in c(1:10)){
#   print(n)
#   nt <- prev_ym(t,n)
#   print(nt)
# }

`%ni%` <- Negate(`%in%`)  

#boolean dataframe function to identify NaNs 
is.nan.data.frame <- function(x){
  do.call(cbind, lapply(x, is.nan))}


#clean and trim string (mainly used for conforming Product Names)
conform_str <- function(x){ sub(".*? (.+)", "\\1",tolower(gsub(" ","",
                                                                    str_replace_all(x,"[^[:alnum:]]", ""))),"")}

#rounding all numeric DEs in a dataframe
round_df <- function(df, digits) {
  nums <- vapply(df, is.numeric, FUN.VALUE = logical(1))
  df[,nums] <- round(df[,nums], digits = digits)
  (df)
}

#getting months elapsed
elapsed_months <- function(end_date, start_date) {
  ed <- as.POSIXlt(end_date)
  sd <- as.POSIXlt(start_date)
  12 * (ed$year - sd$year) + (ed$mon - sd$mon)
}

#capitalize the first letter of each word, lower case for else in a string
#can be used using sapply 
simple_cap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), tolower(substring(s, 2)),
        sep="", collapse=" ")
}

#checking file dependancies are available
file_check = function(data_path,pattern_name,totalfile,filetype){
  #data_path is the path that we do pattern reseaching
  #pattern_name is a case sensitive string/list of string that use to detect files and  e.g."ERROR-" is diff from "error-"
  #totalfile is the total number of files that suppose to contained the list of pattern
  #filetype is either "file" or "folder" that use to identify what file type that we searching for
  
  
  #keep the rootpath in a standard format
  rootpath = ifelse(substr(data_path,nchar(data_path),nchar(data_path)) %in% c("/"),substr(data_path,1,nchar(data_path)-1),data_path)
  patterns = pattern_name
  num = totalfile
  file = filetype
  
  j = 0
  file_num = 0
  #split the study by file type
  #loop for "file"
  if (file %in% c("file")){
    folder_list <- list.dirs(rootpath,full.names = TRUE,recursive = TRUE)
    #folders and subfolders searching:
    for (i in folder_list) {
      j = 0
      for (m in patterns){
        #detect all files that contain the patterns,and exclude the system duplicate version of the original file
        data_find <- grep(list.files(i,pattern = m,all.files = FALSE,full.names = TRUE,ignore.case = FALSE),pattern = "~$",inv=T,value=T)
        
        j <- j+length(data_find)
      }
      file_num <- file_num+j
    }
    output <- ifelse(file_num<num,"Missing file,please check","Contain all files that want to search for")
    return(output)
  } else if (file %in% c("folder")){#loop to check how many folders inside the path and what are they
    for (m in patterns) {
      folder_found <- ifelse(dir.exists(paste0(data_path,m)),1,0)
      j <- j + folder_found
    }
    if (j >= num) {
      return("All folders found under the path")
    } else{
      return("Folder is missing, please check")
    }
  } else{
    return("Please select a correct file type")
  }
}
