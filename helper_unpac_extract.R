#the following are functions used for batch 
#field mapping, QA, printing system files to match with Interface agreement 

########################################################
#Auto Template Mapping - need to follow standard format   
########################################################
Map_Template <- function(dataf,Orig_Temp,Source_lb,QA_Path){
  #dataf is the data frame/table to be mapped
  #Orig_Temp is the original Template File
  #Source_lb is a label for the data source e.g. CMS_Client
  
  #Must look like this: 
  #Map_Template.csv
  
  dat <- NULL
  dat <- dataf
  dat <- as.data.frame(dat)
  
  lb <- NULL
  lb <- as.character(Source_lb)
  
  Data_Temp <- NULL
  Data_Temp <- Orig_Temp
  Data_Temp <- Orig_Temp[,c("sys_field_name",paste0(lb,"_Mapped_Field"))]
  names(Data_Temp) <- c("sys_field_name","Mapped_Field")
  assign(paste0(lb,"_Temp"),Data_Temp,envir = .GlobalEnv)
  
  #treating for Additonal Details
  Data_Temp$Detail_Label <- ifelse(Data_Temp$Mapped_Field==gsub( ": .*$", "",Data_Temp$Mapped_Field),
                                   NA,gsub( ": .*$", "",Data_Temp$Mapped_Field))
  Data_Temp$Detail_Label <-trimws(Data_Temp$Detail_Label)
  Data_Temp$Detail_Value <- trimws(sub(".*?: (.+)", "\\1",Data_Temp$Mapped_Field))
  
  #adding in Additional Details - 'Label: Value'
  Additional_Details <- Data_Temp$Detail_Value[!(Data_Temp$Detail_Label %in% c(NA,""))]
  if(length(Additional_Details)>0){
    for (field in Additional_Details){
      print(paste0("Adding ",field," to Additional Details:"))
      Label_n <- Data_Temp$Detail_Label[(Data_Temp$Detail_Value %in% field)][1]
      dat[[field]] <- ifelse(dat[[field]] %in% c("",NA,0),
                             "",
                             paste0(Label_n,": ",dat[[field]]))
      print(head(unique(dat[[field]])))
    }}
  
  Data_Temp$Mapped_Field <- Data_Temp$Detail_Value
  Data_Temp$Mapped_Field <- trimws(Data_Temp$Mapped_Field)
  Data_Temp <- Data_Temp[!(Data_Temp$Mapped_Field %in% c(NA,"")),]
  Data_Temp$sys_field_name[Data_Temp$sys_field_name %in% c(NA,"")] <- Data_Temp$Mapped_Field[Data_Temp$sys_field_name %in% c(NA,"")]
  
  #checking for missing field names in Mapping
  if(length(setdiff(names(dat),Data_Temp$Mapped_Field))>0){
    q <- as.data.frame(setdiff(names(dat),Data_Temp$Mapped_Field))
    names(q) <- "DE_Missing_in_Mapping"
    fwrite(q,paste0(QA_Path,lb,"-Add_DE_To_Mapping(Optional).csv"))
  }
  
  #checking for incorrect field names in Mapping
  if(length(setdiff(Data_Temp$Mapped_Field,names(dat)))>0){
    q <- as.data.frame(setdiff(Data_Temp$Mapped_Field,names(dat)))
    names(q) <- "Wrong_DE_Mapped"
    fwrite(q,paste0(QA_Path,"ERROR-",lb,"-Wrong_Mapped_DE.csv"))
  }
  
  #Renaming Fields for Consolidation
  dat_Renamed <- dat[,c(Data_Temp$Mapped_Field)]
  names(dat_Renamed) <- Data_Temp$sys_field_name
  print(paste0(lb,"_Renamed"))
  assign(paste0(lb,"_Renamed"),dat_Renamed,envir = .GlobalEnv)
  #View(head(get(paste0(lb,"_Renamed"))))
  #return(dat_Renamed)
}



########################################################
#Auto Template data type control conversion Mapping - need to follow standard format   
########################################################
Conform_DT_Template <- function(dataf_name,Orig_Temp,QA_Path){
  #dataf is the data frame/table to be mapped
  #Orig_Temp is the original Template File
  #Data_Type is cleaning for selected data types - default is all
  # not implemented Data_Types = "ALL"
  #Accepted data types = Boolean, Text, Numeric, Date
  
  #Must look like this: 
  #Map_Template.csv
  
  #Solves for the following:
  #Checks Mandatory fields
  # in_review_Template = Y then apply:
  # Bool -> Text [1 = "Yes", 0 = "No"]
  # Bool -> Bool [1 = "No", 0 = "Yes"] #inverse***
  # Bool acceptable values = "Yes","No", NA
  #significant digits, control sizes -" convert all to string
  #convert date formats: mm/dd/yyyy
  #converts data types
  #returns cleaned and converted data
  TXN_Source <- dataf_name#print(deparse(substitute(dataf_name)))
  
  dat <- NULL
  dat <- get(dataf_name)
  dat <- as.data.frame(dat)
  
  
  Data_Temp <- NULL
  Data_Temp <- Orig_Temp[,c("sys_order","sys_expected","sys_source_file",
                            "sys_field_name","sys_number","sys_data_type",
                            "orig_data_type","sys_size","sys_format","m_o",
                            "sys_expected_values","sys_description")] 
  
  all_mand_fields <- Data_Temp$sys_field_name[Data_Temp$`m_o` %in% "m"]
  
  Data_Temp <- Data_Temp[Data_Temp$sys_field_name %in% names(dat),]
  Data_Temp$Size_Control <- as.numeric(gsub( "\\(.*$", "",Data_Temp$sys_size))
  Data_Temp$Size_Control <- ifelse(Data_Temp$`sys_data_type` %in% "Numeric" & grepl("\\(",Data_Temp$sys_size),
                                   Data_Temp$Size_Control+3,#decimal value = +3 character lengths 
                                   Data_Temp$Size_Control)
  #assume max length for empty size records
  Data_Temp$Size_Control[Data_Temp$Size_Control %in% c("",NA)] <- 250
  
  # Checking Mandatory Values:
  missing_mand_fields <- NULL
  missing_mand_fields <- setdiff(all_mand_fields,names(dat))
  
  if(length(missing_mand_fields)>0){
    fwrite(as.data.frame(missing_mand_fields),paste0(QA_Path,"ERROR-",TXN_Source,"-MF_Missing.csv"))
  }
  
  mand_fields <- dat[,names(dat) %in% Data_Temp$sys_field_name[Data_Temp$`m_o` %in% "m"]]
  mandatory_error <- names(mand_fields)[which(sapply(mand_fields, function(x) sum(as.character(x) %in% c("",NA))>0))]
  for(field in mandatory_error){
    print(paste0("MF for ",field))
    mand_error <- dat[dat[[field]] %in% c(NA,""),]
    fwrite(mand_error,paste0(QA_Path,"ERROR-",TXN_Source,"-",field,"-MF.csv"))
  }
  
  # Coverting date format:
  #dat[,names(dat)[which(sapply(dat,is.Date))]]
  for(field in Data_Temp$sys_field_name[Data_Temp$sys_data_type %in% "Date"]){
    print(paste0("Date for ",field))
    if(is.Date(dat[[field]])){
      dat[[field]] <- format(dat[[field]],"%m/%d/%Y")
    }else{
      #QA_Path File with Date needs to be converted
      #dates should be converted into date fomat from txn source code
      fwrite(head(dat),paste0(QA_Path,"ERROR-",TXN_Source,"-",field,"-ConvertDate.csv"))
    }
    #sapply(data, function(x) !all(is.na(as.Date(as.character(x),format="%d/%m/%Y"))))
  }
  
  # Bool -> Bool [1 = "No", 0 = "Yes"] #inverse*** due to field renaming
  for(field in Data_Temp$sys_field_name[Data_Temp$sys_data_type %in% "Boolean"  
                                         & Data_Temp$orig_data_type %in% "Boolean"]){
    dat[[field]] <- ifelse(dat[[field]] %in% c("1",1)|as.numeric(dat[[field]])>=1,
                           "No","Yes")
  }
  
  # Bool -> Text [1 = "Yes", 0 = "No"] #ifelse - keeps field value
  for(field in Data_Temp$sys_field_name[Data_Temp$sys_data_type %in% "Text"  
                                         & Data_Temp$orig_data_type %in% "Boolean"]){
    dat[[field]] <- ifelse(dat[[field]] %in% c("0",0)|as.numeric(dat[[field]])<1,
                           "No",
                           ifelse(dat[[field]] %in% c("1",1,"Y")|as.numeric(dat[[field]])>=1,
                                  "Yes",dat[[field]]))
  }
  
  # full file lengths check (not accounting for decimals)
  dat_len <- as.data.frame(unlist(lapply(dat, function(x) max(nchar(as.character(x)),na.rm = T))))
  names(dat_len) <- "max_field_length"
  dat_len$field <- rownames(dat_len)
  dat_len <- merge.data.frame(dat_len,Data_Temp[,c("sys_field_name","Size_Control")], by.x = "field",by.y = "sys_field_name",all.x = TRUE)
  dat_len$Size_Control <- ifelse(is.na(dat_len$Size_Control),250,dat_len$Size_Control)
  dat_len$ERROR <- dat_len$Size_Control <= dat_len$max_field_length
  
  if(nrow(dat_len[dat_len$ERROR %in% c(TRUE,NA),])>0){
    
    if(sum(dat_len$field[dat_len$ERROR %in% c(TRUE,NA)] %in% all_mand_fields)>0) {
      msg <- "ERROR-"
    } else {
      msg <- "WARNING-"
    }
    fwrite(dat_len[dat_len$ERROR %in% c(TRUE,NA),],paste0(QA_Path,msg,TXN_Source,"-LengthExceedsMax.csv"))
  }
  View(dat_len)
  
  # lengths control:
  print("Running Lengths Control")
  
  for(field in Data_Temp$sys_field_name){
    #print(paste0("length for ",field))
    sz <- Data_Temp$Size_Control[Data_Temp$sys_field_name %in% field]
    
    if(field %in% Data_Temp$sys_field_name[Data_Temp$sys_data_type %in% "Numeric" 
                                            & grepl("\\(",Data_Temp$sys_size)]){
      #numeric with decimals - round to 2 decimals
      dat[[field]] <- round(as.numeric(dat[[field]]), digits=2)
      #sz <- sz+3 #3 extra digits = 1 decimal and 2 decimal places when converted into string
    } else if(field %in% Data_Temp$sys_field_name[Data_Temp$sys_data_type %in% "Numeric"]){
      #numeric with decimals - round to 2 decimals
      
      if(max(nchar(na.replace(as.character(dat[[field]]),"")))<15){
        #if nchar is greater than 15 significant digits, R will be restricted by the numeric data conversion (int64 must be used)
        dat[[field]] <- round(as.numeric(dat[[field]]), digits=0)
      }
      
      #sz <- sz+3 #3 extra digits = 1 decimal and 2 decimal places when converted into string
    }
    
    dat[[field]] <- trim(as.character(dat[[field]]))
    dat[[field]] <- substr(dat[[field]],1,sz)
  }
  
  return(dat)
}

########################################################
#Auto File Pattern Mapping - make sure unwanted file with a specific pattern does not exist before next step   
########################################################
pattern_searching = function(data_path,pattern_name){
  #data_path is the path that we do pattern reseaching
  #pattern_name is a string that use to detect files that contain the pattern e.g."ERROR-"
  
  rootpath = ifelse(substr(data_path,nchar(data_path),nchar(data_path)) %in% c("/"),substr(data_path,1,nchar(data_path)-1),data_path)
  patterns = pattern_name
  #loop to check how many folders inside the path and what are they
  folder_list <- list.dirs(rootpath,full.names = TRUE)
  
  file_num = 0
  j = 0
  #folders and subfolders searching:
  for (i in folder_list){
    for (m in patterns) {
      #detect all files that contain the patterns
      data_find <- grep(list.files(i,pattern = m,all.files = FALSE,full.names = TRUE,ignore.case = FALSE),pattern = "~$",inv=T,value=T)
      j <- j+length(data_find)
    }
    file_num <- file_num+j
  }
  #"True" - There is file contained the required pattern
  #"False" - There is not file contained the required pattern
  output <- ifelse(file_num>0,TRUE,FALSE)
  return(output)
  #write out all file names contain the patterns
  #data_loop[[j]] <- datalist
  #j = j+1
}

#test
#oPath <- "//CBMCC-FN-00021B.ad.cibc.com/BC_ANLYTCS/Development/Sales Monitoring/Channel/WFMT- All Channels/201810/R Output/Migration/"
#pattern_searching(oPath,"ERROR-")

########################################################
#create headers and trailers and format to final expected ingestion file
#used for employee, transaction, metric, trigger ONLY***
########################################################

PrintSystemFiles <- function(TableF,Load_Type,Max_Size,Business_DateMMMyyyy,FileName,OutPath,OutFileName = FileName){
  #this function is to print out the system required files
  #adds header and trailer row and removes field names, these are assumed to be static files
  #TableF = input data table, HeaderF = header template, TrailerF = trailer template
  #Load_Type = MASTER,DELTA (which type does the output start with?)
  #Max_Size is the size restriction for the file type upon system load
  #MMMyyyy = data month year
  #FileName = output file name, OutPath = directory of output
  # emp_size <- 20000
  # txn_size <- 2000000
  # metrics_size <- 5000000
  # trigger_size <- 1000
  
  File_Creation_Date <- as.character(format(Sys.Date(),format = "%Y%m%d"))
  File_Creation_Time <- paste0(gsub(":","",unlist(strsplit(as.character(Sys.time())," "))[[2]]),"00")
  
  Header <- c("HDR", paste0(FileName,".txt"),File_Creation_Date,File_Creation_Time,"Confidential",Load_Type,Business_DateMMMyyyy)
  Footer <- c("TLR",nrow(TableF)+2)
  Header <- data.frame(t(data.frame(Header)))
  Footer <- data.frame(t(data.frame(Footer)))
  names(Header) <- NULL
  names(Footer) <- NULL
  
  Num_Subsets <- ceiling(nrow(TableF)/Max_Size)
  N_Subset_Size <- ceiling(nrow(TableF)/Num_Subsets)
  
  #initializing start and end
  start_row <- 1
  end_row <- N_Subset_Size
  
  for (i in 1:Num_Subsets){
    
    end_row <- N_Subset_Size*i
    end_row <- min(end_row,nrow(TableF))
    print(i)
    print(paste0(start_row,":",end_row))
    
    TableF_i <- TableF[start_row:end_row,]
    OutFileName_i <- paste0(OutFileName,"_",i,".txt")
    
    if(Load_Type %in% c("MASTER","DELTA")){
      
      #fwrite(Header, file = file_name, sep = "|")
      file_name <- paste0(OutPath, OutFileName_i)
      
      fwrite(Header, file = file_name, sep = "|")
      write.table(TableF_i, file = file_name, sep = "|", na = "", quote = FALSE, col.names = FALSE, row.names = FALSE, append = TRUE)
      write.table(Footer, file = file_name, sep = "|", na = "", quote = FALSE, col.names = FALSE, row.names = FALSE, append = TRUE)
      
      start_row <- end_row+1
      
    }else{print("ERROR CHECK LOAD TYPE INPUT")}
    
  }
  
}



########################################################
#create headers and trailers and format to final expected ingestion file
#used for migration ONLY***
########################################################

MPrintSystemFiles <- function(TableF,Max_Size,FileName,OutPath,OutFileName = FileName){
  #this function is to print out the system required files
  #adds header and trailer row and removes field names, these are assumed to be static files
  #TableF = input data table, HeaderF = header template, TrailerF = trailer template
  #Load_Type = MASTER,DELTA
  #MMMyyyy = data month year
  #FileName = output file name, OutPath = directory of output
  
  File_Creation_Date <- as.character(format(Sys.Date(),format = "%Y%m%d"))
  File_Creation_Time <- paste0(gsub(":","",unlist(strsplit(as.character(Sys.time())," "))[[2]]),"00")
  print(nrow(TableF))
  
  Header <- c("HDR", paste0(FileName,".txt"),File_Creation_Date,File_Creation_Time,"Confidential")
  Footer <- c("TLR",nrow(TableF)+2)
  Header <- data.frame(t(data.frame(Header)))
  Footer <- data.frame(t(data.frame(Footer)))
  names(Header) <- NULL
  names(Footer) <- NULL
  
  Num_Subsets <- ceiling(nrow(TableF)/Max_Size)
  N_Subset_Size <- ceiling(nrow(TableF)/Num_Subsets)
  
  #initializing start and end
  start_row <- 1
  end_row <- N_Subset_Size
  
  for (i in 1:Num_Subsets){  
    end_row <- N_Subset_Size*i
    end_row <- min(end_row,nrow(TableF))
    print(i)
    print(paste0(start_row,":",end_row))
    
    TableF_i <- TableF[start_row:end_row,]
    OutFileName_i <- paste0(OutFileName,"_",i,".txt")
    file_name <- paste0(OutPath, OutFileName_i)
    
    fwrite(Header, file = file_name, sep = "|")
    write.table(TableF_i, file = file_name, sep = "|", na = "", quote = FALSE, col.names = FALSE, row.names = FALSE, append = TRUE)
    write.table(Footer, file = file_name, sep = "|", na = "", quote = FALSE, col.names = FALSE, row.names = FALSE, append = TRUE)
    
    start_row <- end_row+1
  }
  
}

########################################################
#Auto QA Tests for Transformed TXN - need to follow standard format   
########################################################

QA_test_txn <- function(Product_Category,date_start,date_end,fxn,emp_fn,txn_fn,operator_id,emp_dataf,ID_emp_fn,txn_dataf, Orig_Temp, QA_Path){
  ##################
  #Product_Category is the individual product category name or a list of product category names
  #date_start is the date of the start of the observation range (inclusive)
  #date_end is the date of the end of the observation range (not inclusive)
  #fxn is the name of the function used for the QA test... it can be COUNT, RATE, SUM
  #emp_fn is the employee data field name we are qa-ing against
  #txn_fn is the txn field name that undergoes aggregation and is compared to the emp_fn value
  #operator_id is the operator the test is run for
  
  #emp_dataf is the employee dataset used (dataframe)
  #ID_emp_fn is the name of the id field in the employee data to identify the operator
  #txn_dataf is the txn dataset used (dataframe)
  #Orig_Temp is the txn template file for data type conditions (inverse/non inverse calc)
  #QA_Path is the output path of the output QA files
  
  #Must look like this: 
  #Map_Template.csv
  ##################
  #initializing
  ID <- toupper(trim(as.character(operator_id)))
  fx <- toupper(trim(as.character(fxn)))
  emp <- as.data.frame(emp_dataf)
  txn <- txn_dataf[txn_dataf$Operator_ID %in% ID
                   & txn_dataf$Product_Category %in% Product_Category
                   & txn_dataf$Product_Open_Date >= date_start
                   & txn_dataf$Product_Open_Date < date_end,]
  
  
  if(!(emp_fn %in% names(emp))){
    #employee metric name field not found
    fwrite(as.data.frame(1),paste0(QA_TXN_CASES,"ERROR - ",fx, "-",emp_fn,"-EMP_FN-NOT_FOUND",".csv"))
  }else if(!(txn_fn %in% names(txn))){
    #tranasaction metric name field not found
    fwrite(as.data.frame(1),paste0(QA_TXN_CASES,"ERROR - ",fx, "-",emp_fn,"-TXN_FN-NOT_FOUND",".csv"))
  }else{
    #start actual QA tests
    
    Data_Temp <- as.data.frame(Orig_Temp)
    datatype <- Orig_Temp$sys_data_type[Orig_Temp$sys_field_name %in% txn_fn]
    orig_datatype <- Orig_Temp$orig_data_type[Orig_Temp$sys_field_name %in% txn_fn]
    calc_type <- NULL
    if(grepl("bool",orig_datatype,ignore.case=TRUE) & grepl("bool",datatype,ignore.case=TRUE)){
      calc_type <- "inv bool"
    }else if(grepl("bool",orig_datatype,ignore.case=TRUE)){
      calc_type <- "bool"} else{
        calc_type <- "regular" #not used
      }
    
    #employee level test value
    emp_qa_val <- round(as.numeric(gsub("%","",as.character(emp[emp[[ID_emp_fn]] %in% OPID,emp_fn]))),2)
    emp_qa_val <- ifelse(is.na(emp_qa_val),0,emp_qa_val)
    
    if(fx %in% "COUNT"){
      txn[[txn_fn]] <- as.character(txn[[txn_fn]])
      
      if(calc_type %in% "inv bool"){
        #no is bad yes is good - count only bad
        txn_qa_val <- sum(grepl("no",txn[[txn_fn]],ignore.case = TRUE),na.rm = TRUE)
        
      }else if(calc_type %in% "bool"){
        #no|0 is good yes|1 is bad - count only bad
        txn_qa_val <- sum(grepl("yes|1",txn[[txn_fn]],ignore.case = TRUE),na.rm = TRUE)
        
      }else{
        #count all records
        txn_qa_val <- nrow(txn)
      }
      
    }else if(fx %in% "SUM"){
      
      #HOW DO WE ACCOUNT FOR ADDITIONAL DETAILS FIELDS????
      #sum all record values
      txn_qa_val <- sum(as.numeric(txn[[txn_fn]]),na.rm = TRUE)
      
      
    }else if(fx %in% "AVERAGE"){
      
      #avg all record values
      txn_qa_val <- mean(as.numeric(txn[[txn_fn]]),na.rm = TRUE)
      txn_qa_val <- ifelse(is.na(txn_qa_val),0,floor(txn_qa_val*100)/100)
      
      
    }else if(fx %in% "RATE"){
      txn[[txn_fn]] <- as.character(txn[[txn_fn]])
      denom <- ifelse(nrow(txn)<1,1,nrow(txn))
      if(calc_type %in% "inv bool"){
        #no is bad yes is good - count only bad
        txn_qa_val <- round(sum(grepl("no",txn[[txn_fn]],ignore.case = TRUE),na.rm = TRUE)/denom*100,2)#margin of error is nearest %
        
      }else if(calc_type %in% "bool"){
        #no|0 is good yes|1 is bad - count only bad
        txn_qa_val <- round(sum(grepl("yes|1",txn[[txn_fn]],ignore.case = TRUE),na.rm = TRUE)/denom*100,2)#margin of error is nearest %
        
      }else{
        #NOT A BOOLEAN - CANNOT USE RATE
        fwrite(as.data.frame(1),paste0(QA_TXN_CASES,"ERROR - ",fx, "-",emp_fn,"-TXN_FN-NOT_BOOLEAN",".csv"))
      }
    }
    
    df<- data.frame(fx,emp_qa_val,txn_qa_val)
    names(df)<-c("metric",emp_fn,txn_fn)
    
    if(emp_qa_val == txn_qa_val){
      fwrite(as.data.frame(df),paste0(QA_TXN_CASES,"P", fx, "-",emp_fn,"-",ID,".csv"))
    }else{
      fwrite(as.data.frame(df),paste0(QA_TXN_CASES,"E", fx, "-",emp_fn,"-",ID,".csv"))
    }
  }
}