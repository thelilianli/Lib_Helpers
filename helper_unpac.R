########################################################
#Read Extract Files - need to follow standard transformations   
########################################################
Read_Extract <- function(f_Path, print_warnings = FALSE){
  #where f_Path is a string for the extract file you want to read
  #returns read in extract table as dataframe
  #reading in data dump files  - delimiter from sys tool is "
  #issues identified when DE value contains " or \\r or \\n - causes incorrect splitting
  #correction is addressed in this function
  
  dato <- fread(f_Path,sep = "`",quote="",fill=TRUE,skip = 1L,header=FALSE)
  
  
  if(nrow(dato)>=2){
    dat_names <- dato[1,]
    dato$V1 <- gsub("\\r","", dato$V1, perl=T)
    dato$V1 <- gsub("\\n"," ", dato$V1, perl=T)
    dato$V2 <- endsWith(dato$V1, '"')#identify shifted records by seeing if the row ends in a "
    
    #correcting for carrier returns within the data shifting rows and columns
    
    last_record <- NA
    needs_append <- FALSE
    remove_records <- c()
    
    for(ii in 2:nrow(dato)){
      
      if(!needs_append & dato$V2[ii]){
        #this record ends with " (CORRECT)
        #previous record DOES NOT need append
        needs_append <- FALSE
        last_record <- ii
        
      }else if(needs_append & dato$V2[ii]){
        #*APPEND
        #this record ends with " and previous record has need append indicator = TRUE
        dato$V1[last_record] <- paste0(dato$V1[last_record],dato$V1[ii])
        remove_records <- unique(c(remove_records,ii))
        
        #overwrite needs append to false
        needs_append <- FALSE
        
      }else if(needs_append & !dato$V2[ii]){
        #*APPEND
        #this record does not end with " 
        #earlier record has need append indicator = TRUE
        dato$V1[last_record] <- paste0(dato$V1[last_record],dato$V1[ii])
        remove_records <- unique(c(remove_records,ii))
        
      }else{
        #!needs_append & !dato$V2[ii]
        #record is incomplete and does not end with "
        #earlier record DOES NOT need append
        needs_append <- TRUE
        last_record <- ii
      }
      
    }
    if(length(remove_records)>0){
      View(dato[remove_records,])
      dato <- dato[-remove_records,]
    }
    
    #splitting to correct columns  
    dat_names<- cSplit(dat_names, splitCols = 1, sep = ',', direction = "wide")
    
    if ((nrow(dato)-1) >=2){
      #not an empty table
      dat <- dato[2:(nrow(dato)-1),]
      dat <- data.frame(do.call('rbind', strsplit(as.character(dat$V1),'", "',fixed=TRUE)))
      names(dat) <- as.character(unlist(dat_names))}else{
        #create empty table
        dat <- data.frame(matrix(ncol = length(dat_names), nrow = 0))
        names(dat) <- as.character(unlist(dat_names))
      }
    
    #removes any additional qoutations
    dat <- data.frame(lapply(dat, function(x) {gsub('"', "", x)}))
    names(dat) <- conform_ProdName(names(dat))
    # names(dat) <- gsub('^x','',names(dat))
    
    if(print_warnings){
      print("Rows corrected and then removed: ")
      print(remove_records)
    }
    dat <- as.data.frame(dat)
    dati <- sapply(dat, is.factor)
    dat[dati] <- lapply(dat[dati], as.character)
    
    return(dat)
    
  }
  
}


########################################################
#Unzip Extract Files - need standard zip file format   
########################################################
Unzip_Extract <- function(f_Path,f_Name,o_Path){
  # f_Path is the string directory path where the original zip file is stored
  # f_Name is the string name of the original zip file
  # o_Path is the string directory path where the unzipped file will be stored
  
  # mmmddyyyy <- "20181203"
  # install.packages("R.utils")
  # library("R.utils")
  
  if (f_Name %in% list.files(f_Path)){
    dir.create(o_Path)
    # setwd(o_Path)
    
    file.copy(paste0(f_Path, f_Name), o_Path)
    
    
    if (length(list.files(o_Path))==1 & f_Name %in% list.files(o_Path)){
      
      setwd(o_Path)
      
      untar(paste0(o_Path, f_Name))
      
      if (paste0(o_Path, "/files/outbound/sm") %in% list.dirs(o_Path)){
        
        curdir <- paste0(o_Path, "files/outbound/sm/")
        newdir <- o_Path
        setwd(curdir)
        
        files <- list.files(path=curdir, full.names=TRUE)
        files_new <- gsub(curdir, newdir, files)
        
        for (i in 1:length(files)){
          file.copy(files[i], files_new[i])
        }
        
        setwd("~/R")
        
        unlink(paste0(o_Path, "files"), recursive = TRUE, force = TRUE)
        #remove the zip file
        file.remove(paste0(o_Path,f_Name))
      }
    }
  }else {
    print("Source file does not exist.")}
}


########################################################
#Move Extract Files - move selected files   
########################################################
Move_Files <- function(f_Path,f_Name_list,o_Path){
  # f_Path is the string directory path where the original zip file is stored
  # f_Name_List is the list of names of the file to be moved
  # o_Path is the string directory path where the moved file will be stored
  
  dir.create(o_Path)
  
  for (i in f_Name_list){
    file.copy(paste0(f_Path, i), o_Path)
    file.remove(paste0(f_Path, i))
    
  }
}