
#to do create for 1 folder
loadMYTHE <- function(arg2){
        
        library("tabulizer")
        library(pdftools)
        
        files <- list.files( pattern="*.pdf", full.names=FALSE)
        fileListDf <- list()
        iter <- c(1:length(files))
        for(i in iter){
                tmpDf <- do_all(files[i])
                fileListDf[[i]] <- tmpDf
        }
        total <- do.call(rbind, fileListDf)
        setwd(arg2)
        write.table(total, file = "Output_MyTheresa.csv",row.names=FALSE, na="",col.names=TRUE, sep=",")
}

#clean numeric values
clean_numeric_values <- function(df, column_numbers = 5:9){
        df[,column_numbers] <- apply(df[,column_numbers],2, function(y) as.numeric(gsub("[^0-9]", "", y)))
        df[is.na(df)] <- 0
        return (df)
}

# controller
do_all <- function(filePath){
        #to do - remove hardcode
        #file <- "MyTheresa 20170930 2052219017.pdf"
        data <- extractDataFromFile(filePath)
        up1_data <- clean_by_date(data)
        up2_data <- remove_false(up1_data)
        up3_data <- clean_numeric_values(up2_data)
        output <- CreateCorrectOutputDataFrame(up3_data)
        return(output)
}

CreateCorrectOutputDataFrame <- function(df){
        #create empty dataframe
        n <- nrow(df)
        nodata <- data.frame(SOURCE = character(n), XLS_DATE_YYYYMMDD= numeric(n), XLS_DERIVED_STYLE_NUMBER = character(n), XLS_DERIVED_STYLE_DESC = character(n), XLS_DERIVED_STYLE = character(n),
                             XLS_DERIVED_COLOR = character(n), XLS_DERIVED_COLOR_DESC = character(n), BOUGHT = numeric(n), SOLD = numeric(n), XLS_DERIVED_PRICE = numeric(n),
                             XLS_DERIVED_ITEM_CATEGORY = character(n), DateKey = numeric(n), BOUGHT_DLRS = numeric(n), SOLD_DLRS = numeric(n))
        #source
        nodata[,1] <- df[,14]
        #XLS_DATE_YYYYMMDD
        nodata[,2] <- as.numeric(gsub("-", "", df[,16]))
        #XLS_DERIVED_STYLE_NUMBER
        #split by space and take 1 word
        nodata[,3] <- sapply(strsplit( as.character( df[,2] )," "), `[`, 1)
        #XLS_DERIVED_STYLE_DESC
        rexp <- "^(\\w+)\\s?(.*)$"
        nodata[,4] <- sub(rexp,"\\2", df[,1])
        #1 line change
        nodata[,4] <- toupper(nodata[,4])
        #XLS_DERIVED_STYLE_DESC
        nodata[,5] <- sub(rexp,"\\1", df[,1])
        #XLS_DERIVED_COLOR
        nodata[,6] <- gsub( "-.*$", "", df[,3] )
        nodata[,6] <- gsub("^\\s+|\\s+$", "", nodata[,6])  
        #XLS_DERIVED_COLOR_DESC
        nodata[,7] <- gsub( ".*\\-", "", df[,3] )
        nodata[,7] <- gsub("^\\s+|\\s+$", "", nodata[,7]) 
        #BOUGHT
        nodata[,8] <- df[,6]
        #SOLD
        nodata[,9] <- df[,8]
        #XLS_DERIVED_PRICE
        nodata[,10] <- df[,5]
        #XLS_DERIVED_ITEM_CATEGORY
        nodata[,11] <- 'N/A'
        #DateKey
        nodata[,12] <- sub("^\\D*(\\d+).*$", "\\1", df[,14])
        #BOUGHT_DLRS
        nodata[,13] <- df[,7]
        #SOLD_DLRS
        nodata[,14] <- df[,9]
        return (nodata)
}

#remove columns with false value
remove_false <- function(df, column_to_check = 15){
        df <- df[df[,column_to_check] != FALSE,]
        return(df)
}

#looks like date is a key
clean_by_date <- function(df){
        df$bool <- FALSE
        df$new_date <- as.POSIXct(df$`4`, format = '%m/%d/%Y')
        for(q in c(1:nrow(df))){
                if(!is.na.POSIXlt(df[q,c("new_date")])){
                        df[q,c("bool")] <- TRUE
                }
        }
        df
}

#extract data from 1 file 
extractDataFromFile <- function(file, number_of_columns = 12, stepUp = 75.65){
        info <- pdf_info(file)
        pages <- info$pages
        listOfDf <- list()
        for(q in c(1:pages)){
                pageColumns <- list()
                for(z in c(1:number_of_columns)){
                        if(q == 1){
                                starting_area <- getStartPossition(TRUE, z)
                        }else{
                                starting_area <- getStartPossition(FALSE, z)
                        }
                        columnList <- list()
                        #number of rows in file
                        for(i in c(1:10))
                        {
                                out <- extract_tables(file = file, pages = q, guess = FALSE, area = list(starting_area))
                                starting_area <- updateArea(starting_area, step = stepUp)
                                out <- paste(unlist(out), collapse = ' ')
                                columnList[[i]] <- out
                        }
                        pageColumns[[z]] <- do.call(rbind.data.frame, columnList)
                }
                tmp1 <- do.call(cbind.data.frame, pageColumns)
                tmp1$page <- q
                listOfDf[[q]] <- tmp1
        }
        for (i in 1:length(listOfDf)) {
                names(listOfDf[[i]]) <- c("1","2","3","4","5","6","7","8","9","10","11","12", "page")
        }
        tmp <- do.call(rbind.data.frame, listOfDf)
        tmp$file_name <- file
        return  (tmp)
}

#update next column 
updateArea <- function(starting_area, step){
        top <- starting_area[[1]][1] + step
        left <- starting_area[[2]][1]
        bottom  <- starting_area[[3]][1] + step
        right <- starting_area[[4]][1]
        newArea <- c(top,left, bottom, right)
        return (newArea)
}

#return starting possition of chosen column 
getStartPossition <- function(firstPage = TRUE, columnNumber = 1){
        if(columnNumber > 12){print("error")}
        else{
                if(firstPage)
                {
                        if(columnNumber == 1){#name
                                starting_area <- c(86.60,  29.10, 160.17, 179.03)
                                return (starting_area)
                        }   
                        if(columnNumber == 2){#Vendor #
                                starting_area <- c(85.37,   248.35,	  158.80,     348.8481 )
                                return (starting_area)
                        }
                        if(columnNumber == 3){#Colour
                                starting_area <- c(85.61 ,	  348.8482 , 160.52  ,   451.43 )
                                return (starting_area)
                        }
                        if(columnNumber == 4){#Online date
                                starting_area <- c(85.61 ,    451.43,  158.14 ,    514.4)
                                return (starting_area)
                        }
                        if(columnNumber == 5){#? whosale
                                starting_area <- c(  85.61,    514.46,  158.14 ,    567.45)
                                return (starting_area)
                        }
                        if(columnNumber == 6){#Delivered units
                                starting_area <- c(85.61,	567.76 ,	   160	,       623.84)
                                return (starting_area)
                        }
                        if(columnNumber == 7){#Delivered value: At cost
                                starting_area <- c(85.61 ,  623.84 ,    160	,	697.56)
                                return (starting_area)
                        }
                        if(columnNumber == 8){#Sold Units
                                starting_area <- c(85.61 ,  697.56  ,   160	,	746.31)
                                return (starting_area)
                        }
                        if(columnNumber == 9){#Sold value:At cost
                                starting_area <- c(85.61,	 746.31,	  160,	     821)
                                return (starting_area)
                        }
                        if(columnNumber == 10){#Units STR
                                starting_area <- c(85.61,	  821,	  160,	     875.92)
                                return (starting_area)
                        }
                        if(columnNumber == 11){#value STR:At cost
                                starting_area <- c(85.61,	 875.93	,   160	   ,   923.48)
                                return (starting_area)
                        }
                        if(columnNumber == 12){#Return Rate Units
                                starting_area <- c(85.61,	 923.49, 160,	       967)
                                return (starting_area)
                        }
                }
                else
                {
                        if(columnNumber == 1){#name
                                starting_area <- c(65.18,  30.97, 139.68, 178.10)
                                return (starting_area)
                        }   
                        if(columnNumber == 2){#Vendor #
                                starting_area <- c(65.39,	   248.29,    140.30,	  348.74)
                                return (starting_area)
                        }
                        if(columnNumber == 3){#Colour
                                starting_area <- c(65.39 ,    348.75,	140.30,	  450.24 )
                                return (starting_area)
                        }
                        if(columnNumber == 4){#Online date
                                starting_area <- c(65.39,	   450.25,	140.30,	  514.45)
                                return (starting_area)
                        }
                        if(columnNumber == 5){#? whosale
                                starting_area <- c(  65.39,	   514.46,	140.30,	  567.95)
                                return (starting_area)
                        }
                        if(columnNumber == 6){#Delivered units
                                starting_area <- c(65.39,	   567.96,	140.30,	  622.85)
                                return (starting_area)
                        }
                        if(columnNumber == 7){#Delivered value: At cost
                                starting_area <- c(65.39,	   622.95,	140.30,	  696.96974)
                                return (starting_area)
                        }
                        if(columnNumber == 8){#Sold Units
                                starting_area <- c(65.39,	   697.57,	140.30,	  745.12)
                                return (starting_area)
                        }
                        if(columnNumber == 9){#Sold value:At cost
                                starting_area <- c(65.39,	   745.13,	140.30, 	 821.22)
                                return (starting_area)
                        }
                        if(columnNumber == 10){#Units STR
                                starting_area <- c(65.39,	   821.23,	140.30,	 875.92)
                                return (starting_area)
                        }
                        if(columnNumber == 11){#value STR:At cost
                                starting_area <- c(65.39,	   875.93,	140.30,	 922.29)
                                return (starting_area)
                        }
                        if(columnNumber == 12){#Return Rate Units
                                starting_area <- c(65.39,	   922.30,	140.30,	  966.28)
                                return (starting_area)
                        }
                }
        }
}

args <- commandArgs(trailingOnly = TRUE)
arg1 <- as.character(args[1])
arg2 <- as.character(args[2])
setwd(arg1)  
loadMYTHE(arg2)