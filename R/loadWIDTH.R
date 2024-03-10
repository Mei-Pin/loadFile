#' @title load files with fixed-width data
#' @description 
#' Loads files that with fixed-width data, in the same folder and with same 
#' format,then combines them into a dataset.
#' 
#' @param Dataname Default "WIDTHdataset".
#' The dataset's name you want.
#' @param Filepath
#' The folder path.
#' @param File Default \code{FALSE}.
#' Sets one or several files' name with their extension to a vector, or 
#' put all the excel files in the folder into the function in default.
#' @param Combine Default \code{TRUE}.
#' Combines the files to one dataset in default, or loads file each to 
#' be a large list's dataset.
#' @param FileExtension Default "prn".
#' Types in the file extension.
#' @param Header Default \code{TRUE}.
#' Default is setting the first row to column title and read the data from 
#' second row.
#' Sets FALSE to read the data from the first row and use V1, V2,... for 
#' column title instead.
#' @param Width
#' Sets the width for each variable to a vector.
#' @param Colname Default \code{FALSE}.
#' Sets the column title to a vector, or gets the original title in default.
#' @param Encoding Default \code{FALSE}.
#' Fills in what the file's encoding are, and unknown in default.
#' @return A tibble or tibbles in one list
#' @import magrittr
#' @import tidyverse
#' @importFrom dplyr filter
#' @importFrom utils read.fwf
#' @examples
#' 
#' # There are several files in the test folder called "extdata".
#' # The folder path should fill in the folder directory of "extdata".
#' \dontshow{
#'   path1 <- file.path(system.file("extdata", package = "loadFile"))
#'   path <- paste0(path1, "/")
#'   Dataset <- list()
#' }
#' # The files include "xlsx", "csv", "txt", and "prn".
#' # The files with same extension have same format.
#' # "xlsx" files are excel files.
#' # "csv" files' data are comma-seperate.
#' # "txt" files' data are tab-seperate.
#' # "prn" files' data are fixed-width.
#' 
#' # prn file
#' # "a.prn" "b.prn" "c.prn"
#' # The data are "Name", "Height", "Weight", and "Score".
#' # Data widths are 6, 3, 2, 1 in order.
#' # There are no title row in the data. 
#' # Data starts from the first row.
#' 
#' # Load all prn file that is in the folder and combine them.
#' loadWIDTH(Dataname=c("Test_All"), Filepath=path, Header=FALSE, 
#' Width=c(6, 3, 2, 1))
#' 
#' # Load "a.prn" and "c.prn", then become two tibble in "Test_a&c1".
#' loadWIDTH(Dataname=c("Test_a&c"), Filepath=path, Header=FALSE, 
#' File=c("a.csv", "c.csv"), Combine=FALSE, Width=c(6, 3, 2, 1))
#' 
#' # Load all csv file and skip column title, then set the new.
#' loadWIDTH(Dataname=c("Test_Colname"), Filepath=path, Header=FALSE, 
#' Width=c(6, 3, 2, 1), Colname=c("Name_d", "H", "W", "score"))
#' 
#' @export

# "loadWIDTH" FUNCTION ---------------------------------
#set the "loadWIDTH" function
loadWIDTH <- function(Dataname="WIDTHdataset", Filepath, File=FALSE, Combine=TRUE,
                      FileExtension="prn", Header=TRUE, Width, Colname=FALSE, Encoding=FALSE) {

  if (File[1]==FALSE) {
    Filename <- list.files(Filepath)
  } else {
    Filename <- File
  }
  
  File_list <- data.frame(Filename)
  File_input <- File_list %>% 
    dplyr::filter(file_extension(Filename) %in% FileExtension)
  
  if (Colname[1]!=FALSE) {
    if (File[1]==FALSE & Combine==TRUE) {
      for (i in File_input$File) {
        if (exists("Dataset")) {
          Temp_dataset <- read.fwf(paste0(Filepath, "/", i), header=Header, widths=Width)
          Dataset <- rbind(Dataset, Temp_dataset)
          rm(Temp_dataset)
        } else {
          Dataset <- read.fwf(paste0(Filepath, "/", i), header=Header, widths=Width)
        }
      }
      names(Dataset) <- Colname
      
    } else if (Combine==TRUE) {
      for (i in File){
        if (exists("Dataset")) {
          Temp_dataset <- read.fwf(paste0(Filepath, "/", i), header=Header, widths=Width)
          Dataset <- rbind(Dataset, Temp_dataset)
          rm(Temp_dataset)
        } else {
          Dataset <- read.fwf(paste0(Filepath, "/", i), header=Header, widths=Width)
        }
      }
      names(Dataset) <- Colname
      
    } else if (File[1]==FALSE & Combine==FALSE) {
      Dataset <- list()
      
      for (i in File_input$File) {
        Dataset[[i]] <- read.fwf(paste0(Filepath, "/", i), header=Header, widths=Width)
      }
      names(Dataset) <- Colname
      
    } else {
      Dataset <- list()
      
      for (i in File) {
        Dataset[[i]] <- read.fwf(paste0(Filepath, "/", i), header=Header, widths=Width)
      }
      names(Dataset) <- Colname
      
    }
    
  } else if (Encoding!=FALSE) {
    if (File[1]==FALSE & Combine==TRUE) {
      for (i in File_input$File) {
        if (exists("Dataset")) {
          Temp_dataset <- read.fwf(paste0(Filepath, "/", i), header=TRUE, widths=Width, fileEncoding=Encoding)
          Dataset <- rbind(Dataset, Temp_dataset)
          rm(Temp_dataset)
        } else {
          Dataset <- read.fwf(paste0(Filepath, "/", i), header=TRUE, widths=Width, fileEncoding=Encoding)
        }
      }
      
      
    } else if (Combine==TRUE) {
      for (i in File){
        if (exists("Dataset")) {
          Temp_dataset <- read.fwf(paste0(Filepath, "/", i), header=TRUE, widths=Width, fileEncoding=Encoding)
          Dataset <- rbind(Dataset, Temp_dataset)
          rm(Temp_dataset)
        } else {
          Dataset <- read.fwf(paste0(Filepath, "/", i), header=TRUE, widths=Width, fileEncoding=Encoding)
        }
      }
      
    } else if (File[1]==FALSE & Combine==FALSE) {
      Dataset <- list()
      
      for (i in File_input$File) {
        Dataset[[i]] <- read.fwf(paste0(Filepath, "/", i), header=TRUE, widths=Width, fileEncoding=Encoding)
      }
      
    } else {
      Dataset <- list()
      
      for (i in File) {
        Dataset[[i]] <- read.fwf(paste0(Filepath, "/", i), header=TRUE, widths=Width, fileEncoding=Encoding)
      }
    }
    
  } else {
    if (File[1]==FALSE & Combine==TRUE) {
      for (i in File_input$File) {
        if (exists("Dataset")) {
          Temp_dataset <- read.fwf(paste0(Filepath, "/", i), header=Header, widths=Width)
          Dataset <- rbind(Dataset, Temp_dataset)
          rm(Temp_dataset)
        } else {
          Dataset <- read.fwf(paste0(Filepath, "/", i), header=Header, widths=Width)
        }
      }
      
      
    } else if (Combine==TRUE) {
      for (i in File){
        if (exists("Dataset")) {
          Temp_dataset <- read.fwf(paste0(Filepath, "/", i), header=Header, widths=Width)
          Dataset <- rbind(Dataset, Temp_dataset)
          rm(Temp_dataset)
        } else {
          Dataset <- read.fwf(paste0(Filepath, "/", i), header=Header, widths=Width)
        }
      }
      
    } else if (File[1]==FALSE & Combine==FALSE) {
      Dataset <- list()
      
      for (i in File_input$File) {
        Dataset[[i]] <- read.fwf(paste0(Filepath, "/", i), header=Header, widths=Width)
      }
      
    } else {
      Dataset <- list()
      
      for (i in File) {
        Dataset[[i]] <- read.fwf(paste0(Filepath, "/", i), header=Header, widths=Width)
      }
    }
    
  }
  
  Glo <- .GlobalEnv
  assign(Dataname, Dataset, Glo)
  rm(File_list, File_input, i, Dataset, Glo)
}

# "file_extension" FUNCTION ---------------------------------
#get the file extension
#credit "asperamanka" answered Jan 11, 2022 at 17:22
#https://stackoverflow.com/questions/7779037/extract-file-extension-from-file-path
file_extension <- function(filenames) {
  sub(pattern = "^(.*\\.|[^.]+)(?=[^.]*)", replacement = "", filenames, perl = TRUE)
}
