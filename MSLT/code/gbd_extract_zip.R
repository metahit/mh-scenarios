#This script extracts required Global Burden of Disease data from the zip files
#by first extracting zip-files, then reading csv file, adding required data to combined dataframe
#and finally deleiting extracted files. Resulting dataframe is then saved as csv-file.

#Created in February-March 2019 by Marko Tainio (modified by Belen Zapata June 2019 for Metahit project)

#Defining folder where the data is (stored externally in my dropbox as the GBD files are large)
data_folder <- "C:/Users/e95517/Dropbox/Collaborations/James Woodcock/Metahit/Data/GBD2017"
temp_folder <- paste0(data_folder,"/temp") 
result_folder <- paste0(data_folder,"/final")
gbdfile_name <- "/IHME-GBD_2017_DATA-f849372f-"

#Next two lines defines countries that will be extracted
LGAs <- unlist(read.csv("MSLT/data/gbd/LGAs to be extracted.csv")[,2])

data_extracted <- NULL

for (i in 1:18) {
  file_number <- i
  
  file_select <- paste0(data_folder,gbdfile_name, i,".zip")
  
  unzip(file_select, exdir=temp_folder)
  
  data_read <- read.csv((paste0(temp_folder,"/", gbdfile_name, i, ".csv")))
  file.remove(paste0(temp_folder,"/", gbdfile_name, i, ".csv"))
  data_read <- subset(data_read, location_name %in% LGAs) # location name is easier to identify
  
  data_extracted <-rbind(data_extracted,data_read)
}


unlink(paste0(temp_folder), recursive = TRUE)

## Write file to city regions folder

# write_csv(data_extracted, "MSLT/data/city regions/gbd_2017.csv") # Do we want this? or work directly from data extracted so there is no
# # storage

# rm(data_folder, temp_folder, result_folder, gbdfile_name, LGAs, data_extracted, file_number, i, data_read, file_select)