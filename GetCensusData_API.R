# Title: Get Census ACS data via Census API 

# Description: 
#    Gets user specified data (table_field) 
#       for specified set (acs or sf1) 
#        for specified state
#        for specified county
#        at the tract level

# Ex: List of required tables for household disaggregation model
        # Size model: 
#   Table: B01001 - SEX BY AGE
#     Population <- B01001_001E   # _<n> - field number in the dataset, E - Estimate, M- Marinal Error 

#   Table: B08202 - HOUSEHOLD SIZE BY NUMBER OF WORKERS IN HOUSEHOLD
#     HH         <- B08202_001E   
#     HH_SIZE1   <- B08202_006E
#     HH_SIZE2   <- B08202_009E
#     HH_SIZE3   <- B08202_013E
#     HH_SIZE4P  <- B08202_018E

# Notes:
# Required package: rjson  
# Limitations: To get data by tract, the query requires one State & one County and 
#              using a wild card or list will return an error

# Author: Amar Sarvepalli (sarvepalli@pbworld.com, 01-24-2013)

#-------------------------------------------------------------------------------------------------
# Load library
  library(rjson)

# Build query
# to test paste in browser: http://api.census.gov/data/2011/acs5?key=e2e7a5b05c688346094ae9a76cc92473137f1589&get=B08202_001E,NAME&for=tract:*&in=state:37+county:87

# Census 
  devKey     <- "e2e7a5b05c688346094ae9a76cc92473137f1589"       # developer key
  census_URL <- "http://api.census.gov/data"                     # main url
  year       <- "2011"                                           # acs year
  dataSet    <- "acs5"                                           # acs or sf1
  state_id   <- "37"                                             # state                                             
  county_id  <- " "                                              # county: the API handles only one county at a time   
  tract_id   <- "*"                                              # either "*" to get all or "specify number"                                

# Household tables
  hhs_ACSNames  <- c("Population","HH", "HH_SIZE1","HH_SIZE2","HH_SIZE3","HH_SIZE4P")
  hhs_ACSTables <- c("B01001_001E", "B08202_001E", "B08202_006E", "B08202_009E", "B08202_013E" ,"B08202_018E")  

# Function to get the data from the URL and convert it to table format
  getURLData <- function(url) { 
     # Get data from URL
     y <- fromJSON(,url, method = "C", unexpected.escape = "error")
     # Convert JSON array to dataframe
     tab <- as.data.frame(do.call(rbind,lapply(y, function(x) unlist(x))))
     # Note the first row is header
     colnames(tab) <- as.character(unlist(tab[1,]))
     # Move the data_field to last
     tab <- tab[2:nrow(tab),c(2:ncol(tab),1)]
  }


# Get a list of counties in NC State
  counties_in_NC <- "E:/Asheville/SurveyPilot/Counties_in_NC.csv"
  county_list <- read.csv(counties_in_NC)
  counties <- sprintf("%03d",unique(county_list$FIPS.Code))

# Loop by counties
  # counties <- c("21", "87", "89", "115", "175")
  
  for(co in 1:length(counties)){
    county_id = counties[co]
    
    # Loop by tables  
    for(t in 1:length(hhs_ACSTables)){
        
       tableNumber <- hhs_ACSTables[t]
       url_query <- paste(census_URL,"/",year,"/",dataSet,"?key=",devKey,"&get=",tableNumber,",NAME&for=tract:",tract_id,"&in=state:",state_id,"+county:",county_id,sep="")
    
       # Get data from URL (format JSON array)
       tab <- getURLData(url_query)
       
       # Add fields from the table
       ifelse(t == 1, all_tables <- tab, all_tables <- cbind(all_tables,tab[ncol(tab)]))
       
       # Write to csv file
       if (t==length(hhs_ACSTables)) {
         # Rename columns with user specified fields
         colnames(all_tables) <- c(colnames(all_tables[,1:(ncol(all_tables)-length(hhs_ACSTables))]),hhs_ACSNames)
       }
    }
    
    # Join tables for all counties
    ifelse(co==1, all_counties <- all_tables, all_counties <- rbind(all_counties, all_tables))
    
    # Write output
    if(co== length(counites)) {
       write.csv(all_counties,"E:\\Asheville\\subModels\\hh_sub_data.csv", row.names= FALSE)
    }
  } # end county
