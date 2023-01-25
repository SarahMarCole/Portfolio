data_filter_bin<- function(results, site, site_name){
  
  #Create column for Scallop Site Name 
  ScallopSiteName<-rep(site_name, nrow(site))
  
  #Insert scallop site name column
  site$ScallopSiteName<-ScallopSiteName
  
  #reduce only to columns wanted
  library(dplyr)
  
  cols<-c("MonitoringLocationIdentifier", "ScallopSiteName", "OrganizationIdentifier", "OrganizationFormalName",
          "MonitoringLocationName", "MonitoringLocationTypeName", "MonitoringLocationDescriptionText",
          "LatitudeMeasure", "LongitudeMeasure", "HorizontalCoordinateReferenceSystemDatumName",
          "StateCode", "CountyCode","ProviderName")
  site<-site[cols]
  
  #save new file as .csv
  
  write.csv(site, row.names = FALSE,file = paste(site_name, '_site_SC.csv', sep = ''))
  
  # Change Raw Results Data------------------

  
  #reduce only to columns wanted
  
  cols_results<-c("MonitoringLocationIdentifier", "ResultIdentifier", "ActivityStartDate",
                  "ActivityStartTime/Time", "ActivityStartTime/TimeZoneCode", "CharacteristicName",
                  "ResultMeasureValue",  "ResultMeasure/MeasureUnitCode")
  
  results<-results[cols_results]
  
  #save new few as .csv
  
  write.csv(results, file = paste(site_name, '_results_SC.csv', sep = ''),row.names = FALSE)
  
  #combine the raw site info and results files---------------------------
  
  combined<-merge(results, site, by="MonitoringLocationIdentifier")
  
  #save the combined file as .csv
  
  write.csv(combined,file = paste(site_name, '_combined_SC.csv', sep = ''), row.names = FALSE)
  
  #filter the Characteristics wanted------------
  
  want_char<-c(
    "Chlorophyll a",
    "Chlorophyll a, corrected for pheophytin",
    "Chlorophyll a, free of pheophytin",
    "Conductivity",
    "Depth",
    "Depth, bottom",
    "Depth, Secchi disk depth",
    "Dissolved oxygen (DO)",
    "Dissolved oxygen saturation",
    "Inorganic nitrogen (nitrate and nitrite)",
    "Inorganic nitrogen (nitrate and nitrite) as N",
    "Nitrate",
    "Nitrate as N",
    "Nitrate + Nitrite",
    "Nitrite",
    "Nitrite as N",
    "pH",
    "Phosphate-phosphorus",
    "Phosphate-phosphorus as P",
    "Phosphorus",
    "Phosphorus as P",
    "Salinity",
    "Specific conductance",
    "Specific conductivity",
    "Temperature, water",
    "Total dissolved solids",
    "Total suspended solids",
    "Total suspended particulate matter",
    "True color",
    "Turbidity",
    "Turbidity Field"
    
  )
  
  library(dplyr)
  
  combined_filtered<-filter(combined, CharacteristicName %in% want_char )
  
  #save combined and filtered file
  
  write.csv(combined_filtered, file = paste(site_name, '_combined_filtered_SC.csv', sep = ''), row.names = FALSE)
  
  
  # Bin data by month------------------------
  
  #binning is summarizing the data by using the average. Average the data to create a new data set
  
  #bin for each unique station and each characteristic by month
  
  #create month and year columns
  
  combined_filtered$ActivityStartDate<-as.Date(combined_filtered$ActivityStartDate) #turn into date class
  combined_filtered$Month<- format(as.Date(combined_filtered$ActivityStartDate), "%m") #create month column
  combined_filtered$Year<- format(as.Date(combined_filtered$ActivityStartDate), "%Y") #create year column

  combined_filtered$ResultMeasureValue<-as.numeric(combined_filtered$ResultMeasureValue) #make results a number
  
  
  library(dplyr)
  
  
  MBS<-combined_filtered %>%
    dplyr::select(!ActivityStartDate & !`ActivityStartTime/Time`&
                    !`ActivityStartTime/TimeZoneCode`) %>%                          #Take out the ActivityStart Columns
    group_by(MonitoringLocationIdentifier, Month,Year, CharacteristicName, `ResultMeasure/MeasureUnitCode`) %>%      #Group Data by what you want to bin
    mutate(ResultMeasureValue=mean(ResultMeasureValue)) %>%                         #find mean of results
    distinct(MonitoringLocationIdentifier, Month,Year, CharacteristicName,`ResultMeasure/MeasureUnitCode`, .keep_all=TRUE) %>%  #tell what columns are group
    dplyr::select(Month,Year, everything())                                         #Put month and year in front
  
  
  # save the file
  write.csv(MBS, file = paste(site_name, '_MBS_SC.csv', sep = ''), row.names = FALSE)
  
  # Bin data by Season----------------
  
  combined_filtered$Season<-quarters(combined_filtered$ActivityStartDate) #Create Season column that splits into quarters
  
  #Change all Qs to the name of the season
  combined_filtered$Season<-gsub("Q1","Winter", combined_filtered$Season) 
  combined_filtered$Season<-gsub("Q2","Spring", combined_filtered$Season)
  combined_filtered$Season<-gsub("Q3","Summer", combined_filtered$Season)
  combined_filtered$Season<-gsub("Q4","Fall", combined_filtered$Season)
  
  
  SBS<-combined_filtered %>%
    dplyr::select(!ActivityStartDate & !`ActivityStartTime/Time`&
                    !`ActivityStartTime/TimeZoneCode`& !Month) %>%                          #Take out the ActivityStart Columns
    group_by(MonitoringLocationIdentifier, Season,Year, CharacteristicName, `ResultMeasure/MeasureUnitCode`) %>%      #Group Data by what you want to bin
    mutate(ResultMeasureValue=mean(ResultMeasureValue)) %>%                         #find mean of results
    distinct(MonitoringLocationIdentifier, Season,Year, CharacteristicName,`ResultMeasure/MeasureUnitCode`, .keep_all=TRUE) %>%  #tell what columns are group
    dplyr::select(Season,Year, everything())                                         #Put season and year in front
  
  #save new SBS file
  
  write.csv(SBS, file = paste(site_name, '_SBS_SC.csv', sep = ''), row.names = FALSE)
  
  # Bin for all stations combined by month------------
  
  
 MBA<-combined_filtered %>%                        
    group_by(Month,Year, CharacteristicName,`ResultMeasure/MeasureUnitCode`) %>%      #Group Data by what you want to bin
    mutate(ResultMeasureValue=mean(ResultMeasureValue)) %>%                         #find mean of results
    distinct(Month,Year, CharacteristicName,`ResultMeasure/MeasureUnitCode`, .keep_all=TRUE) %>%  #tell what columns are group
    dplyr::select(Month, Year, CharacteristicName,ResultMeasureValue,
                  'ResultMeasure/MeasureUnitCode', ScallopSiteName)                                       #Put season and year in front
  
  #save new MBA file
  
  write.csv(MBA, file = paste(site_name, '_MBA_SC.csv', sep = ''), row.names = FALSE)
  
  #Bin for all stations combined by season---------------
  
  SBA<-combined_filtered %>%                        
    group_by(Season,Year, CharacteristicName,`ResultMeasure/MeasureUnitCode`) %>%      #Group Data by what you want to bin
    mutate(ResultMeasureValue=mean(ResultMeasureValue)) %>%                         #find mean of results
    distinct(Season,Year, CharacteristicName,`ResultMeasure/MeasureUnitCode`, .keep_all=TRUE) %>%  #tell what columns are group
    dplyr::select(Season, Year, CharacteristicName,ResultMeasureValue,
                  'ResultMeasure/MeasureUnitCode', ScallopSiteName) 
  #save new SBA file
  
  write.csv(SBA, file = paste(site_name, '_SBA_SC.csv', sep = ''), row.names = FALSE)
  
  paste("DONE!")
}
