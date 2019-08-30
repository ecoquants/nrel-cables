#------------------------------------------------------------------------------
# R version 3.1
# R script to peform validation code  for the 
# Wind Integration National Dataset (WIND) toolkit
# reads in WIND toolkit data and observations in the configuration file
# 
#
# AUTHOR: Wil Lieberman-Cribbin, Dr. Caroline Draxl, and Dr. Andy Clifton
#         NREL, August 2014
#
# Abbrevations:
#     WS = wind speed 
#     WD = wind direction
#     RMSE = root-mean-squared-error
#     CRMSE = centered-root-mean-squared-error
#     MAE = mean absolute error
#     pe = percent error
#
# Note: Although this code refers to model and obs data, this code can be used to 
# compare any two datasets It is not required to use WIND toolkit as model data.
# 
# WIND toolkit data is presented in 100 m vertical resolution and can be 
# downloaded at http://maps.nrel.gov/wind_prospector.
#
# No corrections were made to account for data at other than 100 m vertical height.
# As such, the user is responsbile for controlling the quality of observational data
# and ensuring data is also at 100 meter vertical resolution. 
# 
# In the validation code: A basic quality control is completed on both obs and model data, 
# using thresholds to provide a cutoff of wind speed and wind direction values. 
# Values below 0 m/s or above 40 m/s for wind speed and values below 0 degrees or above
# 360 degrees were removed from analyses. 
#
# Only obs and model data with coincident timestamps are used in calculating error metrics
#
# INPUTS: 
#   1) Configuration File in correct format and stored in correct location
#
# OUTPUTS:
#   1) observational data (named data_obs) and Model data (named data_model)
#       stored under "data" folder for each unique site name
#   2) Error metrics table and figures, including Diurnal and Annual cycles of
#       wind speed bias, root-mean-squared-error (RMSE), centered-root-mean-
#         squared-error (CRMSE), histograms of wind speed, wind roses of 
#           observations and model data, and quantile-quantile plot of wind speeds
#
# Execution / How to run validation code: 
#   1) data files, including all observational and model data, stored in correct locations
#   2) "data" and "figures" folders created in appropriate location
#       (these folders must be named "data" and "figures" capitalization counts)
#   3) data.root directory appropriately defined (line 91)
#   4) name of configuration file specified as a string (line 92)
#   5) code.dir defined; defines locations where functions are (line 96)
#
# After appropriate steps are taken you can 'Run' the validation code.

# Calls functions WT.obs.model.combination, WT.timestamp, WT.obs.model.Hourly,
#                 WT.error.metrics.Hourly.monthly, WT.error.facets, 
#                 WT.month.Hour.facets, WT.windrose.facets, WT.windrose,
#                 WT.table,  WT.histogram, WT.quantile.quantile, and 
#                 WT.error.metrics.functions (contains bias, RMSE, MAE, pe functions)
#                
# The code will load observational and model data into R,
#  combine observational and model data based on coincident 
#  timestamps / do quality control on data (WT.obs.model.combination), 
#  create timestamps of data (WT.timestamp), calculate error metrics based
#  on hourly averages (WT.obs.model.Hourly ~ used for table of error metrics,
#  WT.error.metrics.Hourly.monthly ~ used for facet plots), create diurnal plots of bias,
#  RMSE, CRMSE for each month (WT.error.facets), create faceted plots of diurnal
#  and monthly averages of bias, RMSE, and CRMSE (Wt.month.Hour.facets),
#  create wind rose plots (WT.windrose.facets / WT.windrose), 
#  create a table of error metrics (WT.table ~ calls functions defined in 
#  WT.error.metrics.functions), create a histogram (WT.histogram), and create a 
#  quantile-quantile plot of wind speeds (WT.quantile.quantile).
#  
# We suggest you pay attention to the comments in this code and read the 
# accompanying documentation read.me for help and answers, including an
# example validation for the National Wind Technology Center.
#
#Happy validating
# ------------------------------------------------------------------------------

#remove all attempts to create data and require necessary packages
rm(list=ls(all=TRUE))
require(ggplot2)
require(RColorBrewer)
require(reshape2)

#User has to change lines 91 and 92 in this code
#User must define a data.root directory and the name of their configuration file
data.root = "C:/Users/wlieberm/Desktop/CONFIG" 
config.file = "load.txt"
# Define location of all the functions we use
#user needs to alter line 96 of this code defining where the functions are located 
#so they can be sourced
code.dir = "C:/Users/wlieberm/Desktop/R Info/functions/"
code.files = dir(code.dir, pattern = "[.R]")

#Source functions
for (file in code.files){
  source(file = file.path(code.dir, file))
}
#Triggers put in to seperate code into parts
# do.read.raw is gateway for section 1 (reading data into R), 
#do.get.error.metrics is gateway for section 2 do metrics / produce figures)
#Setting do.read.raw = TRUE will do the first part of the code
#Setting do.get.error.metrics = TRUE will complete the second part
#Setting do.read.raw and do.get.error.metrics=TRUE will run entire code
do.read.raw <- TRUE
do.get.error.metrics <- TRUE

# loop through the sites listed in configuration file and load into 
#project.files
project.files <- read.delim(file = file.path(data.root,config.file), 
                            sep="\t",
                            stringsAsFactors=FALSE,
                            quote="",
                            comment.char ="#")

#looping for each site in project.files (complete list of sites),
#creates / refers to site.directory, data.directory, and figure.directory
#As mentioned in the documentation, the user needs to create a site.name.folder,
#empty "data" folder and empty "figures folder" for each unique site under the
#data.root directory
if (do.read.raw){
  for (site in unique(project.files$site.name)){  #1
    print(paste("Reading files for", site))
    data.model <- NULL
    data.obs <- NULL
    # Create a directory to put figures in
    site.dir <- file.path(data.root,site)
    data.dir <- file.path(data.root,site,"data")
    figure.dir <- file.path(site.dir,"figures")
    dir.create(path = figure.dir, showWarnings=FALSE)
    # get the files for this site
    site.files <- project.files[project.files$site.name == site,] 
    
    # read the files for this site
    for (site.file.row in seq(1,NROW(site.files))){  #2
      
      data.type = site.files$data.type[site.file.row]
      # read the data file
      if (site.files$sep[site.file.row] == "\\t"){
        sep = "\t"
      }
      else if (site.files$sep[site.file.row] == "\"\""){
        sep = ""
      }
      else {
        sep = site.files$sep[site.file.row]
      }
      #create data.raw with Date/Time information about site
      data.raw <- read.csv(file = file.path(site.dir,
                                            site.files$data.file[site.file.row]),
                           header=FALSE, 
                           sep = sep,
                           colClasses = "character",
                           stringsAsFactors = FALSE,
                           skip = site.files$skip[site.file.row])
      
      # get the data we want (DateTime and DateTime.Format)
      if (is.na(site.files$date.col[site.file.row])){
        DateTime = data.raw[,site.files$time.stamp.col[site.file.row]] 
        DateTime.Format = site.files$time.stamp.format[site.file.row]
      } else {
        DateTime = paste(data.raw[,site.files$date.col[site.file.row]],
                         data.raw[,site.files$time.col[site.file.row]])
        DateTime.Format = paste(site.files$date.format[site.file.row], 
                                site.files$time.format[site.file.row])
      }
      #Define data.new as a timestamp
      data.new <- data.frame(timestamp = as.POSIXct(DateTime,
                                                    tz = site.files$tz[site.file.row],
                                                    format = DateTime.Format))
      attributes(data.new$timestamp)$tzone <- "UTC"
      
      # look to see what data was included , convert data to correct units, 
      #add this data to data.new data.frame
      if (is.na(site.files$WS.col[site.file.row])){
        data.new$WS = NA
      } else {
        data.new$WS = as.numeric(data.raw[,site.files$WS.col[site.file.row]])
      }
      if (is.na(site.files$WD.col[site.file.row])){
        data.new$WD = NA
      } else {
        data.new$WD =   as.numeric(data.raw[,site.files$WD.col[site.file.row]])
      }
      if (is.na(site.files$Temp.col[site.file.row])){
        data.new$Temp = NA
      } else {
        data.new$Temp = as.numeric(data.raw[,site.files$Temp.col[site.file.row]])
      }
      if(site.files$WSms[site.file.row] == TRUE){
        data.new$WS = data.new$WS
      } else {
        data.new$WS = data.new$WS*0.4407
      }
      if(site.files$WDdeg[site.file.row] == TRUE){
        data.new$WD = data.new$WD
      } else {
        data.new$WD = data.new$WD*57.2957795 
      }
      if (is.na(site.files$Temp.col[site.file.row])) {
        data.new$Temp = NA
      } else if (site.files$Temp.C[site.file.row] ==TRUE){
        data.new$Temp = data.new$Temp
      } else if (site.files$Temp.K[site.file.row] ==TRUE){
        data.new$Temp = data.new$Temp-273.15
      } else if (site.files$Temp.F[site.file.row] ==TRUE){
        data.new$Temp = data.new$Temp-32
        data.new$Temp = data.new$Temp*(5/9)
      }
      # set invalid data to NA
      for (col in c("WS", "WD","Temp")){
        try(data.new[data.new[,col] < -990,col]  <- NA,  ##
            silent = TRUE)
      }    
      # Remove rows with invalid wind speed and wind direction
      for (col in c("WS","WD")){      
        data.new <- data.new[!is.na(data.new[col]),]
      }
      #rbinds all data.type= "Obs" or "Model" from the same site into 
      #one file; applicable if you have more than one listing for a site in
      #configuration file
      if (site.files$data.type[site.file.row] == "Obs"){    #3
        data.obs = rbind(data.obs,
                         data.new)
      }else if (site.files$data.type[site.file.row] =="Model"){  #4
        data.model = rbind(data.model,
                           data.new)   
      } #4
      rm(data.new)
    } #2
    #save each site's data in specific file
    save(data.model, file=file.path(data.dir, "data_model.R"))
    save(data.obs, file=file.path(data.dir, "data_obs.R"))
    rm(data.model, data.obs)
  }  #1  
} # END OF DO.READ.RAW

##Section B Error metrics section

#loads data.model and data.obs for each site.
if (do.get.error.metrics){
  for (site in unique(project.files$site)){
    site.dir <- file.path(data.root,site)
    data.dir <- file.path(data.root,site,"data")
    figure.dir <- file.path(site.dir,"figures")
    load(file = file.path(data.dir, "data_model.R"))
    load(file=file.path(data.dir, "data_obs.R"))
    
    #renames the generic WS and WD columns to be specific for obs and model
    colnames(data.obs) <- c("timestamp", "WS.obs", "WD.obs", "Temp.obs")
    colnames(data.model) <- c("timestamp", "WS.model", "WD.model", "Temp.model")  
    
    #removes columns that contain only NA's, for example, if you did not 
    #have WD, faceting plots dont work with all NA's
    data.obs <- data.obs[,colSums(is.na(data.obs)) != nrow(data.obs)]
    data.model <- data.model[,colSums(is.na(data.model)) != nrow(data.model)]
    
    #calls Function WT.obs.model.combination
    #Inputs: data.obs, data.model,   #Outputs: obs.model[data.frame]
    #combines your obs and model data into one data.frame based on coinciding timestamps
    obs.model <- WT.obs.model.combination(data.obs = data.obs,
                                          data.model = data.model)
    
    ###making a long dataframe of obs.model based on what data is included 
    #in the config file. To be used to create timestamps 
    if (isTRUE("WD.obs" %in% colnames(obs.model) & "WD.model" %in% colnames(obs.model)
               &"Temp.obs" %in% colnames(obs.model) & "Temp.model" %in% colnames(obs.model))){
      long.obs.model <- melt(obs.model, 
                             id.vars=c("timestamp", "Year"), 
                             measure.vars = c("WS.obs", "WS.model", "WD.obs", 
                                              "WD.model", "Temp.obs", 
                                              "Temp.model"),
                             variable.name=("variable"), 
                             value.name=("value")) 
    } else if (isTRUE("WD.obs" %in% colnames(obs.model) & "WD.model" %in% colnames(obs.model))){
      long.obs.model <- melt(obs.model, 
                             id.vars=c("timestamp", "Year"), 
                             measure.vars = c("WS.obs", "WS.model",
                                              "WD.obs", "WD.model"),
                             variable.name=("variable"), 
                             value.name=("value")) 
    } else {
      long.obs.model <- melt(obs.model, 
                             id.vars=c("timestamp", "Year"), 
                             measure.vars = c("WS.obs", "WS.model"),
                             variable.name=("variable"), 
                             value.name=("value"))
    }
    
    #Calls function WT.timestamp,
    #Inputs: long.obs.model, figure.dir      #Outputs: obs.model.timestamp (plot)
    #printing timeseries of WS and WS and / or temperature obs and model data
    obs.model.timestamp <- WT.timestamp(data = data.frame(variable = long.obs.model$variable,
                                                          value= long.obs.model$value,
                                                          timestamp = long.obs.model$timestamp),
                                        figure.dir = figure.dir)
    
    
    #Calls function WT.obs.model.Hourly,
    #Inputs: obs.model         #Ouput: obs.model.Hourly
    #Calculates mean wind speed for obs and model parsed by day,Hour,month, and Year
    # Used in creating a table of error metrics (one value)
    #not to be confused with WT.error.metrics.Hourly.monthly, which is used
    # in creating facet plots
    obs.model.Hourly <- WT.obs.model.Hourly(obs.model = obs.model)
    
    #Calls function WT.error.metrics.Hourly.monthly,
    #Inputs: obs.model    #Outputs: error.metrics.Hourly.monthly (data.frame)
    #Creates average wind speed obs, average wind speed model, avg bias, RMSE, and 
    #CRMSE for each Hour and month,to be used in facet plots of bias, RMSE and CRMSE
    error.metrics.Hourly.monthly <- WT.error.metrics.Hourly.monthly(obs.model = obs.model)
    
    #creates a long data frame from error.metrics.Hourly.monthly
    long.error.metrics.Hourly.monthly <- melt(error.metrics.Hourly.monthly,  
                                    id.vars=c("Hour", "month"), 
                                    measure.vars= c("bias", "RMSE", "CRMSE"), 
                                    variable.name="variable", 
                                    value.name="value") 
    
    #Calls function Wt.error.facets
    #Inputs: long.error.metrics.Hourly.monthly error.metrics.Hourly.monthly, figure.dir
    #Outputs error.facets (diurnal plot of avg bias, RMSE, and CRMSE for each
    #each month of the year),
    #also saves individual plots of bias, RMSE, and CRMSE.
    #The facet plot has Hour of Day [UTC] as x axis, wind speed m/s (y axis),
    # and has 12 lines, one for each month 
    error.facets <- WT.error.facets(data.long = data.frame(variable = long.error.metrics.Hourly.monthly$variable,
                                                           value = long.error.metrics.Hourly.monthly$value,
                                                           Hour = long.error.metrics.Hourly.monthly$Hour,
                                                           month = long.error.metrics.Hourly.monthly$month),
                                    data.wide = error.metrics.Hourly.monthly,
                                    figure.dir = figure.dir)
    
    #Calls function Wt.month.Hour.facets
    #Inputs: long.error.metrics.Hourly.monthly, figure.dir      
    #Outputs: error.month.Hour.facets (faceted plot of monthly averages of bias, RMSE
    #CRMSE, and Hourly averages of bias, RMSE, CRMSE)
    #Instead of 12 lines (one for each month), this function returns an
    #averaged Hourly value or averaged monthly value (one line)
    error.month.Hour.facets <- WT.month.Hour.facets(data = data.frame(variable = long.error.metrics.Hourly.monthly$variable,
                                                                      value = long.error.metrics.Hourly.monthly$value,
                                                                      parameter.month = long.error.metrics.Hourly.monthly$month,
                                                                      parameter.Hour = long.error.metrics.Hourly.monthly$Hour),
                                                    figure.dir = figure.dir)
    
    #creating a data frame with obs and model data to be used for windroses, as long
    #as wind direction data for observation and model are included
    if ("WD.obs" %in% colnames(obs.model)| "WD.model" %in% colnames(obs.model)){ 
      temp.windrose.data <- rbind(data.frame(WS = obs.model$WS.obs,
                                             WD = obs.model$WD.obs,
                                             month = obs.model$month,
                                             type = "Observations"),
                                  data.frame(WS = obs.model$WS.model,
                                             WD = obs.model$WD.model,
                                             month = obs.model$month,
                                             type = "Model"))
      #calls function WT.windrose.facets
      #Inputs: temp.windrose.data   #Output: windrose.obs.model (plot)
      #Creates faceted plot of windroses from observational and model data
      #Need to run print(windrose.obs.model) to see both windroses.
      windrose.obs.model <- WT.windrose.facets(data= temp.windrose.data,
                                        spd = "WS",
                                        dir = "WD",
                                        spdres = 2,
                                        dirres= 30,
                                        spdmin = 0,
                                        spdmax = 40,
                                        spdseq = c(0,5,10,15,20,25,30,35,40),
                                        palette = "YlOrRd",
                                        countmax = NA,
                                        debug = 0,
                                        figure.dir = figure.dir) 
      print(windrose.obs.model)
    }
    #Calls function WT.table
    #Inputs: obs.model, obs.model.Hourly  #Outputs: table (data.frame)
    #creates a table of error.metrics (bias, RMSE, CRMSE, mean absolute error (MAE),
    # and percent error (pe) of wind speed. Error metrics calulcated using 
    #interval averages as well as hourly averages. Intervals averages are 
    #calculated the shortest common time interval between obs and model data 
    #(usually 5 or 10 minute intervals ~ WIND toolkit data in 5 minute intervals)
    table <- WT.table(data.interval = obs.model,
                      data.time.avg = obs.model.Hourly)
    
    #Calls function WT.histogram
    #Inputs: obs.model, xlab, figure.dir  #Output: Hist (plot)
    #Creating histogram of WS from observational and model data
    Hist <- WT.histogram(data = obs.model,
                         xlab = "wind speed (m/s)",
                         figure.dir = figure.dir)
    
    #Calls function WT.quantile.quantile
    #Inputs: obs.model, xlab, ylab, figure.dir    #Output: p.quantile.quantile (plot)
    #Quantile Quantile plot of WS from obs and model data
    p.quantile.quantile <- WT.quantile.quantile(data.WS.obs = obs.model$WS.obs,
                                                data.WS.model = obs.model$WS.model,
                                                xlab = "Observed Wind Speeds (m/s)",
                                                ylab = "Modeled Wind Speeds (m/s)",
                                                figure.dir = figure.dir)
  }
} # END do.get.error.metrics

