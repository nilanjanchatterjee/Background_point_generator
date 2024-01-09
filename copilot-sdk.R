##################
## input/output ## adjust!
##################
## Provided testing datasets in `./data/raw`: 
## for own data: file saved as a .rds containing a object of class MoveStack
inputFileName = "./data/raw/Yahatinda_move2.rds" 

## optionally change the output file name
unlink("./data/output/", recursive = TRUE) # delete "output" folder if it exists, to have a clean start for every run
dir.create("./data/output/") # create a new output folder
outputFileName = "./data/output/output.rds" 


args<-list()
#################################################################
########################### Arguments ###########################

args[["type"]] <- "population"
args[["points"]] <- 10
args[["background_bbox"]] <- "FALSE"
#args[["buffer"]]<-NULL
#################################################################
#################################################################

##############################
## source, setup & simulate ## leave as is!
##############################
# this file is the home of your app code and will be bundled into the final app on MoveApps
source("RFunction.R")

# setup your environment
Sys.setenv(
  SOURCE_FILE = inputFileName, 
  OUTPUT_FILE = outputFileName, 
  ERROR_FILE="./data/output/error.log", 
  APP_ARTIFACTS_DIR ="./data/output/"
)

# simulate running your app on MoveApps
source("src/moveapps.R")
simulateMoveAppsRun(args)