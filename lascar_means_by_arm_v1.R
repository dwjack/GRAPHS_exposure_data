require(dplyr)
#create a vector of file names  -- works!!
files<-list.files("~/Dropbox/ghana_exposure_data_shared_2014/Main_study_exposure_assessment",recursive=T,pattern="^CU_CO", full.names=T)
head(files)

files<-files[1:10] # this is just to keep it to a manageable size

#next we need a loop that will do the following
#1.  for each monitoring session, create a dataframe with household, village, session id, and arm variables
#(note that to create the arm variable we will need to map from village codes to arm -- DJ has the look-up table)
#(for now, want to create a dataframe for each exposure monitoring session file; later we may need to stack them)
#2.  create a more manageable name for each data
