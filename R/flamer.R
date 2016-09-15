# Read Flame Scripts and load functions
# Updated June 2016, Luke Loken

# Main script
source("FLAMe_Wrapper_All_Maps.R")

#reading instruments
source("suna_datetime.R")
source("read_instruments.R")
source("flame_cut.R")

#correcting data
source("PeterTurnerCorrection.R")
source("tau_correct.R")
source("tau_correct_2015.R")
source("convert_gases.R")
source("convert_gases_2015.R")
source("sensor_clean2.R")

#exporting data
source("write_shapefile.R")
source("extract_flame.R")

#mapping data
source("Heat_Maps.R")
source("Google_Heat_Maps_ColorBar.R")
source("Google_Heat_Maps.R")
