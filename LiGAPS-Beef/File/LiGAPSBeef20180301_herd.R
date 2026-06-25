#######################################################################################
#                                                                                     #
#                                  LiGAPS-Beef                                        #                                                                                     
# (Livestock simulator for Generic analysis of Animal Production Systems-Beef cattle) #                                                                                 
#                                                                                     #
#                                                                                     #
#               #    #  ##   ##  ###   ###    ###   ###  ###  ###                     #
#               #      #    #  # #  # #       #  # #    #    #                        #
#               #    # # ## #### ###   ##  ## ###  ##   ##   ##                       #
#               #    # #  # #  # #       #    #  # #    #    #                        #
#               #### #  ##  #  # #    ###     ###   ###  ### #                        #
#                                                                                     #
#                                                                                     #
#                                                                                     #
#                                                                                     # 
#                                                                                     #
# The model LiGAPS-Beef is described in the paper:                                    #
# LiGAPS-Beef, a mechanistic model to explore potential and feed-limited beef         # 
# production: 1. Model description and illustration.                                  #
# Authors: A. van der Linden 1,2,*, G.W.J. van de Ven 2, S.J. Oosting 1,              #
# M.K. van Ittersum 2, and I.J.M. de Boer 1.                                          #
#                                                                                     #
#                                                                                     #
# 1                                                                                   #
# Animal Production Systems group                                                     # 
# Wageningen University & Research                                                    #
# P.O. Box 338                                                                        #
# De Elst 1                                                                           #
# 6700 AH  Wageningen                                                                 #
# The Netherlands                                                                     #
#                                                                                     #
# 2                                                                                   #
# Plant Production Systems group                                                      #
# Wageningen University & Research                                                    #
# P.O. Box 430                                                                        #
# Droevendaalsesteeg 1                                                                #
# 6700 AK  Wageningen                                                                 #
# The Netherlands                                                                     #
#                                                                                     #
# * Corresponding author; aart.vanderlinden@wur.nl                                    #                                                   
#                                                                                     #
# LiGAPS-Beef aims to simulate potential and feed-limited production for beef         #
# production systems across the world. Potential and feed-limited production are used #
# to calculate the yield gap, which is the difference between actual and potential or #
# feed-limited production. In addition, the model identifies the defining and         #
# limiting factors for growth and production of beef cattle.                          # 
#                                                                                     #
# Description program code:                                                           #
# This program code contains the model LiGAPS-Beef, including its thermoregulation    #
# sub-model, its feed intake and digestion sub-model, and its energy and protein      #
# utilisation sub-model.                                                              #
#                                                                                     #
# This specific version of LiGAPS-Beef is used to illustrate the model for ten        #
# different cases which are described in the section 'Model illustration' in the      # 
# paper of Van der Linden et al. The cases are described in Table 1, and model        #
# results at herd level are given in Table 3. Figure 5 presents the defining and      #
# limiting factors for growth. This model was developed during the PhD project        #
# 'BenchmarkingAnimal Production Systems', 2012-2016.                                 #
#                                                                                     #
# The PhD project was part of the Dutch IPOP project: 'Mapping for sustainable        #
# intensification'                                                                    # 
# http://www.wageningenur.nl/en/About-Wageningen-UR/Strategic-plan/Mapping-for-       #
# Sustainable-Intensification.htm                                                     #
#                                                                                     #
# Last update: 01-03-2018                                                             #
#                                                                                     #
#######################################################################################

#######################################################################################
#                               Code for model illustration                           #
#######################################################################################

# The vectors below indicate the parameters for the ten cases presented in Table 1 of 
# the paper.

ill.genotype <- c(1,1,4,4,1,1,4,4,1,1) # Genotype or breed used; 1 = Charolais, 
                                       # 4 = 3/4 Brahman x 1/4 Shorthorn
# Optimum slaughter weight of bull calves to maximize feed efficiency for each of the 
# ten cases in Table 3.The slaughter weight is optimized to maximize the feed 
# efficiency at the herd level.
ill.slweight <- c(936.1493, 717.3103, 579.8809, 559.7907, 877.8683, 460 , 574.7075, 
                  638.0943, 717.469, 992.1554)  
# Locations: FRANCE1 = Charolles, France (46.4??N, 4.3??E); AUSTRALIA1 = Kununurra, 
# Australia (15.7??S, 128.7??E)
ill.location <- c("FRANCE1","AUSTRALIA1","FRANCE1","AUSTRALIA1","FRANCE1",
                  "AUSTRALIA1","FRANCE1","AUSTRALIA1","FRANCE1","FRANCE1")

# Cattle in France are kept indoors from December to March, and outdoors from April 
# to November. Cattle in Australia were kept outdoors year-round.
# Search for ill.housing for how this code is used.
ill.housing1 <- c(0,1,0,1,0,1,0,1,0,0) # Housing 0 = housed in a stable; 1 = outdoors 
ill.housing2 <- c(1,1,1,1,1,1,1,1,1,1) # Housing 0 = housed in a stable; 1 = outdoors
ill.housing3 <- c(0,1,0,1,0,1,0,1,0,0) # Housing 0 = housed in a stable; 1 = outdoors

ill.f1 <- c(20,20,20,20,20,20,20,20, 0,(2*0.95)) # Feed availability 1 (kg DM per 
#animal per day), which represents ad libitum feeding at a value of 20 kg DM.
ill.f2 <- c( 0, 0, 0, 0, 0, 0, 0, 0,20, 0) # Feed availability 2 (kg DM per animal per 
# day), which indicates that this feed type is not available, except for the nineth 
# case (barley).

FEEDNR <- c(1,1,1,1,2,3,2,3,4,5) # Diet numbers
# 1 = 65% wheat and 35% good quality hay (diet under potential production, van der 
#     Linden et al. (2015))
# 2 = 5% barley and 95% grass (France) or hay, depending on whether the animals are 
#     housed or not
# 3 = 5% barley and 95% grass (Australia)
# 4 = 1 kg DM barley per day, rest of the diet is grass (France) or hay
# 5 = 5% barley and 95% grass (France) or hay, depending on whether the animals are 
#     housed or not, at most of 2% of the total body weight per day

GENLIMdata       <- c(rep(NA,4000)) # vector to record days when the animal's genotype 
                                    # is the most defining factor for growth
HEATSTRESSdata   <- c(rep(NA,4000)) # vector to record days when heat stress (climate) 
                                    # is the most defining factor for growth
COLDSTRESSdata   <- c(rep(NA,4000)) # vector to record days when cold stress (climate) 
                                    # is the most defining factor for growth
FILLGITGRAPHdata <- c(rep(NA,4000)) # vector to record days when digestion capacity 
                                    # (feed quality) is the most limiting factor 
                                    # for growth
NELIMdata        <- c(rep(NA,4000)) # vector to record days when energy deficiency is 
                                    # the most limiting factor for growth
PROTGRAPHdata    <- c(rep(NA,4000)) # vector to record days when protein deficiency is 
                                    # the most limiting factor for growth

for (z in c(1,3,5,7,9:10)) { # z-loop for each of the cases in France. Number refer to 
                             # the numbers of the cases simulated (Table 1). 

#######################################################################################
#                     Sensitivity analysis (one-at-a-time approach)                   #
#######################################################################################

# Source code for sensitivity analysis (not used for model illustration). Sensitivity 
# analyis is conducted in the second paper: LiGAPS-Beef, a mechanistic, model to 
# explore potential and feed-limited beef production 2. Sensitivity analysis and  
# evaluation of sub-models (Van der Linden et al.)
  
# Settings for sensitivity analysis (par. 119 is the base scenario)
# s indicates the sensitivity loop
NPAR            = 118        # number of parameters in sensitivity analysis
NPAR            = NPAR + 1   # number of parameters plus includes reference scenario
RELDIFF         = -0.10      # relative increase or decrease of parameters (fraction)

SENSDAT <- c(rep(c(1,rep(0,NPAR)),(NPAR-1)),1)
SENSMAT <- matrix(nrow=NPAR, ncol=NPAR, data = (SENSDAT * RELDIFF+1))

SENSMAT[119,119] <- 1.0      # parameters in reference scenario are not changed
SENSMAT <- SENSMAT[1:NPAR,]  

FESENSREPR <- c(rep(0,NPAR)) # Matrix indicating the feed efficiency of the 
                             # reproductive cow
FESENSIND <- c(rep(0,NPAR))  # Matrix indicating the feed efficiency of the bull 
                             # (calf)
FESENSHERD <- c(rep(0,NPAR)) # Matrix indicating the feed efficiency of the herd unit

for(s in 119){ # s-loop for the parameters included in the sensitivity analysis.
  
  # Run number 119 is the base scenario used to calculate the relative change in model 
  # output. 

  starttime <- proc.time() # Start processing time
  
  #####################################################################################
  # 1.                                   Initial section                              #
  #####################################################################################
  
  #####################################################################################
  # 1.1                             Farming system description                        #
  #####################################################################################
  
  # The beef production system is described in this section of the model
  
  # Genotype (i.e. breed), location, and scale (animal or herd level) 
  BREED = ill.genotype[z]       # Breed (1 = Charolais; 2 = Boran; 3 = Parda de 
                                # Montana; 4 = Brahman (3/4) x Shorthorn (1/4); 5 = 
                                # Hereford (only steers)
  
  LOCATION <- ill.location[z]   # See ill.location for the geographical location. 
  
  SCALE = 2                     # Scale/level of the system (1 = individual animal/the 
                                # animal level; 2 = herd unit/herd level) For 
                                # simulations at the animal level, see the results in 
                                # Table 2 of the paper.
  SEX_ANIMAL = 0                # Animal sex (0 = male; 1 = female), only for 
                                # simulations at the animal level (i.e. if SCALE 
                                # equals 1)
  
  # Climate and housing
  
  # Code housing: 0 = stable or feedlot, 1 = free grazing system; 2 = open feedlot
  PHASE1 <- rep(ill.housing1[z],84)  # Housing period 1 (indicates January - March;
                                     # 25th of March = day 84)
  PHASE2 <- rep(ill.housing2[z],260) # Housing period 2 (indicates March - December) 
  PHASE3 <- rep(ill.housing3[z],21)  # Housing period 3 (indicates December)
  # The sum of all phases should equal 365 days (1 full year)
  
  # Changes in outdoor climate conditions to calculate indoor climate conditions:
  WINDMAX = 5          # maximum wind speed (in ms-1)
  RADTRANS = 0.0       # fraction of solar radiation in stable (related to roof 
                       # construction)
  WINDRED = 0.5        # fraction reduction of wind speed in stable (related to 
                       # construction)
  TINCR = 5.6667       # increase in stable temperature compared to outdoor at 0 
                       # degrees Celsius  
  Tdelta = 0.8667      # increase in stable temperature per degree Celsius increase in
                       # outdoor temperature 
  
  # Management
  # See van der Linden et al (2015) for an explanation on cattle management under
  # potential and feed-limited production (Agricultural Systems 139 : 100-109). 
  MAXCALFNR = 8                 # Number of calves per cow (max = 8; only for  
                                # reproductive animals)
  imax = 4000                   # Duration of simulation (# days)
  MAXFATCARC = 0.0              # Maximum fat percentage in the carcass for slaughter
                                # of reproductive animals (0.0 = no minimum fat 
                                # percentage)
  MAXLIFETIME = 11.36           # Maximum # years a productive animal can live
  MAXCONCAGE = 10.00            # Maximum conception age of a reproductive animal 
  CULL = 0.5                    # Culling rate (fraction reproductive cows per year),
                                # which equals 50% per year.
  SWMALES = ill.slweight[z]     # Slaughter weight male calf/calves (kg)
  SWFEMALES = 390               # Slaughter weight female calf/calves (kg)
  STDOY = 1                     # Day of the year in which the first animal is born
  
  #####################################################################################
  # 1.2                                    Weather data                               #
  #####################################################################################
  
  # Library with weather data (file chosen depends on LOCATION)
  # Model users should ensure that the directory of the weather file and the directory
  # given below correspond to each other!
  
  if(LOCATION == "FRANCE1") 
    WEATHER <-read.csv(file="M:/R/FRACHA19982012.csv", head=TRUE,sep=",") else 
    if(LOCATION == "AUSTRALIA1") 
      WEATHER <-read.csv(file="M:/R/AUSTRALIA1992A.csv", head=TRUE,sep=",")
  
  # If wind speeds are exceptionally high, these can be replaced by a maximum wind  
  # speed.
  if(max(WEATHER$WIND) > WINDMAX) WINDHIGH <- "Yes" else WINDHIGH <- "No"
  WEATHER$WIND[WEATHER$WIND > WINDMAX] <- WINDMAX  # Maximum wind speed equals WINDMAX
  
  PHASE <- c(PHASE1,PHASE2,PHASE3)           # Connect all phases in one year
  HOUSING <- rep(PHASE,12)                   # Twelve years with housing (free grazing, 
                                             # stable or feedlot) are constructed
  HOUSING <- HOUSING[STDOY:length(HOUSING)]  # Housing starts at the day the animal is
                                             # born.
  
  # Modify weather data if cattle are housed in stables or feedlots  
  for(i in 1:length(imax)){
    
    # roof over stable reduces radiation levels
    if(HOUSING[i] == 0) WEATHER$RAD[i]   <- WEATHER$RAD[i] * RADTRANS            
    # stable construction reduces wind speed
    if(HOUSING[i] == 0) WEATHER$WIND[i]  <- WEATHER$WIND[i] * WINDRED            
    # increase in stable minimum temperature relative to outdoor temperature
    if(HOUSING[i] == 0) WEATHER$MINT[i]  <- Tdelta * WEATHER$MINT[i] + TINCR     
    # increase in stable maximum temperature relative to outdoor temperature
    if(HOUSING[i] == 0) WEATHER$MAXT[i]  <- Tdelta * WEATHER$MAXT[i] + TINCR     
  }
  
  WEATHER <- WEATHER[STDOY:nrow(WEATHER),] # The weather files starts at the day the 
                                           # first animal is born
  
  DOY <- WEATHER$DOY-floor(WEATHER$DOY/365)*365  # Calculates day of the year (DOY) if
                                                 # days numbered ascending for multiple 
                                                 # years in the weather files. 
  DOY[DOY==0] <-365                              # For simplicity, one year is assumed
                                                 # to have 365 days per year instead of 
                                                 # 365.24 days per year
  
  WEATHERORIG <- WEATHER # Creates a copy of the weather data file 
  
  ###########################################################################################
  # 1.3                                    Parameters                                       #
  ###########################################################################################
  
  ###########################################################################################
  # 1.3.1               Genetic parameters (related to BREED and SEX)                    #
  ###########################################################################################
  
  # This section contains a list of 26 genetic parameters (a LIBRARY) which are specific for  
  # the genotype (i.e. breed) and sex. Numbers before the parameter description refer to the 
  # order of parameters in the LIBRARY, which is not the same as in the Supplementary 
  # Information. Numbers after the parameter description [between brackets] refer to the 
  # parameter numbers in Table S2 of the Supplementary Information.  
  
  # 1 reflectance coat [5]
  # 2 coat length [3]
  # 3 body area (body area : weight factor) [1]
  # 4 maximum cond. body core ??? skin [4]
  # 5 birth weight [9] 
  # 6-10 parameters of the Gompertz curve [9-12,19]  
  # 11-12 lactation curve parameters A and B (A = 0, no milk production male) [13,14]
  # 13 adult max. weight [20]
  # 14 sex (0= male, 1 = female)
  # 15-16 lactation curve parameters A and B (milk available for calf) [13,14]
  # 17 minimum fraction mature TBW for gestation [21]
  # 18 maintenance correction factor [17]
  # 19 minimum fat tissue % in carcass for gestation [22] 
  # 20 lipid bone parameter [16]
  # 21 maximum carcass fraction [18]
  # 22 maximum muscle:bone ratio [19]
  # 23 minimum conduction body core ??? skin [4]  
  # 24-26 latent heat release 1,2, and 3 [6-8]
  # 27 lactation curve parameter C [15]
  
  # Parameters for Charolais bulls                        Parameter number
  LIBRARY10 <- c(0.60, 0.012, 1.00 , 64.1, 48.1,          # 1-5     
                 1616.7, 48.1, 1.6, 1.10, 316.7,          # 6-10      
                 0.0000, 0.068, 1300, 0, 8, 0.068,        # 11-16   
                 0.60, 1.0, 0.32,                         # 17-19    
                 11.1, 0.64, 4.4, 1.00,                   # 20-23     
                 3.08, 1.73, 35.3, 0.00338)               # 24-27         
                 
  # Parameters for Charolais heifers / cows                 Parameter number
  LIBRARY11 <- c(0.60, 0.012, 1.00, 64.1, 45.9,           # 1-5     
                 1178.7, 45.9, 1.6, 1.10, 228.7,          # 6-10    
                 8, 0.068, 950, 1, 8, 0.068,              # 11-16   
                 0.60, 1.0, 0.32,                         # 17-19   
                 11.8, 0.62, 4.1, 1.00,                   # 20-23   
                 3.08, 1.73, 35.3, 0.00338)               # 24-27   
                           
  # Parameters for Boran bulls                              Parameter number
  LIBRARY20 <- c(0.60, 0.012, 1.12, 64.1, 28.0,           # 1-5     
                 608.7, 28.0, 4.2, 1.5, 8.7,              # 6-10    
                 0.0000, 0.150, 600, 0, 0.5510, 0.150,    # 11-16   
                 0.55, 0.91, 0.32,                        # 17-19   
                 13.3, 0.578, 4.1, 1.30,                  # 20-23   
                 4.89, 0.80, 34.5)                        # 24-27             
                         
  # Parameters for Boran heifers / cows                     Parameter number
  LIBRARY21 <- c(0.60, 0.012, 1.12, 64.1, 25.0,           # 1-5     
                 456.5, 25.0, 4.2, 1.5, 6.5,              # 6-10    
                 0.5510, 0.150, 450, 1, 0.5510, 0.150,    # 11-16   
                 0.55, 0.91, 0.32,                        # 17-19   
                 14.3, 0.55, 3.60, 1.30,                  # 20-23   
                 4.89, 0.80, 34.5)                        # 24-27   
  
  # Parameters for Parda de Montana bulls                   Parameter number
  LIBRARY30 <- c(0.56, 0.012, 1.00 , 64.1 , 42.0,         # 1-5      
                 1308.4, 42.0, 1.6, 1.15, 255.7,          # 6-10      
                 0.00, 0.150, 1052.7, 0, 0.46, 0.150,     # 11-16   
                 0.55, 1.0, 0.32,                         # 17-19   
                 11.6, 0.64, 4.8, 1.00,                   # 20-23   
                 3.08, 1.73, 35.3)                        # 24-27      
  
  # Parameters for Parda de Montana heifers / cows          Parameter number
  LIBRARY31 <- c(0.56, 0.012, 1.00, 64.1, 40.0,           # 1-5     
                 769.3, 40.0, 1.6, 1.10, 147.3,           # 6-10    
                 0.4562, 0.150, 622, 1, 0.4562, 0.150,    # 11-16   
                 0.55, 1.0, 0.32,                         # 17-19   
                 12.9, 0.62, 4.3, 1.00,                   # 20-23   
                 3.08, 1.73, 35.3)                        # 24-27   
 
  # Parameters for 3/4 Brahman x 1/4 Shorthorn steers/bulls Parameter number
  LIBRARY40 <- c(0.56, 0.012, 1.09 , 64.1 , 33.0,         # 1-5      
                 962.6, 33.0, 1.6, 1.50, 187.6,           # 6-10      
                 0.0000, 0.068, 775, 0, 5.68, 0.068,      # 11-16   
                 0.50, 0.93, 0.32,                        # 17-19   
                 11.6, 0.5935, 4.1, 1.225,                # 20-23   
                 3.08, 2.15, 35.6, 0.00338)               # 24-27      
  
  # Parameters for 3/4 Brahman x 1/4 Shorthorn heifers      Parameter number
  LIBRARY41 <- c(0.56, 0.012, 1.09 , 64.1 , 30.0,         # 1-5      
                 744.2, 30.0, 1.6, 1.50, 144.2,           # 6-10      
                 5.68, 0.068, 675, 1, 5.68, 0.068,        # 11-16   
                 0.50, 0.93, 0.20,                        # 17-19    
                 11.6, 0.55, 3.6, 1.225,                  # 20-23   
                 3.08, 2.15, 35.6, 0.00338)               # 24-27   

  # Parameters for Hereford steers/bulls                    Parameter number
  LIBRARY50 <- c(0.44, 0.012, 1.00 , 64.1 , 41.0,         # 1-5      
                 1054.6, 41.0, 1.6, 0.99, 204.6,          # 6-10      
                 0.0000, 0.150, 850, 0, 0.300, 0.150,     # 11-16   
                 0.55, 1.00, 0.32,                        # 17-19    
                 11.6, 0.60, 4.275, 1.00,                 # 20-23     
                 3.08, 1.73, 35.3)                        # 24-27     
  
  # Parameters for Hereford heifers                         Parameter number
  LIBRARY51 <- c(0.44, 0.012, 1.00 , 64.1 , 36.9,         # 1-5      
                 768.95, 36.9, 1.6, 0.99, 147.8,          # 6-10      
                 0.4561, 0.150, 621.15, 1, 0.300, 0.150,  # 11-16   
                 0.55, 1.00, 0.20,                        # 17-19   
                 11.6, 0.57, 4.0, 1.00,                   # 20-23     
                 3.08, 1.73, 35.3)                        # 24-27   
  
  ###########################################################################################
  
  # Simulations with individual cows do not include calf birth  
  if(SCALE== 1) MAXCALFNR <- 0 else MAXCALFNR <- MAXCALFNR         
  # Sex of the calves of the reproductive cow (1= female; 0=male)
  # The first number indicates the male calf. This sequence is only valid at a culling rate
  # of 50% per cow per year (van der Linden et al, 2015. Agricultural Systems 139 : 100-109)
  SEX_CALVES <- c(1,0,0,0,0,0,0,0,0,0,0,0,0) # Sex reproductive cow + offspring 
                                             # (1= female; 0=male)
  
  if(SCALE== 1) SEX <- SEX_ANIMAL else SEX <- c(1,SEX_CALVES)     
  jmax <- MAXCALFNR + 1 # Number of animals in the simulation at the animal level or the herd 
                        # level                                           
  
  # Vector to indicate reproductive animals (1= reproductive, 0 = productive)
  if(SCALE== 2) REPRODUCTIVE <- c(1,0,0,0,0,0,0,0,0,0,0,0,0) else  
    REPRODUCTIVE <- c(0,0,0,0,0,0,0,0,0,0,0,0,0) 
  
  # Vector to indicate replacement animals (1= replacement, 0 = other)
  REPLACEMENT  <- c(0,1,0,0,0,0,0,0,0,0,0,0,0,0)                 
  # Vector to indicate productive animals (1= productive, 0 = other)
  PRODUCTIVE   <- c(0,0,1,1,1,1,1,1,1,1,1,1,1,1)                 
  # Auxilliary vector used later on in the code to obtain the right weather files
  ORDER <- c(0,1,2,3,4,5,6,7,8,9,10)                               
  
  # End of the section related to genetic parameters
  
  ###########################################################################################
  # 1.3.2                                Feed parameters                                    #
  ###########################################################################################
  
  # Feed parameters after often from Chilibroste et al. (1997) and Jarrige et al. (1986) 
  
  # Chilibroste P, Aguilar C and Garcia F 1997. Nutritional evaluation of diets. Simulation 
  # model of digestion and passage of nutrients through the rumen-reticulum. Animal Feed 
  # Science and Technology 68, 259-275.
  
  # Jarrige R, Demarquilly C, Dulphy JP, Hoden A, Robelin J, Beranger C, Geay Y, Journet M, 
  # Malterre C, Micol D and Petit M 1986. The INRA fill unit system for predicting the 
  # voluntary intake of forage-based diets in ruminants - a review. Journal of Animal Science
  # 63, 1737-1758.
   
  # List of abbreviations: 
  
  # HIF = Heat Increment of feeding (MJ MJ-1 metabolisable energy, see Table S4 of the 
  # Supplementary information)
  
  # The following abbreviations correspond to the abbreviations used in Table S3 of the
  # Supplementary information:
  
  # FU = Fill Units (-)
  # SNSC = Soluble, Non-Structural Carbohydrates (g kg-1 DM)
  # INSC = Insoluble, Non-Structural Carbohydrates (g kg-1 DM)
  # DNDF = Digestible Neutral Detergent Fibre (g kg-1 DM)
  # SCP = Soluble Crude Protein (g kg-1 DM)
  # DCP = Digestible Crude Protein (g kg-1 DM)
  # kdINSC = digestion rate Insoluble, Non-Structural Carbohydrates (% hr-1)
  # kdNDF = digestion rate Neutral Detergent Fibre (% hr-1)
  # kdDCP = digestion rate Digestible Crude Protein (% hr-1)
  # kdPass = standard passage rate in the rumen (% hr-1)
  # UNDF = Undegradable Neutral Detergent Fibre (g kg-1 DM)
  # pef = physical effectiveness factor for Neutral Detergent Fibre (-)
  # CP = crude protein (g kg DM-1)
  # GE = gross energy (MJ kg DM-1)    
  
  ############################################################################################################################################
  
  # The vectors and abbreviations for feed types given below correspond to Table S3 and S4 
  # of the Supplementary Information. For references to the parameters of feed types, see 
  # Tables S3 and S4
  
  # SBM = soybean meal
  
  #                        HIF    FU    SNSC INSC PNDF     SCP    PICP   LEFT   kdINSC kdPNDF kdPICP kdPASS  UNDF    NDF   peNDF   CP   GE                
  #                         1      2     3    4      5       6      7      8       9      10     11     12      13     14    15    16   17      
  BARLEY             <- c(0.245, 0.573, 389, 214, 156.00,  34.50, 82.80, 116.70, 0.242, 0.145, 0.125, 0.040,  21.00, 0.210, 0.70, 110, 18.4) 
  CONCENTRATE        <- c(0.249, 0.619, 262, 175, 243.10,  72.80, 87.36, 161.74, 0.150, 0.060, 0.100, 0.040,  42.90, 0.286, 0.70, 182, 18.5) 
  HAY                <- c(0.318, 1.120, 100, 150, 345.80,  48.16, 74.30, 281.74, 0.300, 0.040, 0.085, 0.035, 148.20, 0.494, 1.00, 172, 18.5)                      
  HAYPOOR            <- c(0.420, 1.370,  73,  73, 462.00,  20.30,149.10, 347.90, 0.300, 0.040, 0.085, 0.035, 198.00, 0.660, 1.00,  70, 18.2) 
  GRASSSPRING        <- c(0.304, 0.960, 130,  30, 360.00,  66.25, 97.40, 361.30, 0.300, 0.040, 0.085, 0.035, 120.00, 0.400, 0.40, 265, 18.6) 
  GRASSSUMMER        <- c(0.356, 1.120, 100,  60, 376.00,  49.50, 76.50, 385.00, 0.300, 0.040, 0.085, 0.035, 141.00, 0.470, 0.50, 180, 18.4) 
  GRASSSUMMERDRY     <- c(0.447, 1.280,  50,  60, 409.50,  23.00, 69.00, 411.50, 0.300, 0.040, 0.085, 0.035, 175.50, 0.585, 1.00, 115, 18.1)  
  MAIZE              <- c(0.237, 0.438, 202, 532, 101.70,  20.10, 86.56,  57.64, 0.040, 0.051, 0.035, 0.050,  11.30, 0.113, 0.40, 134, 17.0) 
  MOLASSES           <- c(0.050, 0.200, 828,   0,      0,    3.8,   0.2,      0,     0,     0, 0.125, 0.040,      0,     0,    0,   4, 17.0) 
  SBM                <- c(0.242, 0.526, 107,   0, 138.60, 202.80,243.40, 232.00, 0.242, 0.145, 0.125, 0.040,   0.00, 0.210, 0.40, 507, 19.7)  
  STRAWCER           <- c(0.557, 1.800,  14,  78, 401.00,  10.00,  5.00, 370.00, 0.300, 0.040, 0.085, 0.035,   0.00, 0.210, 1.00,  40, 18.3) 
  WHEAT              <- c(0.234, 0.475, 475, 212,  80.00,  39.90, 69.80,    0.0, 0.182, 0.150, 0.080, 0.040,  34.20, 0.114, 0.70, 133, 18.2) 
  MAIZESILAGE        <- c(0.289, 1.000, 100, 351, 239.00,  54.94, 23.00, 483.06, 0.250, 0.040, 0.040, 0.030, 239.00, 0.478, 0.93,  82, 18.5)
 
  PASTURE            <- c(0.323, 1.120, 100,  60, 376.00,  49.50, 76.50, 385.00, 0.300, 0.040, 0.085, 0.035, 141.00, 0.470, 0.50, 180, 18.4)
  PASTURE            <- c(0.323, 1.195, 100,  60, 376.00,  49.50, 76.50, 385.00, 0.300, 0.040, 0.085, 0.035, 141.00, 0.470, 0.50, 180, 18.4)
  PASTURE1           <- c(0.358, 1.12,  50,  60, 551.00,  23.00, 69.00, 411.50, 0.300, 0.040, 0.085, 0.035, 175.50, 0.585, 1.00, 115, 18.1) 
  
  MIX <- (0.69*SBM+0.31*HAY)/1 # Model users can specify a mix of specific feed types. This 
                               # indicates a mix between 69% soybean meal and 31% good 
                               # quality hay. 
  
  ############################################################################################################################################
  
  # Feed quality and available feed quantity (limiting factors for growth of beef cattle) 
  
  # Feed type
  F1 <- matrix(nrow=length(DOY), ncol=length(BARLEY)) # Creates matrices for the types of 
                                                      # feed types fed each day
  F2 <- matrix(nrow=length(DOY), ncol=length(BARLEY))
  F3 <- matrix(nrow=length(DOY), ncol=length(BARLEY))
  
  FEED1 <- matrix(nrow=length(DOY), ncol=length(BARLEY))
  FEED2 <- matrix(nrow=length(DOY), ncol=length(BARLEY))
  FEED3 <- matrix(nrow=length(DOY), ncol=length(BARLEY))
  FEED4 <- HAY # The fourth feed type is fixed, and cannot vary over time. The first three 
               # feed types can vary over time.
  
  FEED1QNTY <- NULL # Creates matrix for the available feed quantity per day for feed 1
  FEED2QNTY <- NULL # Creates matrix for the available feed quantity per day for feed 2
  FEED3QNTY <- NULL # Creates matrix for the available feed quantity per day for feed 3
  
  TIMESTEPS <- c(1:length(DOY)) # Counts the time steps (equal to number of days)
  
  # Selection of feed types and feed quantities over simulation time if a specific diet 
  # (1-5) is chosen
  for(i in 1:length(DOY)){
    
    # Feed type 1
    if(FEEDNR[z]==1) F1[i,] <- WHEAT  
    if(FEEDNR[z]==2) F1[i,] <- BARLEY  
    if(FEEDNR[z]==3) F1[i,] <- BARLEY  
    if(FEEDNR[z]==4) F1[i,] <- BARLEY  
    if(FEEDNR[z]==5) F1[i,] <- BARLEY   
    
    # Feed type 2
    if(FEEDNR[z]==1) F2[i,] <- HAY 
    if(FEEDNR[z]==3) F2[i,] <- PASTURE1  
      
    if(TIMESTEPS[i] >=1 && TIMESTEPS[i] <=84 && FEEDNR[z]==2) F2[i,] <- HAY else 
    if(TIMESTEPS[i] >=85 && TIMESTEPS[i] <=344 && FEEDNR[z]==2)F2[i,] <- PASTURE1 else   
    if(TIMESTEPS[i] >=345 && TIMESTEPS[i] <=449 && FEEDNR[z]==2) F2[i,] <- HAY else
    if(TIMESTEPS[i] >=450 && TIMESTEPS[i] <= 709 && FEEDNR[z]==2) F2[i,] <- PASTURE1 else
    if(TIMESTEPS[i] >=710 && TIMESTEPS[i] <=814 && FEEDNR[z]==2) F2[i,] <- HAY else
    if(TIMESTEPS[i] >=815 && TIMESTEPS[i] <=1074 && FEEDNR[z]==2)F2[i,] <- PASTURE1 else
    if(TIMESTEPS[i] >=1075 && TIMESTEPS[i] <=1179 && FEEDNR[z]==2) F2[i,] <- HAY else
    if(TIMESTEPS[i] >=1180 && TIMESTEPS[i] <= 1439 && FEEDNR[z]==2) F2[i,] <- PASTURE1 else 
    if(TIMESTEPS[i] >=1 && TIMESTEPS[i] <=84 && FEEDNR[z]==5) F2[i,] <- HAY else 
    if(TIMESTEPS[i] >=85 && TIMESTEPS[i] <=344 && FEEDNR[z]==5)F2[i,] <- PASTURE1 else   
    if(TIMESTEPS[i] >=345 && TIMESTEPS[i] <=449 && FEEDNR[z]==5) F2[i,] <- HAY else
    if(TIMESTEPS[i] >=450 && TIMESTEPS[i] <= 709 && FEEDNR[z]==5) F2[i,] <- PASTURE1 else
    if(TIMESTEPS[i] >=710 && TIMESTEPS[i] <=814 && FEEDNR[z]==5) F2[i,] <- HAY else
    if(TIMESTEPS[i] >=815 && TIMESTEPS[i] <=1074 && FEEDNR[z]==5)F2[i,] <- PASTURE1 else
    if(TIMESTEPS[i] >=1075 && TIMESTEPS[i] <=1179 && FEEDNR[z]==5) F2[i,] <- HAY else
    if(TIMESTEPS[i] >=1180 && TIMESTEPS[i] <= 1439 && FEEDNR[z]==5) F2[i,] <- PASTURE1 else 
      F2[i,] <- HAY
    
    # Feed type 3                                                        
    F3[i,] <- HAY
    
    if(TIMESTEPS[i] >=1 && TIMESTEPS[i] <=84 && FEEDNR[z]==4) F3[i,] <- HAY  
    if(TIMESTEPS[i] >=85 && TIMESTEPS[i] <=344 && FEEDNR[z]==4)F3[i,] <- PASTURE1    
    if(TIMESTEPS[i] >=345 && TIMESTEPS[i] <=449 && FEEDNR[z]==4) F3[i,] <- HAY 
    if(TIMESTEPS[i] >=450 && TIMESTEPS[i] <= 709 && FEEDNR[z]==4) F3[i,] <- PASTURE1 
    if(TIMESTEPS[i] >=710 && TIMESTEPS[i] <=814 && FEEDNR[z]==4) F3[i,] <- HAY 
    if(TIMESTEPS[i] >=815 && TIMESTEPS[i] <=1074 && FEEDNR[z]==4)F3[i,] <- PASTURE1 
    if(TIMESTEPS[i] >=1075 && TIMESTEPS[i] <=1179 && FEEDNR[z]==4) F3[i,] <- HAY 
    if(TIMESTEPS[i] >=1180 && TIMESTEPS[i] <= 1439 && FEEDNR[z]==4) F3[i,] <- PASTURE1
    if(TIMESTEPS[i] >= 1439 && FEEDNR[z]==4) F3[i,] <- F3[i,] <- HAY
                     
                                          
    FEED1 <- F1
    FEED2 <- F2
    FEED3 <- F3
    
   # Feed quantity available per animal (kg DM day-1) for feed type 1
   FEED1QNTY[i] <- 20 
   if(FEEDNR[z]==4) FEED1QNTY[i] <- 1
   if(FEEDNR[z]==5) FEED1QNTY[i] <- (2*0.05)
   
   # Feed quantity available per animal (kg DM day-1) for feed type 2
   if(TIMESTEPS[i] <= 100) FEED2QNTY[i] <- ill.f1[z] else 
    if(TIMESTEPS[i] >=330 && TIMESTEPS[i] <=465) FEED2QNTY[i] <- ill.f1[z] else 
      FEED2QNTY[i] <- ill.f1[z] 
   
   # Feed quantity available per animal (kg DM day-1) for feed type 3
   if(TIMESTEPS[i] <= 100) FEED3QNTY[i] <- ill.f2[z] else FEED3QNTY[i] <- ill.f2[z]
  } 
  
  ###########################################################################################
  FEEDQNTYTOT <- FEED1QNTY + FEED2QNTY + FEED3QNTY # Feed quantity available per animal 
                                                   # (kg DM day-1) for feed type 1-3 
   
  # Fractions of feed types in the diet over time (see column 'feed composition' of Table 1)
  # Fraction of feed type 1 in the diet
  if(FEEDNR[z] == 1) FEED1fr <- 0.65 else if(FEEDNR[z] == 2) FEED1fr <- 0.05 else 
    if(FEEDNR[z] == 3) FEED1fr <- 0.05 else if(FEEDNR[z] == 4) FEED1fr <- 0.65 else 
      if(FEEDNR[z] == 5) FEED1fr <- 0.05    
  # Fraction of feed type 2 in the diet
  if(FEEDNR[z] == 1) FEED2fr <- 0.35 else if(FEEDNR[z] == 2) FEED2fr <- 0.95 else 
    if(FEEDNR[z] == 3) FEED2fr <- 0.95 else if(FEEDNR[z] == 4) FEED2fr <- 1.00 else 
      if(FEEDNR[z] == 5) FEED2fr <- 0.95
  # Fraction of feed types 3 and 4 in the diet
  FEED3fr <- 1.00
  FEED4fr <- 1.00
  
  ###########################################################################################
  # 1.3.3                               General parameters                                  #
  ###########################################################################################
  
  # General parameters used in physics and chemistry
  # Numbers between brackets refer to parameter numbers in Table S5 of the Supplementary 
  # Information. For more background information on these parameters, see the subscripts of 
  # Table S5.
  
  CtoK            = 273.15           # [1] absolute zero temperature (K)
  KtoR            = 9/5              # [2] conversion degrees Kelvin to degrees Rankine
  kJdaytoW        = 1000/(3600*24)   # [3] conversion kJ day-1 to Watt
  RUC             = 0.00078          # [4] resistance conversion from s m-1 to W m-2 K-1 
  EMISS           = 0.98             # [5] emissivity factor LWR (dimensionless)
  GRAV            = 9.81             # [6] gravitational constant (m s-2)
  L               = 2260             # [7] latent heat of vapour (kJ kg-1)
  GAMMA           = 66               # [8] psychrometric constant (Pa K-1)
  TR0             = 524              # [9] reference temperature air (degrees Rankine) 
  REFLEgrass      = 0.10             # [10] albedo vegetation (-)
  REFLEconcr      = 0.50             # albedo feedlot made of concrete (-)
  Schmidt         = 0.61             # [11] Schmidt number, dimensionless constant 
                                     # for calculation of the Grashof number
  Rwater          = 461.495          # [12] specific gas constant water vapour (J kg-1 K-1)
  Cp              = 1.005            # [13] specific heat of air (J kg-1 K-1)
  P               = 101325           # [14] standard air pressure at sea level (Pa)
  MuSt            = 1.827 * 10^(-5)  # [15] standard air viscosity (N s-1 m-2)
  SIGMA           = 5.67037 * 10^-8  # [16] Stefan-Boltzmann constant (W m-2 K-4)
  ST              = 120              # [17] Sutherlands constant in standard air (degrees 
                                     # Rankine) for calculation air viscosity    
  Rdair           = 287.058          # [18] universal gas constant (J kg-1 K-1)
  CALTOJOULE      = 4.184            # [19] conversion factor from calories to joules
  NtoCP           = 6.25             # [20] conversion from N to crude protein
  GECARB          = 17.4             # [21] gross energy carbohydrates, combustion value (MJ
                                     # kg-1 DM) 
  GEFEED          = 18.5             # [22] gross energy feed types in general, combustion 
                                     # value (MJ kg-1 DM)  
  GELIPID         = 39.6             # [23] gross energy lipid, combustion value (MJ kg-1 
                                     # DM) (Emmans, 1994)
  GEPROT          = 23.8             # [24] gross energy protein, combustion value (MJ kg-1 
                                     # DM) (Emmans, 1994)
  ###########################################################################################
  
  # Parameters for cattle (not breed-specific)
  # Numbers between brackets indicate parameter numbers as given in Table S6 of the 
  # Supplementary information of Van der Linden et al.
  
  CoatConst       = 1.90 * 10^(-5)  * SENSMAT[ 1,s]  # [9] constant (m) (McGovern and Bruce, 
                                                     # 2000)
  ZC              = 11000           * SENSMAT[ 2,s]  # [10] coat resistance (s m-2) (McGovern 
                                                     # and Bruce, 2000)
  TbodyC          = 39              * SENSMAT[ 3,s]  # [8] body temperature animal (degrees 
                                                     # Celsius) (McGovern and Bruce, 2000) 
  LASMIN          = 10              * SENSMAT[ 4,s]  # [21] minimum latent heat release skin 
                                                     # (W m-2) (Turnpenny et al., 2000a; 
                                                     # Turnpenny et al., 2000b)
  PHFEEDCAP       = 123             * SENSMAT[ 5,s]  # [27] maximum feed intake of reference 
                                                     # grass (g DM kg TBW-0.75) (Estimated 
                                                     # from Jarrige et al., 1986) 
  RESPINCR        = 7.64            * SENSMAT[ 6,s]  # [18] maximum increase in air exchange 
                                                     # rate under heat stress (Calculated 
                                                     # from McGovern and Bruce, 2000)
  PROTFRACBONE    = 0.23            * SENSMAT[ 7,s]  # [48] protein fraction in bone (Field 
                                                     # et al., 1974)
  PROTFRACMUSCLE  = 0.21            * SENSMAT[ 8,s]  # [51] protein fraction in muscle 
                                                     # (Consuleanu et al, 2008)
  LIPFRACMUSCLE   = 0.005           * SENSMAT[ 9,s]  # [47] lipid fraction in muscle (Warren 
                                                     # et al., 2008)
  PROTFRACFAT     = 0.08            * SENSMAT[10,s]  # [49] protein fraction in fat tissue 
                                                     # (Thonney, 2012)
  LIPFRACFAT      = 0.70            * SENSMAT[11,s]  # [46] lipid fraction in fat tissue 
                                                     # (Thonney, 2012)
  INCARC          = 0.50            * SENSMAT[12,s]  # [35] fraction carcass at birth 
                                                     # (estimate)
  RUMENFRAC       = 0.10            * SENSMAT[13,s]  # [52] fraction rumen in total body 
                                                     # weight (estimate)
  NEm             = 311             * SENSMAT[14,s]  # [78] NE for maintenance (kJ NE kg 
                                                     # EBW-0.75, for B. taurus cattle) 
                                                     # (Ouellet et al, 1998)
  NEpha           = 70              * SENSMAT[15,s]  # [79] NE for physical activity (kJ NE 
                                                     # kg EBW-0.75) (CSIRO, 2007)
  BONEFRACMAX     = 0.25            * SENSMAT[16,s]  # [64] maximum fraction bone in carcass 
                                                     # (estimated from Berg and Butterfield, 
                                                     # 1968)
  LIPNONCMAX      = 0.80            * SENSMAT[17,s]  # [65] maximum fraction lipid accretion 
                                                     # in the non-carcass tissue (assumption, 
                                                     # resembles fat tissue)
  LIPNONCMIN      = 0.15            * SENSMAT[18,s]  # [67] minimum fraction lipid accretion 
                                                     # in the non-carcass tissue (assumption) 
  PROTEFF         = 0.54            * SENSMAT[19,s]  # [76] NE efficiency of protein 
                                                     # accretion (MSU, 2014)
  LIPIDEFF        = 0.74            * SENSMAT[20,s]  # [75] NE efficiency of lipid accretion 
                                                     # (MSU, 2014)
  DERMPL          = 0.11            * SENSMAT[21,s]  # [39] dermal protein loss protein 
                                                     # (g kg-0.75 EBW day-1) (CSIRO, 2007)
  PROTNE          = 2.0 / CALTOJOULE* SENSMAT[22,s]  # [88] protein requirement for NE 
                                                     # (g MJ-1 NE) (CSIRO, 2007)
  GestPer         = 286             * SENSMAT[23,s]  # [53] gestation period (days) 
                                                     # (Blanc and Agabriel, 2008)
  GESTINTERVAL    = 365             * SENSMAT[24,s]  # [66] minimum calving interval in days
  WEANINGTIME     = 210             * SENSMAT[25,s]  # [89] weaning time in days 
                                                     # (Jenkins and Ferrell, 1992)
  FtoConcW        = 75/45           * SENSMAT[26,s]  # [37] conversion foetus weight to total 
                                                     # concepta weight (Jarrige et al., 1986, 
                                                     # p. 99)   
  FATFACTOR       = 0.065           * SENSMAT[27,s]  # [45] factor determing fat accretion ()
  RAINEXP         = 0.50            * SENSMAT[28,s]  # [16] fraction animal area exposed to 
                                                     # rain
  FRACVEG         = 0.50            * SENSMAT[29,s]  # [17] fraction of the animal facing the 
                                                     # vegetation in free grazing systems
  COMPFACT        = 4               * SENSMAT[30,s]  # [44] factor indicating the magnitude 
                                                     # in compensatory growth (dimensionless)
  NEIEFFGEST      = 0.766           * SENSMAT[31,s]  # [73] inefficiency of NE for gestation 
                                                     # (1-efficiency) Calculated based on 
                                                     # Jarrige (1989) and Rattray et al. 
                                                     # (1974)                          
  CPGEST          = 4.322           * SENSMAT[32,s]  # [38] protein requirements for 
                                                     # gestation (g protein MJ-1 NE)
  MILKDIG         = 0.95            * SENSMAT[33,s]  # [40] digestible fraction of milk, 
                                                     # based on energy content of milk
  NEEFFMILK       = 0.85            * SENSMAT[34,s]  # [74] efficiency of conversion of NE to 
                                                     # milk (energy basis)
  PROTFRACMILK    = 0.04            * SENSMAT[35,s]  # [50] fraction protein in milk
  PROTEFFMILK     = 0.68            * SENSMAT[36,s]  # [82] protein efficiency for milk 
                                                     # production (CSIRO, 2007)
  COMPFACTTIS     = 1.20            * SENSMAT[37,s]  # [63] maximum multiplicative for 
                                                     # compensatory growth (set at 120% of 
                                                     # genetic potential)
  FATTISCOMP      = 0.80            * SENSMAT[38,s]  # [36] if fat tissue is lower than 80%  
                                                     # of the potential, energy is allocated   
                                                     # to the fat tissue for 'refill'
  TTDIGINSC       = 0.97            * SENSMAT[39,s]  # [31] fraction total tract 
                                                     # digestibility of insoluble, 
                                                     # non-structural carbohydrates 
                                                     # (Moharrery et al, 2014)
  DETOME          = 0.82            * SENSMAT[40,s]  # [26] conversion from digestible 
                                                     # energy (DE) to metabolisable energy 
                                                     # (ME)
  DISSEFF         = 0.90            * SENSMAT[41,s]  # [41] efficiency of dissimilation of 
                                                     # protein and lipid
  RAINFRAC        = 0.3             * SENSMAT[91,s]  # [22] 30% reduction in conductance due
                                                     # to rain (Mount and Brown, 1982)
  BONEGROWTH1     = 0.6436          * SENSMAT[68,s]  # [33] bone growth parameter (kg)
  BONEGROWTH2     = 0.262           * SENSMAT[69,s]  # [34] bone growth parameter (kg)
  MUSCLEGROWTH1   = -2*10^-5        * SENSMAT[70,s]  # [68] muscle growth parameter
  MUSCLEGROWTH2   = 1.564           * SENSMAT[71,s]  # [69] muscle growth parameter
  IMFGROWTH1      = 0.0001          * SENSMAT[72,s]  # [56] intramuscular fat growth 
                                                     # parameter
  IMFGROWTH2      = 0.01            * SENSMAT[73,s]  # [57] intramuscular fat growth 
                                                     # parameter
  IMFGROWTH3      = 0.04            * SENSMAT[74,s]  # [58] intramuscular fat growth 
                                                     # parameter
  PROTNONCM1      = -7.014*(10^-3)  * SENSMAT[75,s]  # [80] max. protein content non-carcass 
  PROTNONCM2      = 20.4            * SENSMAT[76,s]  # [81] max. protein content non-carcass
  RESPDUR         = 0.25            * SENSMAT[77,s]  # [23] fraction day maximum respiration
                                                     # is used
  BODYAREA1       = 0.14            * SENSMAT[78,s]  # [4] parameter to calculate body area 
                                                     # (m-2)
  BODYAREA2       = 0.57            * SENSMAT[79,s]  # [5] parameter to calculate body area 
                                                     # (m-2)
  DIAMETER1       = 0.06            * SENSMAT[80,s]  # [6] parameter to calculate body 
                                                     # diameter (m-2)
  DIAMETER2       = 0.39            * SENSMAT[81,s]  # [7] parameter to calculate body 
                                                     # diameter (m-2)
  BASALRR1        = 73.8            * SENSMAT[82,s]  # [1] basal respiration rate (min-1)
  BASALRR2        = -0.286          * SENSMAT[83,s]  # [2] basal respiration rate
  BASALTV         = 0.0117          * SENSMAT[84,s]  # [3] basal tidal volume (L min-1) 
  TEXHALED1       = 17              * SENSMAT[85,s]  # [12] exhaled temperature (degrees 
                                                     # Celsius)
  TEXHALED2       = 0.3             * SENSMAT[86,s]  # [13] exhaled temperature
  TEXHALED3       = 0.01611         * SENSMAT[87,s]  # [14] exhaled temperature
  TEXHALED4       = 0.0387          * SENSMAT[88,s]  # [15] exhaled temperature
  MINCCS1         = 0.03            * SENSMAT[89,s]  # [19] min. conductance core-skin (W 
                                                     # m-2 K-1)
  MINCCS2         = 0.33            * SENSMAT[90,s]  # [20] min. conductance core-skin 
                                                     # (kg-1 total body weight)
  RAINEVAP1       = 0.15            * SENSMAT[92,s]  # [11] evaporation rain from coat
  GEMILK1         = 5.5109          * SENSMAT[94,s]  # [54] gross energy milk (kJ L-1)
  GEMILK2         = 2589            * SENSMAT[95,s]  # [55] gross energy milk (kJ L-1)
  LIPBONE1        = 0.075           * SENSMAT[96,s]  # [59] lipid fraction bone 
  LIPBONE2        = 3.0496          * SENSMAT[97,s]  # [60] lipid fraction bone
  LIPBONE3        = 3.3268          * SENSMAT[98,s]  # [61] lipid fraction bone
  LIPNONC1        = 4.7915*10^-7    * SENSMAT[99,s]  # [62] lipid fraction non-carcass
  LIPNONC2        = 0.00010757      * SENSMAT[100,s] # [62] lipid fraction non-carcass
  LIPNONC3        = 0.105717        * SENSMAT[101,s] # [62] lipid fraction non-carcass
  LIPNONC4        = 2.1723          * SENSMAT[102,s] # [62] lipid fraction non-carcass
  PROTNONC1       = 8.7492*10^-10   * SENSMAT[103,s] # [83] protein fraction non-carcass  
  PROTNONC2       = 9.0732*10^-7    * SENSMAT[104,s] # [84] protein fraction non-carcass 
  PROTNONC3       = 0.00033117      * SENSMAT[105,s] # [85] protein fraction non-carcass 
  PROTNONC4       = 0.061756        * SENSMAT[106,s] # [86] protein fraction non-carcass 
  PROTNONC5       = 22.26           * SENSMAT[107,s] # [87] protein fraction non-carcass 
  RUMENDEV1       = 0.007246        * SENSMAT[108,s] # [29] parameter rumen development
  RUMENDEV2       = 0.101449        * SENSMAT[109,s] # [30] parameter rumen development
  NDFDIGEST       = 0.9             * SENSMAT[110,s] # [32] total tract DNDF digestibility
  NDFPASS         = 0.125           * SENSMAT[111,s] # [28] passage rate DNDF 
  LUCAS1          = 0.9             * SENSMAT[112,s] # [24] slope Lucas equation
  LUCAS2          = 32              * SENSMAT[113,s] # [25] intercept Lucas equation 
                                                     # (g kg-1 DM)
  ENNONC1         = 0.60            * SENSMAT[114,s] # [42] energy partitioning non-carcass 
  ENNONC2         = 0.03            * SENSMAT[115,s] # [43] energy partitioning non-carcass
  NRECYCL1        = 121.7           * SENSMAT[116,s] # [70] N recycling 
  NRECYCL2        = 12.01           * SENSMAT[117,s] # [71] N recycling
  NRECYCL3        = 0.3235          * SENSMAT[118,s] # [72] N recycling
  
  # Gross energy content fat tissue (MJ kg-1)
  GEFATTIS        = GEPROT * PROTFRACFAT + GELIPID * LIPFRACFAT       
  # Gross energy content muscle tissue (MJ kg-1)
  GEMUSCLETIS     = GEPROT * PROTFRACMUSCLE + GELIPID * LIPFRACMUSCLE 
  # Passage reduction factors for different classes of rumen fill (Chilibroste et al, 1997)
  PASSRED         <- c(1,0.85,0.65,0.55)                          
  
  ###########################################################################################
  # 1.4                             Specification of variables                              #
  ###########################################################################################
  
  ###########################################################################################
  # 1.4.1     Specification of variables for the feed intake and digestion sub-model        #
  ###########################################################################################
  
  # Available feed quantity for feed type 1 (kg DM per animal per day)
  FEED1QNTY <- matrix(nrow=length(DOY), ncol = jmax, byrow = F, rep(FEED1QNTY,jmax))
  FEED1QNTY <- FEED1QNTY[1:imax,] 
  
  # Available feed quantity for feed type 2 (kg DM per animal per day)
  FEED2QNTY <- matrix(nrow=length(DOY), ncol = jmax, byrow = F, rep(FEED2QNTY,jmax))
  FEED2QNTY <- FEED2QNTY[1:imax,]
  
  # Available feed quantity for feed type 3 (kg DM per animal per day)
  FEED3QNTY <- matrix(nrow=length(DOY), ncol = jmax, byrow = F, rep(FEED3QNTY,jmax))
  FEED3QNTY <- FEED3QNTY[1:imax,]
  
  # Available feed quantity for feed type 4 (kg DM per animal per day)              
  FEED4QNTY <- c(rep(rep( 0.0,imax),jmax))  
  
  FEED1QNTY <- matrix(nrow=imax, ncol=jmax, FEED1QNTY)  
  FEED2QNTY <- matrix(nrow=imax, ncol=jmax, FEED2QNTY) 
  FEED3QNTY <- matrix(nrow=imax, ncol=jmax, FEED3QNTY) 
  FEED4QNTY <- matrix(nrow=imax, ncol=jmax, FEED4QNTY) 

  FEEDQNTY <- FEED1QNTY + FEED2QNTY + FEED3QNTY + FEED4QNTY
  
  FRACFEED1 <- matrix(nrow=imax, ncol=jmax)
  FRACFEED2 <- matrix(nrow=imax, ncol=jmax)
  FRACFEED3 <- matrix(nrow=imax, ncol=jmax)
  FRACFEED4 <- matrix(nrow=imax, ncol=jmax)
  
  FEED1QNTYA <- matrix(nrow=imax, ncol=jmax) 
  FEED2QNTYA <- matrix(nrow=imax, ncol=jmax) 
  FEED3QNTYA <- matrix(nrow=imax, ncol=jmax) 
  FEED4QNTYA <- matrix(nrow=imax, ncol=jmax) 
  
  PASSDIFF <- matrix(nrow=imax, ncol=jmax)
  
  PENDF           <- matrix(nrow=imax, ncol=jmax)
  Digestfracfeed  <- matrix(nrow=imax, ncol=jmax)
  INSC            <- matrix(nrow=imax, ncol=jmax)
  INSCTOTAL       <- matrix(nrow=imax, ncol=jmax)
  INSCDIG         <- matrix(nrow=imax, ncol=jmax)
  INSCINT         <- matrix(nrow=imax, ncol=jmax)
  INSCINTDIG      <- matrix(nrow=imax, ncol=jmax)
  NDF             <- matrix(nrow=imax, ncol=jmax)
  NDFTOTAL        <- matrix(nrow=imax, ncol=jmax)
  NDFDIG          <- matrix(nrow=imax, ncol=jmax)
  NDFINT          <- matrix(nrow=imax, ncol=jmax)
  NDFINTDIG       <- matrix(nrow=imax, ncol=jmax)
  NDFINTDIGTOT    <- matrix(nrow=imax, ncol=jmax)
  PICP            <- matrix(nrow=imax, ncol=jmax)
  PROTTOTAL       <- matrix(nrow=imax, ncol=jmax)
  PROTINT         <- matrix(nrow=imax, ncol=jmax)
  PROTUPT         <- matrix(nrow=imax, ncol=jmax)
  PROTEXCR        <- matrix(nrow=imax, ncol=jmax)
  PROTDIGRU       <- matrix(nrow=imax, ncol=jmax)
  PROTDIGWT       <- matrix(nrow=imax, ncol=jmax)
  PROTBAL         <- matrix(nrow=imax, ncol=jmax)
  PROTREDFACT     <- matrix(nrow=imax, ncol=jmax)
  DIGFRAC         <- matrix(nrow=imax, ncol=jmax)
  CHEXCR          <- matrix(nrow=imax, ncol=jmax)
  EXCRFRAC        <- matrix(nrow=imax, ncol=jmax)
  GEEXCR          <- matrix(nrow=imax, ncol=jmax)
  GEUPTAKE        <- matrix(nrow=imax, ncol=jmax)
  MEUPTAKE        <- matrix(nrow=imax, ncol=jmax)
  Q               <- matrix(nrow=imax, ncol=jmax)
  
  PHFEEDINT   = matrix(nrow=imax, ncol=jmax)
  PHFEEDINTKG = matrix(nrow=imax, ncol=jmax)
  PASSAGE     = matrix(nrow=imax, ncol=jmax)
  PASSAGE1    = matrix(nrow=imax, ncol=jmax)
  FUFEED1     = matrix(nrow=imax, ncol=jmax)
  FUFEED2     = matrix(nrow=imax, ncol=jmax)
  FUFEED3     = matrix(nrow=imax, ncol=jmax)
  FUFEED4     = matrix(nrow=imax, ncol=jmax)
  AVGDIGFRAC  = matrix(nrow=imax, ncol=jmax)
  MEDAILYMAX  = matrix(nrow=imax, ncol=jmax)
  MEDIGLIMGR  = matrix(nrow=imax, ncol=jmax) 
  FEEDINTAKE  = matrix(nrow=imax, ncol=jmax)
  FILLGIT     = matrix(nrow=imax, ncol=jmax)
  
  ###########################################################################################
  # 1.4.2         Specification of variables for the thermoregulation sub-model             #
  ###########################################################################################
  
  # 1. Respiration
  TBW          = matrix(nrow=imax+1, ncol=jmax)
  AREA         = matrix(nrow=imax, ncol=jmax)
  DIAMETER     = matrix(nrow=imax, ncol=jmax)
  LENGTH       = matrix(nrow=imax, ncol=jmax)
  brr          = matrix(nrow=imax, ncol=jmax)
  btv          = matrix(nrow=imax, ncol=jmax)
  Vtb          = matrix(nrow=imax, ncol=jmax)
  brv          = matrix(nrow=imax, ncol=jmax)
  irv          = matrix(nrow=imax, ncol=jmax)
  TAVGC        = matrix(nrow=imax, ncol=jmax)
  TAVGK        = matrix(nrow=imax, ncol=jmax)
  VPSATAIR     = matrix(nrow=imax, ncol=jmax) 
  VPAIRTOT     = matrix(nrow=imax, ncol=jmax)
  RHAIR        = matrix(nrow=imax, ncol=jmax)
  RHOVP        = matrix(nrow=imax, ncol=jmax)
  RHODAIR      = matrix(nrow=imax, ncol=jmax)
  RHOAIR       = matrix(nrow=imax, ncol=jmax)
  CHIAIR       = matrix(nrow=imax, ncol=jmax)
  VISCAIR      = matrix(nrow=imax, ncol=jmax)
  Texh         = matrix(nrow=imax, ncol=jmax)
  VPSATAIROUT  = matrix(nrow=imax, ncol=jmax)
  RHOVPOUT     = matrix(nrow=imax, ncol=jmax)
  RHODAIROUT   = matrix(nrow=imax, ncol=jmax)
  RHOAIROUT    = matrix(nrow=imax, ncol=jmax)
  CHIAIROUT    = matrix(nrow=imax, ncol=jmax)
  AIREXCH      = matrix(nrow=imax, ncol=jmax)
  LHEATRESP    = matrix(nrow=imax, ncol=jmax)
  CHEATRESP    = matrix(nrow=imax, ncol=jmax)
  TGRESP       = matrix(nrow=imax, ncol=jmax)
  TNRESP       = matrix(nrow=imax, ncol=jmax)
  TNRESPH      = matrix(nrow=imax, ncol=jmax)
  NERESP       = matrix(nrow=imax, ncol=jmax)
  NERESPWM     = matrix(nrow=imax, ncol=jmax)
  NERESPC      = matrix(nrow=imax, ncol=jmax)
  MetheatSKIN  = matrix(nrow=imax, ncol=jmax)
  TskinC       = matrix(nrow=imax, ncol=jmax)
  TskinCH      = matrix(nrow=imax, ncol=jmax)
  CBSMIN       = matrix(nrow=imax, ncol=jmax)
  CONDBS       = matrix(nrow=imax, ncol=jmax)
  
  # 2. Latent heat release from the skin 
  DLC          = matrix(nrow=imax, ncol=jmax)
  DIFFC        = matrix(nrow=imax, ncol=jmax)
  RV           = matrix(nrow=imax, ncol=jmax)
  VPSKINTOT    = matrix(nrow=imax, ncol=jmax)
  LASMAXENV    = matrix(nrow=imax, ncol=jmax)
  LASMAXPHYS   = matrix(nrow=imax, ncol=jmax)
  LASMAXCORR   = matrix(nrow=imax, ncol=jmax)
  ACTSW        = matrix(nrow=imax, ncol=jmax)
  ACTSWH       = matrix(nrow=imax, ncol=jmax)
  CSC          = matrix(nrow=imax, ncol=jmax)
  MetheatCOAT  = matrix(nrow=imax, ncol=jmax)
  TcoatC       = matrix(nrow=imax, ncol=jmax)
  TcoatCH      = matrix(nrow=imax, ncol=jmax)
  TcoatK       = matrix(nrow=imax, ncol=jmax)
  
  # 3.LWR heat balance of the coat
  LWRSKY       = matrix(nrow=imax, ncol=jmax)
  LWRENV       = matrix(nrow=imax, ncol=jmax)
  LB           = matrix(nrow=imax, ncol=jmax)
  LWRCOAT      = matrix(nrow=imax, ncol=jmax)
  LWRCOATH     = matrix(nrow=imax, ncol=jmax)
  
  # 4.Convective heat losses from the coat
  TAVGR        = matrix(nrow=imax, ncol=jmax)
  Ea           = matrix(nrow=imax, ncol=jmax)                                        
  Ec           = matrix(nrow=imax, ncol=jmax)                      
  GRASHOF    = matrix(nrow=imax, ncol=jmax)
  WINDSP       = matrix(nrow=imax, ncol=jmax)
  REYNOLDS     = matrix(nrow=imax, ncol=jmax)
  ReH          = matrix(nrow=imax, ncol=jmax)                                                                 
  ReL          = matrix(nrow=imax, ncol=jmax)
  NUSSELTH     = matrix(nrow=imax, ncol=jmax)
  NUSSELTL     = matrix(nrow=imax, ncol=jmax)
  NUSSELT      = matrix(nrow=imax, ncol=jmax)
  NUSSELTM     = matrix(nrow=imax, ncol=jmax)
  ka           = matrix(nrow=imax, ncol=jmax)
  CONVCOAT     = matrix(nrow=imax, ncol=jmax)
  CONVCOATH    = matrix(nrow=imax, ncol=jmax)
  
  # 5. Incoming SWR (solar radiation) to coat
  SAAC         = matrix(nrow=imax, ncol=jmax)
  SWRS         = matrix(nrow=imax, ncol=jmax)
  SWRC         = matrix(nrow=imax, ncol=jmax)
  ISWRC        = matrix(nrow=imax, ncol=jmax)
  REFLE        = NULL
  SWR          = matrix(nrow=imax, ncol=jmax)
  RAINEVAP     = matrix(nrow=imax, ncol=jmax)
  
  # Synthesis and optimization with repeat {} function
  MetheatBAL   = matrix(nrow=imax, ncol=jmax)
  Metheatopt   = matrix(nrow=imax, ncol=jmax)
  METABFEED0   = matrix(rep(100,imax*jmax),nrow=imax, ncol=jmax)
  
  ###########################################################################################
  # 1.4.3     Specification of variables for energy and protein utilisation sub-model       #
  ###########################################################################################
  
  # Synthesis and optimisation
  EX           = matrix(nrow=imax, ncol=jmax)
  
  # Weight and derivative body tissues
  ADGHIGH      = matrix(nrow=imax+1, ncol=jmax)
  ADGHIGH[1,1:jmax] <- 0
  TBWCHECK     = matrix(nrow=imax+1, ncol=jmax)
  CARCW        = matrix(nrow=imax+1, ncol=jmax)
  BONETIS      = matrix(nrow=imax+1, ncol=jmax)
  MUSCLETIS    = matrix(nrow=imax+1, ncol=jmax)
  INTRAMFTIS   = matrix(nrow=imax+1, ncol=jmax)
  MISCFATTIS   = matrix(nrow=imax+1, ncol=jmax)
  NONCARCTIS   = matrix(nrow=imax+1, ncol=jmax)
  RUMEN        = matrix(nrow=imax+1, ncol=jmax)
  
  DERBONE      = matrix(nrow=imax+1, ncol=jmax)
  DERMUSCLE    = matrix(nrow=imax+1, ncol=jmax)
  DERINTRAMF   = matrix(nrow=imax+1, ncol=jmax)
  DERMISCFAT   = matrix(nrow=imax+1, ncol=jmax)
  DERNONC      = matrix(nrow=imax+1, ncol=jmax)
  DERRUMEN     = matrix(nrow=imax+1, ncol=jmax)
  DERTOTAL     = matrix(nrow=imax+1, ncol=jmax)
  
  # Lipid and protein concentrations in body tissues
  LIPIDFRACBONE    = matrix(nrow=imax+1, ncol=jmax)
  LIPIDFRACBONEBF  = matrix(nrow=imax+1, ncol=jmax)
  LIPIDFRACNONC    = matrix(nrow=imax+1, ncol=jmax)
  LIPIDFRACNONCBF  = matrix(nrow=imax+1, ncol=jmax)
  PROTFRACNONC     = matrix(nrow=imax+1, ncol=jmax)
  PROTFRACNONCBF   = matrix(nrow=imax+1, ncol=jmax)
  ENFEEDGROWTH     = matrix(nrow=imax+1, ncol=jmax)
  ENFEEDGROWTHQ    = matrix(nrow=imax+1, ncol=jmax)
  
  # Bone carcass
  LIPIDBONE    = matrix(nrow=imax+1, ncol=jmax)
  PROTBONE     = matrix(nrow=imax+1, ncol=jmax)
  ENGRBONE     = matrix(nrow=imax+1, ncol=jmax)
  
  # Muscle carcass
  LIPIDMUSCLE  = matrix(nrow=imax+1, ncol=jmax)
  PROTMUSCLE   = matrix(nrow=imax+1, ncol=jmax)
  ENGRMUSCLE   = matrix(nrow=imax+1, ncol=jmax)
  
  # Intramuscular fat
  LIPIDIMF     = matrix(nrow=imax+1, ncol=jmax)               
  PROTIMF      = matrix(nrow=imax+1, ncol=jmax)
  ENGRIMF      = matrix(nrow=imax+1, ncol=jmax)
  
  # Subcutaneous and intermuscular fat
  LIPIDFAT     = matrix(nrow=imax+1, ncol=jmax)                
  PROTFAT      = matrix(nrow=imax+1, ncol=jmax)
  ENGRFAT      = matrix(nrow=imax+1, ncol=jmax)
  
  # Non carcass tissue
  LIPIDNONC    = matrix(nrow=imax+1, ncol=jmax)                 
  PROTNONC     = matrix(nrow=imax+1, ncol=jmax)
  ENGRNONC     = matrix(nrow=imax+1, ncol=jmax)
  
  # Growth influenced by defining and limiting biophyscial factors
  ENGRNONCBF   = matrix(nrow=imax+1, ncol=jmax)
  ENGRBONEBF   = matrix(nrow=imax+1, ncol=jmax)
  ENGRIMFBF    = matrix(nrow=imax+1, ncol=jmax)
  ENGRMUSCLEBF = matrix(nrow=imax+1, ncol=jmax)
  ENGRFATBF    = matrix(nrow=imax+1, ncol=jmax)
  ENGRTOTAL     = matrix(nrow=imax+1, ncol=jmax)
  ENGRTOTALHIGH = matrix(nrow=imax+1, ncol=jmax)
  ENGRTOTALHIGH[1,1:jmax] <- 0
  ENGRTOTALHIGH1 =  matrix(nrow=imax+1, ncol=jmax)
  ENGRTOTALHIGH1[1,1:jmax] <- 0
  REL           =   matrix(nrow=imax+1, ncol=jmax)
  REL[1,1:jmax] <- 0
  ENGRTOTALORIG =   matrix(nrow=imax+1, ncol=jmax)
  
  FRENGRNONCBF   = matrix(nrow=imax+1, ncol=jmax)
  FRENGRBONEBF   = matrix(nrow=imax+1, ncol=jmax)
  FRENGRIMFBF    = matrix(nrow=imax+1, ncol=jmax)
  FRENGRMUSCLEBF = matrix(nrow=imax+1, ncol=jmax)
  FRENGRFATBF    = matrix(nrow=imax+1, ncol=jmax)
  FRENGRTOTAL     = matrix(nrow=imax+1, ncol=jmax)
  
  BONETISBF      = matrix(nrow=imax+1, ncol=jmax)
  MUSCLETISBF    = matrix(nrow=imax+1, ncol=jmax)
  INTRAMFTISBF   = matrix(nrow=imax+1, ncol=jmax)
  MISCFATTISBF   = matrix(nrow=imax+1, ncol=jmax)
  NONCARCTISBF   = matrix(nrow=imax+1, ncol=jmax)
  TBWBF          = matrix(nrow=imax+1, ncol=jmax)
  EBWBFMET       = matrix(nrow=imax+1, ncol=jmax)
  
  MISCFATFRAC     = matrix(nrow=imax+1, ncol=jmax)
  LIPIDBONEBF     = matrix(nrow=imax+1, ncol=jmax)
  LIPIDNONCBF     = matrix(nrow=imax+1, ncol=jmax)
  PROTNONCBF      = matrix(nrow=imax+1, ncol=jmax)
  LIPIDMUSCLEBF   = matrix(nrow=imax+1, ncol=jmax)
  LIPIDIMFBF      = matrix(nrow=imax+1, ncol=jmax)
  LIPIDFATBF      = matrix(nrow=imax+1, ncol=jmax)
  LIPIDTOTW       = matrix(nrow=imax+1, ncol=jmax)
  LIPIDFRACCARC   = matrix(nrow=imax+1, ncol=jmax)
  
  ENCONTENTNONCBF = matrix(nrow=imax+1, ncol=jmax)
  
  # Maintenance
  NEMAINT     = matrix(nrow=imax, ncol=jmax)
  NEMAINTWM   = matrix(nrow=imax, ncol=jmax)
  PROTDERML   = matrix(nrow=imax, ncol=jmax)
  PROTMAINT   = matrix(nrow=imax, ncol=jmax)
  PROTRESP    = matrix(nrow=imax, ncol=jmax)
  
  # Physical activity
  NEPHYSACT   = matrix(nrow=imax, ncol=jmax)
  NEPHYSACTWM = matrix(nrow=imax, ncol=jmax)
  PROTPHACT   = matrix(nrow=imax, ncol=jmax)
  
  # Gestation
  CALFTBW   = matrix(nrow=imax+1, ncol=jmax)
  CALFNR    = matrix(nrow=imax+1, ncol=jmax)
  BIRTHW1   = matrix(nrow=imax, ncol=jmax)
  
  GEST1 = matrix(nrow=imax, ncol=jmax)
  GEST2 = matrix(nrow=imax, ncol=jmax)
  GEST3 = matrix(nrow=imax, ncol=jmax)
  GEST4 = matrix(nrow=imax, ncol=jmax)
  GEST5 = matrix(nrow=imax, ncol=jmax)
  GEST6 = matrix(nrow=imax+1, ncol=jmax)
  GEST  = matrix(nrow=imax, ncol=jmax) 
  
  GESTDAY         = matrix(nrow=imax+1, ncol=jmax)
  NEREQGEST       = matrix(nrow=imax, ncol=jmax)
  NEREQGESTADD    = matrix(nrow=imax+1, ncol=jmax)
  NEREQGESTTOT    = matrix(nrow=imax, ncol=jmax)
  HEATGEST        = matrix(nrow=imax, ncol=jmax)
  PROTGESTG       = matrix(nrow=imax, ncol=jmax)
  TBWADD          = matrix(nrow=imax+1, ncol=jmax)
  
  # Milk production
  MILKDAYST       = matrix(nrow=imax, ncol=jmax)
  MILKDAY         = matrix(nrow=imax+1, ncol=jmax)
  MILKWEEK        = matrix(nrow=imax+1, ncol=jmax)
  ADDMILK1        = matrix(nrow=imax, ncol=jmax)
  ADDMILK2        = matrix(nrow=imax, ncol=jmax)
  
  MAXMILKPROD     = matrix(nrow=imax, ncol=jmax)
  GEMILK          = matrix(nrow=imax, ncol=jmax)
  GEMILKTOT       = matrix(nrow=imax, ncol=jmax)
  MEMILKCALF      = matrix(nrow=imax, ncol=jmax)
  MEMILKCALFINIT  = matrix(nrow=imax, ncol=jmax)
  NEMILKCOW       = matrix(nrow=imax, ncol=jmax)
  CALFLIVENR      = matrix(nrow=imax, ncol=jmax)
  CALFWEANNR      = matrix(nrow=imax, ncol=jmax)
  MILKPRODBF      = matrix(nrow=imax, ncol=jmax)
  HEATMILK        = matrix(nrow=imax, ncol=jmax)
  NETMILKEN       = matrix(nrow=imax+1, ncol=jmax) 
  PROTMILK        = matrix(nrow=imax, ncol=jmax)
  PROTMILKG       = matrix(nrow=imax, ncol=jmax)
  
  # ME total
  MEREQTOTAL       = matrix(nrow=imax, ncol=jmax)
  MEREQTOTAL2      = matrix(nrow=imax, ncol=jmax)
  NETMILKEN        = matrix(nrow=imax, ncol=jmax)
  
  # Cold stress
  Metheatcold      = matrix(rep(NA,imax*jmax), nrow=imax, ncol=jmax)
  METABSTARTCOLD   = matrix(rep(100,imax*jmax),nrow=imax, ncol=jmax)
  TOTHEAT          = matrix(nrow=imax, ncol=jmax)
  FATBURN          = matrix(nrow=imax, ncol=jmax)
  REDTIS2          = matrix(nrow=imax, ncol=jmax)
  REDTIS3          = matrix(nrow=imax, ncol=jmax)
  MAINTFRAC        = matrix(nrow=imax, ncol=jmax)
  FATFRACCARC      = matrix(nrow=imax, ncol=jmax)
  
  ###########################################################################################
  # 1.4.4                   Variables for integration of sub-models                         #
  ###########################################################################################
  
  COMPGROWTH     = matrix(nrow=imax+1, ncol=jmax)
  COMPGROWTH1    = matrix(nrow=imax+1, ncol=jmax)
  COMPGROWTH2    = matrix(nrow=imax+1, ncol=jmax)
  COMPGROWTH3    = matrix(nrow=imax+1, ncol=jmax)
  COMPGROWTH4    = matrix(nrow=imax+1, ncol=jmax)
  COMPGROWTH5    = matrix(nrow=imax+1, ncol=jmax)
  
  ENGRTOTALCOMP  = matrix(nrow=imax+1, ncol=jmax)
  
  HEATBONEACT    = matrix(nrow=imax+1, ncol=jmax)
  HEATMUSCLEACT  = matrix(nrow=imax+1, ncol=jmax)
  HEATIMFACT     = matrix(nrow=imax+1, ncol=jmax)
  HEATMISCFATACT = matrix(nrow=imax+1, ncol=jmax)
  HEATNONCACT    = matrix(nrow=imax+1, ncol=jmax)
  
  HEATTOTALACT   = matrix(nrow=imax+1, ncol=jmax)
  
  ENBONEACT      = matrix(nrow=imax+1, ncol=jmax)
  ENMUSCLEACT    = matrix(nrow=imax+1, ncol=jmax)
  ENIMFACT       = matrix(nrow=imax+1, ncol=jmax)
  ENMISCFATACT   = matrix(nrow=imax+1, ncol=jmax)
  ENNONCACT      = matrix(nrow=imax+1, ncol=jmax)
  ENTOTALACT     = matrix(nrow=imax+1, ncol=jmax)
  
  PROTBONEACT    = matrix(nrow=imax+1, ncol=jmax)
  PROTMUSCLEACT  = matrix(nrow=imax+1, ncol=jmax)
  PROTIMFACT     = matrix(nrow=imax+1, ncol=jmax)
  PROTMISCFATACT = matrix(nrow=imax+1, ncol=jmax)
  PROTNONCBF1    = matrix(nrow=imax+1, ncol=jmax)
  PROTTOTALACT   = matrix(nrow=imax+1, ncol=jmax)
  PROTGROSS      = matrix(nrow=imax+1, ncol=jmax)
  UREABL         = matrix(nrow=imax+1, ncol=jmax)
  NRECYCLPT      = matrix(nrow=imax+1, ncol=jmax)
  PROTNETT       = matrix(nrow=imax+1, ncol=jmax)
  PROTACCR       = matrix(nrow=imax+1, ncol=jmax)
  
  HEATCLIMGEN    = matrix(nrow=imax+1, ncol=jmax)
  DIFFEN         = matrix(nrow=imax+1, ncol=jmax)
  
  HEATIFEEDMAINT     = matrix(nrow=imax, ncol=jmax)
  HEATIFEEDMAINTWM   = matrix(nrow=imax, ncol=jmax)
  HEATIFEEDGROWTH    = matrix(nrow=imax, ncol=jmax)
  HEATIFEEDGROWTHWM  = matrix(nrow=imax, ncol=jmax)
  HEATIFEEDGROWTHC   = matrix(nrow=imax, ncol=jmax)
  HEATIFEEDGROWTHCWM = matrix(nrow=imax, ncol=jmax)
  REDMAINT           = matrix(nrow=imax, ncol=jmax)
  REDMAINT2          = matrix(nrow=imax, ncol=jmax)
  REDMAINT3          = matrix(nrow=imax, ncol=jmax)
  REDTIS             = matrix(nrow=imax, ncol=jmax)
  REDTISPROT         = matrix(nrow=imax, ncol=jmax)
  CHECK              = matrix(nrow=imax, ncol=jmax)
  REDHP              = matrix(nrow=imax, ncol=jmax)
  
  ###########################################################################################
  # 1.4.5                     Variables for herd dynamics and output                        #
  ###########################################################################################
  
  TIME       = matrix(nrow=imax, ncol=jmax)
  TIME2      = matrix(nrow=imax+1, ncol=jmax)
  
  TIMEYEAR   = matrix(nrow=imax, ncol=jmax)
  TIMEYEAR2  = matrix(nrow=imax+1, ncol=jmax)
  
  BIRTHDAYCALF1 = 1 # Initial values for birthdays calves (days) 
  BIRTHDAYCALF2 = 1 # Calculated as days after birth reproductive animal
  BIRTHDAYCALF3 = 1 # Values are recalculated
  BIRTHDAYCALF4 = 1
  BIRTHDAYCALF5 = 1
  BIRTHDAYCALF6 = 1
  BIRTHDAYCALF7 = 1
  BIRTHDAYCALF8 = 1
  BIRTHDAYCALF9 = 1
  BIRTHDAY = NULL
  WNDAY  = NULL
  
  # Modelling calves and cow parity
  
  PARITY1 = matrix(nrow=imax, ncol=jmax)  # Cow has been or is in the first parity 
  PARITY2 = matrix(nrow=imax, ncol=jmax)  # Cow has been or is in the second parity 
  PARITY3 = matrix(nrow=imax, ncol=jmax)  # Cow has been or is in the third parity 
  PARITY4 = matrix(nrow=imax, ncol=jmax)  # Cow has been or is in the fourth parity 
  PARITY5 = matrix(nrow=imax, ncol=jmax)  # Cow has been or is in the fifth parity 
  PARITY6 = matrix(nrow=imax, ncol=jmax)  # Cow has been or is in the sixth parity 
  PARITY7 = matrix(nrow=imax, ncol=jmax)  # Cow has been or is in the seventh parity 
  PARITY8 = matrix(nrow=imax, ncol=jmax)  # Cow has been or is in the eighth parity 
  PARITY9 = matrix(nrow=imax, ncol=jmax)  # Cow has been or is in the nineth parity 
  
  # Herd dynamics
  BEEFPROD          = matrix(nrow=imax+1, ncol=jmax)
  BEEFPRODYEAR      = matrix(nrow=imax+1, ncol=jmax)
  
  BEEFPRODACT       = matrix(nrow=imax+1, ncol=jmax)
  LWPRODACT         = matrix(nrow=imax+1, ncol=jmax)
  CARCPRODACT       = matrix(nrow=imax+1, ncol=jmax)
  LWPROD            = matrix(nrow=imax+1, ncol=jmax)
  LWPRODYEAR        = matrix(nrow=imax+1, ncol=jmax)
  LWPRODHERD        = NULL
  
  SLAUGHTERDAYACT       = matrix(nrow=imax+1, ncol=jmax)
  SLAUGHTERDAYACTpl     = matrix(nrow=imax+1, ncol=jmax)
  SLAUGHTERDAYACTHEIFER = matrix(nrow=imax+1, ncol=jmax)
  ENDDAY   = c(1,1,1,1,1,1,1,1,1)
  
  SUMFEED1 = matrix(nrow=imax, ncol=jmax)
  SUMFEED2 = matrix(nrow=imax, ncol=jmax)
  SUMFEED3 = matrix(nrow=imax, ncol=jmax)
  SUMFEED4 = matrix(nrow=imax, ncol=jmax)
  SUMFEED  = matrix(nrow=imax, ncol=jmax)
  
  CUMULFEED1 = matrix(nrow=imax, ncol=jmax)
  CUMULFEED2 = matrix(nrow=imax, ncol=jmax)
  CUMULFEED3 = matrix(nrow=imax, ncol=jmax)
  CUMULFEED4 = matrix(nrow=imax, ncol=jmax)
  CUMULFEED  = matrix(nrow=imax, ncol=jmax)
  
  FATBURNCUMUL   = matrix(nrow=imax+1, ncol=jmax)
  HEATBURNCUMUL  = matrix(nrow=imax, ncol=jmax)
  ALIVE          = matrix(nrow=imax+1, ncol=jmax)
  
  FCR           = matrix(nrow=imax, ncol=jmax)
  FCRBEEF       = matrix(nrow=imax, ncol=jmax)
  FCRBEEFENDDAY = matrix(nrow=imax, ncol=jmax)
  
  MILKSTART     = matrix(nrow=imax, ncol=jmax)
  MILKSTARTPR   = matrix(nrow=imax, ncol=jmax)
  MILKSTARTPRHF = matrix(nrow=imax, ncol=jmax)
  
  METABFEED     = matrix(nrow=imax, ncol=jmax)
  METABFEEDC    = matrix(nrow=imax, ncol=jmax)  
  METABFEEDCH   = matrix(nrow=imax, ncol=jmax)
  
  CHECKHEAT1    = matrix(nrow=imax, ncol=jmax, NA)
  CHECKHEAT2    = matrix(nrow=imax, ncol=jmax, NA)
  CHECKHEAT3    = matrix(nrow=imax, ncol=jmax, NA)
  CHECKCOMP     = matrix(nrow=imax, ncol=jmax)
  
  MAXW1           = NULL
  CALVESPERANIMAL = NULL
  BEEFPRODHERD    = NULL
  FCRHERDBEEF     = NULL
  CUMULFEEDHERD   = NULL
  CUMULFEED1HERD  = NULL
  CUMULFEED2HERD  = NULL
  CUMULFEED3HERD  = NULL
  CUMULFEED4HERD  = NULL
  ANIMALYEARS     = NULL
  AVANWEIGHT      = matrix(nrow=imax, ncol=jmax)
  AVANMETWEIGHT   = matrix(nrow=imax, ncol=jmax)
  
  ANIMALINFO      = NULL
  HERDINFO        = NULL
  HERDINFO1       = NULL
  FATCOMP         = matrix(nrow=imax, ncol=jmax)
  NONCF           = matrix(nrow=imax, ncol=jmax)
  PERCFI          = matrix(nrow=imax, ncol=jmax)
  REPS            = matrix(nrow=imax, ncol=jmax)
  
  MEMET           = matrix(nrow=imax, ncol=jmax)
  MERED           = matrix(nrow=imax, ncol=jmax)
  
  PROTNONG        = matrix(nrow=imax, ncol=jmax)
  HIFM            = matrix(nrow=imax, ncol=jmax)
  PROTNONGM       = matrix(nrow=imax, ncol=jmax)
  CPAVG           = matrix(nrow=imax, ncol=jmax)
  
  OUTPUTHERDS     = NULL # Matrix with information for one herd unit
  
  ###########################################################################################
  #                            Dynamic part of the model (animals)                          #
  ###########################################################################################
  
  HOUSING1 <- HOUSING # Creates a copy of the vector HOUSING
  FEED11   <- FEED1   # Creates a copy of the matrix FEED1  
  FEED21   <- FEED2   # Creates a copy of the matrix FEED2
  FEED31   <- FEED2   # Creates a copy of the matrix FEED3
  
  breakFlaganim <- FALSE # breakFlaganim indicates whether the simulation of an animal should
                         # be continued (if FALSE) or terminated (if TRUE), e.g. when the 
                         # maximum number of calves is reached per reproductive animal.
  
  for (j in 1:jmax){ # Loop for individual animals starts here (j = jth animal)
  
    imax <- c(imax,2500,2500,2500,2500,2500,2500,2500,2500)   # maximum of 2500 day life span
                                                              # for productive animals
    if(j>1) HOUSING <- HOUSING1[BIRTHDAY[j]:length(HOUSING1)] # adjusts HOUSING for offspring
    if(j>1) FEED1 <- FEED11[BIRTHDAY[j]:length(HOUSING1),]    # adjusts FEED1 for offspring
    if(j>1) FEED2 <- FEED21[BIRTHDAY[j]:length(HOUSING1),]    # adjusts FEED2 for offspring
    if(j>1) FEED3 <- FEED31[BIRTHDAY[j]:length(HOUSING1),]    # adjusts FEED3 for offspring
    
    for (i in 1:imax[j]) { # Loop for daily time step starts here (i = ith day)
    
    breakFlagtime <- FALSE # breakFlagtime indicates whether the simulation of an animal 
                           # should be slaughtered.  
  
  ###########################################################################################
  # 1.5                          Initial values for individual animals                      #
  ########################################################################################### 
  
    # Code below selects the library for genotype (i.e. breed) and sex  
    if(BREED ==1 & SEX[j] == 0) LIBRARY <- LIBRARY10 else
      if(BREED ==1 & SEX[j] == 1) LIBRARY <- LIBRARY11 else
        if(BREED ==2 & SEX[j] == 0) LIBRARY <- LIBRARY20 else
          if(BREED ==2 & SEX[j] == 1) LIBRARY <- LIBRARY21 else 
            if(BREED ==3 & SEX[j] == 0) LIBRARY <- LIBRARY30 else
              if(BREED ==3 & SEX[j] == 1) LIBRARY <- LIBRARY31 else
                if(BREED ==4 & SEX[j] == 0) LIBRARY <- LIBRARY40 else 
                  if(BREED ==4 & SEX[j] == 1) LIBRARY <- LIBRARY41 else
                    if(BREED ==5 & SEX[j] == 0) LIBRARY <- LIBRARY50
    
    # During sensitivity analysis, the parameters in the library are changed (not used)
    LIBRARY <- LIBRARY * SENSMAT[42:67,s]
    
    # Matrix with time steps (days, starts at day 1)
    TIME <- matrix(rep(1:imax[j],jmax), nrow=imax[j], ncol=jmax)  
    # Matrix with time steps (days, starts at day 0)
    TIME2 <- matrix(rep(0:imax[j],jmax), nrow=imax[j]+1, ncol=jmax)   
    # Matrix with time steps (years, starts at day 1)
    TIMEYEAR <- matrix(rep(1:imax[j]/365,jmax), nrow=imax[j], ncol=jmax)
    # Matrix with time steps (years, starts at day 0)
    TIMEYEAR2 <- matrix(rep(0:imax[j]/365,jmax), nrow=imax[j]+1, ncol=jmax)
    
    # A few parameters from the LIBRARY are re-named here. Values between brackets indicate
    # the parameter numbers in Table S2 of the Supplementary Information
    REFLC      = LIBRARY[1]   # [5]  Reflectance coat (-)
    LC         = LIBRARY[2]   # [3]  Coat length (m)
    AREAFACTOR = LIBRARY[3]   # [1]  Body area (Body area : weight factor)
    CBSMAX     = LIBRARY[4]   # [2]  Max. conduction body core ??? skin (W m-2 K-1)                          
    MAXW1[j]   = LIBRARY[13]  # [20] Maximum adult weight (kg)
    MILKPARA   = LIBRARY[11]  # [13] Lactation curve 1
    MILKPARB   = LIBRARY[12]  # [14] Lactation curve 2
    MILKPARC   = LIBRARY[27]  # [15] Lactation curve 3
    RBCSf      = LIBRARY[23]  # [4]  Minimum conduction body core-skin (-)
    MAXW       = LIBRARY[6]   # Maximum adult weight for the Gompertz curve (kg);
                              # parameter accounts for the reduction parameter (EPAR)
    BIRTHW     = LIBRARY[7]   # [9]  Birth weight (kg)
    CPAR       = LIBRARY[8]   # [10] Constant of integration Gompertz curve
    DPAR       = LIBRARY[9]   # [11] Rate constant Gompertz curve 
    EPAR       = LIBRARY[10]  # [12] Reduction parameter Gompertz curve
  
    # Initial total body weight (kg live weight), genetic potential
    TBW[1,j]           = LIBRARY[5] 
    # Initial carcass weight (kg)
    CARCW[1,j]         = LIBRARY[5]*INCARC  
    # Initial bone weight (kg)
    BONETIS[1,j]       = CARCW[1,j]*min(BONEFRACMAX,(BONEGROWTH1 * SENSMAT[68,s])*
                         CARCW[1,j]^-(BONEGROWTH2 * SENSMAT[69,s]))
    # Initial muscle weight (kg)
    MUSCLETIS[1,j]     = BONETIS[1,j]*min(LIBRARY[22],(MUSCLEGROWTH1*SENSMAT[70,s])*
                         CARCW[1,j]^2+LIBRARY[22]/100*CARCW[1,j]+
                         (MUSCLEGROWTH2*SENSMAT[71,s]))
    # Initial weight intramuscular fat (kg)
    INTRAMFTIS[1,j]    = ((IMFGROWTH1*SENSMAT[72,s])*(BONETIS[1,j]+MUSCLETIS[1,j])^2+
                            (IMFGROWTH2*SENSMAT[73,s])*(BONETIS[1,j]+MUSCLETIS[1,j])-
                            (IMFGROWTH3*SENSMAT[74,s]))
    # Initial weight of the intermuscular and subcutaneous (i.e. miscellaneous ) fat tissue 
    # (kg)
    MISCFATTIS[1,j]    = CARCW[1,j]-BONETIS[1,j]-MUSCLETIS[1,j]-INTRAMFTIS[1,j]
    # Initial weight of the non-carcass tissue (kg)
    NONCARCTIS[1,j]    = TBW[1,j]*(1-RUMENFRAC)-CARCW[1,j]
    # Initial weight of the rumen contents
    RUMEN[1,j]         = TBW[1,j]*RUMENFRAC
    
    # Initial weight of lipid in bone tissue (-)
    LIPIDBONE[1,j] = BONETIS[1,j]*(LIBRARY[20]* log(BONETIS[1,j]))/100
    # Initial weight of protein in bone tissue (-)
    PROTBONE[1,j] = BONETIS[1,j]*PROTFRACBONE
    # Initial weight of lipid in muscle tissue (-)
    LIPIDMUSCLE[1,j] = MUSCLETIS[1,j]*LIPFRACMUSCLE
    # Initial weight of protein in muscle tissue (-)
    PROTMUSCLE[1,j] = MUSCLETIS[1,j]*PROTFRACMUSCLE
    # Initial weight of lipid in intramuscular fat tissue (-)
    LIPIDIMF[1,j] = INTRAMFTIS[1,j]*LIPFRACFAT
    # Initial weight of protein in intramuscular fat tissue (-)
    PROTIMF[1,j] = INTRAMFTIS[1,j]*PROTFRACFAT
    # Initial weight of lipid in the miscellaneous fat tissue (-)
    LIPIDFAT[1,j] = MISCFATTIS[1,j]*LIPFRACFAT
    # Initial weight of protein in the miscellaneous fat tissue (-)
    PROTFAT[1,j] = MISCFATTIS[1,j]*PROTFRACFAT
    # Initial weight of lipid in the non-carcass tissue (-)
    LIPIDNONC[1,j] = 1.00
    # Initial weight of protein in the non-carcass tissue (-)
    PROTNONC[1,j] = NONCARCTIS[1,j]*((PROTNONCM1*SENSMAT[75,s])*TBW[1,j] + 
                                       (PROTNONCM2*SENSMAT[76,s])) / 100
    
    # The animal is at its potential weight at the first time step.
    # Later on, the weight of the animal can deviate from its potential weight, since other
    # biophysical factors than the genotype can affect growth.
    # Note: LiGAPS-Beef does not account for growth reduction during the gestation period.
    
    # Initial bone weight (kg)
    BONETISBF[1,j] = BONETIS[1,j]                     
    # Initial muscle weight (kg)
    MUSCLETISBF[1,j] = MUSCLETIS[1,j]
    # Initial intramuscular fat weight (kg)
    INTRAMFTISBF[1,j] = INTRAMFTIS[1,j]
    # Initial miscellaneous fat weight (kg)
    MISCFATTISBF[1,j] = MISCFATTIS[1,j]
    # Initial non-carcass weight (kg)
    NONCARCTISBF[1,j] = NONCARCTIS[1,j]
    # Initial total body weight (TBW, in kg live weight)
    TBWBF[1,j] = TBW[1,j]
    # Initial metabolic body weight (TBW^0.75)
    EBWBFMET[1,j] = (TBWBF[1,j]*(1-RUMENFRAC))^0.75
    # Initial fraction of miscellaneous fat in the body 
    MISCFATFRAC[1,j] = MISCFATTISBF[1,j]/TBWBF[1,j]
    # Initial weight lipids in the bone tissue (kg)
    LIPIDBONEBF[1,j] = LIPIDBONE[1,j]
    # Initial weight lipids in non-carcass tissue (kg)
    LIPIDNONCBF[1,j] = LIPIDNONC[1,j]
    # Initial weight protein in non-carcass tissue (kg)
    PROTNONCBF[1,j] = PROTNONC[1,j]
    # Initial weight lipids in muscle tissue (kg)
    LIPIDMUSCLEBF[1,j] = LIPIDMUSCLE[1,j]
    # Initial weight lipids in the intramuscular fat tissue (kg)
    LIPIDIMFBF[1,j] = LIPIDIMF[1,j]
    # Initial weight lipids in the miscellaneous fat tissue (kg)
    LIPIDFATBF[1,j] = LIPIDFAT[1,j]
    
    # Initial weight of total lipids in the body (kg)
    LIPIDTOTW[1,j] = (LIPIDBONEBF[1,j]+LIPIDNONCBF[1,j]+LIPIDMUSCLEBF[1,j]+
                        LIPIDIMFBF[1,j]+LIPIDFATBF[1,j])
    # Initial fraction of lipids in the carcass 
    LIPIDFRACCARC[1,j]  = (LIPIDBONEBF[1,j]+LIPIDMUSCLEBF[1,j]+LIPIDIMFBF[1,j]+
                             LIPIDFATBF[1,j])/(TBWBF[1,j]-NONCARCTISBF[1,j])
    
  ###########################################################################################
    
    HEATTOTALACT[1,j] = 9.00 # Assumption at the first day for heat release.
    FATBURNCUMUL[1,j] = 0    # Cumulative amount of fat dissimilated used to maintain body 
                             # temperature (cold stress) is zero (MJ)
    HEATBURNCUMUL[1,j] = 0   # Cumulative amount of heat used to maintain body temperature  
                             # (cold stress) is zero (MJ)
    ALIVE[1,j] = 1           # The animal is alive at the first time step
  
    # Requirements for gestation:
    CALFTBW[1,j] = 0.0    # No calf born yet at the first time step, weight is zero (kg)
    CALFNR[1,j] = 0       # Zero calves are born (only for reproductive cows)
    
    GEST1[1,j]= 0         # The total body weight is not higher than the body weight required
                          # for gestation
    GEST2[1,j]= 0         # Gestation not applicable at the first time step
    GEST3[1,j]= 1         # Minimum calving interval is not applicable at the first time step
    GEST4[1,j]= 0         # Fat tissue in the carcass is assumed to be below the minimum
                          # required for conception
    GEST5[1,j]= 0         # Calves cannot conceive at the first time step and after reaching
                          # the maximum age for conception
    GEST6[1,j]= 1         # Maximum number of calves per cow is not achieved yet        
    GESTDAY[1,j] = 0      # No gestation at the first time step, days in gestation not 
                          # applicable (days)
    NEREQGESTADD[1,j] = 0 # No gestation at the first time step, no NE for gestation (MJ per 
                          # day)
    TBWADD[1,j] = 0   # No gestation at the first time step, no weight of foetus (kg)
    
    MILKDAY[1,j]  = 0 # No milk production at the first time step, days in milk not 
                      # applicable (days)
    MILKWEEK[1,j] = 0 # No milk production at the first time step, days in milk not
                      # applicable (days)
    
    PARITY1[1,j] = 0 # Cow parity is not equal to or higher than 1 at the first time step 
    PARITY2[1,j] = 0 # Cow parity is not equal to or higher than 2 at the first time step
    PARITY3[1,j] = 0 # Cow parity is not equal to or higher than 3 at the first time step
    PARITY4[1,j] = 0 # Cow parity is not equal to or higher than 4 at the first time step
    PARITY5[1,j] = 0 # Cow parity is not equal to or higher than 5 at the first time step
    PARITY6[1,j] = 0 # Cow parity is not equal to or higher than 6 at the first time step
    PARITY7[1,j] = 0 # Cow parity is not equal to or higher than 7 at the first time step
    PARITY8[1,j] = 0 # Cow parity is not equal to or higher than 8 at the first time step
    
  ###########################################################################################
  # 2.                                   Dynamic section                                    #
  #                                     (time and animals)                                  #                                                                  
  ###########################################################################################
    
  ###########################################################################################
  # 2.1                             Thermoregulation submodel                               #
  ###########################################################################################
  
  # Aim: To calculate the maximum and minimum heat release (W m-2) of an animal with its 
  # environment. 
  
  # Five flows of energy between an animal and its environment
  #   1. Latent and convective heat release from respiration
  #   2. Latent heat release from the skin
  #   3. Long wave radiation balance of the coat
  #   4. Convective heat losses from the coat
  #   5. Solar radiation intecepted by the coat
   
  ###########################################################################################
  # 2.1.1                             Maximum heat release                                  #
  ###########################################################################################    
    
    # Heat release mechanisms of cattle at maximum heat release
    TISSUEFRAC = 1.00 # Vasodilatation (0 = minimum and 1 = maximum vasodilatation)
    LHRskin    = 1.00 # Latent heat release from the skin (0 = basal and 1 = maximum 
                      # physiological 'sweating' rate)
    PANTING    = RESPDUR*SENSMAT[77,s] # Panting (0 = basal respiration, 1 = maximum panting)
  
    # Calculations related to weather conditions
  
    # average temperature (degrees Celsius)
    TAVGC[i,j]     <- (WEATHER$MINT[i]+WEATHER$MAXT[i])/2                     
    # average temperature (degrees Kelvin)
    TAVGK[i,j]     <- CtoK + TAVGC[i,j]                                       
    # saturated vapour pressure air (Pa)
    VPSATAIR[i,j]  <- 6.1078*10^((7.5*TAVGC[i,j])/(TAVGC[i,j]+237.3))*100     
    # real vapour pressure air (kPa)
    VPAIRTOT[i,j]  <- WEATHER$VPR[i]*1000                                     
    # relative humidity (-)
    RHAIR[i,j]     <- VPAIRTOT[i,j] / VPSATAIR[i,j] *100                      
    # water vapour density (kg m-3)
    RHOVP[i,j]     <- VPAIRTOT[i,j]/ (Rwater*TAVGK[i,j])                      
    # dry air density (kg m-3)
    RHODAIR[i,j]   <- (P-VPAIRTOT[i,j]) / (Rdair*TAVGK[i,j])                  
    # air density (kg m-3)
    RHOAIR[i,j]    <- RHOVP[i,j] + RHODAIR[i,j]                               
    # water vapour density (kg kg-1)
    CHIAIR[i,j]    <- RHOVP[i,j]*RHOAIR[i,j]                                  
              
    #########################################################################################
    #                1. Latent and convective heat release from respiration                 #
    #########################################################################################
        
    # Animal surface area (m2), McGovern and Bruce (2000)
    # This equation corresponds to Eq. 2 of the Supplementary Information
    AREA[i,j] = (BODYAREA1*SENSMAT[78,s])*TBWBF[i,j]^(BODYAREA2*SENSMAT[79,s]) * AREAFACTOR 
    # This equation corresponds to Eq. 3 of the Supplementary Information
    # Animal diameter (m), McGovern and Bruce (2000)
    DIAMETER[i,j] = (DIAMETER1*SENSMAT[80,s])*TBWBF[i,j]^(DIAMETER2*SENSMAT[81,s])           
    # Animal length (m)
    LENGTH[i,j] = (AREA[i,j]-0.5*pi*DIAMETER[i,j]^2)/(pi*DIAMETER[i,j])     
        
    # Basal respiration rate (min-1), McGovern and Bruce (2000)
    brr[i,j] <- (BASALRR1*SENSMAT[82,s]) * TBWBF[i,j]^(BASALRR2*SENSMAT[83,s])   
    # Basal tidal volume (L) McGovern and Bruce (2000)
    btv[i,j] <- (BASALTV*SENSMAT[84,s]) * TBWBF[i,j]                                         
    # Basal respiration volume (L min-1)
    # This equation corresponds to Eq. 5 of the Supplementary Information
    brv[i,j] <- brr[i,j]*btv[i,j]                                            
    # Increased respiration volume (L min-1)
    # This equation corresponds to Eq. 6 of the Supplementary Information
    irv[i,j] <- brv[i,j] + PANTING*((RESPINCR-1)*brv[i,j])                   
    # Temperature exhaled air (degrees Celsius), Stevens (1981)
    # This equation corresponds to Eq. 7 of the Supplementary Information
    Texh[i,j] <- (TEXHALED1*SENSMAT[85,s]) + (TEXHALED2*SENSMAT[86,s]) * TAVGC[i,j] + 
      exp((TEXHALED3*SENSMAT[87,s]) * RHAIR[i,j]  + (TEXHALED4*SENSMAT[88,s]) * TAVGC[i,j])  
        
    # Assumption: exhaled air is saturated with water
    
    # Saturated vapour pressure exhaled air (Pa)
    VPSATAIROUT[i,j]  <- 6.1078*10^((7.5*Texh[i,j])/(Texh[i,j]+237.3))*100    
    # Water vapour density exhaled air (kg m-3)
    RHOVPOUT[i,j]     <- VPSATAIROUT[i,j]/ (Rwater*(Texh[i,j]+CtoK))          
    # Dry air density exhaled air (kg m-3)
    RHODAIROUT[i,j]   <- (P-VPSATAIROUT[i,j]) / (Rdair*(Texh[i,j]+CtoK))      
    # Air density exhaled air (kg m-3)
    RHOAIROUT[i,j]    <- RHOVPOUT[i,j] + RHODAIROUT[i,j]                      
    # Water vapour density exhaled air (kg kg-1) 
    CHIAIROUT[i,j]    <- RHOVPOUT[i,j]*RHOAIROUT[i,j]                         
    # Air exchange between the animal and its environment (kg air m-2 day-1)
    AIREXCH[i,j] <- (irv[i,j]*60*24/1000*RHOAIR[i,j])/AREA[i,j]                      
    # Latent heat release from respiration (W m-2)    
    LHEATRESP[i,j] <- AIREXCH[i,j] * L *(CHIAIROUT[i,j]-CHIAIR[i,j])* kJdaytoW   
    # Convective heat release from respiration (W m-2)
    CHEATRESP[i,j] <- AIREXCH[i,j] * Cp *(Texh[i,j]-TAVGC[i,j]) * kJdaytoW       
    # Gross heat loss from the respiratory system (W m-2)
    # This equation corresponds to Eq. 8 of the Supplementary Information
    TGRESP[i,j] <- LHEATRESP[i,j] + CHEATRESP[i,j]                               
        
    # NE required for respiration (W m-2), i.e. panting, McGovern and Bruce (2000)
    # This equation is similar to Eq. 9 of the Supplementary Information
    NERESPWM[i,j] <- 1.1*(RESPINCR*brr[i,j])^2.78 * 10^-5 * PANTING   
    # NE required for respiration (kJ NE day-1)
    NERESP[i,j] <-NERESPWM[i,j] / kJdaytoW    
    # Total heat loss from the respiratory system (W m-2)    
    TNRESP[i,j] <- TGRESP[i,j]-NERESPWM[i,j]                                       
       
    TNRESPH[i,j] <- TNRESP[i,j]
    #########################################################################################
    #                                    1a. Skin temperature                               #
    #########################################################################################
    
    # Minimum conductance between body core to skin (W m-2 K-1), McGovern and Bruce (2000)
    # This equation corresponds to Eq. 10 of the Supplementary Information
    CBSMIN[i,j] = RBCSf/((MINCCS1*SENSMAT[89,s]) * TBWBF[i,j]^(MINCCS2*SENSMAT[90,s]))  
    # Conduction body core to skin (W m-2 K-1), McGovern and Bruce (2000)
    CONDBS[i,j] = CBSMIN[i,j] + TISSUEFRAC*(CBSMAX-CBSMIN[i,j])                    
    
    # Notes:
    # 100 s m-1 = 0.078 K m2 W-1 (Cena and Clark, 1978) 
    # Cattle --> 50 s m-1 (Turnpenny, 2000a) --> 0.039 K m-2 W-1  = 25.6 W m-2 K-1
  
    #########################################################################################
    #                            2. Latent heat release from the skin                       #
    #########################################################################################
    
    # Reduction in coat depth (m), McGovern and Bruce (2000)
    DLC[i,j] = (CoatConst * WEATHER$WIND[i])/((CoatConst * WEATHER$WIND[i])/LC+1/(ZC*LC)) 
    # Diffusion constant water vapour in air (m2 s-1), (Denny, 1993)
    DIFFC[i,j] = 0.187 * 10^-9 * TAVGK[i,j]^2.072                               
    
    #########################################################################################
    #                                    2a. Coat temperature                               #
    #########################################################################################
    
    # Resistance and conductivity between skin and coat
    # Conductance skin to coat (W m-2 K-1)
    # This equation corresponds to Eq. 14 of the Supplementary Information
    CSC[i,j] = 1 /(RUC * ZC * (LC-DLC[i,j]))                       
    # Increase in conductance due to precipitation/rain, Mount and Brown (1982)
    CSC[i,j] <- CSC[i,j]/ (1-min(RAINFRAC, WEATHER$RAIN[i]*RAINFRAC/24))     
    
    #########################################################################################
    #                            3. Long wave radiation from the coat                       #
    #########################################################################################
    
    # Incoming LWR from the sky (W m-2), McGovern and Bruce (2000)
    # This equation corresponds to Eq. 19 of the Supplementary Information
    LWRSKY[i,j] = (1-WEATHER$OKTA[i]/8)*(SIGMA*TAVGK[i,j]^4)*
      (1-0.261*exp(-0.000777*(273-TAVGK[i,j])^2)) + (WEATHER$OKTA[i]/8) * 
      (SIGMA * TAVGK[i,j]^4 - 9) 
    # Incoming LWR from soil surface (W m-2)
    LWRENV[i,j] = SIGMA * TAVGK[i,j]^4                                             
    
    # Cattle in stables do not receive LWR from the sky, but from the roofs and wall of the
    # stable they are housed in.
    if(HOUSING[i] == 0) LWRSKY[i,j] <- LWRENV[i,j] 
      
    #########################################################################################
    #                          4. Convective heat losses from the coat                      #
    #########################################################################################
    
    # Calculation of the air viscosity, Smits and Dussaunge (2006) 
    # Average air temperature in degrees Rankine
    TAVGR[i,j] = TAVGK[i,j] * KtoR                                              
    # Actual air viscosity (N s-1 m-2), Smits and Dussaunge (2006)
    VISCAIR[i,j] =(MuSt*((0.555*TR0+ST)/(0.555*TAVGR[i,j]+ST)*(TAVGR[i,j]/TR0)^(3/2)))   
    
    # Calculation of the Grashof number  
    # Vapour pressure of the ambient air (mBar)
    Ea[i,j] = WEATHER$VPR[i]*10                                             
    
    # Calculation of the Reynolds number 
    # Wind speed (m s-1)
    WINDSP[i,j] = WEATHER$WIND[i]                                                 
    # Reynolds number
    REYNOLDS[i,j] = WINDSP[i,j] * DIAMETER[i,j] * RHOAIR[i,j] / VISCAIR[i,j]         
    
    # Calculation step for natural convection
    ReH[i,j] = 16*REYNOLDS[i,j]^2
    # Calculation step for forced convection
    ReL[i,j] = 0.1*REYNOLDS[i,j]^2                                             
    # Themal conductance of the ambient air (W m-1 K-1)
    # This equation corresponds to Eq. 17 of the Supplementary Information
    ka[i,j] = 1.5207 * 10^(-11) * TAVGK[i,j]^3 - 4.8574 * 10^(-8) * TAVGK[i,j]^2 + 1.0184 * 
      10^-4 *TAVGK[i,j] - 0.00039333 
    
    #########################################################################################
    #                        5. Solar radiation intercepted by the coat                     #
    #########################################################################################
    
    # Ah/A factor: Shade area / animal coat area (m2 m-2)
    SAAC[i,j] <- WEATHER$AHA[i]                                       
    # Incoming direct solar radiation on the soil surface (Wm-2)
    SWRS[i,j] <- WEATHER$RAD[i]*kJdaytoW                              
    # Incoming direct solar radiation on the animal's coat (Wm-2)
    # This equation covers part of Eq. 4 of the Supplementary Information
    SWRC[i,j] <- SWRS[i,j]*SAAC[i,j]*(1-REFLC)                        
    
    # Indirect solar radiation
    if(HOUSING[i]==1) REFLE[i] <- REFLEgrass else 
      if(HOUSING[i]==2) REFLE[i] <- REFLEconcr else REFLE[i] <- 0  
    # Incoming indirect solar radiation on an animal's coat (W m-2)
    # This equation covers part of Eq. 4 of the Supplementary Information
    ISWRC[i,j] <- FRACVEG*REFLE[i]*SWRS[i,j]                  
    
    # Total solar radiation on an animal's coat (W m-2)                                    
    SWR[i,j] <- SWRC[i,j] + ISWRC[i,j]                                           
    
    # Heat loss by evaporation of (rain) water (W m-2) 
    # This equation corresponds to Eq. 20 of the Supplementary Information
    RAINEVAP[i,j] <- (RAINEVAP1*SENSMAT[92,s])*(LENGTH[i,j]*DIAMETER[i,j])/AREA[i,j] * 
      min(24,WEATHER$RAIN[i]) * L * kJdaytoW 
    
    # Selects an initial maximum level for heat release and heat production (W m-2)
    METABFEED[i,j] <- 170     
    
    repeat { # start repeat loop for maximum heat release  
     
        #####################################################################################
        #                                    1a. Skin temperature                           #
        #####################################################################################
        
        # Heat transfer from body core to skin (W m-2)
        MetheatSKIN[i,j] = METABFEED[i,j] - TNRESP[i,j]                                            
        # Skin temperature (degrees Celsius)
        # This equation corresponds to Eq. 11 of the Supplementary Information
        TskinC[i,j] = TbodyC - MetheatSKIN[i,j]/CONDBS[i,j]                          
        
        METABFEEDCH[i,j] = METABFEED[i,j]
        
        #####################################################################################
        #                            2. Latent heat release from the skin                   #
        #####################################################################################
           
        # Maximum physological latent heat release from skin (W m-2)
        # This equation corresponds to Eq. 12 of the Supplementary Information
        LASMAXPHYS[i,j] = LASMIN + LIBRARY[24]*exp(LIBRARY[25]*(TskinC[i,j]-LIBRARY[26])) * 
                          L/3600 
        
        # Resistance vapour transfer (s m-1), Thompson et al. (2011)
        RV[i,j] = (LC-DLC[i,j])/(DIFFC[i,j]*
                                   (1+1.54*((LC-DLC[i,j])/DIAMETER[i,j]) * 
                                   (TskinC[i,j]-min(TAVGC[i,j],TskinC[i,j]))^0.7))                     
        # Saturated vapour pressure skin (Pa)
        VPSKINTOT[i,j] = 6.1078*10^((7.5*TskinC[i,j])/(TskinC[i,j]+237.3))*100       
        
        # Maximum latent heat release from skin due to the ambient environment (W m-2)
        # This equation corresponds to Eq. 13 of the Supplementary Information
        LASMAXENV[i,j] = (RHOAIR[i,j] * Cp * 1000) / GAMMA * (VPSKINTOT[i,j]-VPAIRTOT[i,j]) / 
          RV[i,j] 
        # Maximum latent heat release from skin (W m-2)
        LASMAXCORR[i,j] = min(LASMAXPHYS[i,j],LASMAXENV[i,j])                           
        # Actual latent heat release from skin (W m-2)
        ACTSW[i,j] = LASMIN + LHRskin * (LASMAXCORR[i,j]-LASMIN)                       
        
        ACTSWH[i,j] = ACTSW[i,j] 
        
        #####################################################################################
        #                                    2a. Coat temperature                           #
        #####################################################################################
        
        # Heat transfer from the skin to the coat (W m-2)
        MetheatCOAT[i,j] = MetheatSKIN[i,j] - ACTSW[i,j]
        
        # Coat temperature (degrees Celsius)
        # This equation corresponds to Eq. 15 of the Supplementary Information
        TcoatC[i,j] = TskinC[i,j] - MetheatCOAT[i,j]/CSC[i,j]                        
        # Coat temperature (degrees Kelvin)
        TcoatK[i,j] = TcoatC[i,j] + CtoK                                             
        
        #####################################################################################
        #                            3. Long wave radiation from the coat                   #
        #####################################################################################
        
        # LWR release from the coat (W m-2)
        LB[i,j] = EMISS * SIGMA * TcoatK[i,j]^4                                                
        
        # LWR balance (W m-2) (net energy loss is a negative value!)
        # This equation corresponds partly to Eq. 18 of the Supplementary Information
        LWRCOAT[i,j] = (EMISS * ((LWRSKY[i,j]+LWRENV[i,j])/2) - LB[i,j]) * 
          (1-min(RAINFRAC,WEATHER$RAIN[i]*RAINFRAC/24))                  
        
        LWRCOATH[i,j] = LWRCOAT[i,j]
        
        #####################################################################################
        #                        4. Convective heat losses from the coat                    #
        #####################################################################################
        
        # Vapour pressure at the skin (mBar)
        Ec[i,j] = ((6.1078*10^((7.5*TskinC[i,j])/(TskinC[i,j]+237.3)))+Ea[i,j])/2      
        
        # Grashof number
        GRASHOF[i,j] = (GRAV*DIAMETER[i,j]^3*P/100*(TcoatC[i,j]-TAVGC[i,j])+Schmidt*
                          (Ec[i,j]*TcoatC[i,j]-Ea[i,j]*TAVGC[i,j]))/
                          (273*P/100*VISCAIR[i,j]^2) 
        
        # Calculation of the Nusselt number, Turnpenny et al. (2000a)
        if(GRASHOF[i,j]>ReH[i,j]) NUSSELT[i,j] <- 0.48*GRASHOF[i,j]^0.25 else 
          if(GRASHOF[i,j]<ReL[i,j]) NUSSELT[i,j] <- 0.0112*REYNOLDS[i,j]^0.875 else
            NUSSELT[i,j] <- max(0.48*GRASHOF[i,j]^0.25,0.0112*REYNOLDS[i,j]^0.875)    
            
        # Heat transfer from the coat by convection (W m-2)
        # This equation corresponds partly to Eq. 16 of the Supplementary Information
        CONVCOAT[i,j] = (ka[i,j] * NUSSELT[i,j]) / DIAMETER[i,j] *(TcoatC[i,j]-TAVGC[i,j]) / 
          (1-min(RAINFRAC,WEATHER$RAIN[i]*RAINFRAC/24)) 
        CONVCOATH[i,j] =  CONVCOAT[i,j] 
        
        #####################################################################################
        #                                         Synthesis                                 #
        #####################################################################################
        
        # Heat balance (W m-2)
        # This equation is similar to Eq. 1 of the Supplementary Information
        MetheatBAL[i,j] <- (MetheatCOAT[i,j] + SWR[i,j] - RAINEVAP[i,j] + LWRCOAT[i,j] - 
                              CONVCOAT[i,j] )     
        
        # If the estimate for heat production is too high, it is decreased
        if(MetheatBAL[i,j] > 0.1) METABFEED[i,j] <- (METABFEED[i,j]-0.1*MetheatBAL[i,j])
        # If the estimate for heat production is too low, it is increased
        if(MetheatBAL[i,j] < -0.1) METABFEED[i,j] <-(METABFEED[i,j]-0.1*MetheatBAL[i,j])
        # Heat release and production
        Metheatopt[i,j] <- METABFEED[i,j]
        # If the heat release and production differ more than 0.1 W m-2, the loop is run
        # another time
        if(MetheatBAL[i,j] < 0.1 & MetheatBAL[i,j] > -0.1) CHECKHEAT1[i,j] <- "CORRECT" else 
          CHECKHEAT1[i,j] <- "FALSE"
        if(CHECKHEAT1[i,j] == "CORRECT") {break}
        
      } # end repeat loop for maximum heat release
      
  ###########################################################################################
  # 2.1.2                             Minimum heat release                                  #
  ###########################################################################################
  
    # Heat release mechanisms of cattle at minimum heat release
    TISSUEFRAC = 0.0            # Vasodilatation (0 = minimum and 1 = maximum vasodilatation)
    LHRskin    = 0.0            # Latent heat release (0 = minimum and 1 = maximum 
                                # physiological 'sweating' rate)
    PANTING    = 0.0            # Panting (0 = basal respiration, 1 is maximum panting)   
                                             
    #########################################################################################
    #                1. Latent and convective heat release from respiration                 #
    #########################################################################################
    
    # Actual respiration rate (L min-1)
    irv[i,j] <- brv[i,j] + PANTING*((RESPINCR-1)*brv[i,j])                         
    # Air exchange between the animal and its environment (kg air m-2 day-1)
    AIREXCH[i,j] <- (irv[i,j]*60*24/1000*RHOAIR[i,j])/AREA[i,j]                  
    # Latent heat release via respiratory system (W m-2)
    LHEATRESP[i,j] <- AIREXCH[i,j] * L *(CHIAIROUT[i,j]-CHIAIR[i,j])*kJdaytoW  
    # Concective heat release via respiratory system (W m-2)
    CHEATRESP[i,j] <- AIREXCH[i,j] * Cp *(Texh[i,j]-TAVGC[i,j])*kJdaytoW       
    # Gross heat loss from the respiratory system (W m-2)
    TGRESP[i,j] <- LHEATRESP[i,j] + CHEATRESP[i,j]                               
    # Total heat loss from the respiratory system (W m-2)
    TNRESP[i,j] <- TGRESP[i,j]                                                   
    
    #########################################################################################
    #                                    1a. Skin temperature                               #
    #########################################################################################
    
    # Conductance body core to skin (W m-2 K-1)
    CONDBS[i,j] = CBSMIN[i,j]    
    
    #########################################################################################
    #                            2. Latent heat release from the skin                       #
    #########################################################################################
    
    # Actual latent heat release from the skin (W m-2)
    ACTSW[i,j] = LASMIN          
    
    # Selects an initial minimum level for heat release and heat production (W m-2)
    METABFEEDC[i,j] <-80 

    repeat { # start repeat loop for minimum heat release  
      
      #######################################################################################
      #                                    1a. Skin temperature                             #
      #######################################################################################
      
      # Heat transfer from the body core to skin (W m-2)      
      MetheatSKIN[i,j] = METABFEEDC[i,j] - TNRESP[i,j]                              
      # Skin temperature (degrees Celsius)
      TskinC[i,j] = TbodyC - MetheatSKIN[i,j]/CONDBS[i,j]                          
      
      TskinCH[i,j] =  TskinC[i,j]
      
      #######################################################################################
      #                                    2a. Coat temperature                             #
      #######################################################################################
      
      # Heat transfoer from skin to coat (W m-2)
      MetheatCOAT[i,j] = MetheatSKIN[i,j] - ACTSW[i,j]
      
      # Coat temperature (degrees Celsius)
      TcoatC[i,j] = TskinC[i,j] - MetheatCOAT[i,j]/CSC[i,j]        
      # Coat temperature (degrees Kelvin)
      TcoatK[i,j] = TcoatC[i,j] + CtoK                                           
      
      TcoatCH[i,j] = TcoatC[i,j] 
      
      #######################################################################################
      #                            3. Long wave radiation from the coat                     #
      #######################################################################################
      
      # LWR release from the coat (W m-2)
      LB[i,j] = EMISS * SIGMA * TcoatK[i,j]^4                                                
      
      # LWR from coat to the environment (net energy loss is a negative value) (W m-2)
      
      LWRCOAT[i,j] = (EMISS * ((LWRSKY[i,j]+LWRENV[i,j])/2) - LB[i,j]) * 
        (1-min(RAINFRAC,WEATHER$RAIN[i]*RAINFRAC/24))                 
      
      #######################################################################################
      #                          4. Convective heat loss from the coat                      #
      #######################################################################################
      
      # Vapour pressure at the coat (mBar)
      Ec[i,j] = ((6.1078*10^((7.5*TcoatC[i,j])/(TcoatC[i,j]+237.3)))+Ea[i,j])/2      
      
      # Grashof number
      GRASHOF[i,j] = (GRAV*DIAMETER[i,j]^3*P/100*(TcoatC[i,j]-TAVGC[i,j])+Schmidt*
                        (Ec[i,j]*TcoatC[i,j]-Ea[i,j]*TAVGC[i,j]))/(273*P/100*VISCAIR[i,j]^2) 
      # Calculation of the Nusselt number, Turnpenny et al. (2000a)
      if(GRASHOF[i,j]>ReH[i,j]) NUSSELT[i,j] <- 0.48*GRASHOF[i,j]^0.25 else 
        if(GRASHOF[i,j]<ReL[i,j]) NUSSELT[i,j] <- 0.0112*REYNOLDS[i,j]^0.875 else
          NUSSELT[i,j] <- max(0.48*GRASHOF[i,j]^0.25,0.0112*REYNOLDS[i,j]^0.875)    
      
      # Convective heat transfer between coat and air (W m-2)
      CONVCOAT[i,j] = (ka[i,j] * NUSSELT[i,j]) / DIAMETER[i,j] *(TcoatC[i,j]-TAVGC[i,j]) / 
        (1-min(RAINFRAC,WEATHER$RAIN[i]*RAINFRAC/24))   
      
      #######################################################################################
      #                                         Synthesis                                   #
      #######################################################################################
      
      # Heat balance (W m-2)
      # This equation is similar to Eq. 1 of the Supplementary Information
      MetheatBAL[i,j] <- (MetheatCOAT[i,j] + SWR[i,j] - RAINEVAP[i,j] + LWRCOAT[i,j] - 
                            CONVCOAT[i,j])     
      
      # If the estimate for heat production is too high, it is decreased
      if(MetheatBAL[i,j] > 0.1) METABFEEDC[i,j] <- (METABFEEDC[i,j]-0.05*MetheatBAL[i,j])
      # If the estimate for heat production is too low, it is increased
      if(MetheatBAL[i,j] < -0.1) METABFEEDC[i,j] <-(METABFEEDC[i,j]-0.05*MetheatBAL[i,j])
      # Heat release and production
      Metheatcold[i,j] <- METABFEEDC[i,j]                                             
      # If the heat release and production differ more than 0.1 W m-2, the loop is run
      # another time
      if(MetheatBAL[i,j] < 0.1 & MetheatBAL[i,j] > -0.1) CHECKHEAT2[i,j] <- "CORRECT" else 
        CHECKHEAT2[i,j] <- "FALSE"
      if(CHECKHEAT2[i,j] == "CORRECT") {break}
      
    }
  
    #########################################################################################
    # 2.2                     Energy and protein utilisation sub-model                      #
    #########################################################################################
      
    ###################### 
    # Growth (potential) #
    ######################
    
      # TBW and carcass weight
      # Gompertz curve with breed specific parameters, total body weight (TBW) (kg live 
      # weight per animal)
      # This equation corresponds to Eq. 31 in the Supplementary Information
      TBW[i+1,j]           = (BIRTHW+(MAXW-BIRTHW)*exp(-CPAR*exp(TIME[i,j]/365*-DPAR)))-EPAR    
      # Carcass weight (kg per animal)
      # This equation corresponds to Eq. 32 in the Supplementary Information
      CARCW[i+1,j]         = TBW[i+1,j]*INCARC+TBW[i+1,j]*(LIBRARY[21]-INCARC)*
                             (TBW[i+1,j]-BIRTHW)/(MAXW1[j]-BIRTHW)
      # Records the highest average daily gain (ADG) of an animal during its lifespan (kg LW 
      # day-1)
      ADGHIGH[i+1,j]       = max(ADGHIGH[i,j],TBW[i+1,j]-TBW[i,j])
    
      # Bone and muscle weight
      # Derivative potential growth bone tissue (kg day-1), according to Gompertz curve
      # This equation contains Eq. 33 of the Supplementary Information
      DERBONE[i,j]         = CARCW[i+1,j]*min(BONEFRACMAX,(BONEGROWTH1 * SENSMAT[68,s])*
                             CARCW[i+1,j]^-(BONEGROWTH2 * SENSMAT[69,s]))-CARCW[i,j]*
                             min(BONEFRACMAX, (BONEGROWTH1 * SENSMAT[68,s])*CARCW[i,j]^
                                   -(BONEGROWTH2 * SENSMAT[69,s]))
      # Derivative potential growth muscle tissue (kg day-1)
      # This equation contains Eqs 34 and 35 of the Supplementary Information
      DERMUSCLE[i,j]       = DERBONE[i,j]*min(LIBRARY[22],(MUSCLEGROWTH1*SENSMAT[70,s])*
                             CARCW[i+1,j]^2+LIBRARY[22]/100*CARCW[i+1,j]+
                               (MUSCLEGROWTH2*SENSMAT[71,s]))
      # Weight bone tissue (kg per animal)
      # Note: new state = previous state + rate of change over a time step
      BONETIS[i+1,j]       = BONETIS[i,j] + DERBONE[i,j]         
      # Weight muscle tissue (kg per animal)
      MUSCLETIS[i+1,j]     = MUSCLETIS[i,j] + DERMUSCLE[i,j]
      
      # Intramuscular fat, miscellaneous fat and non-carcass tissues
      # Derivative potential growth intramuscular fat (kg day-1)
      # This equation contains Eq. 36 of the Supplementary Information
      DERINTRAMF[i,j]      = ((IMFGROWTH1*SENSMAT[72,s])*(BONETIS[i+1,j]+MUSCLETIS[i+1,j])^2+
                                (IMFGROWTH2*SENSMAT[73,s])*(BONETIS[i+1,j]+MUSCLETIS[i+1,j])-
                                (IMFGROWTH3*SENSMAT[74,s]))-((IMFGROWTH1*SENSMAT[72,s])*
                                (BONETIS[i,j]+MUSCLETIS[i,j])^2+(IMFGROWTH2*SENSMAT[73,s])*
                                  (BONETIS[i,j]+MUSCLETIS[i,j])-(IMFGROWTH3*SENSMAT[74,s]))
      # Derivative potential growth subcutaneous and intermuscular (i.e. miscellaneous) fat 
      # tissue (kg day-1)
      # This equation corresponds to Eq. 37 of the Supplementary Information
      DERMISCFAT[i,j]      = (CARCW[i+1,j]-CARCW[i,j])-DERBONE[i,j] - DERMUSCLE[i,j] - 
                             DERINTRAMF[i,j]              
      # Derivative potential growth non carcass tissue (kg day-1)
      DERNONC[i,j]         = (TBW[i+1,j]*(1-RUMENFRAC)-TBW[i,j]*(1-RUMENFRAC))-
                             (CARCW[i+1,j]-CARCW[i,j])                              
      # Derivative potential growth of the rumen (kg day-1)
      # This equation is similar to Eq. 39 of the Supplementary Information
      DERRUMEN[i,j]        = (TBW[i+1,j]*RUMENFRAC-TBW[i,j]*RUMENFRAC)
      # Derivative potential growth of all body tissues (kg day-1)
      DERTOTAL[i,j]        = DERBONE[i,j]+DERMUSCLE[i,j]+DERINTRAMF[i,j]+DERMISCFAT[i,j]+
                             DERNONC[i,j]+DERRUMEN[i,j] 
      
      # Weight intramuscular fat tissue (kg per animal)
      INTRAMFTIS[i+1,j]    = INTRAMFTIS[i,j] + DERINTRAMF[i,j]
      # Weight miscellaneous fat tissue (kg per animal)
      MISCFATTIS[i+1,j]    = MISCFATTIS[i,j] + DERMISCFAT[i,j]
      # Weight non-carcass tissue (kg per animal)
      NONCARCTIS[i+1,j]    = NONCARCTIS[i,j] + DERNONC[i,j]
      # Weight rumen content (kg per animal)
      RUMEN[i+1,j]         = RUMEN[i,j] + DERRUMEN[i,j]
      
      # Check: sum of tissues and rumen content must correspond to the TBW (kg per animal)
      TBWCHECK[i+1,j]      = BONETIS[i+1,j]+ MUSCLETIS[i+1,j]+INTRAMFTIS[i+1,j]+
                             MISCFATTIS[i+1,j]+NONCARCTIS[i+1,j]+RUMEN[i+1,j] 
      
      # Fraction lipid in bone tissue (-)
      # This equation corresponds to Eq. 40 of the Supplementary Information
      LIPIDFRACBONE[i,j]   = (LIBRARY[20]* log10(BONETIS[i,j]))/100                                                             
      # Fraction lipid accreted in new non-carcass tissue (-)
      # This equation corresponds to Eq. 41 of the Supplementary Information
      LIPIDFRACNONC[i,j]   = min(LIPNONCMAX,max(LIPNONCMIN, LIPNONCMIN+(NONCARCTIS[i,j]/
                                 (LIBRARY[13]*(1-RUMENFRAC)*(1-LIBRARY[21])))^2*LIPNONCMAX))     
      # Fraction protein accreted in new non-carcass tissue (-)
      # This equation corresponds to Eq. 42 of the Supplementary Information
      PROTFRACNONC[i,j]    = ((PROTNONCM1*SENSMAT[75,s])*TBW[i,j] + 
                                (PROTNONCM2*SENSMAT[76,s])) / 100                                                                        
      
      # Weight of lipids and protein in body tissues
      # Note: new state = previous state + rate of change over one time step 
      # Weight of lipids in bone tissue (kg per animal)
      LIPIDBONE[i+1,j]     = LIPIDBONE[i,j] + DERBONE[i,j]*LIPIDFRACBONE[i,j] 
      # Weight of protein in bone tissue (kg per animal)
      PROTBONE[i+1,j]      = PROTBONE[i,j] + DERBONE[i,j]*PROTFRACBONE
      # Energy (combustion energy + inefficiency) accreted in bone tissue (MJ per day)
      # Note: 53.5 MJ kg-1 for lipid, 44.0 MJ kg-1 gross energy for protein 
      ENGRBONE[i+1,j]      = DERBONE[i,j]*LIPIDFRACBONE[i,j]*GELIPID/LIPIDEFF + 
                             DERBONE[i,j] * PROTFRACBONE*GEPROT/PROTEFF   
      
      # Weight of lipids in bone tissue (kg per animal)
      LIPIDMUSCLE[i+1,j]   = LIPIDMUSCLE[i,j] + DERMUSCLE[i,j]*LIPFRACMUSCLE 
      # Weight of protein in muscle tissue (kg per animal)
      PROTMUSCLE[i+1,j]    = PROTMUSCLE[i,j] + DERMUSCLE[i,j]*PROTFRACMUSCLE
      # Energy (combustion energy + inefficiency) accreted in muscle tissue (MJ per day)
      ENGRMUSCLE[i+1,j]    = DERMUSCLE[i,j]*LIPFRACMUSCLE*GELIPID/LIPIDEFF + 
                             DERMUSCLE[i,j] * PROTFRACMUSCLE*GEPROT/PROTEFF
      
      # Weight of lipids in the intermuscular fat tissue (kg per animal)
      LIPIDIMF[i+1,j]      = LIPIDIMF[i,j] + DERINTRAMF[i,j]*LIPFRACFAT 
      # Weight of protein in the intermuscular fat tissue (kg per animal)
      PROTIMF[i+1,j]       = PROTIMF[i,j] + DERINTRAMF[i,j]*PROTFRACFAT
      # Energy (combustion energy + inefficiency) accreted in the intermuscular fat tissue
      # (MJ per day)
      ENGRIMF[i+1,j]       = DERINTRAMF[i,j]*LIPFRACFAT*GELIPID/LIPIDEFF + 
                             DERINTRAMF[i,j] * PROTFRACFAT*GEPROT/PROTEFF
      
      # Weight of lipids in the miscellaneous fat tissue (kg per animal)
      LIPIDFAT[i+1,j]      = LIPIDFAT[i,j] + DERMISCFAT[i,j]*LIPFRACFAT 
      # Weight of protein in the miscellaneous fat tissue (kg per animal)
      PROTFAT[i+1,j]       = PROTFAT[i,j] + DERMISCFAT[i,j]*PROTFRACFAT
      # Energy (combustion energy + inefficiency) accreted in miscellaneous fat tissue 
      # (MJ per day)
      ENGRFAT[i+1,j]       = DERMISCFAT[i,j]*LIPFRACFAT*GELIPID/LIPIDEFF + 
                             DERINTRAMF[i,j] * PROTFRACFAT*GEPROT/PROTEFF
      
      # Weight of lipids in the non-carcass tissue (kg per animal)
      LIPIDNONC[i+1,j]     = LIPIDNONC[i,j] + DERNONC[i,j] * LIPIDFRACNONC[i,j] 
      # Weight of protein in the non-carcass tissue (kg per animal)
      PROTNONC[i+1,j]      = PROTNONC[i,j] + DERNONC[i,j] * PROTFRACNONC[i,j]
      # Energy (combustion energy + inefficiency) accreted in non-carcass tissue (MJ per day)
      ENGRNONC[i+1,j]      = DERNONC[i,j]*LIPIDFRACNONC[i,j]*GELIPID/LIPIDEFF + 
                             DERNONC[i,j] * PROTFRACNONC[i,j]*GEPROT/PROTEFF 
      
      # Total net energy (NE) to realise potential growth (MJ per day)
      # This equation is similar to Eq. 43 of the Supplementary Information
      ENGRTOTAL[i+1,j]     = ENGRBONE[i+1,j]+ENGRNONC[i+1,j]+ENGRMUSCLE[i+1,j]+
                             ENGRIMF[i+1,j]+ENGRFAT[i+1,j]  
      ENGRTOTALORIG[i+1,j] = ENGRTOTAL[i+1,j] 
      
      # Records the highest NE for growth throughout an animals life span (MJ per day)
      ENGRTOTALHIGH[i+1,j] = if(TIME[i,j] <= WEANINGTIME) ENGRTOTALHIGH[i+1,j] <- 0 else 
        ENGRTOTALHIGH[i+1,j] <- max(ENGRTOTALHIGH[i,j], ENGRTOTAL[i+1,j]) 

      # Records the highest NE for growth throughout an animals life span, including
      # compensatory growth (MJ per day)
      # This equation corresponds to Eq. 45 of the Supplementary Information, and contains
      # Eq. 44.
      ENGRTOTALHIGH1[i+1,j] = ENGRTOTALHIGH[i+1,j] * 
                              min(1.0,(1-(TBWBF[i,j]/TBW[i,j]))*COMPFACT)
      
      # Relative proportion between NE for growth with and without compensatory growth (-) 
      REL[i+1,j]  = ENGRTOTALHIGH1[i+1,j]/ENGRTOTAL[i+1,j]
      if(REL[i+1,j] <1) REL[i+1,j] <- 1
      
      # The NE for growth based on the genetic potential and the scope for compensatory 
      # growth (MJ per day)
      if(TIME[i,j] <= WEANINGTIME) ENGRTOTAL[i+1,j] <- ENGRTOTAL[i+1,j] else 
        ENGRTOTAL[i+1,j] <- max(ENGRTOTAL[i+1,j], min(1.0,(1-(TBWBF[i,j]/TBW[i,j]))*
                                                      COMPFACT)*ENGRTOTALHIGH[i+1,j]) 
      # Percentage of lipid in the carcass (consists of bone tissue, muscle tissue, and fat 
      # tissues) (%)    
  
      LIPIDFRACCARC[i+1,j] = (LIPIDBONE[i+1,j]+LIPIDMUSCLE[i+1,j]+LIPIDIMF[i+1,j]+
                             LIPIDFAT[i+1,j])/(TBW[i+1,j]-NONCARCTIS[i+1,j]-RUMEN[i+1,j])*100 

    #########################################################################################
    # Maintenance #
    ###############    
    
        # Energy balance
        # NE for (fasting) maintenance (kJ per animal per day) 
        NEMAINT[i,j] = EBWBFMET[i,j] * NEm * LIBRARY[18]                    
        # NE for (fasting) maintenance (W m-2)
        NEMAINTWM[i,j] = NEMAINT[i,j] * 1000 / (3600 * 24 * AREA[i,j])         
       
        # Protein balance
        # Dermal loss of protein (CSIRO, 2007) (g protein day-1)
        PROTDERML[i,j] <- DERMPL* EBWBFMET[i,j]                               
        # Protein requirement for (fasting) maintenance (CSIRO, 2007) (g protein day-1)
        # Note: PROTNE = 2g N / 4.18 = 0.478
        PROTMAINT[i,j] <- NEMAINT[i,j] * PROTNE / 1000 * NtoCP                   
    
    #########################################################################################
    # Physical activity #
    #####################
        
        # Energy balance
        # NE for physical activity (kJ per animal per day), function of metabolic body weight
        # Note: If necessary, the line of code below can also be written as a function of the
        #       total body weight (TBW)
        if(HOUSING[i] >= 1) NEPHYSACT[i,j] = EBWBFMET[i,j] * NEpha else NEPHYSACT[i,j] <- 0                          
        # NE for physical activity (W m-2)
        NEPHYSACTWM[i,j] = NEPHYSACT[i,j] * 1000 / (3600 * 24 * AREA[i,j])                                  
        
        # Protein balance
        # Protein requirement for physical activity (g protein day-1)
        # Note: PROTNE = 2g N / 4.18 = 0.478
        PROTPHACT[i,j] <- NEPHYSACT[i,j] * PROTNE / 1000 * NtoCP                                        
  
    #########################################################################################
    # Gestation #
    #############    
    
        # Cows can conceive if seven requirements are met (0= no conception, 1 = conception): 
        
        # 1. The TBW must be higher than a specific fraction of their maximum adult TBW    
        if(TBWBF[i,j]<(LIBRARY[17]*MAXW1[j])) GEST1[i,j] <- 0 else GEST1[i,j] <- 1                   
        # 2. Cows cannot conceive while gestating
        if(CALFTBW[i,j]==0) GEST2[i,j] <- 1 else GEST2[i,j] <- 0                                      
        # 3. Cows cannot conceive directly after parturition (depending on the minimum 
        # calving interval)
        if(sum(CALFTBW[i,j])-sum(CALFTBW[max(0,i-(GESTINTERVAL-GestPer-1)),j])==0) 
          GEST3[i,j] <- 1 else GEST3[i,j] <- 0     
        # 4. Cows can conceive if the fat tissues cover a certain fraction of the carcass 
        # tissue
        if((MISCFATTISBF[i,j]+INTRAMFTISBF[i,j])/
           (TBWBF[i,j]-NONCARCTISBF[i,j]-RUMEN[i,j]) < LIBRARY[19]) 
          GEST4[i,j] <- 0 else GEST4[i,j] <- 1                
        # 5. Cattle conceive at a specific date (STDOY), in case of seasonal calving
        # Note: for year-round calving, GEST5 must be 1
        if(DOY[i] == STDOY+(GESTINTERVAL-GestPer)) GEST5[i,j] <- 1 else GEST5[i,j] <- 1                                       
        # 6. Cows cannot conceive after their maximum age for conception
        if(TIME[i,j]/365<MAXCONCAGE) GEST5[i,j] <- GEST5[i,j] else GEST5[i,j] <- 0                          
        
        # 7. Cows cannot conceive anymore if the maximum number of calves per cow is achieved
        # If not eight calves after 10 years, reduce MAXCALFNR
        if(TIME[i,j]/365>MAXCONCAGE) MAXCALFNR <- CALFNR[i,j] else MAXCALFNR <- MAXCALFNR                   
        if(CALFNR[i,j] < MAXCALFNR) GEST6[i+1,j] <- 1 else GEST6[i+1,j] <- 0                          
        
        # Check whether a cow meets all seven conditions (0= no conception, 1 = conception)
        GEST[i,j] <- GEST1[i,j] * GEST2[i,j] * GEST3[i,j] * GEST4[i,j] * GEST5[i,j] * 
          GEST6[i+1,j] * LIBRARY[14] * REPRODUCTIVE[j]  
        
        # Counts number of calves (incl. gestation) per cow
        CALFNR[i+1,j] = GEST[i,j] + CALFNR[i,j]                                                      
        
        # Gestation starts when all seven requirements are met
        if(GEST[i,j]==1) GESTDAY[i+1,j] <- 1 else 
          if (GESTDAY[i,j]>0) GESTDAY[i+1,j] <- GESTDAY[i,j] + 1 else GESTDAY[i+1,j] <- 0 
        if(GEST[i,j]==1) GESTDAY[i,j] <-0
        
        # Breed- and sex-specific birth weights (kg live weight)
        if(BREED == 1)  BIRTHW1[i,j] <- LIBRARY10[5]-SEX[CALFNR[i,j]+1]*
          (LIBRARY10[5]-LIBRARY11[5]) else   
          if(BREED == 2)  BIRTHW1[i,j] <- LIBRARY20[5]-SEX[CALFNR[i,j]+1]*
            (LIBRARY20[5]-LIBRARY21[5]) else  
            if(BREED == 3)  BIRTHW1[i,j] <- LIBRARY30[5]-SEX[CALFNR[i,j]+1]*
              (LIBRARY30[5]-LIBRARY31[5]) else
              if(BREED == 4)  BIRTHW1[i,j] <- LIBRARY40[5]-SEX[CALFNR[i,j]+1]*
                (LIBRARY40[5]-LIBRARY41[5]) else
                if(BREED == 5)  BIRTHW1[i,j] <- LIBRARY50[5]-SEX[CALFNR[i,j]+1]*
                  (LIBRARY50[5]-LIBRARY50[5])
        
        # Code for sensitivity analysis on birth weight (not used in this version)
        BIRTHW1[i,j] <- BIRTHW1[i,j] * SENSMAT[46,s]
        
        # Net energy (NE) requirements for gestation, Fox et al. (1988)
        # This is an empirical equation.
        # This equation is similar to Eq. 28 and 29 in the Supplementary Information
        if(GESTDAY[i+1,j] >=1 & GESTDAY[i+1,j] <= GestPer)                       
          NEREQGEST[i,j]    <- (SENSMAT[93,s]) *((9.527001*(0.0000000681-0.000000000197*
                                GESTDAY[i,j])*(exp((0.0885-0.0001282*GESTDAY[i,j])*
                                GESTDAY[i,j])) + 5.505*((0.00003452-0.0000001094*
                                GESTDAY[i,j])*(exp((0.0589-0.00009334*GESTDAY[i,j])*
                                GESTDAY[i,j]))))*CALTOJOULE*10*(BIRTHW1[i,j]/37.2)*0.6) else 
                                  NEREQGEST[i,j] = 0       
        
        # Note: The efficiency of energy accretion gestation is only 14% (Jarrige, 1989, 
        # Rattray et al., 1974) for the calf, and 9.33% for extra tissue of the 
        # reproductive cow (Jarrige, 1989)
        
        # Heat production from gestation (MJ per cow per day)
        HEATGEST[i,j] = NEREQGEST[i,j] * NEIEFFGEST                                        
        # Cumulative NE during gestation (MJ)
        NEREQGESTADD[i+1,j] = NEREQGESTADD[i,j]+NEREQGEST[i,j]                                                            
        
        # Breed- and sex-specific NE requirements for the complete gestation period
        if(BREED == 1 & SEX[CALFNR[i,j]+1] == 0) NEREQGESTTOT = 58.658*LIBRARY10[7]+0.5502  
        if(BREED == 1 & SEX[CALFNR[i,j]+1] == 1) NEREQGESTTOT = 58.658*LIBRARY11[7]+0.5502 
        if(BREED == 2 & SEX[CALFNR[i,j]+1] == 0) NEREQGESTTOT = 58.658*LIBRARY20[7]+0.5502 
        if(BREED == 2 & SEX[CALFNR[i,j]+1] == 1) NEREQGESTTOT = 58.658*LIBRARY21[7]+0.5502 
        if(BREED == 3 & SEX[CALFNR[i,j]+1] == 0) NEREQGESTTOT = 58.658*LIBRARY30[7]+0.5502 
        if(BREED == 3 & SEX[CALFNR[i,j]+1] == 1) NEREQGESTTOT = 58.658*LIBRARY31[7]+0.5502 
        if(BREED == 4 & SEX[CALFNR[i,j]+1] == 0) NEREQGESTTOT = 58.658*LIBRARY40[7]+0.5502 
        if(BREED == 4 & SEX[CALFNR[i,j]+1] == 1) NEREQGESTTOT = 58.658*LIBRARY41[7]+0.5502 
        if(BREED == 5 & SEX[CALFNR[i,j]+1] == 0) NEREQGESTTOT = 58.658*LIBRARY50[7]+0.5502
        
        # The weight of the foetus is calculated from the cumulative NE for gestation and the 
        # complete NE required during the gestation period (kg).            
        CALFTBW[i+1,j] = NEREQGESTADD[i+1,j]/NEREQGESTTOT*BIRTHW1[i,j]
  
        # At the end of the gestation period, the calf reaches its birth weight (kg live 
        # weight)
        if(GESTDAY[i+1,j] >=1 & GESTDAY[i+1,j] <= GestPer) 
          CALFTBW[i+1,j] <- CALFTBW[i+1,j] else CALFTBW[i+1,j] <- 0     
        # The (cumulative) NE requirements for gestation stop after parturition
        if((CALFTBW[i,j]-CALFTBW[i+1,j])>BIRTHW1[i,j]-1) NEREQGESTADD[i+1,j] <- 0                                                  
        
        # Additional TBW of the cow, without an increase in the NE requirements for 
        # maintenance. The weight increase of the cow includes the weight of the foetus and 
        # the concepta (Jarrige, 1986, p. 99)
        TBWADD[i+1,j] = CALFTBW[i+1,j] * FtoConcW                                             
        
        # Protein balance
        # Protein requirements for gestation (g protein day-1)
        # Note: * based on total NE and protein requirements (CSIRO, 2007)
        #       * the conversion is 4.322 g protein per MJ NE for gestation
        #       * the formula given in CSIRO (2007) is assumed to present the gross protein 
        #         requirement for gestation
        PROTGESTG[i,j]   <- NEREQGEST[i,j] * CPGEST    
        
    #########################################################################################    
    # Milk production #
    ###################
        
        # Days in milk, milk production starts at parturition 
        if(GESTDAY[i,j] == GestPer-1) MILKDAYST[i,j] <- 1 else MILKDAYST[i,j] <- 0                    
        
        # Start of milk production after parturition 
        if (MILKDAYST[i,j]==1) ADDMILK2[i,j] <- 1 else ADDMILK2[i,j] <- 0                       
        # Adds up days after parturition
        if (MILKDAY[i,j] >0 ) ADDMILK1[i,j] <- 1 else ADDMILK1[i,j] <- 0                        
        # Days after parturition
        MILKDAY[i+1,j] = MILKDAY[i,j] + ADDMILK1[i,j] + ADDMILK2[i,j]   
        # Milk production ends at weaning; cow and calf are separated at weaning
        if(MILKDAY[i+1,j] == WEANINGTIME+1) MILKDAY[i+1,j] <-0                                
        
        # The number of calves born per reproductive cow is calculated from the number of
        # conceptions
        if(CALFNR[i,j] == 0) CALFLIVENR[i,j] <- 0 else CALFLIVENR[i,j] <- CALFNR[i-GestPer,j]   
        # The number of calves weaned per reproductive cow is calculated from the number of
        # calves born.
        if(CALFLIVENR[i,j] == 0) CALFWEANNR[i,j] <- 0 else 
          CALFWEANNR[i,j] <- CALFLIVENR[i-WEANINGTIME,j]
        
        # Converts days in milk to weeks in milk (weeks) 
        MILKWEEK[i,j] = MILKDAY[i,j]/7                                                        
        
        # Maximum milk production based on genotype (L day-1)
        # This equation corresponds to Eq. 30 in the Supplementary Information
        if(MILKWEEK[i,j] >0) MAXMILKPROD[i,j] <- MILKPARA*MILKDAY[i,j]^MILKPARB*
          exp(-MILKPARC*MILKDAY[i,j]) else MAXMILKPROD[i,j] <- 0
        
        # Milk received by the calf from its mother (MJ ME day-1)
        if(TIME[i,j] > WEANINGTIME)  MEMILKCALFINIT[i,j] <- 0 else 
          MEMILKCALFINIT[i,j] <- MEMILKCALFINIT[i,j]
        
        # Assumption: milk production is the maximum milk production based on the genotype
        # (L day-1)
        MILKPRODBF[i,j] = MAXMILKPROD[i,j] 
    
        # Energy balance
        # Gross energy (GE, combustion value) in milk (MJ GE L-1)
        GEMILK[i,j] =(((GEMILK1*SENSMAT[94,s]) *MILKDAY[i,j])+(GEMILK2*SENSMAT[95,s]))/1000          
        # Maximum gross energy from milk production (MJ GE day-1)
        GEMILKTOT[i,j] = GEMILK[i,j] * MAXMILKPROD[i,j]         
        # Maximum metabolisable energy (ME) in milk (MJ ME day-1)
        MEMILKCALF[i,j] = MILKDIG * GEMILKTOT[i,j]                 
        # Net energy (NE) requirement for milk production for reproductive cow (MJ NE day-1)
        NEMILKCOW[i,j] = GEMILKTOT[i,j] / NEEFFMILK
        # Heat generation for milk synthesis (MJ day-1)
        HEATMILK[i,j] = NEMILKCOW[i,j] * (1-NEEFFMILK)                     
        
        # Protein balance
        # Protein in milk (g protein day-1)
        PROTMILK[i,j]    <- MILKPRODBF[i,j] * PROTFRACMILK * 1000     
        # Gross amount of protein required for milk production (g protein day-1)
        PROTMILKG[i,j]   <- PROTMILK[i,j] / PROTEFFMILK                
    
    #########################################################################################
    # Growth #
    ##########
    
    # Compensatory growth: potential growth can be exceeded. Assumption is that the  
    # compensatory growth isproportional to the difference between actual TBW and genetic 
    # potential TBW of the animal. 
    
    # Factor for compensatory growth for non carcass tissue (-)     
    COMPGROWTH1[i,j] <- min(COMPFACTTIS,  max(1,NONCARCTIS[i,j]/NONCARCTISBF[i,j])) 
    # Factor for compensatory growth for bone tissue (-)
    COMPGROWTH2[i,j] <- min(COMPFACTTIS,  max(1,BONETIS[i,j]/BONETISBF[i,j]))
    # Factor for compensatory growth for muscle tissue (-)
    COMPGROWTH3[i,j] <- min(COMPFACTTIS,  max(1,MUSCLETIS[i,j]/MUSCLETISBF[i,j]))
    # Factor for compensatory growth for intramuscular fat tissue (-)
    COMPGROWTH4[i,j] <- min(COMPFACTTIS,  max(1,INTRAMFTIS[i,j]/INTRAMFTISBF[i,j]))
    # Factor for compensatory growth for miscellaneous fat tissue (-)
    COMPGROWTH5[i,j] <- min(COMPFACTTIS,  max(1,MISCFATTIS[i,j]/MISCFATTISBF[i,j]))
    
    # Weighted average of the factor for compensatory growth (-)  
    COMPGROWTH[i,j]  <- COMPGROWTH1[i,j] * (NONCARCTISBF[i,j]/(TBWBF[i,j]*(1-RUMENFRAC))) + 
                        COMPGROWTH2[i,j] * (BONETISBF[i,j]   /(TBWBF[i,j]*(1-RUMENFRAC))) +
                        COMPGROWTH3[i,j] * (MUSCLETISBF[i,j] /(TBWBF[i,j]*(1-RUMENFRAC))) +
                        COMPGROWTH4[i,j] * (INTRAMFTISBF[i,j]/(TBWBF[i,j]*(1-RUMENFRAC))) +
                        COMPGROWTH5[i,j] * (MISCFATTISBF[i,j]/(TBWBF[i,j]*(1-RUMENFRAC))) 
    
    # Fraction lipid in the bone tissue (-)
    LIPIDFRACBONEBF[i,j]     = max((LIPBONE1*SENSMAT[96,s]),((LIPBONE2*SENSMAT[97,s])*
                               log(BONETISBF[i,j]) + (LIPBONE3*SENSMAT[98,s]))/100)                                                         
    # Fraction lipid in the non-carcass tissue (-)
    LIPIDFRACNONCBF[i,j]     = ((LIPNONC1*SENSMAT[99,s])*NONCARCTISBF[i,j]^3 - 
                                  (LIPNONC2*SENSMAT[100,s])*NONCARCTISBF[i,j]^2 + 
                                  (LIPNONC3*SENSMAT[101,s])*NONCARCTISBF[i,j] - 
                                  (LIPNONC4*SENSMAT[102,s]))/100 * 
                                  (3.916E-10* ((1-LIBRARY[21]-RUMENFRAC)*LIBRARY[13])^4 - 
                                  7.058E-07* ((1-LIBRARY[21]-RUMENFRAC)*LIBRARY[13])^3 + 
                                  4.868E-04* ((1-LIBRARY[21]-RUMENFRAC)*LIBRARY[13])^2 - 
                                  1.593E-01* ((1-LIBRARY[21]-RUMENFRAC)*LIBRARY[13]) + 
                                  2.286E+01)   
    
    # Fraction protein in the  non-carcass tissue
    PROTFRACNONCBF[i,j]    = ((PROTNONC1*SENSMAT[103,s])*NONCARCTISBF[i,j]^4 - 
                                (PROTNONC2*SENSMAT[104,s])*NONCARCTISBF[i,j]^3 + 
                                (PROTNONC3*SENSMAT[105,s])*NONCARCTISBF[i,j]^2 - 
                                (PROTNONC4*SENSMAT[106,s])*NONCARCTISBF[i,j] + 
                                (PROTNONC5*SENSMAT[107,s]))/100 
    
    # Energy for growth, consists of three parts: 
    # 1. Genetic potential growth, including compensatory growth
    # 2. Energy requirements to recover the depleted subcutaneous and intermuscular fat 
    #    tissues
    # 3. Reduction in energy for growth mentioned in 1 and 2 due to climatic conditions.
    
    # Under energy limitation, tissues get energy according to their position in the 
    # hierarchy: 1. Non carcass tissue 2. Bone tissue 3. Muscle tissue 4. Intramuscular fat 
    # tissue 5. Subcutaneous and intermuscular fat tissue
    
    # Factor to counter low weights of the miscellaneous and non-carcass tissues (-)  
    # This equation corresponds to Eq. 46 of the Supplementary Information
    FATCOMP[i,j] <- max(0, FATTISCOMP-MISCFATTISBF[i,j]/MISCFATTIS[i,j])*
      (TBWBF[i,j]*(1-RUMENFRAC))^0.75*FATFACTOR 
     
  ###########################################################################################
  # 2.3                       Feed intake and digestion sub-model                           #
  ###########################################################################################
    
    # Digestion capacity limitation (related to the feed quality)
    # Maximum digestion capacity (Fill Units per animal per day)
    # This equation corresponds partly to Eq. 22 of the Supplementary Information
    PHFEEDINT[i,j] = TBWBF[i,j]^0.75 * PHFEEDCAP/1000 * 
      max(0,min(1, ((RUMENDEV1*SENSMAT[108,s]) * TIME[i,j] -(RUMENDEV2*SENSMAT[109,s]))))   
    
    # Code below enables to feed animal per 100 kg TBW (fixed % of the TBW)
    if(FEEDNR[z] ==5) FEED1QNTY[i,j] <- FEED1QNTY[i,j] * TBWBF[i,j]/100 
    if(FEEDNR[z] ==5) FEED2QNTY[i,j] <- FEED2QNTY[i,j] * TBWBF[i,j]/100 
  
    # Feed intake cannot exceed the digestive capacity of the rumen
    # Fill units feed type 1
    FUFEED1[i,j] <- FEED1QNTY[i,j]*FEED1[i,2] 
    
    # Maximum intake of feed type 1 based on the digestive capacity of the rumen (kg DM)
    if((PHFEEDINT[i,j] - FUFEED1[i,j]) < 0) FEED1QNTYA[i,j] <- PHFEEDINT[i,j]/
      (FEED1fr*FEED1[i,2]+(1-FEED1fr)*FEED2[i,2])*FEED1fr else 
        FEED1QNTYA[i,j] <- FEED1QNTY[i,j] 
    
    # Fill units feed type 1 + feed type 2 (FU)
    FUFEED2[i,j] <- FEED1QNTYA[i,j]*FEED1[i,2] + FEED2QNTY[i,j]*FEED2[i,2]
    # Maximum intake of feed type 2 based on the digestive capacity of the rumen (kg DM)
    if((PHFEEDINT[i,j] - FUFEED2[i,j]) < 0) FEED2QNTYA[i,j] <- PHFEEDINT[i,j]/
      (FEED1fr*FEED1[i,2]+(1-FEED1fr)*FEED2[i,2])*(1-FEED1fr) else 
        FEED2QNTYA[i,j] <- FEED2QNTY[i,j]  
    
    # Fill units feed type 1 + feed type 2 + feed type 3 (FU)
    FUFEED3[i,j] <- FEED1QNTYA[i,j]*FEED1[i,2] + FEED2QNTYA[i,j]*FEED2[i,2] + 
      FEED3QNTY[i,j]*FEED3[i,2] 
    # Maximum intake of feed type 3 based on the digestive capacity of the rumen (kg DM)
    if((PHFEEDINT[i,j] - FUFEED3[i,j]) < 0) 
      FEED3QNTYA[i,j] <- max(0,(PHFEEDINT[i,j]-FUFEED2[i,j])/FEED3[i,2]) else 
        FEED3QNTYA[i,j] <- FEED3QNTY[i,j]  
    
    # Fill units feed type 1 + feed type 2 + feed type 3 + feed type 4 (FU)
    FUFEED4[i,j] <- FEED1QNTY[i,j]*FEED1[i,2] + FEED2QNTY[i,j]*FEED2[i,2] + 
      FEED3QNTY[i,j]*FEED3[i,2] + FEED4QNTY[i,j]*FEED4[2]
    # Maximum intake of feed type 4 based on the digestive capacity of the rumen (kg DM)
    if((PHFEEDINT[i,j] - FUFEED4[i,j]) < 0) 
      FEED4QNTYA[i,j] <- max(0,(PHFEEDINT[i,j]-FUFEED3[i,j])/FEED4[2]) else 
        FEED4QNTY[i,j] <- FEED4QNTY[i,j] 
    FEED4QNTYA[i,j] <- min(FEED4QNTY[i,j], FEED4fr*PHFEEDINT[i,j]/FEED4[2])
   
    # Rumen fill classes according to Chilibroste et al. (1997)
    if(FUFEED4[i,j] > PHFEEDINT[i,j]* 0.85) PASSAGE[i,j] <- 1 else         
      if(FUFEED4[i,j] > PHFEEDINT[i,j]* 0.65 & FUFEED4[i,j] < PHFEEDINT[i,j]* 0.85) 
        PASSAGE[i,j] <- 2 else
        if(FUFEED4[i,j] > PHFEEDINT[i,j]* 0.45 & FUFEED4[i,j] < PHFEEDINT[i,j]* 0.65) 
          PASSAGE[i,j] <- 3 else PASSAGE[i,j] <- 4
    
    # Average crude protein (CP) in the diet (g CP)
    CPAVG[i,j] <- (FEED1QNTYA[i,j]*FEED1[i,16] + FEED2QNTYA[i,j]*FEED2[i,16] + 
                     FEED3QNTYA[i,j]*FEED3[i,16] + FEED4QNTYA[i,j]*FEED1[16]) /   
                  (FEED1QNTYA[i,j] + FEED2QNTYA[i,j] + FEED3QNTYA[i,j] + FEED4QNTYA[i,j])  
    
  ###########################################################################################
  # 2.4 Integration thermoregulation, feed intake and digestion, and utilisation sub-models #
  ###########################################################################################
  
    # Initial four assumptions for the integration loop: 
    
    # 1. The reduction in NE availability due to heat stress is assumed to be zero   
    # (guesstimate, indicates absence of heatstress)
    REDHP[i,j] <- 0               
    
    # 2. The increase in heat production due to cold stress is asssumed to be zero 
    # (guesstimate, indicates absence of cold stress)   
    HEATIFEEDGROWTHC[i,j] <- 0    
    
    # 3. The feed intake is 20 kg DM per animal per day. This number is generally too high
    # for beef cattle, and this number is reduced later in the loop.
    FEEDINTAKE[i,j] <- 20           
    
    # 4. The animal is assumed to be fed above the maintenance level  
    REDMAINT[i,j] <- 0             
    
    REPS[i,j] <- 0 # Indicates and counts the times the integration loop is repeated
    
    
    repeat { # Start of the integration loop   
    
      # Minimum feed quantities based on rumen digestive capacity and the available feed 
      # quantity. Feed intake is reduced here from 20 kg DM head-1 day-1 to the correct  
      # amount
      
      # Intake feed type 1 (kg DM per animal per day)
      if(REPRODUCTIVE[j] == 1) 
        FEED1QNTY[i,j] <- max(0,min(FEED1QNTYA[i,j], FEED1fr*FEEDINTAKE[i,j])) else
        if(PRODUCTIVE[j] == 1 && SEX[j] == 0) 
          FEED1QNTY[i,j] <- max(0,min(FEED1QNTYA[i,j], FEED1fr*FEEDINTAKE[i,j])) * 1 else
          if(PRODUCTIVE[j] == 1 && SEX[j] == 1) 
            FEED1QNTY[i,j] <- max(0,min(FEED1QNTYA[i,j], FEED1fr*FEEDINTAKE[i,j])) * 1 else
            FEED1QNTY[i,j] <- max(0,min(FEED1QNTYA[i,j], FEED1fr*FEEDINTAKE[i,j]))
      
      # Intake feed type 2 (kg DM per animal per day)  
      if(REPRODUCTIVE[j] == 1) 
        FEED2QNTY[i,j] <- max(0,min(FEED2QNTYA[i,j], FEED2fr*FEEDINTAKE[i,j], 
                                    FEEDINTAKE[i,j]-FEED1QNTY[i,j])) else
        if(PRODUCTIVE[j] == 1 && SEX[j] == 0) 
          FEED2QNTY[i,j] <- max(0,min(FEED2QNTYA[i,j], FEED2fr*FEEDINTAKE[i,j], 
                                      FEEDINTAKE[i,j]-FEED1QNTY[i,j])) * 1 else
          if(PRODUCTIVE[j] == 1 && SEX[j] == 1) 
            FEED2QNTY[i,j] <- max(0,min(FEED2QNTYA[i,j], FEED2fr*FEEDINTAKE[i,j], 
                                        FEEDINTAKE[i,j]-FEED1QNTY[i,j])) * 1 else
            FEED2QNTY[i,j] <- max(0,min(FEED2QNTYA[i,j], FEED2fr*FEEDINTAKE[i,j], 
                                        FEEDINTAKE[i,j]-FEED1QNTY[i,j]))
      
      # Intake feed type 2 (kg DM per animal per day)    
      if(REPRODUCTIVE[j] == 1) 
        FEED3QNTY[i,j] <- max(0,min(FEED3QNTYA[i,j], FEED3fr*FEEDINTAKE[i,j], 
                                    FEEDINTAKE[i,j]-FEED1QNTY[i,j]-FEED2QNTY[i,j])) else
      if(PRODUCTIVE[j] == 1 && SEX[j] == 0) 
         FEED3QNTY[i,j] <- max(0,min(FEED3QNTYA[i,j], FEED3fr*FEEDINTAKE[i,j], 
                                     FEEDINTAKE[i,j]-FEED1QNTY[i,j]-FEED2QNTY[i,j])) * 1 else
      if(PRODUCTIVE[j] == 1 && SEX[j] == 1) 
         FEED3QNTY[i,j] <- max(0,min(FEED3QNTYA[i,j], FEED3fr*FEEDINTAKE[i,j], 
                                     FEEDINTAKE[i,j]-FEED1QNTY[i,j]-FEED2QNTY[i,j])) * 1 else
         FEED3QNTY[i,j] <- max(0,min(FEED3QNTYA[i,j], FEED3fr*FEEDINTAKE[i,j], 
                                     FEEDINTAKE[i,j]-FEED1QNTY[i,j]-FEED2QNTY[i,j]))

      if(REPRODUCTIVE[j] == 1) 
        FEED4QNTY[i,j] <- max(0,min(FEED4QNTYA[i,j], FEED4fr*FEEDINTAKE[i,j], 
                                    FEEDINTAKE[i,j]-FEED1QNTY[i,j]-
                                      FEED2QNTY[i,j]-FEED3QNTY[i,j])) else
      if(PRODUCTIVE[j] == 1 && SEX[j] == 0) 
        FEED4QNTY[i,j] <- max(0,min(FEED4QNTYA[i,j], FEED4fr*FEEDINTAKE[i,j], 
                                      FEEDINTAKE[i,j]-FEED1QNTY[i,j]-
                                        FEED2QNTY[i,j]-FEED3QNTY[i,j])) else
      if(PRODUCTIVE[j] == 1 && SEX[j] == 1) 
        FEED4QNTY[i,j] <- max(0,min(FEED4QNTYA[i,j], FEED4fr*FEEDINTAKE[i,j], 
                                    FEEDINTAKE[i,j]-FEED1QNTY[i,j]-
                                      FEED2QNTY[i,j]-FEED3QNTY[i,j])) else
      FEED4QNTY[i,j] <- max(0,min(FEED4QNTYA[i,j], FEED4fr*FEEDINTAKE[i,j], 
                                  FEEDINTAKE[i,j]-FEED1QNTY[i,j]-
                                    FEED2QNTY[i,j]-FEED3QNTY[i,j]))
      
      # Total feed intake (kg DM per animal per day)      
      FEEDQNTY[i,j] <- FEED1QNTY[i,j] + FEED2QNTY[i,j] + FEED3QNTY[i,j] + FEED4QNTY[i,j] 
       
      # Crude protein content of the diet (g kg DM-1 feed)
      if(FEEDQNTY[i,j] == 0) CPAVG[i,j] <- 0 else 
        CPAVG[i,j] <- (FEED1QNTY[i,j]*FEED1[i,16] + FEED2QNTY[i,j]*FEED2[i,16] + 
                         FEED3QNTY[i,j]*FEED3[i,16] + FEED4QNTY[i,j]*FEED1[16]) / 
                       FEEDQNTY[i,j] 
      
      # Calculates the fraction of each feed type in the diet
      if(TIME[i,j] <= 14 | FEEDQNTY[i,j] == 0) FRACFEED1[i,j] <- 0 else 
        FRACFEED1[i,j] <- FEED1QNTY[i,j]/FEEDQNTY[i,j] # Fraction feed type 1 in diet
      if(TIME[i,j] <= 14 | FEEDQNTY[i,j] == 0) FRACFEED2[i,j] <- 0 else 
        FRACFEED2[i,j] <- FEED2QNTY[i,j]/FEEDQNTY[i,j] # Fraction feed type 2 in diet
      if(TIME[i,j] <= 14 | FEEDQNTY[i,j] == 0) FRACFEED3[i,j] <- 0 else 
        FRACFEED3[i,j] <- FEED3QNTY[i,j]/FEEDQNTY[i,j] # Fraction feed type 3 in diet
      if(TIME[i,j] <= 14 | FEEDQNTY[i,j] == 0) FRACFEED4[i,j] <- 0 else 
        FRACFEED4[i,j] <- FEED4QNTY[i,j]/FEEDQNTY[i,j] # Fraction feed type 4 in diet
      
      #######################################################################################
      #                         Feed intake and digestion sub-model (again)                 #
      #######################################################################################
      
      # Maximum feed intake in kg, based on fill units (FU FU-1 kg)
      PHFEEDINTKG[i,j] <- PHFEEDINT[i,j]/(FRACFEED1[i,j]*FEED1[i,2]+
                          FRACFEED2[i,j]*FEED2[i,2]+FRACFEED3[i,j]*FEED3[i,2]+
                            FRACFEED4[i,j]*FEED4[2])   
      
      # Digestion of carbohydrates
      # INSC = insoluble, non-structural carbohydrates, which are assumed to mainly consist 
      # of starch) 
      
      # INSC digestion in the rumen (g INSC per animal per day)
      INSC[i,j] <-       FEED1QNTY[i,j] * FEED1[i,4] * FEED1[i,9]  / 
                         (FEED1[i,9]  + FEED1[i,12]*PASSRED[PASSAGE[i,j]]) + 
                         FEED2QNTY[i,j] * FEED2[i,4] * FEED2[i,9]  / 
                         (FEED2[i,9]  + FEED2[i,12]*PASSRED[PASSAGE[i,j]]) +
                         FEED3QNTY[i,j] * FEED3[i,4] * FEED3[i,9]  / 
                         (FEED3[i,9]  + FEED3[i,12]*PASSRED[PASSAGE[i,j]]) +
                         FEED4QNTY[i,j] * FEED4[4] * FEED4[9]  / 
                         (FEED4[9]  + FEED4[12]*PASSRED[PASSAGE[i,j]]) 
      # Total intake INSC in the feed (g INSC per animal per day)
      INSCTOTAL[i,j] <-  FEED1QNTY[i,j] * FEED1[i,4] + FEED2QNTY[i,j] * FEED2[i,4] + 
                         FEED3QNTY[i,j] * FEED3[i,4] + FEED4QNTY[i,j] * FEED4[4]  
      # Fraction insoluble, non-structural carbohydrates digested in the rumen (-) 
      # (compare to Owens, 1986) 
      INSCDIG[i,j]   <-  INSC[i,j]/INSCTOTAL[i,j] 
      # Digestibility of INSC in the intestines is assumed to be 97% for all feeds 
      # (Moharrery et al, 2014)
      INSCINT[i,j]   <-  max(0,(INSCTOTAL[i,j]*TTDIGINSC)-INSC[i,j]) 
      # Fraction INSC digested in the intestines (-)
      INSCINTDIG[i,j] <- INSCINT[i,j]/INSCTOTAL[i,j]  
      # Digestion degradable neutral detergent fibre (NDF, g day-1)
      NDF[i,j] <-        FEED1QNTY[i,j] * FEED1[i,5] * FEED1[i,10] / 
                         (FEED1[i,10] + FEED1[i,12]*PASSRED[PASSAGE[i,j]]) +  
                         FEED2QNTY[i,j] * FEED2[i,5] * FEED2[i,10] / 
                         (FEED2[i,10] + FEED2[i,12]*PASSRED[PASSAGE[i,j]]) +
                         FEED3QNTY[i,j] * FEED3[i,5] * FEED3[i,10] / 
                         (FEED3[i,10] + FEED3[i,12]*PASSRED[PASSAGE[i,j]]) +
                         FEED4QNTY[i,j] * FEED4[5] * FEED4[10] / 
                         (FEED4[10] + FEED4[12]*PASSRED[PASSAGE[i,j]])  
      # Total intake NDF (g day-1)
      NDFTOTAL[i,j] <-   FEED1QNTY[i,j] * FEED1[i,5] + FEED2QNTY[i,j] * FEED2[i,5] + 
                         FEED3QNTY[i,j] * FEED3[i,5] + FEED4QNTY[i,j] * FEED4[5]    
      # Fraction degradable NDF digested in the rumen (-)
      NDFDIG[i,j]   <-   NDF[i,j]/NDFTOTAL[i,j] 
      
      # NDF digestion in the intestines (g day-1)
      # Cabral et al., 2011 (http://www.scielo.br/pdf/rbz/v40n9/a20v40n9.pdf)
      # Volative fatty acids released are assumed not to be taken up by the animal
      NDFINT[i,j]   <-   FEED1QNTY[i,j] * FEED1[i,5] * (1- FEED1[i,10] / 
                         (FEED1[i,10] + FEED1[i,12]*PASSRED[PASSAGE[i,j]])) * 
                         (FEED1[i,10]*NDFDIGEST*SENSMAT[110,s]) / 
                         (FEED1[i,10]*NDFDIGEST*SENSMAT[110,s] + NDFPASS*SENSMAT[111,s]) +  
                         FEED2QNTY[i,j] * FEED2[i,5] * (1- FEED2[i,10] / 
                         (FEED2[i,10] + FEED2[i,12]*PASSRED[PASSAGE[i,j]])) * 
                         (FEED2[i,10]*NDFDIGEST*SENSMAT[110,s]) / 
                         (FEED2[i,10]*NDFDIGEST*SENSMAT[110,s] + NDFPASS*SENSMAT[111,s]) +   
                         FEED3QNTY[i,j] * FEED3[i,5] * (1- FEED3[i,10] / 
                         (FEED3[i,10] + FEED3[i,12]*PASSRED[PASSAGE[i,j]])) * 
                         (FEED3[i,10]*NDFDIGEST*SENSMAT[110,s]) / 
                         (FEED3[i,10]*NDFDIGEST*SENSMAT[110,s] + NDFPASS*SENSMAT[111,s]) +  
                         FEED4QNTY[i,j] * FEED4[5] * (1- FEED4[10] / 
                         (FEED4[10] + FEED4[12]*PASSRED[PASSAGE[i,j]]))* 
                         (FEED4[10]*NDFDIGEST*SENSMAT[110,s]) / 
                         (FEED4[10]*NDFDIGEST*SENSMAT[110,s] + NDFPASS*SENSMAT[111,s]) 
      
      # Fraction degradable NDF digested in intestines (-)
      NDFINTDIG[i,j] <-  NDFINT[i,j]/NDFTOTAL[i,j]
      # Fraction degradable NDF digested in intestines (g kg-1 DM feed)
      NDFINTDIGTOT[i,j] <- NDFINT[i,j]/(FEEDQNTY[i,j]*1000)  
      
      # Protein digestion
      # Degradable crude protein (DCP) digestion in the rumen (g day-1)
      PICP[i,j] <-       FEED1QNTY[i,j] * FEED1[i,7] * FEED1[i,11] / 
                         (FEED1[i,11] + FEED1[i,12]*PASSRED[PASSAGE[i,j]]) +  
                         FEED2QNTY[i,j] * FEED2[i,7] * FEED2[i,11] / 
                         (FEED2[i,11] + FEED2[i,12]*PASSRED[PASSAGE[i,j]]) +
                         FEED3QNTY[i,j] * FEED3[i,7] * FEED3[i,11] / 
                         (FEED3[i,11] + FEED3[i,12]*PASSRED[PASSAGE[i,j]]) +
                         FEED4QNTY[i,j] * FEED4[7] * FEED4[11] / 
                         (FEED4[11] + FEED4[12]*PASSRED[PASSAGE[i,j]])  
      # Total crude protein intake (g day-1)
      PROTTOTAL[i,j]  <- (FEED1QNTY[i,j] * FEED1[i,16] + FEED2QNTY[i,j] * FEED2[i,16] +
                          FEED3QNTY[i,j] * FEED3[i,16] + FEED4QNTY[i,j] * FEED4[16])  
      # Protein ending up in the intestines (g day-1)
      PROTINT[i,j]    <- PROTTOTAL[i,j] - (FEED1QNTY[i,j] * FEED1[i,6] + 
                                           FEED2QNTY[i,j] * FEED2[i,6] + 
                                           FEED3QNTY[i,j] * FEED3[i,6] + 
                                           FEED4QNTY[i,j] * FEED4[6]) - PICP[i,j]                        
      
      # Lucas equation (g protein day-1), whole digestive tract, plus the effect of recycling
      # This equation corresponds partly to Eq. 24 of the Supplementary Information and 
      # indicates the amount of protein taken up in the whole digestive tract.
      PROTUPT[i,j]    <- ((LUCAS1*SENSMAT[112,s]) * PROTTOTAL[i,j] - 
                            (LUCAS2*SENSMAT[113,s]) * FEEDQNTY[i,j]) 
      # Protein excreted (g protein day-1)
      PROTEXCR[i,j]   <- PROTTOTAL[i,j] - PROTUPT[i,j]   
      # Fraction protein digested in rumen (-)
      PROTDIGRU[i,j]  <- (PROTTOTAL[i,j]-PROTINT[i,j])/ PROTTOTAL[i,j] 
      # Fraction protein digested in the whole digestive tract (-)
      PROTDIGWT[i,j]  <- PROTUPT[i,j] / PROTTOTAL[i,j] 
      
      # Digestion and excretion
      # Feed digested in the whole digestive tract (g day-1)
      DIGFRAC[i,j] <-    FEED1QNTY[i,j] * (FEED1[i,3]+FEED1[i,6]) +           
                         FEED2QNTY[i,j] * (FEED2[i,3]+FEED2[i,6]) +
                         FEED3QNTY[i,j] * (FEED3[i,3]+FEED3[i,6]) +
                         FEED4QNTY[i,j] * (FEED4[3]+FEED4[6]) +
                         INSC[i,j] + INSCINT[i,j] + NDF[i,j] + NDFINT[i,j] + PROTUPT[i,j]     
      
      # Carbohydrates excreted (g day-1)
      CHEXCR[i,j]   <- FEEDQNTY[i,j]*1000-DIGFRAC[i,j]-PROTEXCR[i,j]                                                                                   
      # Manure dry matter excreted (g day-1)
      EXCRFRAC[i,j] <- FEEDQNTY[i,j]*1000-DIGFRAC[i,j]                       
      # Gross energy (GE) content excreted dry matter (MJ GE kg-1 DM) 
      GEEXCR[i,j]   <- (PROTEXCR[i,j] * GEPROT + CHEXCR[i,j] * GECARB) / 
        (PROTEXCR[i,j] + CHEXCR[i,j]) 
      
      # GE content digested feed (MJ GE kg-1 DM)  
      # This equation is similar to Eq. 25 in the Supplementary Information
      GEUPTAKE[i,j] <- (PROTUPT[i,j] * GEPROT + (DIGFRAC[i,j]-PROTUPT[i,j]) * GECARB) / 
        (DIGFRAC[i,j]) 
      
      # ME uptake (MJ ME day-1); 0.82 is conversion DE --> ME  
      if(EXCRFRAC[i,j] == 0) MEUPTAKE[i,j] <-0 else MEUPTAKE[i,j] <- DIGFRAC[i,j]/1000 * GEUPTAKE[i,j] * DETOME      
      
      # Digestibility feed (g g-1), on an energy basis
      if(EXCRFRAC[i,j] == 0) Q[i,j] <-0 else Q[i,j] = DIGFRAC[i,j]/(FEEDQNTY[i,j]*1000) * (GEUPTAKE[i,j] /GEFEED)   

      # Average heat increment of feeding (MJ MJ-1)
      if((FEED1QNTY[i,j] + FEED2QNTY[i,j] + FEED3QNTY[i,j] + FEED4QNTY[i,j]) == 0) 
        Digestfracfeed[i,j] <- 0.3 else 
        Digestfracfeed[i,j] <- (FEED1QNTY[i,j] * FEED1[i,1] + FEED2QNTY[i,j] * FEED2[i,1] +
                                  FEED3QNTY[i,j] * FEED3[i,1] + FEED4QNTY[i,j] * FEED4[1])/ 
                          (FEED1QNTY[i,j] + FEED2QNTY[i,j] + FEED3QNTY[i,j] + FEED4QNTY[i,j])                    
      
      # If feed intake is not reduced, no energy requirement for respiration above 
      # maintenance
      # Increase in net energy (NE) requirements due to heat stress (MJ NE day-1)
      if(REDHP[i,j] == 0) NERESPC[i,j] <- 0 else NERESPC[i,j] <- NERESP[i,j]/1000    
      # Increase in protein requirements under maximum heat release and heat stress (g day-1)
      PROTRESP[i,j] <- NERESPC[i,j] * PROTNE * NtoCP    
      
      #######################################################################################

      # Milk for the calf from the cow until weaning (MJ GE day-1)
      if(TIME[i,j]<=WEANINGTIME) 
        MILKSTART[i,j]   <- LIBRARY[15]*TIME[i,j]^MILKPARB*exp(-MILKPARC*TIME[i,j]) * 
        (((GEMILK1*TIME[i,j])+2771)/1000) * MILKDIG else MILKSTART[i,j] <- 0  
      
      # Digested protein from milk for the calf (g day-1)
      # 95% protein digestibility; 3.5% protein in milk, heat increment of feeding taken into
      # account 
      if(TIME[i,j]<=WEANINGTIME) 
        MILKSTARTPR[i,j] <- LIBRARY[15]*TIME[i,j]^MILKPARB*exp(-MILKPARC*TIME[i,j]) * 35 * 
        MILKDIG else MILKSTARTPR[i,j] <- 0 
      
      MILKSTARTPRHF[i,j] <- MILKSTARTPR[i,j] * (1+(Digestfracfeed[i,j]/(1-Digestfracfeed[i,j])))
      
      # Milk from reproductive cow for calf (MJ ME day-1)
      MEMILKCALFINIT[i,j] <- MILKSTART[i,j] * 
                             (1+(Digestfracfeed[i,j]/(1-Digestfracfeed[i,j])))  
      
      # Protein for non-growth purposes
      # Total fixed protein requirements (g protein day-1)
      PROTNONG[i,j]  <- PROTDERML[i,j] + PROTMAINT[i,j] + PROTPHACT[i,j] + PROTGESTG[i,j] + 
                        PROTMILKG[i,j] + PROTRESP[i,j]  
      # Heat increment of feeding (MJ day-1)
      HIFM[i,j]      <- (NEMAINT[i,j]/1000+NEPHYSACT[i,j]/1000+NEREQGEST[i,j]+
                           NEMILKCOW[i,j]+NERESPC[i,j]/1000) * 
                           (Digestfracfeed[i,j]/(1-Digestfracfeed[i,j]))    
      # Protein requirements for fixed processes, i.e. excluding growth (g protein day-1)
      PROTNONGM[i,j] <- PROTNONG[i,j] + HIFM[i,j] * PROTNE * NtoCP      
      
      # Energy not invested in growth is converted into heat (MJ day-1)
      HEATIFEEDMAINT[i,j] = NEMAINT[i,j]/1000 + NEPHYSACT[i,j]/1000 + HEATGEST[i,j] + 
        HEATMILK[i,j] + NERESPC[i,j]/1000 + HIFM[i,j] + HEATIFEEDGROWTHC[i,j]/DISSEFF + 
        REDMAINT[i,j] 
      
      # Sum of all heat released that is not related to growth, includes heat increment of 
      # feeding (MJ day-1)
      HEATIFEEDMAINTWM[i,j] = HEATIFEEDMAINT[i,j]/(3600*24* AREA[i,j])*1000000 
      
      # Heat release under maintenance level is not taken into account yet.
      # Maximum heat release from growth (W m-2)
      HEATIFEEDGROWTHWM[i,j] = Metheatopt[i,j]-HEATIFEEDMAINTWM[i,j]                          
      # Maximum heat release from growth (MJ day-1)
      HEATIFEEDGROWTH[i,j] = HEATIFEEDGROWTHWM[i,j]*(3600*24* AREA[i,j])/1000000             
      
      # Superfluous heat produced under sub-maintenance level is taken into account here 
      # (MJ day-1)
      if(HEATIFEEDGROWTH[i,j] < 0) REDMAINT[i,j] <- HEATIFEEDGROWTH[i,j] else 
        REDMAINT[i,j] <- 0
      
      # Energy available for growth, after accounting for heat increment of feeding (MJ NE 
      # day-1)
      ENFEEDGROWTHQ[i+1,j] <- ((MEUPTAKE[i,j]) - HEATIFEEDMAINT[i,j] + MEMILKCALFINIT[i,j]) / 
        (1+(Digestfracfeed[i,j]/(1-Digestfracfeed[i,j])))           
      ENFEEDGROWTHQ[i+1,j] <- ENFEEDGROWTHQ[i+1,j] - 0.134*NEREQGEST[i,j] - 
        (NEMILKCOW[i,j]-HEATMILK[i,j])
      
      # Integrates the net energy for growth, based on the genetic potential (ENGRTOTAL), 
      # the climate (REDHP), and feed-limitation (ENFEEDGROWTHQ)
      # Energy available for growth (MJ NE day-1)
      ENFEEDGROWTH[i+1,j]  = min(ENFEEDGROWTHQ[i+1,j], ENGRTOTAL[i+1,j] * COMPGROWTH[i,j]) + 
        REDHP[i,j]    
      
      # Fraction NE for growth allocated to the non-carcass tissue (-)
      FRENGRNONCBF[i+1,j] = max(0,(ENFEEDGROWTH[i+1,j]-FATCOMP[i,j])/ENFEEDGROWTH[i+1,j])         
      if(NONCARCTISBF[i,j]/NONCARCTIS[i,j] < 1) 
        FRENGRNONCBF[i+1,j] <- FRENGRNONCBF[i+1,j] else FRENGRNONCBF[i+1,j] <- 0
      # NE for growth allocated to the non-carcass tissue (MJ day-1)
      ENGRNONCBF[i+1,j]   = FRENGRNONCBF[i+1,j]* (ENGRNONC[i+1,j]/ENGRTOTALORIG[i+1,j]) * 
        (ENFEEDGROWTH[i+1,j]) * COMPGROWTH1[i,j] * ((TBWBF[i,j]/LIBRARY[13])*-
        (ENNONC1*SENSMAT[114,s]-ENNONC2*SENSMAT[115,s])+ENNONC1*SENSMAT[114,s])/
        (ENGRNONC[i+1,j]/ENGRTOTALORIG[i+1,j])  
      ENGRNONCBF[i+1,j]   = max(ENGRNONCBF[i+1,j],0) # NE for growth cannot be negative    
      
      # Fraction NE for growth allocated to the bone tissue (-)    
      FRENGRBONEBF[i+1,j] = max(0,(ENFEEDGROWTH[i+1,j]-FATCOMP[i,j])/(ENFEEDGROWTH[i+1,j]))
      if(BONETISBF[i,j]/BONETIS[i,j] < 1) 
        FRENGRBONEBF[i+1,j] <- FRENGRBONEBF[i+1,j] else FRENGRBONEBF[i+1,j] <- 0
      # NE for growth allocated to the bone tissue (MJ day-1)
      ENGRBONEBF[i+1,j]   = FRENGRBONEBF[i+1,j]* (ENGRBONE[i+1,j]/ENGRTOTALORIG[i+1,j]) * 
        (ENFEEDGROWTH[i+1,j]) * COMPGROWTH2[i,j]
      ENGRBONEBF[i+1,j]   = max(ENGRBONEBF[i+1,j],0) # NE for growth cannot be negative  
      
      # Fraction NE for growth allocated to the muscle tissue (-)
      FRENGRMUSCLEBF[i+1,j] = max(0,(ENFEEDGROWTH[i+1,j]-FATCOMP[i,j])/(ENFEEDGROWTH[i+1,j]))          
      if(MUSCLETISBF[i,j]/MUSCLETIS[i,j] < 1) 
        FRENGRMUSCLEBF[i+1,j] <- FRENGRMUSCLEBF[i+1,j] else FRENGRMUSCLEBF[i+1,j] <- 0
      # NE for growth allocated to the muscle tissue (MJ day-1)
      ENGRMUSCLEBF[i+1,j]   = FRENGRMUSCLEBF[i+1,j]* (ENGRMUSCLE[i+1,j]/
                               ENGRTOTALORIG[i+1,j]) * (ENFEEDGROWTH[i+1,j]) * 
                               COMPGROWTH3[i,j]
      ENGRMUSCLEBF[i+1,j]   = max(ENGRMUSCLEBF[i+1,j],0) # NE for growth cannot be negative
      
      # Fraction NE for growth allocated to the intramuscular fat tissue (-)
      FRENGRIMFBF[i+1,j]  = max(0,(ENFEEDGROWTH[i+1,j]-FATCOMP[i,j])/(ENFEEDGROWTH[i+1,j]))
      if(INTRAMFTISBF[i,j]/INTRAMFTIS[i,j] < 1) 
        FRENGRIMFBF[i+1,j] <- FRENGRIMFBF[i+1,j] else FRENGRIMFBF[i+1,j] <- 0
      # NE for growth allocated to the intramuscular fat tissue (MJ day-1)
      ENGRIMFBF[i+1,j]    = FRENGRIMFBF[i+1,j]* (ENGRIMF[i+1,j]/ENGRTOTALORIG[i+1,j]) * 
        (ENFEEDGROWTH[i+1,j]) * COMPGROWTH4[i,j] 
      ENGRIMFBF[i+1,j]    = max(ENGRIMFBF[i+1,j],0) # NE for growth cannot be negative
      
      # NE for growth allocated to the miscellaneous fat tissue (MJ day-1); balancing 
      # variable  
      ENGRFATBF[i+1,j]    = ENFEEDGROWTH[i+1,j]-ENGRNONCBF[i+1,j]-ENGRBONEBF[i+1,j]-
        ENGRMUSCLEBF[i+1,j]-ENGRIMFBF[i+1,j] 
      ENGRFATBF[i+1,j]    = max(ENGRFATBF[i+1,j],0) # NE for growth cannot be negative
      
      # Check on NE for growth (MJ day-1); positive values CHECKCOMP are wrong
      ENGRTOTALCOMP[i+1,j] = ENGRNONCBF[i+1,j] + ENGRBONEBF[i+1,j] + 
        ENGRMUSCLEBF[i+1,j] + ENGRIMFBF[i+1,j] + ENGRFATBF[i+1,j]     
      CHECKCOMP[i+1,j] = ENFEEDGROWTH[i+1,j] - ENGRTOTALCOMP[i+1,j]  
      
      # Heat production actual growth (13.9 MJ kg-1 for lipid, and 20.2 MJ kg-1 for protein)
      # Heat production related to growth of bone tissue (MJ day-1)
      HEATBONEACT[i,j]     = DERBONE[i,j]    * ENGRBONEBF[i+1,j]/ENGRBONE[i+1,j] * 
        (LIPIDFRACBONEBF[i,j] * (GELIPID/LIPIDEFF-GELIPID) + PROTFRACBONE * 
           (GEPROT/PROTEFF-GEPROT))  
      # Heat production related to growth of muscle tissue (MJ day-1)
      HEATMUSCLEACT[i,j]   = DERMUSCLE[i,j]  * ENGRMUSCLEBF[i+1,j]/ENGRMUSCLE[i+1,j] * 
        (LIPFRACMUSCLE * (GELIPID/LIPIDEFF-GELIPID) + PROTFRACMUSCLE * 
           (GEPROT/PROTEFF-GEPROT)) 
      # Heat production related to growth of intramuscular fat tissue (MJ day-1)
      HEATIMFACT[i,j]      = DERINTRAMF[i,j] * ENGRIMFBF[i+1,j]/ENGRIMF[i+1,j] * 
        (LIPFRACFAT * (GELIPID/LIPIDEFF-GELIPID) + PROTFRACFAT * (GEPROT/PROTEFF-GEPROT))
      # Heat production related to growth of miscellaneous fat tissue (MJ day-1)
      HEATMISCFATACT[i,j]  = DERMISCFAT[i,j] * ENGRFATBF[i+1,j]/ENGRFAT[i+1,j] * 
        (LIPFRACFAT * (GELIPID/LIPIDEFF-GELIPID) + PROTFRACFAT * (GEPROT/PROTEFF-GEPROT))
      # Heat production related to growth of non-carcass tissue (MJ day-1)
      HEATNONCACT[i,j]     = DERNONC[i,j]    * ENGRNONCBF[i+1,j]/ENGRNONC[i+1,j] * 
        (LIPIDFRACNONCBF[i,j] * (GELIPID/LIPIDEFF-GELIPID) + PROTFRACNONCBF[i,j] * 
           (GEPROT/PROTEFF-GEPROT)) 
      # Total heat production related to NE for growth (MJ day-1)  
      HEATTOTALACT[i,j]    = HEATBONEACT[i,j] + HEATMUSCLEACT[i,j] + HEATIMFACT[i,j] + 
        HEATMISCFATACT[i,j] + HEATNONCACT[i,j]   
      
      # Net energy (NE) requirements for growth (MJ day-1) 
      # 44.0 MJ kg-1 for protein, 53.7 MJ kg-1 for lipid        
      # NE requirements for growth of the bone tissue (MJ day-1)
      ENBONEACT[i,j]     = DERBONE[i,j]    * ENGRBONEBF[i+1,j]/ENGRBONE[i+1,j] * 
        (LIPIDFRACBONEBF[i,j] * GELIPID/LIPIDEFF + PROTFRACBONE * GEPROT/PROTEFF) 
      # NE requirements for growth of the muscle tissue (MJ day-1)
      ENMUSCLEACT[i,j]   = DERMUSCLE[i,j]  * ENGRMUSCLEBF[i+1,j]/ENGRMUSCLE[i+1,j] * 
        (LIPFRACMUSCLE * GELIPID/LIPIDEFF + PROTFRACMUSCLE * GEPROT/PROTEFF) 
      # NE requirements for growth of the intramuscular fat tissue (MJ day-1)
      ENIMFACT[i,j]      = DERINTRAMF[i,j] * ENGRIMFBF[i+1,j]/ENGRIMF[i+1,j] * 
        (LIPFRACFAT * GELIPID/LIPIDEFF + PROTFRACFAT * GEPROT/PROTEFF)
      # NE requirements for growth of the miscellaneous fat tissue (MJ day-1)
      ENMISCFATACT[i,j]  = DERMISCFAT[i,j] * ENGRFATBF[i+1,j]/ENGRFAT[i+1,j] * 
        (LIPFRACFAT * GELIPID/LIPIDEFF + PROTFRACFAT * GEPROT/PROTEFF)
      # NE requirements for growth of the non-carcass tissue (MJ day-1)
      ENNONCACT[i,j]     = DERNONC[i,j]    * ENGRNONCBF[i+1,j]/ENGRNONC[i+1,j] * 
        (LIPIDFRACNONCBF[i,j] * GELIPID/LIPIDEFF + PROTFRACNONCBF[i,j] * GEPROT/PROTEFF)
      # Total NE requirements for growth of all tissues (MJ day-1)
      ENTOTALACT[i,j]    = ENBONEACT[i,j] + ENMUSCLEACT[i,j] + ENIMFACT[i,j] + 
        ENMISCFATACT[i,j] + ENNONCACT[i,j]   
      
      #######################################################################################
      
      # Protein requirements for growth
      # Protein use efficiency for growth is assumed to be 54% (23.8/44.0 = 0.54)
      # Protein requirement for growth of bone tissue (g day-1) 
      PROTBONEACT[i,j]    = DERBONE[i,j]    * ENGRBONEBF[i+1,j]/ENGRBONE[i+1,j] * 
        PROTFRACBONE / PROTEFF * 1000             
      # Protein requirement for growth of muscle tissue (g day-1)
      PROTMUSCLEACT[i,j]  = DERMUSCLE[i,j]  * ENGRMUSCLEBF[i+1,j]/ENGRMUSCLE[i+1,j] * 
        PROTFRACMUSCLE / PROTEFF * 1000    
      # Protein requirement for growth of intramuscular fat tissue (g day-1)
      PROTIMFACT[i,j]     = DERINTRAMF[i,j] * ENGRIMFBF[i+1,j]/ENGRIMF[i+1,j] * 
        PROTFRACFAT / PROTEFF * 1000              
      # Protein requirement for growth of intermuscular and subcutaneous fat tissue (g day-1)
      PROTMISCFATACT[i,j] = DERMISCFAT[i,j] * ENGRFATBF[i+1,j]/ENGRFAT[i+1,j] * 
        PROTFRACFAT / PROTEFF * 1000            
      # Protein requirement for growth of non-carcass tissue (g day-1)
      PROTNONCBF1[i,j]    = DERNONC[i,j]    * ENGRNONCBF[i+1,j]/ENGRNONC[i+1,j] * 
        PROTFRACNONCBF[i,j] / PROTEFF * 1000    
      # Total protein requirements for growth (g protein day-1)  
      PROTTOTALACT[i,j]   = PROTBONEACT[i,j] + PROTMUSCLEACT[i,j] + PROTIMFACT[i,j] + 
        PROTMISCFATACT[i,j] + PROTNONCBF1[i,j]                    
        
      # Total protein requirement (g protein day-1), excluding recycling of protein  
      PROTGROSS[i,j] <- PROTNONGM[i,j] + PROTTOTALACT[i,j] + 
        ENTOTALACT[i,j]*(Digestfracfeed[i,j]/(1-Digestfracfeed[i,j])) * PROTNE * NtoCP + 
        HEATTOTALACT[i,j] * PROTNE * NtoCP  
      
      # Digested protein not accreted in tissues (g day-1)  
      UREABL[i,j]    <- PROTGROSS[i,j] - PROTDERML[i,j] + PROTEFF*PROTTOTALACT[i,j] + 
        0.85*PROTGESTG[i,j] + PROTEFFMILK*PROTMILKG[i,j]    
      # Percentage of urea N recycled, percentage from N intake (%) (Russel et al, 1992)
      NRECYCLPT[i,j] <- NRECYCL1*SENSMAT[116,s] - NRECYCL2*SENSMAT[117,s]*(CPAVG[i,j]/10) + 
        NRECYCL3*SENSMAT[118,s]*(CPAVG[i,j]/10)^2              
      if(TIME[i,j]<=14) NRECYCLPT[i,j] <- 0 # No recycling when only milk is supplied 
      
      # Net protein requirement (g protein day-1), including recycling     
      PROTNETT[i,j]  <- PROTGROSS[i,j] - (NRECYCLPT[i,j]/100) * (CPAVG[i,j] * FEEDQNTY[i,j])  
      
      # Additional energy requirements under cold stress (MJ day-1)
      HEATIFEEDGROWTHC[i,j] <- HEATIFEEDGROWTHC[i,j] + max(0,(Metheatcold[i,j]-
                               HEATIFEEDMAINTWM[i,j])*(3600*24* AREA[i,j])/1000000 -                        
                               HEATTOTALACT[i,j] - ENTOTALACT[i,j]*
                               (Digestfracfeed[i,j]/(1-Digestfracfeed[i,j])))
      
    #########################################################################################
    #                                   ME to feed conversion                               #
    #########################################################################################
    
    # Metabolisable energy (ME) requirements (MJ per animal per day)
    # Only for feed, energy for calf (MEMILKCALFINIT) is subtracted 
    # The 0.134 can be defined more precisely: (1-NEIEFFGEST)*(45/75)
    MEREQTOTAL[i,j] = max(0,HEATIFEEDMAINT[i,j]-MEMILKCALFINIT[i,j]+(ENFEEDGROWTH[i+1,j]+
                         (0.134)*NEREQGEST[i,j]+(NEMILKCOW[i,j]-HEATMILK[i,j]))*
                           (1+(Digestfracfeed[i,j]/(1-Digestfracfeed[i,j])))  
                          + REDMAINT[i,j]/(Digestfracfeed[i,j]-0.10)*
                           (1-(Digestfracfeed[i,j]-0.10)))
    
    # Metabolisable energy (ME) requirements (MJ per day)  
    MEMET[i,j] <- HEATIFEEDMAINT[i,j]-MEMILKCALFINIT[i,j]+(ENFEEDGROWTH[i+1,j]+0.134*
                  NEREQGEST[i,j]+(NEMILKCOW[i,j]-HEATMILK[i,j]))*(1+(Digestfracfeed[i,j]/
                  (1-Digestfracfeed[i,j])))  
    # Reduction in ME (MJ per day)
    MERED[i,j] <- REDMAINT[i,j]/(Digestfracfeed[i,j]-0.10)*(1-(Digestfracfeed[i,j]-0.10))
    
    # Feed intake (kg DM per day)
    if(MEUPTAKE[i,j] == 0) FEEDINTAKE[i,j] <- 0 else 
      FEEDINTAKE[i,j] <- (MEREQTOTAL[i,j] / (Q[i,j]*GEFEED))/DETOME       
    # Fraction of the digestion capacity used (-)
    if(MEUPTAKE[i,j] == 0) FILLGIT[i,j] <- 0 else 
      FILLGIT[i,j] <- FEEDQNTY[i,j] / PHFEEDINTKG[i,j]       
    
    # Rumen/ digestion capacity classes as defined by Chilibroste et al. (1997)
    if(FILLGIT[i,j] > 0.85) PASSAGE1[i,j] <- 1 else         
      if(FILLGIT[i,j] <= 0.85 & FILLGIT[i,j] > 0.65) PASSAGE1[i,j] <- 2 else
        if(FILLGIT[i,j] <= 0.65 & FILLGIT[i,j] > 0.45) PASSAGE1[i,j] <- 3 else 
          PASSAGE1[i,j] <- 4    
    
    # Possible difference in digestion capacity class after calculation of FILLGIT
    PASSDIFF[i,j] <- max(0,PASSAGE1[i,j]-PASSAGE[i,j]) 
    if(TIME[i,j] > 15) PASSAGE[i,j] <- PASSAGE[i,j] + PASSDIFF[i,j]  

    REPS[i,j] <- REPS[i,j] + 1 # Times the integration loop has run 
    
    # Optimization statement in the integration loop (among the different sub-models)
    PROTBAL[i,j] <- PROTUPT[i,j]* (1+NRECYCLPT[i,j]/100) + 
      MILKSTARTPRHF[i,j] - PROTGROSS[i,j] 
    PROTREDFACT[i,j] <- 1- min(1,max(0,((PROTBAL[i,j]*-1)/(PROTGROSS[i,j]-PROTNONGM[i,j]))))
        
    # This statement indicates that heat production from growth (HEATTOTALACT plus HIF for 
    # ENTOTALACT) cannot exceed the maximum heat release (HEATIFEEDGROWTH) by more than 0.5 
    # MJ day-1       
    DIFFEN[i,j]        = HEATTOTALACT[i,j]+ENTOTALACT[i,j]*(Digestfracfeed[i,j]/
                         (1-Digestfracfeed[i,j])) - max(0,HEATIFEEDGROWTH[i,j]) 
    # If heat production exceeds the maximum heat release, feed intake is reduced via REDHP.
    # REDHP accounts for heat stress
    if(DIFFEN[i,j] >  0.5) REDHP[i,j] <- (REDHP[i,j]-0.1*DIFFEN[i,j]) else 
      REDHP[i,j] <- REDHP[i,j] 
         
    # If the passage rate is not correct, the integration loop does not change passage rate  
    # and feed intake at the same time  
    if(TIME[i,j] > 15 & PASSDIFF[i,j] != 0) REDHP[i,j] <- 0 
    
    # The loop has to run at least two times
    if(REPS[i,j] < 2)       CHECKHEAT3[i,j] <- "FALSE" else CHECKHEAT3[i,j] <- "CORRECT" 
    # Passage rate has to be correct before the loop terminates
    if(TIME[i,j] > 15 && PASSDIFF[i,j] != 0) CHECKHEAT3[i,j] <- "FALSE" 
    
    # Heat production cannot exceed maximum heat release before the loop terminates     
    if(DIFFEN[i,j] > 0.5)    CHECKHEAT3[i,j] <- "FALSE" 
    
    # If all conditions are met, the integration loop terminates
    if(CHECKHEAT3[i,j] == "CORRECT") {break} 
    
    #########################################################################################    
    
    } # End of the integration loop
      
    # Fraction physically effective neutral detergent fibre (peNDF) in the diet (-)
    # Note: diets must contain sufficient peNDF to avoid unrealistic simulations where rumen 
    # functioning cannot be sustained.
    PENDF[i,j]     <- FRACFEED1[i,j]*FEED1[i,14]*FEED1[i,15] + 
                      FRACFEED2[i,j]*FEED2[i,14]*FEED2[i,15] +      
                      FRACFEED3[i,j]*FEED3[i,14]*FEED3[i,15] + 
                      FRACFEED4[i,j]*FEED4[14]*FEED4[15]
    
    # Tissue weights are corrected for protein deficiency (PROTREDFACT)
    
    # Weight of lipids in bone tissue (kg per animal)
    LIPIDBONEBF[i+1,j]     = LIPIDBONEBF[i,j] + DERBONE[i,j]* ENGRBONEBF[i+1,j]/
      ENGRBONE[i+1,j] * LIPIDFRACBONEBF[i,j] * PROTREDFACT[i,j]
    # Weight of lipids in the non-carcass tissue (kg per animal)
    LIPIDNONCBF[i+1,j]     = LIPIDNONCBF[i,j] + DERNONC[i,j]* ENGRNONCBF[i+1,j]/
      ENGRNONC[i+1,j] * LIPIDFRACNONCBF[i,j] * PROTREDFACT[i,j]
    # Weight of protein in the non-carcass tissue (kg per animal)
    PROTNONCBF[i+1,j]      = PROTNONCBF[i,j] + DERNONC[i,j]* ENGRNONCBF[i+1,j]/
      ENGRNONC[i+1,j] * PROTFRACNONCBF[i,j] * PROTREDFACT[i,j]
    
    # Bone tissue (kg per animal) 
    BONETISBF[i+1,j]    = BONETISBF[i,j] + DERBONE[i,j] * 
      ENGRBONEBF[i+1,j]/ENGRBONE[i+1,j] * PROTREDFACT[i,j] 
    # Muscle tissue (kg per animal) 
    MUSCLETISBF[i+1,j]  = MUSCLETISBF[i,j] + DERMUSCLE[i,j]  * 
      ENGRMUSCLEBF[i+1,j]/ENGRMUSCLE[i+1,j] * PROTREDFACT[i,j]
    # Intramuscular fat tissue (kg per animal) 
    INTRAMFTISBF[i+1,j] = INTRAMFTISBF[i,j] + DERINTRAMF[i,j] * 
      ENGRIMFBF[i+1,j]/ENGRIMF[i+1,j] * PROTREDFACT[i,j]
    # Subcutaneous and intermuscular fat tissue (kg per animal) 
    MISCFATTISBF[i+1,j] = MISCFATTISBF[i,j] + DERMISCFAT[i,j] * 
      ENGRFATBF[i+1,j]/ENGRFAT[i+1,j] * PROTREDFACT[i,j]
    # Non-carcass tissue (kg per animal) 
    NONCARCTISBF[i+1,j] = NONCARCTISBF[i,j] + DERNONC[i,j] * 
      ENGRNONCBF[i+1,j]/ENGRNONC[i+1,j] * PROTREDFACT[i,j]
    
    # Gross energy content of the non-carcass tissue (MJ kg-1)    
    ENCONTENTNONCBF[i,j]  = (LIPIDNONCBF[i,j] * GELIPID + PROTNONCBF[i,j] * GEPROT) / 
      NONCARCTISBF[i,j]     
      
    # Muscle tissue is dissimilated when protein supply is below maintenance
    # Protein dissimilation: efficiency of 90% assumed
    # Reduction in muscle tissue (kg day-1)
    REDTISPROT[i,j] <- min(0, PROTGROSS[i,j]+PROTBAL[i,j]) / 
      (PROTFRACMUSCLE* DISSEFF * 1000) 
    MUSCLETISBF[i+1,j] <- MUSCLETISBF[i+1,j] + REDTISPROT[i,j]     
  
    # REDTIS1 Heat stress: reduction in feed intake heat release in under sub-maintenance 
    # intake
    # Weight loss due to heat stress, in (kg fat per day), 29.624 MJ kg-1 is the 
    # energy content of fat tissue
    # inefficiency fat dissimilation = 10%; i.e. 90% efficiency
    REDTIS[i,j] = REDMAINT[i,j]/(Digestfracfeed[i,j]-(1-DISSEFF))*
      (1-(Digestfracfeed[i,j]-(1-DISSEFF)))/GEFATTIS 
    REDTIS[is.nan(REDTIS)] <- 0            
    
    # Fat tissue (cumulative energy) dissimulated due to heat stress (MJ)  
    HEATBURNCUMUL[i,j] = sum(REDTIS[1:i,j])*GEFATTIS 
    
    # To avoid heat stress, subcutaneous and intermuscular fat are dissimilated (kg day-1)
    # (and feed intake is reduced)  
    MISCFATTISBF[i+1,j] <- MISCFATTISBF[i+1,j] + REDTIS[i,j] * 0.9  
    # To avoid heat stress, non carcass tissue is dissimilated (kg day-1)
    NONCARCTISBF[i+1,j] <- NONCARCTISBF[i+1,j] + REDTIS[i,j] * 0.1 * 
      (GEFATTIS/ENCONTENTNONCBF[i,j])/DISSEFF   
    
    # Check to ensure feed intake is not negative
    if((NEMAINT[i,j]+NEPHYSACT[i,j]+NERESP[i,j])*1/DISSEFF < -1*REDTIS[i,j]) 
      CHECK[i,j] <- "wrong" else CHECK[i,j] <-"good"  
    
    #########################################################################################  
    
    # REDTIS2 Negative growth, fat tissue is dissimilated (kg day-1)
    # This equation is similar to Eq. 47 of the Supplementary Information
    if(REDTIS[i,j] == 0 && ENFEEDGROWTH[i+1,j] < 0) 
      REDTIS2[i,j] <- (-ENFEEDGROWTH[i+1,j]/GEFATTIS)/DISSEFF else REDTIS2[i,j] <- 0
  
    # To correct for negative growth, subcutaneous and intermuscular fat tissue are 
    # dissimilated (kg day-1) 
    MISCFATTISBF[i+1,j] <- MISCFATTISBF[i+1,j] - REDTIS2[i,j] * 0.9  
    NONCARCTISBF[i+1,j] <- NONCARCTISBF[i+1,j] - REDTIS2[i,j] * 0.1 * 
      (GEFATTIS/ENCONTENTNONCBF[i,j])/DISSEFF 
 
    # REDTIS3 Cold stress    
    # Cumulative energy required to maintain body temperature (MJ) 
    FATBURNCUMUL[i+1,j] = FATBURNCUMUL[i,j] + HEATIFEEDGROWTHC[i,j] 
    
    # Total body weight (TBW) based on the genotype, climate (heat stress; cold stress), feed
    # quality, and available feed quantity (kg live weight)
    TBWBF[i+1,j]        = (BONETISBF[i+1,j] + MUSCLETISBF[i+1,j] + INTRAMFTISBF[i+1,j] + 
                             MISCFATTISBF[i+1,j] + NONCARCTISBF[i+1,j])/(1-RUMENFRAC) 
    # Metabolic body weight (TBW) based on the genotype, climate (heat stress; cold stress), 
    # feed quality, and available feed quantity (kg live weight) 
    EBWBFMET[i+1,j]     <- (TBWBF[i+1,j]*(1-RUMENFRAC))^0.75 
      
    # Protein accretion in body tissues and milk (g day-1)
    PROTACCR[i,j] <- PROTGESTG[i,j]*0.5 + PROTMILK[i,j] + PROTTOTALACT[i,j] 
    # Fraction metabolisable energy from feed used for maintenance (-)
    MAINTFRAC[i,j] = (HEATIFEEDMAINT[i,j]-HEATIFEEDGROWTHC[i,j]+MEMILKCALFINIT[i,j])/
      MEREQTOTAL[i,j] 
      
    #########################################################################################
    #         Culling and slaughtering cattle         #
    ###################################################
    
    # Cattle slaughtered (cattle from reproductive herd)
    FATFRACCARC[i,j] = (MISCFATTISBF[i,j]+INTRAMFTISBF[i,j])/(MISCFATTISBF[i,j]+
                       INTRAMFTISBF[i,j]+MUSCLETISBF[i,j]+BONETISBF[i,j])
      
    # Maximum number of calves per animal
    CALVESPERANIMAL <- REPRODUCTIVE * MAXCALFNR 
    
    # Beef production (kg per animal) 
    # (Beef is deboned carcass) 
    # Option 1: fat content reaches a specific level for cows, and CALFLIVENR should be met 
    # for cows. This equation contains Eq. 38 of the Supplementary Information
    if(TBWBF[i+1,j] > SWMALES && SEX[j] == 0) 
      BEEFPRODACT[i,j] <- (MUSCLETISBF[i,j] + INTRAMFTISBF[i,j] + MISCFATTISBF[i,j]) else
    if(TBWBF[i+1,j] > SWFEMALES && SEX[j] == 1 && REPRODUCTIVE[j] == 0) 
      BEEFPRODACT[i,j] <- (MUSCLETISBF[i,j] + INTRAMFTISBF[i,j] + MISCFATTISBF[i,j]) else
    if(SEX[j] == 1  && FATFRACCARC[i,j] > MAXFATCARC & CALFWEANNR[i,j] == 
       CALVESPERANIMAL[j] & TIME[i,j] > 800) 
    {BEEFPRODACT[i,j] <- (MUSCLETISBF[i,j] + INTRAMFTISBF[i,j] + MISCFATTISBF[i,j])} else 
        {BEEFPRODACT[i,j] = 0}
      
    # Option 2: maximum number of years in (re)productive herd
    if(TIME[i,j]/365 > MAXLIFETIME & BEEFPRODACT[i,j] == 0) 
      BEEFPRODACT[i,j] <- (MUSCLETISBF[i,j] + INTRAMFTISBF[i,j] + MISCFATTISBF[i,j]) 
    if(REPRODUCTIVE[j] == 1 & CALFWEANNR[i,j] == CALVESPERANIMAL[j] & TIME[i,j]/365 > 
       MAXLIFETIME) 
      BEEFPRODACT[i,j] <- (MUSCLETISBF[i,j] + INTRAMFTISBF[i,j] + MISCFATTISBF[i,j]) 
      
    # Option 3: cattle death
    # If fat reserves are fully depleted, there is no beef production
    if(MISCFATTISBF[i,j] < 0) BEEFPRODACT[i,j] <- -1  
    if(BEEFPRODACT[i,j]!=0)  SLAUGHTERDAYACT[i,j] <- TIME[i,j]  else 
      SLAUGHTERDAYACT[i,j] <- 9999
    
    #########################################################################################
    
    # Live weight production (kg total body weight per animal)
    # Option 1: fat content reaches a specific level for cows, and CALFLIVENR should be met 
    # for cows  
    if(TBWBF[i+1,j] > SWMALES && SEX[j] == 0) LWPRODACT[i,j] <- TBWBF[i+1,j] else
    if(TBWBF[i+1,j] > SWFEMALES && SEX[j] == 1 && REPRODUCTIVE[j] == 0) 
      LWPRODACT[i,j] <- TBWBF[i+1,j] else
    if(SEX[j] == 1  && FATFRACCARC[i,j] > MAXFATCARC & CALFWEANNR[i,j] == 
       CALVESPERANIMAL[j] & TIME[i,j] > 800) LWPRODACT[i,j] <- TBWBF[i+1,j] else 
         LWPRODACT[i,j] = 0
    
    # Option 2: maximum number of years in (re)productive herd
    if(TIME[i,j]/365 > MAXLIFETIME & BEEFPRODACT[i,j] == 0) LWPRODACT[i,j] <- TBWBF[i+1,j] 
    if(REPRODUCTIVE[j] == 1 & CALFWEANNR[i,j] == CALVESPERANIMAL[j] & TIME[i,j]/365 > 
       MAXLIFETIME) LWPRODACT[i,j] <- TBWBF[i+1,j] 
    
    # Option 3: cattle death
    # If fat reserves are fully depleted, there is no live weight production
    if(MISCFATTISBF[i,j] < 0) LWPRODACT[i,j] <- -1 
    
    #########################################################################################
    
    # Carcass production (kg)
    # Option 1: fat content reaches a specific level for cows, and CALFLIVENR should be met 
    # for cows  
    if(TBWBF[i+1,j] > SWMALES && SEX[j] == 0) CARCPRODACT[i,j] <- (MUSCLETISBF[i,j] + 
    INTRAMFTISBF[i,j] + MISCFATTISBF[i,j]  + BONETISBF[i,j]) else
    if(TBWBF[i+1,j] > SWFEMALES && SEX[j] == 1 && REPRODUCTIVE[j] == 0) 
      CARCPRODACT[i,j] <- (MUSCLETISBF[i,j] + INTRAMFTISBF[i,j] + MISCFATTISBF[i,j] + 
                             BONETISBF[i,j]) else if(SEX[j] == 1  && FATFRACCARC[i,j] > 
                           MAXFATCARC & CALFWEANNR[i,j] == CALVESPERANIMAL[j] & 
                           TIME[i,j] > 800) 
      CARCPRODACT[i,j] <- (MUSCLETISBF[i,j] + INTRAMFTISBF[i,j] + MISCFATTISBF[i,j] + 
                             BONETISBF[i,j]) else CARCPRODACT[i,j] = 0
    
    # Option 2: maximum number of years in (re)productive herd
    if(TIME[i,j]/365 > MAXLIFETIME & BEEFPRODACT[i,j] == 0) 
      CARCPRODACT[i,j] <- (MUSCLETISBF[i,j] + INTRAMFTISBF[i,j] + MISCFATTISBF[i,j] + 
                             BONETISBF[i,j]) 
    if(REPRODUCTIVE[j] == 1 & CALFWEANNR[i,j] == CALVESPERANIMAL[j] & TIME[i,j]/365 >
       MAXLIFETIME) CARCPRODACT[i,j] <- (MUSCLETISBF[i,j] + INTRAMFTISBF[i,j] + 
                                           MISCFATTISBF[i,j] + BONETISBF[i,j]) 
    
    # Option 3: cattle death
    # If fat reserves are fully depleted, there is no carcass production
    if(MISCFATTISBF[i,j] < 0) BEEFPRODACT[i,j] <- -1   
    
    #########################################################################################
      
    # Day the animal is slaughtered (days)
    # Is initially 9999, but is replaced by the correct number of days at slaughter
    ENDDAY[j] <- min(SLAUGHTERDAYACT[1:i,j]) 
      
    # Beef production at slaughter (kg per animal)
    if(ENDDAY[j] < 9999) BEEFPROD[j] <- BEEFPRODACT[ENDDAY[j],j] 
    # Beef production (kg beef per head per year)
    if(ENDDAY[j] < 9999) BEEFPRODYEAR[j] <- BEEFPRODACT[ENDDAY[j],j]/(TIME[ENDDAY[j],j]/365) 
    # Live weight production at slaughter (kg per animal)
    if(ENDDAY[j] < 9999) LWPROD[j] <- LWPRODACT[ENDDAY[j],j] 
    # Live weight production (kg live weight per head per year)
    if(ENDDAY[j] < 9999) LWPRODYEAR[j] <- LWPRODACT[ENDDAY[j],j]/(TIME[ENDDAY[j],j]/365) 
      
    # If the cow/bull is slaughtered, the weight is set to 0 via the vector ALIVE.
    # ALIVE produces a vector that indicates whether the cow/bull is alive or slaughtered
    # (1= true, 0=not true)
    if(ENDDAY[j] == 9999) ALIVE[i+1,j] <- 1 else ALIVE[i+1,j] <- 0     
      
    # The code below keeps track of the parity of the reproductive cow
    # Code indicates whether a cow is in or has had the nth parity (1= true, 0=not true).
    if(CALFLIVENR[i,j] >= 1) PARITY1[i,j] = 1 else PARITY1[i,j] = 0  
    if(CALFLIVENR[i,j] >= 2) PARITY2[i,j] = 1 else PARITY2[i,j] = 0 
    if(CALFLIVENR[i,j] >= 3) PARITY3[i,j] = 1 else PARITY3[i,j] = 0 
    if(CALFLIVENR[i,j] >= 4) PARITY4[i,j] = 1 else PARITY4[i,j] = 0 
    if(CALFLIVENR[i,j] >= 5) PARITY5[i,j] = 1 else PARITY5[i,j] = 0 
    if(CALFLIVENR[i,j] >= 6) PARITY6[i,j] = 1 else PARITY6[i,j] = 0 
    if(CALFLIVENR[i,j] >= 7) PARITY7[i,j] = 1 else PARITY7[i,j] = 0 
    if(CALFLIVENR[i,j] >= 8) PARITY8[i,j] = 1 else PARITY8[i,j] = 0 
    if(CALFLIVENR[i,j] >= 9) PARITY9[i,j] = 1 else PARITY9[i,j] = 0 
    
    # Birth days of calves 1-9 (days after birth of the reproductive cow)
    if(CALFLIVENR[i,j] == 1) BIRTHDAYCALF1 <- TIME[i]-sum(PARITY1[1:i,j]) else 
      BIRTHDAYCALF1 <- BIRTHDAYCALF1 # Birth day calf 1
    if(CALFLIVENR[i,j] == 2) BIRTHDAYCALF2 <- TIME[i]-sum(PARITY2[1:i,j]) else 
      BIRTHDAYCALF2 <- BIRTHDAYCALF2 # Birth day calf 2 
    if(CALFLIVENR[i,j] == 3) BIRTHDAYCALF3 <- TIME[i]-sum(PARITY3[1:i,j]) else 
      BIRTHDAYCALF3 <- BIRTHDAYCALF3 # Birth day calf 3
    if(CALFLIVENR[i,j] == 4) BIRTHDAYCALF4 <- TIME[i]-sum(PARITY4[1:i,j]) else 
      BIRTHDAYCALF4 <- BIRTHDAYCALF4 # Birth day calf 4
    if(CALFLIVENR[i,j] == 5) BIRTHDAYCALF5 <- TIME[i]-sum(PARITY5[1:i,j]) else 
      BIRTHDAYCALF5 <- BIRTHDAYCALF5 # Birth day calf 5
    if(CALFLIVENR[i,j] == 6) BIRTHDAYCALF6 <- TIME[i]-sum(PARITY6[1:i,j]) else 
      BIRTHDAYCALF6 <- BIRTHDAYCALF6 # Birth day calf 6
    if(CALFLIVENR[i,j] == 7) BIRTHDAYCALF7 <- TIME[i]-sum(PARITY7[1:i,j]) else 
      BIRTHDAYCALF7 <- BIRTHDAYCALF7 # Birth day calf 7
    if(CALFLIVENR[i,j] == 8) BIRTHDAYCALF8 <- TIME[i]-sum(PARITY8[1:i,j]) else 
      BIRTHDAYCALF8 <- BIRTHDAYCALF8 # Birth day calf 8 
    if(CALFLIVENR[i,j] == 9) BIRTHDAYCALF9 <- TIME[i]-sum(PARITY9[1:i,j]) else 
      BIRTHDAYCALF9 <- BIRTHDAYCALF9 # Birth day calf 9 
  
    #########################################################################################
    
    # Average fraction of the diet digested (-)
    AVGDIGFRAC[i,j] = FRACFEED1[i,j]*FEED1[i,1] + FRACFEED2[i,j]*FEED2[i,1] + 
      FRACFEED3[i,j]*FEED3[i,1] + FRACFEED4[i,j]*FEED4[1]
      
    # Cumulative feed intake (kg DM per animal per day)
    CUMULFEED1[i,j] = sum(FEED1QNTY[1:i,j]) # cumulative amount of  feed type 1  
    CUMULFEED2[i,j] = sum(FEED2QNTY[1:i,j]) # cumulative amount of  feed type 2
    CUMULFEED3[i,j] = sum(FEED3QNTY[1:i,j]) # cumulative amount of  feed type 3
    CUMULFEED4[i,j] = sum(FEED4QNTY[1:i,j]) # cumulative amount of  feed type 4
      
    # Cumulative kg feed intake (whole diet) (kg DM per animal per day) 
    CUMULFEED[i,j]  = sum(CUMULFEED1[i,j]+ CUMULFEED2[i,j] + CUMULFEED3[i,j] + 
                            CUMULFEED4[i,j]) 
      
    # Feed conversion ratio (FCR; kg feed per kg live weight)
    FCR[i,j] = CUMULFEED[i,j]/(TBWBF[i,j]-TBWBF[1,j])  
    # Feed conversion ratio (kg feed per kg beef)  
    FCRBEEF[i,j] = CUMULFEED[i,j]/((MUSCLETISBF[i+1,j] + INTRAMFTISBF[i+1,j] + 
                  MISCFATTISBF[i+1,j])-(MUSCLETISBF[1,j] + INTRAMFTISBF[1,j] + 
                  MISCFATTISBF[1,j]))       
    # Feed conversion ratio (kg feed per kg beef at slaughter)  
    if(ENDDAY[j] < 9999) FCRBEEFENDDAY[j] <- CUMULFEED[ENDDAY[j]]/BEEFPROD[ENDDAY[j]] 
    # Percentage feed intake relative to the total body weight (%)  
    PERCFI[i,j] <- FEEDQNTY[i,j]/TBWBF[i,j]*100 
      
    # Simulating one animal is sufficient for simulations at the animal level.  
    # The concept of the herd unit is used to simulate beef cattle at the herd level,
    # where multiple animals are simulated (one reproductive cow and her offspring, minus a
    # replacement heifer)
    
    # If the animal is slaughtered, the time loop for the animal is terminated
    if(SLAUGHTERDAYACT[i,j] < 9000) {breakFlagtime <- TRUE 
                                     break}
    #########################################################################################
    
    # } for the time loop
  }
  
  # Shift the weather files of the calves, based on their birthday
  WEATHERCALF1 <- WEATHERORIG[BIRTHDAYCALF1:(imax[j]+BIRTHDAYCALF1+2),]
  WEATHERCALF2 <- WEATHERORIG[BIRTHDAYCALF2:(imax[j]+BIRTHDAYCALF2+2),]
  WEATHERCALF3 <- WEATHERORIG[BIRTHDAYCALF3:(imax[j]+BIRTHDAYCALF3+2),]
  WEATHERCALF4 <- WEATHERORIG[BIRTHDAYCALF4:(imax[j]+BIRTHDAYCALF4+2),]
  WEATHERCALF5 <- WEATHERORIG[BIRTHDAYCALF5:(imax[j]+BIRTHDAYCALF5+2),]
  WEATHERCALF6 <- WEATHERORIG[BIRTHDAYCALF6:(imax[j]+BIRTHDAYCALF6+2),]
  WEATHERCALF7 <- WEATHERORIG[BIRTHDAYCALF7:(imax[j]+BIRTHDAYCALF7+2),]
  WEATHERCALF8 <- WEATHERORIG[BIRTHDAYCALF8:(imax[j]+BIRTHDAYCALF8+2),]
  WEATHERCALF9 <- WEATHERORIG[BIRTHDAYCALF9:(imax[j]+BIRTHDAYCALF9+2),]
  
  # Selects the right weather file for each calf 
  if(ORDER[j] == 0) {WEATHER <- WEATHERCALF1}
  if(ORDER[j] == 1) {WEATHER <- WEATHERCALF2}
  if(ORDER[j] == 2) {WEATHER <- WEATHERCALF3}
  if(ORDER[j] == 3) {WEATHER <- WEATHERCALF4}
  if(ORDER[j] == 4) {WEATHER <- WEATHERCALF5}
  if(ORDER[j] == 5) {WEATHER <- WEATHERCALF6}
  if(ORDER[j] == 6) {WEATHER <- WEATHERCALF7}
  if(ORDER[j] == 7) {WEATHER <- WEATHERCALF8}
  if(ORDER[j] == 8) {WEATHER <- WEATHERCALF9}

  ###########################################################################################
  
  # Data on beef production and feed intake 
  
  # Beef production (kg per head)
  BEEFPRODHERD[j]                   <- c(BEEFPRODACT[ENDDAY[j],j]) 
  # Live weight production (kg per head)
  LWPRODHERD[j]                     <- c(LWPRODACT[ENDDAY[j],j])   
  # Feed conversion ratio (kg DM feed per kg beef)
  FCRHERDBEEF[j]                    <- c(FCRBEEF[ENDDAY[j],j])     
  # Cumulative feed intake whole life span (kg DM)
  CUMULFEEDHERD[j]                  <- c(CUMULFEED[ENDDAY[j],j])   
  # Cumulative feed type 1 intake whole life span (kg DM)
  CUMULFEED1HERD[j]                 <- c(CUMULFEED1[ENDDAY[j],j])  
  # Cumulative feed type 2 intake whole life span (kg DM)
  CUMULFEED2HERD[j]                 <- c(CUMULFEED2[ENDDAY[j],j])  
  # Cumulative feed type 3 intake whole life span (kg DM)
  CUMULFEED3HERD[j]                 <- c(CUMULFEED3[ENDDAY[j],j])  
  # Cumulative feed type 4 intake whole life span (kg DM)
  CUMULFEED4HERD[j]                 <- c(CUMULFEED4[ENDDAY[j],j])  
  
  # Life span of the animal (years)
  ANIMALYEARS[j]                    <- ENDDAY[j]/365                 
  # Average weight (kg total body weight)
  AVANWEIGHT[j]                     <- mean(TBWBF[1:ENDDAY[j],j])    
  # Average metabolic weight (kg empty body weight^0.75)       
  AVANMETWEIGHT[j]                  <- mean(EBWBFMET[1:ENDDAY[j],j]) 
  
  # Vector with birthdays
  # The reproductive cow in a herd unit is born at day 0
  BIRTHDAY <- c(0,BIRTHDAYCALF1,BIRTHDAYCALF2,BIRTHDAYCALF3,BIRTHDAYCALF4,BIRTHDAYCALF5,
                BIRTHDAYCALF6,BIRTHDAYCALF7,BIRTHDAYCALF8) 
  # Weaning time for calves (days after birth of the reproductive cow)
  WNDAY  <- BIRTHDAY+WEANINGTIME 
  WNDAY[1] <- ENDDAY[1]
    
  # The vector ANIMALINFO lists key information on animal performance 
  ANIMALINFO <- cbind(REPRODUCTIVE[j], REPLACEMENT[j], PRODUCTIVE[j], SEX[j], 
                      BEEFPRODHERD[j], CUMULFEEDHERD[j], FCRHERDBEEF[j], ANIMALYEARS[j], 
                      AVANWEIGHT[j], AVANMETWEIGHT[j], ENDDAY[j], BIRTHDAY[j], WNDAY[j], 
                      LWPRODHERD[j], CUMULFEED1HERD[j], CUMULFEED2HERD[j], CUMULFEED3HERD[j], 
                      CUMULFEED4HERD[j])
  
  # Information for individual animals is added to information for other animals in the
  # herd unit
  HERDINFO   <- rbind(HERDINFO,ANIMALINFO) 
  
  # Runs are terminated before when the maximum number of calves per cow is reached.
  # This saves processing time.  
  if(j == MAXCALFNR+1) {breakFlaganim <- TRUE         
                        break}                        
    
  } # } for the animal loop
  
  ###########################################################################################
  #         Upscaling to the herd level         #
  ############################################### 
  
  HERDINFO1   <- rbind(HERDINFO, matrix(nrow=0, ncol=ncol(HERDINFO), data=0))
  
  # Culling of reproductive cows, vector with probabilities for survival
  # Culling starts after weaning the first calf
  AZZAMCUMCORR <- c(CULL, (1-CULL)-(1-CULL)^2, (1-CULL)^2-(1-CULL)^3, (1-CULL)^3-(1-CULL)^4, 
                    (1-CULL)^4-(1-CULL)^5, (1-CULL)^5-(1-CULL)^6, (1-CULL)^6-(1-CULL)^7, 
                    (1-CULL)^7-(1-CULL)^8)
  
  # Calculate chance that a cow is still in the herd after weaning a calf
  AA <- (WNDAY[WNDAY >0]/365)-(GestPer+WEANINGTIME)/365 # Moment of conception (year)
  BB <- floor(AA)+1                                     # Moment of conception rounded (year)
  BB[1] <- 1
  
  # Vector with probabilities for survival
  # The probability for giving birth to the first calf (replacement) in a herd unit equals 1
  CC <- AZZAMCUMCORR
  CC[length(CC)] <- 1-sum(CC[1:length(CC)-1]) 
  
  # Specification of key metrics for the reproductive cow over her total life span  
  # (accounting for culling)
  REPRBEEF     = NULL # Beef production (kg)
  REPRLW       = NULL # Live weight (kg)
  REPRFEED     = NULL # Feed intake (kg DM)
  REPRFEED1    = NULL # Intake feed type 1 (kg DM)
  REPRFEED2    = NULL # Intake feed type 2 (kg DM)
  REPRFEED3    = NULL # Intake feed type 3 (kg DM)
  REPRFEED4    = NULL # Intake feed type 4 (kg DM)
  
  REPRFCR      = NULL # Feed conversion ratio (kg DM feed per kg live weight)
  REPRAVANW    = NULL # Average total body weight (kg live weight)
  REPRAVANWMET = NULL # Average metabolic body weight (kg0.75 empty body weight)
  
  # Calculates the beef production and feed intake for different culling scenarios for the 
  # cow
  
  for(p in 1:length(AA-8+MAXCALFNR)){
    # Beef production (kg)
    REPRBEEF[p]     <- MUSCLETISBF[WNDAY[p],1] + INTRAMFTISBF[WNDAY[p],1] + 
      MISCFATTISBF[WNDAY[p],1]
    # Live weight (kg)
    REPRLW[p]       <- TBWBF[WNDAY[p],1]
    # Feed intake (kg DM)
    REPRFEED[p]     <- CUMULFEED[WNDAY[p],1]
    # Intake feed type 1 (kg DM)
    REPRFEED1[p]    <- CUMULFEED1[WNDAY[p],1]
    # Intake feed type 2 (kg DM)
    REPRFEED2[p]    <- CUMULFEED2[WNDAY[p],1]
    # Intake feed type 3 (kg DM)
    REPRFEED3[p]    <- CUMULFEED3[WNDAY[p],1]
    # Intake feed type 4 (kg DM)
    REPRFEED4[p]    <- CUMULFEED4[WNDAY[p],1]
    # Feed conversion ratio (kg DM feed per kg live weight)
    REPRFCR[p]      <- FCR[WNDAY[p],1]
    # Average total body weight (kg live weight)
    REPRAVANW[p]    <- mean(TBWBF[1:WNDAY[p],1])
    # Average metabolic body weight (kg0.75 empty body weight)
    REPRAVANWMET[p] <- mean(EBWBFMET[1:WNDAY[p],1])
  }

  CC1 <- CC # CC is duplicated
  
  # Probabilities for the scenarios
  CC1 <- c(0,CC1[1:8])
  CC1 <- c(CC1,rep(0,(9-length(CC1))))
  
  # Vector REPRINFO lists key information on the reproductive cow in a herd unit
  REPRINFO  <- matrix(nrow=(length(AA)), ncol = 18, 
                      data= c(REPRODUCTIVE[1:length(AA)], REPLACEMENT[1:length(AA)], 
                              PRODUCTIVE[1:length(AA)],SEX[1:length(AA)],REPRBEEF,REPRFEED,
                              REPRFCR, WNDAY[1:length(AA)]/365, REPRAVANW,REPRAVANWMET,
                              WNDAY[1:length(AA)], REPRLW, CC1, CC1, REPRFEED1, REPRFEED2, 
                              REPRFEED3, REPRFEED4))
  
  # Multiplies production and feed intake with probabilities (add up to a probability of 1)
  REPRINFO1 <- REPRINFO * CC1  
  # Lists production and feed intake of the cow, including the culling probabilities
  REPRINFO2 <- colSums(REPRINFO1) 
  
  # works only at a MAXCALFNR equal to or higher than 3! (Otherwise warnings appear)
  if(MAXCALFNR == 0) PRODINFO <- rep(0,18) else 
    if(MAXCALFNR < 3) PRODINFO <- HERDINFO1[3,] else PRODINFO  <- HERDINFO1[3:(MAXCALFNR+1),] 
  
  # Probabilities for all animals in a herd unit to exist
  DD <- c(1,1,(1-(CC[1])),(1-(sum(CC[1:2]))),(1-(sum(CC[1:3]))),(1-(sum(CC[1:4]))),
          (1-(sum(CC[1:5]))),(1-(sum(CC[1:6]))), (1-(sum(CC[1:7]))),(1-(sum(CC[1:8]))),
          (1-(sum(CC[1:9])))) 
  
  # Multiply production and feed intake of calves not used for replacement with probabilities  
  if(MAXCALFNR < 3) PRODINFO1 <- PRODINFO else PRODINFO1 <- PRODINFO * DD[3:(MAXCALFNR+1)] 
  # Lists production and feed intake for calves not used for replacement, including the 
  # culling probabilities
  if(MAXCALFNR < 3) PRODINFO2 <- PRODINFO else PRODINFO2 <- colSums(PRODINFO1) 
  
  # The vectors below list the information for the herd unit
  HERDINFO2 <- colSums(rbind(REPRINFO2, PRODINFO2))
  
  OUTPUTHERD <- rbind(REPRINFO2, PRODINFO2, HERDINFO2)
  OUTPUT1 <- cbind(Metheatopt, TNRESP, ACTSW, LWRCOAT, CONVCOAT, SWR, TskinC, TcoatC, TAVGC, 
                   MetheatBAL)
  OUTPUT2 <- cbind(HERDINFO2[5], HERDINFO2[6], HERDINFO2[5]/HERDINFO2[6], 
                   HERDINFO2[6]/HERDINFO2[5])
  
  # Matrix with key information for one herd unit
  OUTPUTHERDS  <- rbind(OUTPUTHERDS,OUTPUTHERD) 
  
  # Feed efficiency (g beef kg DM intake) for the calves in a herd unit
  FESENSIND[s] <- ANIMALINFO[5]/ANIMALINFO[6]*1000        
  # Feed efficiency (g beef kg DM intake) for cow in a herd unit
  FESENSREPR[s] <- OUTPUTHERDS[1,5]/OUTPUTHERDS[1,6]*1000 
  # Feed efficiency (g beef kg DM intake) for the herd unit
  FESENSHERD[s] <- OUTPUTHERDS[3,5]/OUTPUTHERDS[3,6]*1000 

  ###########################################################################################
  
  } # End of the s-loop for sensitivity analysis
 
 # Vector COLNAMES indicates the parameters used for sensitivity analysis (sensitivity 
 # analysis not performed in this code) 
 COLNAMES <- c("CoatConst", "ZC", "TbodyC", "LASMIN", "PHFEEDCAP", "RESPINCR", 
               "PROTFRACBONE", "PROTFRACMUSCLE", "LIPFRACMUSCLE", "PROTFRACFAT", 
               "LIPFRACFAT", "INCARC", "RUMENFRAC", "NEm", "NEpha", "BONEFRACMAX",
               "LIPNONCMAX", "LIPNONCMIN", "PROTEFF", "LIPIDEFF", "DERMPL", "PROTNE", 
               "GestPer", "GESTINTERVAL", "WEANINGTIME","FtoConcW", "FATFACTOR", "RAINEXP", 
               "FRACVEG", "COMPFACT", "NEIEFFGEST", "CPGEST", "MILKDIG", "NEEFFMILK",       
               "PROTFRACMILK", "PROTEFFMILK", "COMPFACTTIS", "FATTISCOMP", "TTDIGINSC", 
               "DETOME", "DISSEFF",  
               
               "reflectivity coat", "coat length", "area corr", 
               "max body core-skin conductance", "birth weight", "milk A", "milk B", 
               "adult max. weight", "F", "milk A calf", "milk B calf", 
               "fraction TBW fertility", "maintenance factor", "min perc. for gestation", 
               "fat fraction bone parameter", "carcass fraction", "muscle:bone ratio", 
               "min. cond. core-skin. par.", "LHRskin A", "LHRskin B", "LHRskin C",
               
               "BONE A", "BONE B", "MUSCLE A", "MUSCLE B", "INTRAMF A", "INTRAMF B", 
               "INTRAMF C", "PROTNONC A", "PROTNONC B", "RESP", "AREA A", "AREA B", "DIAM A",
               "DIAM B", "BRR A", "BRR B", "BTV A", "TEXH A","TEXH B", "TEXH C","TEXH D", 
               "CBSMIN A", "CBSMIN B", "RAINFRAC", "RAINEVAP", "NEGEST", "GEMILK A",
               "GEMILK B", "FATBONE A", "FATBONE B", "FATBONE C", "FATNONC A", "FATNONC B", 
               "FATNONC C", "FATNONC D","PROTNONC A", "PROTNONC B", "PROTNONC C", 
               "PROTNONC D", "PROTNONC E", "PHFEED A", "PHFEED B","NDFDIG A", "NDFDIG B", 
               "LUCAS A", "LUCAS B", "NONCGR A", "NONCGR B", "NRECYCL A", "NRECYCL B",
               "NRECYCL C", "Reference") 

# Feed efficiency for individual cattle under sensitivity analysis (not part of this code) 
FESENSIND <- matrix(ncol=1, nrow=NPAR, data = FESENSIND) 

#############################################################################################
#                   3. Output section                      #
############################################################

############################################################
#                     Graph for cows                       #
############################################################

# 1. TBW over time
if(SCALE==1) maxgr <- 1000 else maxgr <- 4000

layout(matrix(c(1,1,2,2,3,3), 3, 2, byrow = TRUE), 
       widths=c(1,1,1), heights=c(1.8,1.1,1.4))

par(mar=c(0.5,8,1,2))
plot(TBW[1:maxgr,1]~c(1:maxgr), type = "l", ylim = c(0,LIBRARY[13]), las=1, 
     xlab = "age (days)", ylab = "TBW (kg)", xaxt = "n", lty = "solid", lwd = 1.5, xaxs = "i", 
     yaxs = "i")
lines(TBWBF[1:maxgr,1]~c(1:maxgr), lty = "dashed", lwd = 1.5)

legend("topleft", c("Genetic potential TBW", "Simulated TBW"),
       col= c("black","black"), 
       lty = c("solid", "dashed"),
       bty = "n")

# 2. Feed intake over time
bars <- rbind(FEED1QNTY[1:maxgr],FEED2QNTY[1:maxgr],FEED3QNTY[1:maxgr],FEED4QNTY[1:maxgr])

if(FEEDNR[z]==2) bars <- rbind(FEED1QNTY[1:maxgr],FEED2QNTY[1:maxgr]-FEED2QNTY[1:maxgr]*
                               HOUSING1[1:maxgr], FEED2QNTY[1:maxgr]*HOUSING1[1:maxgr])
if(FEEDNR[z]==4) bars <- rbind(FEED1QNTY[1:maxgr],FEED3QNTY[1:maxgr]-FEED3QNTY[1:maxgr]*
                               HOUSING1[1:maxgr], FEED3QNTY[1:maxgr]*HOUSING1[1:maxgr])
if(FEEDNR[z]==5) bars <- rbind(FEED1QNTY[1:maxgr],FEED2QNTY[1:maxgr]-FEED2QNTY[1:maxgr]*
                               HOUSING1[1:maxgr], FEED2QNTY[1:maxgr]*HOUSING1[1:maxgr])

bars[,ENDDAY[1]:maxgr] <- NA

par(mar=c(0.5,8,0.5,2))
plot(0~0, xaxt="n", yaxt="n", pch = 19, col="white", ylab="", xlab="", xaxs = "i")

par(new=T)
if(FEEDNR[z]==1){
barplot(as.matrix(bars), space = 0, border = NA, xaxt = "n", ylim=c(0,19), xaxs= "i",
        col= c("gold","darkkhaki"), las=1,
        ylab = expression(paste("Feed intake (kg day"^"-1"*")")))

legend("topleft",
       legend=c("Hay","Wheat"), 
       fill = c("darkkhaki","gold"), 
       cex=1, bty = "n")}

if(FEEDNR[z]==2){
  barplot(as.matrix(bars), space = 0, border = NA, xaxt = "n", ylim=c(0,19),xaxs= "i",
          col= c("orange","darkkhaki","seagreen"),las=1,
          ylab = expression(paste("Feed intake (kg day"^"-1"*")")))
  
  legend("topleft",
         legend=c("Grass","Hay","Barley"), 
         fill = c("seagreen","darkkhaki","orange"), 
         cex=1, bty = "n")}

if(FEEDNR[z]==3){
  barplot(as.matrix(bars), space = 0, border = NA, xaxt = "n", ylim=c(0,19),xaxs= "i",
          col= c("orange","seagreen"),las=1,
          ylab = expression(paste("Feed intake (kg day"^"-1"*")")))
  
  legend("topleft",
         legend=c("Grass","Barley"), 
         fill = c("seagreen","orange"), 
         cex=1, bty = "n")}

if(FEEDNR[z]==4){
  barplot(as.matrix(bars), space = 0, border = NA, xaxt = "n", ylim=c(0,19),xaxs= "i",
          col= c("orange","darkkhaki","seagreen"), las=1,
          ylab = expression(paste("Feed intake (kg day"^"-1"*")")))
  
  legend("topleft",
         legend=c("Grass","Hay","Barley"), 
         fill = c("seagreen","darkkhaki","orange"), 
         cex=1, bty = "n")}

if(FEEDNR[z]==5){
  barplot(as.matrix(bars), space = 0, border = NA, xaxt = "n", ylim=c(0,19),xaxs= "i",
          col= c("orange","darkkhaki","seagreen"), las=1,
          ylab = expression(paste("Feed intake (kg day"^"-1"*")")))
  
  legend("topleft",
         legend=c("Grass","Hay","Barley"), 
         fill = c("seagreen","darkkhaki","orange"), 
         cex=1, bty = "n")}

# 3. Defining and limiting factors for growth over time
HEATSTRESS <- REDHP
HEATSTRESS[HEATSTRESS<0] <- 4.5
HEATSTRESS[HEATSTRESS!=4.5] <- NA

COLDSTRESS1 <- Metheatcold-HEATIFEEDMAINTWM
COLDSTRESS <- COLDSTRESS1
COLDSTRESS[COLDSTRESS>0] <- 3.5
COLDSTRESS[COLDSTRESS!=3.5] <- NA

FILLGITGRAPH <- FILLGIT
FILLGITGRAPH[FILLGITGRAPH>=0.97] <-2.5
FILLGITGRAPH[FILLGITGRAPH!=2.5] <-NA

PROTGRAPH <- PROTBAL
PROTGRAPH[PROTGRAPH<0] <- 0.5
PROTGRAPH[PROTGRAPH!=0.5] <- NA
PROTGRAPH[FILLGITGRAPH>=0.999] <- NA

HEATSTRESS[is.na(HEATSTRESS)] <- 0
COLDSTRESS[is.na(COLDSTRESS)] <- 0
FILLGITGRAPH[is.na(FILLGITGRAPH)] <- 0
PROTGRAPH[is.na(PROTGRAPH)] <- 0

TBWBF[is.na(TBWBF)] <- 0
if(FEEDNR[z] == 5) FEEDQNTYTOT <- FEEDQNTYTOT[1:(imax[1]+1)] * TBWBF/100

NELIM <- FEEDQNTYTOT[1:imax[1]] - FEEDQNTY  
NELIM <- NELIM + HEATSTRESS + PROTGRAPH + FILLGITGRAPH 

NELIM[NELIM>0.00001] <- NA
NELIM[NELIM<-0.00001] <- NA
NELIM[ENDDAY[1]:maxgr] <- NA

NELIM[NELIM<0.00001] <- 1.5
NELIM[NELIM>-0.00001] <- 1.5
NELIM[is.na(NELIM)] <- 0.0

GENLIM <- HEATSTRESS + FILLGITGRAPH + PROTGRAPH + NELIM  
GENLIM[GENLIM > 0] <- NA
GENLIM[GENLIM == 0] <- 5.5
GENLIM[ENDDAY[1]:maxgr] <- NA

HEATSTRESS[HEATSTRESS==0] <- NA
COLDSTRESS[COLDSTRESS==0] <- NA
FILLGITGRAPH[FILLGITGRAPH==0] <- NA
NELIM[NELIM==0] <- NA
PROTGRAPH[PROTGRAPH==0] <- NA

par(mar=c(5,8,0.5,2))
plot(HEATSTRESS[1:maxgr,1]~c(1:maxgr), pch = "|", col="#D55E00", cex = 0.9, 
     ylim = c(0.3,5.8), xlab = "Age (days)", ylab = NA, yaxt = "n", xaxs = "i")
axis(2,at = c(0.5:5.5), labels = c("protein", "energy","digestion cap.", "cold stress", 
                                   "heat stress", "genotype"), las = 1, cex.axis = 1.0)

points(COLDSTRESS[1:maxgr,1]~c(1:maxgr), pch = "|", col="#0072B2", cex = 0.9)
points(FILLGITGRAPH[1:maxgr,1]~c(1:maxgr), pch = "|", col="#009E73", cex = 0.9)
points(NELIM[1:maxgr,1]~c(1:maxgr), pch = "|", col="#E69F00", cex = 0.9)
points(PROTGRAPH[1:maxgr,1]~c(1:maxgr), pch = "|", col="#CC79A7", cex = 0.9)
points(GENLIM[1:maxgr,1]~c(1:maxgr), pch = "|", col="#999999", cex = 0.9)

# Lists of the defining and limiting factors

GENLIMdata       <- cbind(GENLIMdata, GENLIM[1:4000])
HEATSTRESSdata   <- cbind(HEATSTRESSdata, HEATSTRESS[1:4000])
COLDSTRESSdata   <- cbind(COLDSTRESSdata, COLDSTRESS[1:4000])
FILLGITGRAPHdata <- cbind(FILLGITGRAPHdata, FILLGITGRAPH[1:4000])
NELIMdata        <- cbind(NELIMdata, NELIM[1:4000])
PROTGRAPHdata    <- cbind(PROTGRAPHdata, PROTGRAPH[1:4000])

#############################################################################################
#                     Graph for calves                     #
############################################################

# 1. TBW over time
maxgr <- 1000

layout(matrix(c(1,1,2,2,3,3), 3, 2, byrow = TRUE), 
       widths=c(1,1,1), heights=c(1.8,1.1,1.4))

# Plot on body weight dynamics
par(mar=c(0.5,8,1,2))
plot(TBW[1:maxgr,3]~c(1:maxgr), type = "l", ylim = c(0,LIBRARY[13]),
     xlab = "age (days)", ylab = "TBW (kg)", xaxt = "n", lty = "solid", lwd = 1.5, 
     xaxs = "i", yaxs = "i")
lines(TBWBF[1:ENDDAY[3],3]~c(1:ENDDAY[3]), lty = "dashed", lwd = 1.5)

legend("topleft", c("Genetic potential TBW", "Simulated TBW"),
       col= c("black","black"), 
       lty = c("solid", "dashed"),
       bty = "n")

# 2. feed intake over time
bars <- rbind(FEED1QNTY[1:maxgr,3],FEED2QNTY[1:maxgr,3],FEED3QNTY[1:maxgr,3],
              FEED4QNTY[1:maxgr,3])

if(FEEDNR[z]==2) bars <-rbind(FEED1QNTY[1:maxgr,3],FEED2QNTY[1:maxgr,3]-FEED2QNTY[1:maxgr,3]*
                                 HOUSING[1:maxgr], FEED2QNTY[1:maxgr,3]*HOUSING[1:maxgr])
if(FEEDNR[z]==4) bars <-rbind(FEED1QNTY[1:maxgr,3],FEED3QNTY[1:maxgr,3]-FEED3QNTY[1:maxgr,3]*
                                 HOUSING[1:maxgr], FEED3QNTY[1:maxgr,3]*HOUSING[1:maxgr])
if(FEEDNR[z]==5) bars <-rbind(FEED1QNTY[1:maxgr,3],FEED2QNTY[1:maxgr,3]-FEED2QNTY[1:maxgr,3]*
                                 HOUSING[1:maxgr], FEED2QNTY[1:maxgr,3]*HOUSING[1:maxgr])

bars[,ENDDAY[3]:maxgr] <- NA

par(mar=c(0.5,8,0.5,2))
plot(0~0, xaxt="n", yaxt="n", pch = 19, col="white", ylab="", xlab="", xaxs = "i")

par(new=T)
if(FEEDNR[z]==1){
  barplot(as.matrix(bars), space = 0, border = NA, xaxt = "n", ylim=c(0,19),xaxs="i",
          col= c("gold","darkkhaki"),
          ylab = expression(paste("Feed intake (kg day"^"-1"*")")))
  
  legend("topleft",
         legend=c("Hay","Wheat"), 
         fill = c("darkkhaki","gold"), 
         cex=1, bty = "n")}

if(FEEDNR[z]==2){
  barplot(as.matrix(bars), space = 0, border = NA, xaxt = "n", ylim=c(0,19),xaxs= "i", 
          col= c("orange","darkkhaki","seagreen"),
          ylab = expression(paste("Feed intake (kg day"^"-1"*")")))
  
  legend("topleft",
         legend=c("Grass","Hay","Barley"), 
         fill = c("seagreen","darkkhaki","orange"), 
         cex=1, bty = "n")}

if(FEEDNR[z]==3){
  barplot(as.matrix(bars), space = 0, border = NA, xaxt = "n", ylim=c(0,19),xaxs= "i",
          col= c("orange","seagreen"),
          ylab = expression(paste("Feed intake (kg day"^"-1"*")")))
  
  legend("topleft",
         legend=c("Grass","Barley"), 
         fill = c("seagreen","orange"), 
         cex=1, bty = "n")}

if(FEEDNR[z]==4){
  barplot(as.matrix(bars), space = 0, border = NA, xaxt = "n", ylim=c(0,19),xaxs= "i",
          col= c("orange","darkkhaki","seagreen"),
          ylab = expression(paste("Feed intake (kg day"^"-1"*")")))
  
  legend("topleft",
         legend=c("Grass","Hay","Barley"), 
         fill = c("seagreen","darkkhaki","orange"), 
         cex=1, bty = "n")}

if(FEEDNR[z]==5){
  barplot(as.matrix(bars), space = 0, border = NA, xaxt = "n", ylim=c(0,19),xaxs= "i",
          col= c("orange","darkkhaki","seagreen"),
          ylab = expression(paste("Feed intake (kg day"^"-1"*")")))
  
  legend("topleft",
         legend=c("Grass","Hay","Barley"), 
         fill = c("seagreen","darkkhaki","orange"), 
         cex=1, bty = "n")}

# 3. Defining and limiting factors for growth over time
HEATSTRESS <- REDHP
HEATSTRESS[HEATSTRESS<0] <- 4.5
HEATSTRESS[HEATSTRESS!=4.5] <- NA

COLDSTRESS1 <- Metheatcold-HEATIFEEDMAINTWM
COLDSTRESS <- COLDSTRESS1
COLDSTRESS[COLDSTRESS>0] <- 3.5
COLDSTRESS[COLDSTRESS!=3.5] <- NA

FILLGITGRAPH <- FILLGIT
FILLGITGRAPH[FILLGITGRAPH>=0.97] <-2.5
FILLGITGRAPH[FILLGITGRAPH!=2.5] <-NA

PROTGRAPH <- PROTBAL
PROTGRAPH[PROTGRAPH<0] <- 0.5
PROTGRAPH[PROTGRAPH!=0.5] <- NA
PROTGRAPH[FILLGITGRAPH>=0.999] <- NA

HEATSTRESS[is.na(HEATSTRESS)] <- 0
COLDSTRESS[is.na(COLDSTRESS)] <- 0
FILLGITGRAPH[is.na(FILLGITGRAPH)] <- 0
PROTGRAPH[is.na(PROTGRAPH)] <- 0

TBWBF[is.na(TBWBF)] <- 0
if(FEEDNR[z] == 5) FEEDQNTYTOT <- FEEDQNTYTOT * TBWBF/100

NELIM <- c(rep(FEEDQNTYTOT[1:4000],9)) - FEEDQNTY  
NELIM <- NELIM + HEATSTRESS + PROTGRAPH + FILLGITGRAPH 

NELIM[NELIM>0.00001] <- NA
NELIM[NELIM<-0.00001] <- NA

NELIM[NELIM<0.00001] <- 1.5
NELIM[NELIM>-0.00001] <- 1.5
NELIM[ENDDAY[3]:maxgr] <- NA

NELIM[is.na(NELIM)] <- 0.0

GENLIM <- HEATSTRESS + FILLGITGRAPH + PROTGRAPH + NELIM  
GENLIM[GENLIM > 0] <- NA
GENLIM[GENLIM == 0] <- 5.5
GENLIM[ENDDAY[3]:maxgr] <- NA

HEATSTRESS[HEATSTRESS==0] <- NA
COLDSTRESS[COLDSTRESS==0] <- NA
FILLGITGRAPH[FILLGITGRAPH==0] <- NA
NELIM[NELIM==0] <- NA
PROTGRAPH[PROTGRAPH==0] <- NA

par(mar=c(5,8,0.5,2))
plot(HEATSTRESS[1:maxgr,3]~c(1:maxgr), pch = "|", col="#D55E00", cex = 0.9, 
     ylim = c(0.3,5.8), xlab = "Age (days)", ylab = NA, yaxt = "n", xaxs = "i")
axis(2,at = c(0.5:5.5), labels = c("protein", "energy","digestion cap.", "cold stress", 
                                   "heat stress", "genotype"), las = 1, cex.axis = 1.0)

points(COLDSTRESS[1:maxgr,3]~c(1:maxgr), pch = "|", col="#0072B2", cex = 0.9)
points(FILLGITGRAPH[1:maxgr,3]~c(1:maxgr), pch = "|", col="#009E73", cex = 0.9)
points(NELIM[1:ENDDAY[3],3]~c(1:ENDDAY[3]), pch = "|", col="#E69F00", cex = 0.9)
points(PROTGRAPH[1:maxgr,3]~c(1:maxgr), pch = "|", col="#CC79A7", cex = 0.9)
points(GENLIM[1:ENDDAY[3],3]~c(1:ENDDAY[3]), pch = "|", col="#999999", cex = 0.9)

# End of the graph for calves

#############################################################################################

# Table with key information about cattle performance (information also presented in Table 3, 
# paper Van der Linden et al. (2017a)) 

DATAt3 <-c(FESENSHERD[s], FESENSREPR[s], FESENSIND[s], OUTPUTHERDS[1,6]/OUTPUTHERDS[3,6], 
           OUTPUTHERDS[3,5], OUTPUTHERDS[1,5], OUTPUTHERDS[2,5], TBWBF[ENDDAY[3],3]) 
TABLEDATA <- matrix(ncol=1, nrow=8, data=DATAt3)
colnames(TABLEDATA) <- "Herd level"
rownames(TABLEDATA) <- c("Feed efficiency herd unit (g beef kg-1 DM)", 
                         "Feed efficiency repr. cow (g beef kg-1 DM)", 
                         "Feed efficiency bull calf (g beef kg-1 DM)",
                         "Feed fraction repr. cow (-)", "Beef production herd unit (kg)",
                         "Beef production repr. cow (kg)", "Beef production bull calf (kg)",
                         "Slaughter weight bull calf (kg)")

print("Case number") # Case number corresponds to the case number in Table 3
print(z) # z is the case number
print(TABLEDATA) 

} # End z-loop for the cases