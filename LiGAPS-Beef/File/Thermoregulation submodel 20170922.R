#######################################################################################
#                                                                                     #
# LiGAPS-Beef (Livestock simulator for Generic analysis of Animal Production Systems) #
#                                                                                     #
# Sub-model thermoregulation 2017-09-22                                               #
#                                                                                     #
# Aim: perform a sensitivity analysis for weather input parameters                    #
#                                                                                     #
# The results of the sensivity analysis are described in the paper                    #
# LiGAPS-Beef, a mechanistic model to explore potential and feed-limited beef         # 
# production: 2. Sensitivity analysis and evaluation of sub-models                    #
# Authors: A. van der Linden 1,2,*, G.W.J. van de Ven 2, S.J. Oosting 1,              #
# M.K. van Ittersum 2, and I.J.M. de Boer 1.                                          #
#                                                                                     #
# This code reproduces Figure 2 from the paper.                                       #
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
# * Contact: aart.vanderlinden@wur.nl (Aart van der Linden)                           #
#                                                                                     #
#######################################################################################

######################################################
# Part I: sensitivity analysis on weather conditions #
######################################################

# Weather parameters included in the sensitivity analysis:

# 1. Wind speed (m s-1)
# 2. Relative humidity (%)
# 3. Solar radiation (kJ m-2 day-1)
# 4. Cloud cover (Octa)
# 5. Rainfall (mm)
# 6. Total body weight (kg)
# 7. Heat production (x maintenance heat production)

GRAPHNR <- NULL
GRAPHNR[1] <- 1

# Default values for each of the seven weather inputs (4th value is jmax)
DWIND  <- c(    4,     4, 0, 2) # default value wind speed: 4 ms-1
DRH    <- c(   50,    50, 0, 2) # default value relative humidity: 40%
DSWR   <- c(20000, 20000, 0, 2) # default value solar radiation: 20000 kJ m-2 day-1
DCC    <- c(    4,     4, 0, 2) # default value cloud cover: 4 okta
DRAIN  <- c(    0,     0, 0, 2) # default value precipitation: 0 mm day-1
DTBW   <- c(  450,   450, 0, 2) # default value total body weight (TBW): 450 kg
DHP    <- c( 0.36,  0.36, 0, 2) # default value heat production (on top of maintenance heat production,
# total heat production is 1.36 times the maintenance heat production)

DEFAULT <- rbind(DWIND, DRH, DSWR, DCC, DRAIN, DTBW, DHP)

# Values for sensitivity analysis (4th value is jmax)
DWIND  <- c(  0.1,     8,  0.1,  80) # range wind speed: 0.1-8 ms-1
DRH    <- c(   10,   100,    1,  91) # range relative humidity: 10-100%
DSWR   <- c(    0, 30000,  300, 101) # range solar radiation: 0-30000 kJ m-2 day-1
DCC    <- c(    0,     8,  0.1,  81) # range cloud cover: 0-8 okta
DRAIN  <- c(    0,    30,  0.3, 101) # range precipitation: 0-30 mm day-1
DTBW   <- c(   50,  1300, 12.5, 101) # range total body weight (TBW): 50-1300 kg
DHP    <- c(    0,     1, 0.01, 101) # range heat production (on top of maintenance heat production,
# varies from 1-2 times the maintenance heat production)

library(plot3D) # Make sure this R-package is installed!

BREED <- 1                # Choose breed    (1= Charolais; 2 = Boran; 3 = Brahman (3/4) x Shorthorn(1/4))
HOUSING <- 1              # Stable == 0; outdoor conditions == 1, open feedlot = 2 
imax <- 81                # Number of steps (days) for model simulations

smax <- 7                 # Five weather variables, and TBW and heat production are included.

######################################################
# Part II: sensitivity analysis on parameters        #
######################################################

for(s in 1:smax) {

GRAPHNR[s+1] <- GRAPHNR[s] + 1
print(GRAPHNR[s]) 

DEFAULT1 <- DEFAULT

if(GRAPHNR[s] == 1) DEFAULT1[1,] <- DWIND else
  if(GRAPHNR[s] == 2) DEFAULT1[2,] <- DRH else
    if(GRAPHNR[s] == 3) DEFAULT1[3,] <- DSWR else
      if(GRAPHNR[s] == 4) DEFAULT1[4,] <- DCC else
        if(GRAPHNR[s] == 5) DEFAULT1[5,] <- DRAIN else
          if(GRAPHNR[s] == 6) DEFAULT1[6,] <- DTBW else
            if(GRAPHNR[s] == 7) DEFAULT1[7,] <- DHP   

jmax <- DEFAULT1[s,4]

if(GRAPHNR[s] == 6) TBWACT <- seq(DEFAULT1[6,1], DEFAULT1[6,2], DEFAULT1[6,3]) else TBWACT <- rep(DEFAULT1[6,1],jmax)   
#TBWACT <- rep(450,jmax) #seq(50, 800, 10)  # Weight of the animal (kg) # 

if(GRAPHNR[s] == 7) MAINTT <- seq(DEFAULT1[7,1], DEFAULT1[7,2], DEFAULT1[7,3]) else MAINTT <- rep(DEFAULT1[7,1],jmax)  
#MAINTT <- rep(0.36,jmax)              #seq(from = 0, to = 1, by= 0.01)  # rep(0.36,jmax) # 

TIME <- c(1:imax)

###########################################################################################
#                                       Weather data                                      #
###########################################################################################

# Wind speed
WSSTABLE <- matrix(nrow=imax, ncol=jmax, data= rep(seq(from=DEFAULT1[1,1], to= DEFAULT1[1,2], by= DEFAULT1[1,3]),imax), byrow=T)
#WSSTABLE <- rep(1,imax+1)    #c(rep(1,7),rep(1.5,7),rep(2,7*4))

# Average ambient temperature
#TSTABLE <- c(rep(33.8,7),rep(30.9,7),rep(27.5,7),rep(25.3,7),rep(22.8,7),rep(22.3,7)) 
TSTABLE <- matrix(nrow=imax, ncol=jmax, data= rep(seq(from=-40, to= 40, by= 1),jmax))
#TSTABLE <-  rep(20,imax+1)  #seq(from=10, to= 40, by= 1)

# Relative humidity
RHSTABLE <- matrix(nrow=imax, ncol=jmax, data= rep(seq(from=DEFAULT1[2,1], to= DEFAULT1[2,2], by= DEFAULT1[2,3]),imax), byrow=T)
#RHSTABLE <- rep(60,imax+1) #c(rep(50,7),rep(60,7),rep(70,7*4))

# Solar radiation
SWRAD <- matrix(nrow=imax, ncol=jmax, data= rep(seq(from=DEFAULT1[3,1], to= DEFAULT1[3,2], by= DEFAULT1[3,3]),imax), byrow=T)

# Conversion factor solar radiation on soil to solar radiation on coat
AHAdata <- matrix(nrow=imax, ncol=jmax, data= rep(seq(from=0.5, to= 0.5, by= 0),imax), byrow=T)

# Cloud cover
OCTA <- matrix(nrow=imax, ncol=jmax, data= rep(seq(from=DEFAULT1[4,1], to= DEFAULT1[4,2], by= DEFAULT1[4,3]),imax), byrow=T)

# Rain
RAIN <- matrix(nrow=imax, ncol=jmax, data= rep(seq(from=DEFAULT1[5,1], to= DEFAULT1[5,2], by= DEFAULT1[5,3]),imax), byrow=T)

MATRIX <- rep(0,imax)
MATRIX1 <- rep(0,imax)
MATRIXSK <- rep(0,imax)



###########################################################################################
#                     Genetic parameters (related to BREED and GENDER)                    #
###########################################################################################

# This genetic parameter section contains a list with parameters which are specific for breed and gender
# No significant difference between males and females in the thermoregulation model (except for TBWACT)
# Parameters for Charolais 
LIBRARY10 <- c(0.6, 0.012, 1.00, 64.1,                # 1-4     reflectivity coat, coat length, area corr, max. body core-skin conductance 
               3.08, 1.73, 1.000, 35.3,               # 5-7     5-6 = sweating parameters (Thompson et al, 2011), 7 = min. body core-skin conductance  
               1.00)                                  # 9       Maintenance factor (heat production)    
               
# Parameters for Borans
LIBRARY20 <- c(0.6, 0.012, 1.12, 64.1,                # 1-4     reflectivity coat, coat length, area corr, max. body core-skin conductance 
               4.89, 0.80, 1.30, 34.5,                # 5-7     5-6 = sweating parameters (Thompson et al, 2011), 7 = min. body core-skin conductance  
               0.91)
               
# Parameters for 3/4 Brahman x 1/4 Shorthorns 
LIBRARY30 <- c(0.56, 0.012, 1.09, 64.1,               # 1-4     reflectivity coat, coat length, area corr, max. body core-skin conductance 
               4.44, 1.03, 1.000, 34.7,              # 5-7     5-6 = sweating parameters (Thompson et al, 2011), 7 = min. body core-skin conductance    
               0.93)



if(BREED ==1) LIBRARY <- LIBRARY10 else
  if(BREED ==2) LIBRARY <- LIBRARY20 else
    if(BREED ==3) LIBRARY <- LIBRARY30 
      


###########################################################################################
#                         Initial values thermoregulation sub-model                       #
###########################################################################################

# Constants from 'LIBRARY'
REFLC = LIBRARY[1]                              # fraction light reflected from coat (-) (e.g. Da Silva, 2003)
LC = LIBRARY[2]                                 # coat length (m) (Turnpenny, 2000)
AREAFACTOR = LIBRARY[3]                         # surface area temperate Bos taurus (1.00) or Bos indicus/tropical Bos taurus (1.12) 
CBSMAX = LIBRARY[4]                             # maximum body - skin conductivity (W m-2 K-1)
RBCSf = LIBRARY[7]  

# General constants used in physics
pi = 3.14159265                                 # pi
P = 101325                                      # air pressure at sea level (Pa)
Rdair = 287.058                                 # air pressure constant (J kg-1 K-1)
Rwater = 461.495                                # water pressure constant (J kg-1 K-1)
CtoK = 273.15                                   # conversion from C to K (K)
Cp = 1.005                                      # specific heat air (kJ kg-1 K-1)
L = 2260                                        # latent heat of vapour (kJ kg-1)
REFLEgrass = 0.10                               # albedo grassland (-)
REFLEconcr = 0.50                               # albedo feedlot made of concrete (-)
GRAV = 9.81                                     # gravitational constant (m s-2)
SIGMA = 5.67037 * 10^-8                         # Stefan-Boltzmann constant (W m-2 K-4)
GAMMA = 66                                      # psychrometric constant (Pa K-1)
EMISS = 0.98                                    # emissivity factor LWR (dimensionless)
MuSt = 1.827 * 10^(-5)                          # standard air viscosity (N s-1 m-2)
TR0 = 527                                       # standard temperature in degrees Rankine for calculation air viscosity
CCONV1 = 120                                    # dimensionless constant (?) for calculation air viscosity
CCONV2 = 0.61                                   # dimensionless constant (?) for calculation of the grashof number
kJdaytoW = 1000/(3600*24)                       # conversion kJ day-1 to Watt
KtoR = 9/5
Schmidt = 0.61

# Cattle specific constants
CoatConst = 1.90 * 10^(-5)                      # constant (m)  (McGovern and Bruce, 2000)
ZC = 11000                                      # coat resistance (s m-2) (McGovern and Bruce, 2000, from McArthur, 1981)
TbodyC = 39                                     # body temperature animal (degrees Celsius) (Turnpenny, 2000)
LASMIN = 10                                     # minimum latent heat release skin (W m-2) (Turnpenny et al, 2000, from Alexander and Williams, 1962; Ingram, 1974; Richards 1974, 1976)
RESPINCR        = 7.64
RAINFRAC = 0.3

# Variables
# 1. Respiration
TBW          = NULL  # total body weight (kg)
AREA         = NULL  # body surface area (m2)
DIAMETER     = NULL  # diameter of the cylinder (m)
LENGTH       = NULL  # lenght of the cylinder (m)
brr          = NULL  # basal respiration rate (breaths per minute)
btv          = NULL  # basal tidal volume (L)
brv          = NULL  # basal respiration flow (L min-1)
irv          = NULL  # actual respiration flow (L min-1)
TAVGC        = NULL  # average temperature (degrees Celsius)
TAVGK        = NULL  # average temperature (degrees Kelvin)
VPSATAIR     = NULL  # saturated vapour pressure exhaled air (Pa)
VPAIRTOT     = NULL  # real vapour pressure air (kPa)
RHAIR        = NULL  # relative humidity air (-)
RHOVP        = NULL  # water vapour density (kg m-3)  
RHODAIR      = NULL  # dry air density (kg m-3)  
RHOAIR       = NULL  # air density (kg m-3)  
CHIAIR       = NULL  # water vapour density exhaled air (kg kg-1)
VISCAIR      = NULL  # actual air viscosity (N s-1 m-2) 
Texh         = NULL  # temperature exhaled air (degrees Celsius)
VPSATAIROUT  = NULL  # saturated vapour pressure exhaled air (Pa) 
RHOVPOUT     = NULL
RHODAIROUT   = NULL
RHOAIROUT    = NULL
CHIAIROUT    = NULL
AIREXCH      = NULL
LHEATRESP    = NULL
CHEATRESP    = NULL
TGRESP       = NULL
TNRESP       = NULL
TNRESPH      = NULL
NERESP       = NULL
NERESPWM     = NULL
ENRESPC      = NULL
MetheatSKIN  = NULL
TskinC       = NULL
TskinCH      = NULL
CBSMIN       = NULL
CONDBS       = NULL

# 2. Sweating 
DLC          = NULL
DIFFC        = NULL
RV           = NULL
VPSKINTOT    = NULL
VPSKINHALF   = NULL
LASMAXENV    = NULL
LASMAXPHYS   = NULL
LASMAXCORR   = NULL
ACTSW        = NULL
ACTSWH       = NULL
CSC          = NULL  # conductance skin to coat (W m-2 K-1)
MetheatCOAT  = NULL  # heat flow to coat (W m-2) 
TcoatC       = NULL  # coat temperature (degrees Celsius)
TcoatCH      = NULL  # coat temperature (degrees Celsius)
TcoatK       = NULL  # coat temperature (degrees Kelvin)
QSC          = NULL  # energy flow from skin to coat (W m-2)

# 3.LWR heat balance of the coat
LWRSKY       = NULL  # incoming long wave radiation from the sky (W m-2)
LWRENV       = NULL  # incoming long wave radiation from the soil (W m-2)
LB           = NULL  # outgoing long wave radiation from the coat (W m-2)
LWRCOAT      = NULL  # net long wave radiation balance (W m-2)
LWRCOATH     = NULL  # net long wave radiation balance (W m-2)

# 4.Convective heat losses from the coat
TAVGR        = NULL  # average air temperature in degrees Rankine
Ea           = NULL  # vapour pressure air (mBar)                                        
Ec           = NULL  # vapour pressure coat (mBar)                     
GRASHOF      = NULL  # grashof number
WINDSP       = NULL  # wind speed (m s-1)
REYNOLDS     = NULL  # Reynolds number
ReH          = NULL  # Reynolds number high (intermediate Reynolds number)                                                             
ReL          = NULL  # Reynolds number low (intermediate Reynolds number)
NUSSELTH     = NULL  # Nussel number high (intermediate Nusselt number) 
NUSSELTL     = NULL  # Nussel number low (intermediate Nusselt number)
NUSSELT      = NULL  # Nussel number 
NUSSELTM     = NULL  # Nussel number medium (intermediate Nusselt number)
ka           = NULL  # themal conductivity air (W m-1 K-1)
CONVCOAT     = NULL  # convective heat transfer (W m-2)
CONVCOATH    = NULL  # convective heat transfer (W m-2)

# 5. Incoming SWR (solar radiation) to coat
SAAC         = NULL  # conversion factor solar radiation m-2 soil to solar radiation m-2 coat (-)
SWRS         = NULL  # incoming direct solar radiation at the soil surface, daily basis (W m-2)
SWRC         = NULL  # incoming direct solar radiation at the animals coat (W m-2)
ISWRC        = NULL  # incoming indirect solar radiation at the animals coat (W m-2)                              
SWR          = NULL  # total solar radiation at the animals coat (W m-2)
REFLE        = NULL  # albedo underground animal (-)
RAINEVAP     = NULL

# Synthesis and optimization with repeat {} function
MetheatAIR   = NULL  # heat balance of the animal (should be close to zero)
Metheatopt   = NULL  # maximum heat release from the animal (Wm-2)
METABFEED    = NULL  # maximum heat release from the animal (Wm-2)
CHECKHEAT1   = NULL  # whether the heat loop worked fine (correct/false)
METABFEEDCH  = NULL  # maximum heat release from the animal (Wm-2)
METABFEEDC   = NULL  # minimum heat release from the animal (Wm-2)
Metheatcold  = NULL  # minimum heat release from the animal (Wm-2)
CHECKHEAT2   = NULL  # whether the cold loop worked fine (correct/false)


METTBWACT    = NULL  # Rumen is 10% of TBW
MAINTME      = NULL  # ME requirement for maintenance (kJ day-1)
TOTNE        = NULL  # 2 x times maintenance                               
WM2          = NULL  # Heat production (Wm-2)




for(j in 1:jmax){
for(i in 1:imax) {

  ###########################################################################################
  # 2.                                   Dynamic section                                    #
  #                                     (time and animals)                                  #                                                                  
  ###########################################################################################
  
  ###########################################################################################
  # 2.1                             Thermoregulation submodel                               #
  ###########################################################################################
  
  # Aim: To calculate the maximum and minimum heat release (W m-2) of an animal with its environment 
  
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
  TISSUEFRAC = 1.00           # Vasodilatation (0 = minimum and 1 = maximum vasodilatation)
  SWEATING   = 1.00           # Sweating (0 = basal and 1 = maximum physiological sweating rate)
  PANTING    = 0.25           # Panting (0 = basal respiration, 1 = maximum panting)
  
  # Calculations related to weather conditions
  
  TAVGC[i]     <- TSTABLE[i,j]                                        # average temperature (degrees Celsius)
  TAVGK[i]     <- CtoK + TAVGC[i]                                     # average temperature (degrees Kelvin)
  VPSATAIR[i]  <- 6.1078*10^((7.5*TAVGC[i])/(TAVGC[i]+237.3))*100     # saturated vapour pressure air (Pa)
  VPAIRTOT[i]  <- VPSATAIR[i]*RHSTABLE[i,j]/100                       # real vapour pressure air (kPa)
  RHAIR[i]     <- VPAIRTOT[i] / VPSATAIR[i]                           # relative humidity (-)
  RHOVP[i]     <- VPAIRTOT[i]/ (Rwater*TAVGK[i])                      # water vapour density (kg m-3)
  RHODAIR[i]   <- (P-VPAIRTOT[i]) / (Rdair*TAVGK[i])                  # dry air density (kg m-3)
  RHOAIR[i]    <- RHOVP[i] + RHODAIR[i]                               # air density (kg m-3)
  CHIAIR[i]    <- RHOVP[i]*RHOAIR[i]                                  # water vapour density (kg kg-1)                                      

###########################################################################################
#                1. Latent and convective heat release from respiration                   #
###########################################################################################

# Animal
AREA[i] = 0.14*(TBWACT[j])^0.57 * AREAFACTOR                        # animal surface area (m2) , McGovern and Bruce, 2000
DIAMETER[i] = 0.06*TBWACT[j]^0.39                                   # animal diameter (m)        McGovern and Bruce, 2000
LENGTH[i] = (AREA[i]-pi*DIAMETER[i]^2/2)/(pi*DIAMETER[i])           # animal length (m)
   
# In the thermoneutral zone    
brr[i] <- 73.8 * TBWACT[j]^(-0.286)                                 # basal respiration rate (min-1)         McGovern and Bruce, 2000
btv[i] <- 0.0117 * TBWACT[j]                                        # basal tidal volume (L)                 McGovern and Bruce, 2000
brv[i] <- brr[i]*btv[i]                                             # basal respiration volume (L min-1) 
irv[i] <- brv[i] + PANTING*((RESPINCR-1)*brv[i])                    # increased respiration volume (L min-1) 

Texh[i] <- 17 + 0.3 * TAVGC[i] + exp(0.01611 * RHAIR[i]  + 0.0387 * TAVGC[i])  # temperature exhaled air (degrees Celsius) Stevens (1981)

# Assumption: exhaled air is saturated with water
VPSATAIROUT[i]  <- 6.1078*10^((7.5*Texh[i])/(Texh[i]+237.3))*100    # saturated vapour pressure exhaled air (Pa)
RHOVPOUT[i]     <- VPSATAIROUT[i]/ (Rwater*(Texh[i]+CtoK))          # water vapour density exhaled air (kg m-3)
RHODAIROUT[i]   <- (P-VPSATAIROUT[i]) / (Rdair*(Texh[i]+CtoK))      # dry air density exhaled air (kg m-3)
RHOAIROUT[i]    <- RHOVPOUT[i] + RHODAIROUT[i]                      # air density exhaled air (kg m-3)
CHIAIROUT[i]    <- RHOVPOUT[i]*RHOAIROUT[i]                         # water vapour density exhaled air (kg kg-1) 

RHAIR[i]        <- VPAIRTOT[i] / VPSATAIR[i] *100

AIREXCH[i] <- (irv[i]*60*24/1000*RHOAIR[i])/AREA[i]                      # air in and out (kg air m-2 day-1)

LHEATRESP[i] <- AIREXCH[i] * L *(CHIAIROUT[i]-CHIAIR[i])* kJdaytoW   # latent heat release (W m-2) 
CHEATRESP[i] <- AIREXCH[i] * Cp *(Texh[i]-TAVGC[i]) * kJdaytoW       # convective heat release (W m-2)

TGRESP[i] <- LHEATRESP[i] + CHEATRESP[i]                               # gross energy loss from the respiratory system (W m-2)

#Energy for respiration (panting)
NERESPWM[i] <- 1.1*(RESPINCR*brr[i])^2.78 * 10^-5 * PANTING   # NE required for respiration (W m-2) McGovern and Bruce, 2000)
NERESP[i] <-NERESPWM[i] / kJdaytoW                            # NE required for respiration (kJ NE day-1)   

TNRESP[i] <- TGRESP[i]-NERESPWM[i]                                       # nett energy loss from the respiratory system (W m-2)

TNRESPH[i] <- TNRESP[i]
###########################################################################################
#                                    1a. Skin temperature                                 #
###########################################################################################

# Resistance body core and skin
CBSMIN[i] = RBCSf/(0.03 * TBWACT[j]^0.33)                                  # minimum body - skin conductivity (W m-2 K-1) McGovern and Bruce (2000)
CONDBS[i] = CBSMIN[i] + TISSUEFRAC*(CBSMAX-CBSMIN[i])                    # conductivity body core to skin (W m-2 K-1)   McGovern and Bruce (2000)

# 100 S m-1 = 0.078 K m2 W-1 (Cena and Clark, 1978) 
# Cattle --> 50 s m-1 (Turnpenny, 2000a) --> 0.039 K m-2 W-1  = 25.6 W m-2 K-1

###########################################################################################
#                            2. Latent heat release from the skin                         #
###########################################################################################

# Latent energy flow between skin and air
DLC[i] = (CoatConst * WSSTABLE[i,j])/((CoatConst * WSSTABLE[i,j])/LC+1/(ZC*LC)) # reduction in coat depth (m)
DIFFC[i] = 0.187 * 10^-9 * TAVGK[i]^2.072                              # diffusion constant water vapour in air (m2 s-1) 

###########################################################################################
#                                    2a. Coat temperature                                 #
###########################################################################################

# Resistance and conductivity between skin and coat
CSC[i] = 1/(ZC * (LC-DLC[i]) * (0.078/100))                            # conductance skin to coat (W m-2 K-1)

CSC[i] <- CSC[i]/ (1-min(RAINFRAC, RAIN[i,j]*RAINFRAC/24)) # 30% reduction in conductance due to rain (Mount and Brown, 1982)

###########################################################################################
#                            3. Long wave radiation from the coat                         #
###########################################################################################

# Incoming LWR from the sky
LWRSKY[i] = (1-OCTA[i,j]/8)*(SIGMA*TAVGK[i]^4)*(1-0.261*exp(-0.000777*(273-TAVGK[i])^2)) + (OCTA[i,j]/8) * (SIGMA * TAVGK[i]^4 - 9) # (W m-2)

# Incoming LWR from soil surface
LWRENV[i] = SIGMA * TAVGK[i]^4                                             # incoming LWR from soil surface (W m-2)

if(HOUSING == 0) LWRSKY[i] <- LWRENV[i]  
###########################################################################################
#                          4. Convective heat losses from the coat                        #
###########################################################################################

# Calculation of the air viscosity 
TAVGR[i] = TAVGK[i] * KtoR                                              # average air temperature in degrees Rankine
VISCAIR[i] =(MuSt*((0.555*TR0+CCONV1)/(0.555*TAVGR[i]+CCONV1)*(TAVGR[i]/TR0)^(3/2))) # actual air viscosity (N s-1 m-2) 

# Calculation of the grashof number  
Ea[i] = VPAIRTOT[i]*10                                             # vapour pressure air (mBar)

# Calculation of the Reynolds number 
WINDSP[i] = WSSTABLE[i,j]                                           # wind speed (m s-1)      
REYNOLDS[i] = WINDSP[i] * DIAMETER[i] * RHOAIR[i] / VISCAIR[i]         # Reynolds number

ReH[i] = 16*REYNOLDS[i]^2                                              # calculation step representing natural convection                                 
ReL[i] = 0.1*REYNOLDS[i]^2                                             # calculation step representing forced convection
ka[i] = 1.5207 * 10^(-11) * TAVGK[i]^3 - 4.8574 * 10^(-8) * TAVGK[i]^2 + 1.0184 * 10^-4 *TAVGK[i] - 0.00039333 # themal conductivity air (W m-1 K-1)

###########################################################################################
#                        5. Solar radiation intercepted by the coat                       #
###########################################################################################

# Direct solar radiation
SAAC[i] <- AHAdata[i,j]                                              # Ah/A factor: Shade area / animal coat area (m2 m-2)
SWRS[i] <- SWRAD[i,j]*1000/(3600*24)                              # incoming direct solar radiation soil surface, daily basis (Wm-2)
SWRC[i] <- SWRS[i]*SAAC[i]*(1-REFLC)                                    # incoming direct solar radiation animal coat, daily basis (Wm-2)

# Indirect solar radiation
if(HOUSING==1) REFLE[i] <- REFLEgrass else if(HOUSING==2) REFLE[i] <- REFLEconcr else REFLE[i] == 0    
ISWRC[i] <- 0.5*REFLE[i]*SWRAD[i,j]*1000/(3600*24)                  # incoming indirect solar radiation,animal coat, daily basis (Wm-2)

# Total solar radiation                                    
SWR[i] <- SWRC[i] + ISWRC[i]                                           # total solar radiation for animal (W m-2)

# Heat loss by evaporation of rain
RAINEVAP[i] <- 0.15* (LENGTH[i]*DIAMETER[i])/AREA[i] * min(24, RAIN[i,j]) * L * kJdaytoW # (W m-2)


METABFEED[i] <- 170     

repeat {  
  
  ###########################################################################################
  #                                    1a. Skin temperature                                 #
  ###########################################################################################
  
  MetheatSKIN[i] = METABFEED[i] - TNRESP[i]                              # amount of heat from body core to skin (W m-2)     
  TskinC[i] = TbodyC - MetheatSKIN[i]/CONDBS[i]                          # skin temperature (degrees Celsius)
  
  METABFEEDCH[i] = METABFEED[i]
  ###########################################################################################
  #                            2. Latent heat release from the skin                         #
  ###########################################################################################
  
  LASMAXPHYS[i] = LASMIN + LIBRARY[5]*exp(LIBRARY[6]*(TskinC[i]-LIBRARY[8])) * L/3600 # maximum physological latent heat release from skin (W m-2)
  
  RV[i] = (LC-DLC[i])/(DIFFC[i]*(1+1.54*((LC-DLC[i])/DIAMETER[i])*(TskinC[i]-min(TAVGC[i],TskinC[i]))^0.7)) # resistance vapour transfer (s m-1)
  VPSKINTOT[i] = 6.1078*10^((7.5*TskinC[i])/(TskinC[i]+237.3))*100       # saturated vapour pressure skin (Pa)
  
  LASMAXENV[i] = (RHOAIR[i] * Cp * 1000 / GAMMA) * (VPSKINTOT[i]-VPAIRTOT[i]) / RV[i] # maximum environmental latent heat release from skin (W m-2)
  LASMAXCORR[i] = min(LASMAXPHYS[i],LASMAXENV[i])                           # maximum latent heat release from skin (W m-2)
  ACTSW[i] = LASMIN + SWEATING * (LASMAXCORR[i]-LASMIN)                    # actual latent heat release from skin (W m-2)
  
  ACTSWH[i] = ACTSW[i] 
  
  ###########################################################################################
  #                                    2a. Coat temperature                                 #
  ###########################################################################################
  
  MetheatCOAT[i] = MetheatSKIN[i] - ACTSW[i]
  
  TcoatC[i] = TskinC[i] - MetheatCOAT[i]/CSC[i]                        # coat temperature (degrees Celsius)
  TcoatK[i] = TcoatC[i] + CtoK                                             # coat temperature (Kelvin)
  
  ###########################################################################################
  #                            3. Long wave radiation from the coat                         #
  ###########################################################################################
  
  QSC[i] = CSC[i] * (TskinC[i] - TcoatC[i])                              # energy flow from skin to coat (W m-2)
  LB[i] = EMISS * SIGMA * TcoatK[i]^4                                                # LWR release from body (W m-2)
  
  # LWR balance (net energy loss is a negative value)
  LWRCOAT[i] = (EMISS * ((LWRSKY[i]+LWRENV[i])/2) - LB[i]) / (1-min(RAINFRAC, RAIN[i,j]*RAINFRAC/24))
  LWRCOATH[i] = LWRCOAT[i]
  
  ###########################################################################################
  #                          4. Convective heat losses from the coat                        #
  ###########################################################################################
  
  Ec[i] = ((6.1078*10^((7.5*TcoatC[i])/(TcoatC[i]+237.3)))+Ea[i])/2      # vapour pressure coat (mBar)
  
  GRASHOF[i] = (GRAV*DIAMETER[i]^3*P/100*(TcoatC[i]-TAVGC[i])+Schmidt*(Ec[i]*TcoatC[i]-Ea[i]*TAVGC[i]))/(273*P/100*VISCAIR[i]^2) 
  
  if(GRASHOF[i]>ReH[i]) NUSSELT[i] <- 0.48*GRASHOF[i]^0.25 else 
    if(GRASHOF[i]<ReL[i]) NUSSELT[i] <- 0.0112*REYNOLDS[i]^0.875 else
      NUSSELT[i] <- max(0.48*GRASHOF[i]^0.25,0.0112*REYNOLDS[i]^0.875)    #Formula from Turnpenny (2000a)
  
  # Energy flow between coat and air
  
  CONVCOAT[i] = (ka[i] * NUSSELT[i]) / DIAMETER[i] *(TcoatC[i]-TAVGC[i]) / (1-min(RAINFRAC, RAIN[i,j]*RAINFRAC/24)) # convective heat transfer (W m-2)
  CONVCOATH[i] = CONVCOAT[i]  
  
  ###########################################################################################
  #                                         Synthesis                                       #
  ###########################################################################################
  
  MetheatAIR[i] <- (MetheatCOAT[i] + SWR[i] - RAINEVAP[i] + LWRCOAT[i] - CONVCOAT[i])     # net energy balance (W m-2)
  
  if(MetheatAIR[i] > 1) METABFEED[i] <- (METABFEED[i]-0.01*MetheatAIR[i])
  if(MetheatAIR[i] < -1) METABFEED[i] <-(METABFEED[i]-0.01*MetheatAIR[i])
  Metheatopt[i] <- METABFEED[i]                                             # heat metabolic processes
  if(MetheatAIR[i] < 1 & MetheatAIR[i] > -1) CHECKHEAT1[i] <- "CORRECT" else CHECKHEAT1[i] <- "FALSE"
  if(CHECKHEAT1[i] == "CORRECT") {break}
  
}

###########################################################################################
# 2.1.2                             Minimum heat release                                  #
###########################################################################################

# Heat release mechanisms of cattle at minimum heat release
TISSUEFRAC = 0.0            # Vasodilatation (0 = minimum and 1 = maximum vasodilatation)
SWEATING   = 0.0            # Sweating (0 = minimum and 1 = maximum physiological sweating rate)
PANTING    = 0.0            # Panting (0 = basal respiration, 1 is maximum panting)   

###########################################################################################
#                1. Latent and convective heat release from respiration                   #
###########################################################################################

irv[i] <- brv[i] + PANTING*(6.64*brv[i])                       # actual respiration rate (L min-1)  

AIREXCH[i] <- (irv[i]*60*24/1000*RHOAIR[i])/AREA[i]                  # air in and out (kg air m-2 day-1)

LHEATRESP[i] <- AIREXCH[i] * L *(CHIAIROUT[i]-CHIAIR[i])*1000/(24*3600)  # latent heat release via respiratory system (W m-2)
CHEATRESP[i] <- AIREXCH[i] * Cp *(Texh[i]-TAVGC[i])*1000/(24*3600)       # concective heat release via respiratory system (W m-2)

TGRESP[i] <- LHEATRESP[i] + CHEATRESP[i]                               # gross energy loss from the respiratory system (W m-2)

TNRESP[i] <- TGRESP[i]                                                   # nett energy loss from the respiratory system (W m-2)

###########################################################################################
#                                    1a. Skin temperature                                 #
###########################################################################################

CONDBS[i] = CBSMIN[i]    # conductivity body core to skin (W m-2 K-1)

###########################################################################################
#                            2. Latent heat release from the skin                         #
###########################################################################################

ACTSW[i] = LASMIN          # actual latent heat release from skin (W m-2)


METABFEEDC[i] <-80 



repeat {  
  
  ###########################################################################################
  #                                    1a. Skin temperature                                 #
  ###########################################################################################
  
  # Skin temperature
  
  MetheatSKIN[i] = METABFEEDC[i] - TNRESP[i]                              # amount of heat from body core to skin (W m-2)      
  TskinC[i] = TbodyC - MetheatSKIN[i]/CONDBS[i]                          # skin temperature (degrees Celsius)
  TskinCH[i] =  TskinC[i]
  
  ###########################################################################################
  #                                    2a. Coat temperature                                 #
  ###########################################################################################
  
  # Coat temperature
  
  MetheatCOAT[i] = MetheatSKIN[i] - ACTSW[i]
  
  TcoatC[i] = TskinC[i] - MetheatCOAT[i]/CSC[i]        # coat temperature (degrees Celsius)
  TcoatK[i] = TcoatC[i] + CtoK                                           # coat temperature (Kelvin)
  TcoatCH[i] = TcoatC[i] 
  
  ###########################################################################################
  #                            3. Long wave radiation from the coat                         #
  ###########################################################################################
  
  QSC[i] = CSC[i] * (TskinC[i] - TcoatC[i])                              # energy flow from skin to coat (W m-2)
  
  LB[i] = SIGMA * TcoatK[i]^4                                                # LWR release from body (W m-2)
  
  # LWR balance (net energy loss is a negative value)
  
  LWRCOAT[i] = (EMISS * ((LWRSKY[i]+LWRENV[i])/2) - EMISS * LB[i]) / (1-min(RAINFRAC, RAIN[i,j]*RAINFRAC/24))  # Net LWR loss (W m-2)
  
  ###########################################################################################
  #                          4. Convective heat losses from the coat                        #
  ###########################################################################################
  
  Ec[i] = ((6.1078*10^((7.5*TcoatC[i])/(TcoatC[i]+237.3)))+Ea[i])/2      # vapour pressure coat (mBar)
  
  GRASHOF[i] = (GRAV*DIAMETER[i]^3*P/100*(TcoatC[i]-TAVGC[i])+Schmidt*(Ec[i]*TcoatC[i]-Ea[i]*TAVGC[i]))/(273*P/100*VISCAIR[i]^2) # grashof number
  
  if(GRASHOF[i]>ReH[i]) NUSSELT[i] <- 0.48*GRASHOF[i]^0.25 else 
    if(GRASHOF[i]<ReL[i]) NUSSELT[i] <- 0.0112*REYNOLDS[i]^0.875 else
      NUSSELT[i] <- max(0.48*GRASHOF[i]^0.25,0.0112*REYNOLDS[i]^0.875)    #Formula from Turnpenny (2000a)
  
  # Energy flow between coat and air
  
  CONVCOAT[i] = (ka[i] * NUSSELT[i]) / DIAMETER[i] *(TcoatC[i]-TAVGC[i]) / (1-min(RAINFRAC, RAIN[i,j]*RAINFRAC/24)) # convective heat transfer (W m-2)
  
  ###########################################################################################
  #                                         Synthesis                                       #
  ###########################################################################################
  
  MetheatAIR[i] <- (MetheatCOAT[i] + SWR[i] - RAINEVAP[i] + LWRCOAT[i] - CONVCOAT[i])     # net energy balance (W m-2)
  
  if(MetheatAIR[i] > 1) METABFEEDC[i] <- (METABFEEDC[i]-0.01*MetheatAIR[i])
  if(MetheatAIR[i] < -1) METABFEEDC[i] <-(METABFEEDC[i]-0.01*MetheatAIR[i])
  Metheatcold[i] <- METABFEEDC[i]                                             # heat metabolic processes
  if(MetheatAIR[i] < 1 & MetheatAIR[i] > -1) CHECKHEAT2[i] <- "CORRECT" else CHECKHEAT2[i] <- "FALSE"
  if(CHECKHEAT2[i] == "CORRECT") {break}
  
}
}

MATRIX <- cbind(MATRIX, Metheatopt)
MATRIX1 <- cbind(MATRIX1, Metheatcold)
MATRIXSK <- cbind(MATRIXSK,TskinC)

METTBWACT[j] <- (TBWACT[j]*0.9)^0.75    # Rumen is 10% of TBW
MAINTME[j] <- 311*METTBWACT[j]             # ME requirement for maintenance (kJ day-1)
TOTNE[j] <- MAINTME[j]+MAINTME[j]*MAINTT[j]        # 2 x times maintenance          
HIF <- 0.30                          # standard 0.30
WM2[j] <- TOTNE[j] * (1+HIF/(1-HIF)) / AREA[i]*1000/(3600*24)  # Heat production (Wm-2)

}

MATRIX <- MATRIX[,2:ncol(MATRIX)]
MATRIX1 <- MATRIX1[,2:ncol(MATRIX1)]
MATRIXSK <- MATRIXSK[,2:ncol(MATRIXSK)]

MATRIXWM2 <- matrix(ncol=jmax, nrow=imax, data= rep(WM2,imax), byrow=T)

# Model output

x <- TSTABLE[1:imax]
y <- RHSTABLE[1,1:jmax]
z <- MATRIX


RASTER <- matrix(nrow=nrow(MATRIX), ncol=ncol(MATRIX), data= 1)

RASTER[MATRIX<MATRIXWM2] <- 2
RASTER[MATRIX1>MATRIXWM2] <- 0

if(GRAPHNR[s] == 1) RASTERWIND <- RASTER else
  if(GRAPHNR[s] == 2) RASTERRH <- RASTER else
    if(GRAPHNR[s] == 3) RASTERSWR <- RASTER else
      if(GRAPHNR[s] == 4) RASTERCC <- RASTER else
        if(GRAPHNR[s] == 5) RASTERRAIN <- RASTER else
          if(GRAPHNR[s] == 6) RASTERTBW <- RASTER else
            if(GRAPHNR[s] == 7) RASTERHP <- RASTER   

if(GRAPHNR[s] == 1) {
# Wind speed
par(mar=c(5, 5, 4, 6))

image2D (RASTERWIND, x = x,
         y = seq(DWIND[1], DWIND[2], DWIND[3]), 
         xlab = expression(paste("temperature ("^"o"*"Celsius)")),
         ylab = expression(paste("wind speed (ms"^"-1"*")")),
         col = c("blue", "green", "red"),
         colkey = F)

par(mar=c(5, 0, 4, 0))
legend(42, DWIND[2], fill= c("blue", "green", "red"), c("<TNZ","TNZ", ">TNZ"), 
       inset=c(-0.2,0), border = T,bty = "n", cex=1.0)
}

if(GRAPHNR[s] == 2) {
# Relative humidity
par(mar=c(5, 5, 4, 6))

image2D (RASTERRH, x = x,
         y = seq(DRH[1], DRH[2], DRH[3]), 
         xlab = expression(paste("temperature ("^"o"*"Celsius)")),
         ylab = expression(paste("relative humidity (%)")),
         col = c("blue", "green", "red"),
         colkey = F)

par(mar=c(5, 0, 4, 0))
legend(42, DRH[2], fill= c("blue", "green", "red"), c("<TNZ","TNZ", ">TNZ"), 
       inset=c(-0.2,0), border = T,bty = "n", cex=1.0)
}

if(GRAPHNR[s] == 3) {
  # Solar radiation
  par(mar=c(5, 5, 4, 6))
  
  image2D (RASTERSWR, x = x,
           y = seq(DSWR[1]/1000, DSWR[2]/1000, DSWR[3]/1000), 
           xlab = expression(paste("temperature ("^"o"*"Celsius)")),
           ylab = expression(paste("solar radiation (MJ m"^"-2"*"day"^"-1"*")")),
           col = c("blue", "green", "red"),
           colkey = F)
  
  par(mar=c(5, 0, 4, 0))
  legend(42, DSWR[2]/1000, fill= c("blue", "green", "red"), c("<TNZ","TNZ", ">TNZ"), 
         inset=c(-0.2,0), border = T,bty = "n", cex=1.0)
}

if(GRAPHNR[s] == 4) {
  # Cloud cover
  par(mar=c(5, 5, 4, 6))
  
  image2D (RASTERCC, x = x,
           y = seq(DCC[1], DCC[2], DCC[3]), 
           xlab = expression(paste("temperature ("^"o"*"Celsius)")),
           ylab = expression(paste("cloud cover   ", (Omega))),
           col = c("blue", "green", "red"),
           colkey = F)
  
  par(mar=c(5, 0, 4, 0))
  legend(42, DCC[2], fill= c("blue", "green", "red"), c("<TNZ","TNZ", ">TNZ"), 
         inset=c(-0.2,0), border = T,bty = "n", cex=1.0)
}

if(GRAPHNR[s] == 5) {
  # Rain
  par(mar=c(5, 5, 4, 6))
  
  image2D (RASTERRAIN, x = x,
           y = seq(DRAIN[1], DRAIN[2], DRAIN[3]), 
           xlab = expression(paste("temperature ("^"o"*"Celsius)")),
           ylab = expression(paste("precipitation (mm day"^"-1"*")")),
           col = c("blue", "green", "red"),
           colkey = F)
  
  par(mar=c(5, 0, 4, 0))
  legend(42, DRAIN[2], fill= c("blue", "green", "red"), c("<TNZ","TNZ", ">TNZ"), 
         inset=c(-0.2,0), border = T,bty = "n", cex=1.0)
}

if(GRAPHNR[s] == 6) {
  # Total body weight
  par(mar=c(5, 5, 4, 6))
  
  image2D (RASTERTBW, x = x,
           y = seq(DTBW[1], DTBW[2], DTBW[3]), 
           xlab = expression(paste("temperature ("^"o"*"Celsius)")),
           ylab = expression(paste("total body weight (kg)")),
           col = c("blue", "green", "red"),
           colkey = F)
  
  par(mar=c(5, 0, 4, 0))
  legend(42, DTBW[2], fill= c("blue", "green", "red"), c("<TNZ","TNZ", ">TNZ"), 
         inset=c(-0.2,0), border = T,bty = "n", cex=1.0)
}

if(GRAPHNR[s] == 7) {
  # Heat production
  par(mar=c(5, 5, 4, 6))
  
  image2D (RASTERHP, x = x,
           y = seq(DHP[1]+1, DHP[2]+1, DHP[3]), 
           xlab = expression(paste("temperature ("^"o"*"Celsius)")),
           ylab = expression(paste("heat production (x maintenance)")),
           col = c("blue", "green", "red"),
           colkey = F)
  
  par(mar=c(5, 0, 4, 0))
  legend(42, DHP[2]+1, fill= c("blue", "green", "red"), c("<TNZ","TNZ", ">TNZ"), 
         inset=c(-0.2,0), border = T,bty = "n", cex=1.0)
}

}

tiff("D:/P2Fig2col.tiff", width = 4.2, height = 8.4, units = 'in', res = 200) # Reproduces Figure 2 of the main paper in colour

# First joint graph
par(mfrow = c(4,2))

# Solar radiation
par(mar=c(5, 5, 1, 1))

image2D (RASTERSWR, x = x,
         y = seq(DSWR[1]/1000, DSWR[2]/1000, DSWR[3]/1000), 
         xlab = expression(paste("Temperature ("^"o"*"Celsius)")),
         ylab = expression(paste("Solar radiation (MJ m"^"-2"*"day"^"-1"*")")),
         col = c("blue", "green", "red"), las=1,
         colkey = F)

# Relative humidity
par(mar=c(5, 5, 1, 1))

image2D (RASTERRH, x = x,
         y = seq(DRH[1], DRH[2], DRH[3]), 
         xlab = expression(paste("Temperature ("^"o"*"Celsius)")),
         ylab = expression(paste("Relative humidity (%)")),
         col = c("blue", "green", "red"), las=1,
         colkey = F)

# Wind speed
par(mar=c(5, 5, 1, 1))

image2D (RASTERWIND, x = x,
         y = seq(DWIND[1], DWIND[2], DWIND[3]), 
         xlab = expression(paste("Temperature ("^"o"*"Celsius)")),
         ylab = expression(paste("Wind speed (ms"^"-1"*")")),
         col = c("blue", "green", "red"), las=1,
         colkey = F)

# Rain
par(mar=c(5, 5, 1, 1))

image2D (RASTERRAIN, x = x,
         y = seq(DRAIN[1], DRAIN[2], DRAIN[3]), 
         xlab = expression(paste("Temperature ("^"o"*"Celsius)")),
         ylab = expression(paste("Precipitation (mm day"^"-1"*")")),
         col = c("blue", "green", "red"), las=1,
         colkey = F)

# Cloud cover
par(mar=c(5, 5, 1, 1))

image2D (RASTERCC, x = x,
         y = seq(DCC[1], DCC[2], DCC[3]), 
         xlab = expression(paste("Temperature ("^"o"*"Celsius)")),
         ylab = expression(paste("Cloud cover  ", (Omega))),
         col = c("blue", "green", "red"), las=1,
         colkey = F)

# Total body weight
par(mar=c(5, 5, 1, 1))

image2D (RASTERTBW, x = x,
         y = seq(DTBW[1], DTBW[2], DTBW[3]), 
         xlab = expression(paste("Temperature ("^"o"*"Celsius)")),
         ylab = expression(paste("Total body weight (kg)")),
         col = c("blue", "green", "red"), las=1,
         colkey = F)

# Heat production
par(mar=c(5, 5, 1, 1))

image2D (RASTERHP, x = x,
         y = seq(DHP[1]+1, DHP[2]+1, DHP[3]), 
         xlab = expression(paste("Temperature ("^"o"*"Celsius)")),
         ylab = expression(paste("Heat production (x maintenance)")),
         col = c("blue", "green", "red"), las=1,
         colkey = F)

image2D (NA)
legend("topleft", fill= c("blue", "green", "red"), c("< TNZ","TNZ", "> TNZ"), 
       border = T, cex=1.0, bg = "white", bty="n")

dev.off()

tiff("D:/P2Fig2.tiff", width = 4.2, height = 8.4, units = 'in', res = 200) # Reproduces Figure 2 of the main paper

# First joint graph
par(mfrow = c(4,2))

# Solar radiation
par(mar=c(5, 5, 1, 1))

image2D (RASTERSWR, x = x,
         y = seq(DSWR[1]/1000, DSWR[2]/1000, DSWR[3]/1000), 
         xlab = expression(paste("Temperature ("^"o"*"Celsius)")),
         ylab = expression(paste("Solar radiation (MJ m"^"-2"*"day"^"-1"*")")),
         col = c("grey", "white", "black"), las=1,
         colkey = F)

# Relative humidity
par(mar=c(5, 5, 1, 1))

image2D (RASTERRH, x = x,
         y = seq(DRH[1], DRH[2], DRH[3]), 
         xlab = expression(paste("Temperature ("^"o"*"Celsius)")),
         ylab = expression(paste("Relative humidity (%)")),
         col = c("grey", "white", "black"), las=1,
         colkey = F)

# Wind speed
par(mar=c(5, 5, 1, 1))

image2D (RASTERWIND, x = x,
         y = seq(DWIND[1], DWIND[2], DWIND[3]), 
         xlab = expression(paste("Temperature ("^"o"*"Celsius)")),
         ylab = expression(paste("Wind speed (ms"^"-1"*")")),
         col = c("grey", "white", "black"), las=1,
         colkey = F)

# Rain
par(mar=c(5, 5, 1, 1))

image2D (RASTERRAIN, x = x,
         y = seq(DRAIN[1], DRAIN[2], DRAIN[3]), 
         xlab = expression(paste("Temperature ("^"o"*"Celsius)")),
         ylab = expression(paste("Precipitation (mm day"^"-1"*")")),
         col = c("grey", "white", "black"), las=1,
         colkey = F)

# Cloud cover
par(mar=c(5, 5, 1, 1))

image2D (RASTERCC, x = x,
         y = seq(DCC[1], DCC[2], DCC[3]), 
         xlab = expression(paste("Temperature ("^"o"*"Celsius)")),
         ylab = expression(paste("Cloud cover  ", (Omega))),
         col = c("grey", "white", "black"), las=1,
         colkey = F)

# Total body weight
par(mar=c(5, 5, 1, 1))

image2D (RASTERTBW, x = x,
         y = seq(DTBW[1], DTBW[2], DTBW[3]), 
         xlab = expression(paste("Temperature ("^"o"*"Celsius)")),
         ylab = expression(paste("Total body weight (kg)")),
         col = c("grey", "white", "black"), las=1,
         colkey = F)

# Heat production
par(mar=c(5, 5, 1, 1))

image2D (RASTERHP, x = x,
         y = seq(DHP[1]+1, DHP[2]+1, DHP[3]), 
         xlab = expression(paste("Temperature ("^"o"*"Celsius)")),
         ylab = expression(paste("Heat production (x maintenance)")),
         col = c("grey", "white", "black"), las=1,
         colkey = F)

image2D (NA)
legend("topleft", fill= c("grey", "white", "black"), c("< TNZ","TNZ", "> TNZ"), 
       border = T, cex=1.0, bg = "white", bty="n")

dev.off()

