############################################################################
#                         Weather file synthesis                           #                    
############################################################################

# Aim: convert weather files for crop growth models into weather files 
# for livestock growth models

# Two additional parameters are required for livestock growth models:
# 1. Cloud cover (Octa)
# 2. A factor that converts solar radiation per unit area at a horizontal 
#    surface to solar radiation per unit area coat of the animal 

# Author: Aart van der Linden (aart.vanderlinden@wur.nl)
# Date: 31-07-2014

# Literature:
# Allen et al, 2006
# Agricultural and Forest Meteorology 139 : 55-73

# Location specific input

LAT         <- 52  # latitude, in degrees (northern hemisphere +, southern hemisphere -)
ALT         <- 10  # altidude, in meters above sea level (MASL)

# New users: adapt file locations on disk!
FILE     <- "C:/R/filename.csv" # The original and new version of the weather file can be given different names!
FILENEW  <- "C:/R/filename.csv"

# Other parameters 
SOLCONST    <- 1367            # solar constant (W m-2)           
ATMST       <- 101.325         # atmospheric pressure at sea level (kPa)
TURB        <- 1.00            # empirical turbidity coefficient (Allen et al, 2006) 
ANAZIM      <- 45              # animal azimuth, relative to south (degrees)
SRAN        <- 7.28            # size ratio animal (2 x length / diameter) (McGovern and Bruce, 2000)

# Weather file (suited for crop growth models)

WEATHER <- read.csv(file= FILE, head=TRUE,sep=",")

ATM <- ATMST - 1.2* (ALT/100)                             # atmospheric pressure at location (kPa)

DOY <- ((WEATHER$DOY/365)-floor(WEATHER[,3]/365))*365     # day of the year construction (only if 
DOY[DOY==0] <- 365                                        # the file starts at 1st January

dES <- 1/(1 + 0.033*cos(DOY*2*pi/365))                     # relative distance earth-sun (fraction)

decl <- 23.5*cos((DOY-171)*2*pi/365)                      # declination angle (degrees)
hours <- (seq(from = 0, to = 23.5, by = 0.25))             # hour angle, interval is 30 minutes (degrees)
hourangle <- -15*(12-hours)                               # negative in morning; positive in afternoon (Allen et al, 2006)

w <- 0.14 * WEATHER$VPR * ATM  + 2.1                      # depth precipitable water in atmosphere (mm) (Allen et al, 2006)

coshor <- matrix(nrow=nrow(WEATHER), ncol=length(hourangle))  # cosine solar angle at horizontal surface (radians)
Ra24 <- matrix(nrow=nrow(WEATHER), ncol=length(hourangle))    # solar radiation at horizontal surface (W m-2)
KBo <- matrix(nrow=nrow(WEATHER), ncol=length(hourangle))     # clearness index for direct beam radiation for cloudless conditions (Allen et al, 2006) 
KDo <- matrix(nrow=nrow(WEATHER), ncol=length(hourangle))     # index for diffuse beam radiation (Allen et al, 2006)
Rso <- matrix(nrow=nrow(WEATHER), ncol=length(hourangle))     # direct and diffuse solar radiation at horizontal surface (W m-2)

for(i in 1:nrow(WEATHER)){
  for(j in 1:length(hours)){

    coshor[i,j] <- sin(decl[i]/(365/(2*pi))) * sin(LAT/(365/(2*pi))) + cos(decl[i]/(365/(2*pi))) * cos(LAT/(365/(2*pi))) * cos(hourangle[j]/(365/(2*pi))) 
       

    Ra24[i,j] <- (SOLCONST/dES[i])*max(0,coshor[i,j])  # no negative solar radiation when between sunset and sunrise  

    KBo[i,j] <- 0.98 * exp((-0.00146*ATM/(TURB*max(0,coshor[i,j])))-0.075*(w[i]/max(0,coshor[i,j]))^0.4)

    if(KBo[i,j] <= 0.065) KDo[i,j] <- 0.10 + 2.08 * KBo[i,j] else
      if(KBo[i,j] >= 0.15) KDo[i,j] <- 0.35 - 0.36 * KBo[i,j] else
        KDo[i,j] <- 0.18 + 0.82 * KBo[i,j]

    Rso[i,j] <- Ra24[i,j] * (KBo[i,j] + KDo[i,j])

}}
  
RADMJM2 <- (rowSums(Rso)/length(hourangle)) * 3600 * 24 / 1000000  # potential solar radiation in MJ m-2 day-1

# 1. Cloud cover calculation

FRAC <- (WEATHER$RAD/1000)/RADMJM2  # actual / potential solar radiation on a horizontal surface

OCTA <- -5.1666*FRAC^2 - 3.3382*FRAC + 8.8679 # empirical formula, fitted from http://nora.nerc.ac.uk/3872/1/ir8.pdf

OCTA[OCTA < 0] <- 0
OCTA[OCTA > 8] <- 8

#OCTA <- round(OCTA)  # round to the nearest eightst

# 2. Conversion solar radiation on horizontal surface to coat surface

solelev <- matrix(nrow=nrow(WEATHER), ncol=length(hourangle)) # solar elevation above horizon (radians)
azim    <- matrix(nrow=nrow(WEATHER), ncol=length(hourangle)) # solar azimuth relative to the south, is positive in morning,  
                                                              # negative in the afternoon. (radians) 
dazim   <- matrix(nrow=nrow(WEATHER), ncol=length(hourangle)) # difference azimuth animal and sun (radians)
AhA     <- matrix(nrow=nrow(WEATHER), ncol=length(hourangle)) # multiplication solar radiation on horizontal surface to coat surface
sinhor  <- matrix(nrow=nrow(WEATHER), ncol=length(hourangle)) # sinus solar radiation at horizontal surface
sinan   <- matrix(nrow=nrow(WEATHER), ncol=length(hourangle)) # sinus solar radiation at an animals coat

for(i in 1:nrow(WEATHER)){
  for(j in 1:length(hours)){  
 
 solelev[i,j] <- asin(cos(LAT/(365/(2*pi)))*cos(decl[i]/(365/(2*pi)))*cos(15*(12-hours[j])/(365/(2*pi)))+sin(LAT/(365/(2*pi)))*sin(decl[i]/(365/(2*pi))))
 
 azim[i,j] <- (-sin(15*(12-hours[j])/(365/(2*pi))) * cos(decl[i]/(365/(2*pi)))) / cos(solelev[i,j])
 
 dazim[i,j] <- abs(azim[i,j]-ANAZIM/(365/(2*pi)))

 if(solelev[i,j] < 0) AhA [i,j] <- 0 else AhA [i,j] <- (2/sin(solelev[i,j])*(pi^-1*SRAN*(1-cos(solelev[i,j])^2*cos(dazim[i,j])^2)^0.5)+(cos(solelev[i,j])/sin(solelev[i,j]))*cos(dazim[i,j]))/(2*(SRAN+1))
 
 if(solelev[i,j] < 0) sinhor[i,j] <- 0 else sinhor[i,j] <- sin(solelev[i,j])  
 
 if(solelev[i,j] < 0) sinan[i,j] <- 0 else sinan[i,j] <- sinhor[i,j]*AhA [i,j] # sinan is a weighted factor that includes
 # solar radiation and the multiplication factor Aha at a specific time of the day
 
}}


SINELEVhor <- rowSums(sinhor)  # sinus sum of solar radiation at a horizontal surface (per day)
SINELEVan  <- rowSums(sinan)   # sinus sum of solar radiation at an animals coat (per day)

AhA24 <- SINELEVan/SINELEVhor  # AhA24 is multiplied with the solar radiation on a horizontal surface to
                               # end up with solar radiation on an animals coat.

AHA <- AhA24


# Graphical output for data check (plot potential and actual solar radiation)

plot(WEATHER$RAD[1:2000]/1000~seq(from = 1, to = 2000, by = 1), ylim = c(0,40), ylab = "Solar radiation (MJ m-2 day-1)",
     xlab = "time (days)", pch = 19, cex = 0.5)
lines(RADMJM2[1:2000]~seq(from = 1, to = 2000, by = 1), col = "red", lwd = 2)
legend("topright", legend = c("Potential solar radiation", "Actual solar radiation"), border = FALSE, bty = "n", 
       lty = c("solid", NA),
       pch = c(NA, 19),
       pt.cex = c(NA, 0.5),
       col = c("red", "black"))

plot(AHA[1:2000]~seq(from=1, to = 2000, by = 1), pch=19, col = "black", ylim = c(0,1.5), xlab = "time (days)", ylab = "Conversion factor")
#lines(WEATHER$AHA[1:2000]~seq(from=1, to = 2000, by = 1), pch=19, col = "red")

# Add cloud cover (OCTA) and solar radiation conversion factor (AhA24) to the existing file

WEATHERNEW <- cbind(WEATHER[,1:9], AHA, OCTA)

write.csv(WEATHERNEW, file= FILENEW)

