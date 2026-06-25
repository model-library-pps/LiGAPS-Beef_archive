#######################################################################################
#                                                                                     #
# LiGAPS-Beef (Livestock simulator for Generic analysis of Animal Production Systems) #
#                                                                                     #
# Feed intake and digestion sub-model                                                 # 
#                                                                                     #
# The model LiGAPS-Beef is described in the paper:                                    #
# LiGAPS-Beef, a mechanistic model to explore potential and feed-limited beef         # 
# production: 1. Model description and illustration.                                  #
# Authors: A. van der Linden, G.W.J. van de Ven, S.J. Oosting, M.K. van Ittersum,     #
# and I.J.M. de Boer.                                                                 #
#                                                                                     #
# Contact: aart.vanderlinden@wur.nl (Aart van der Linden)                             #                                                   
#                                                                                     #
# Description:                                                                        #
# This program code simulates the Informationmaterial. No additional files are        # 
# required to run the feed intake and digestion sub-model.                            # 
#                                                                                     #
#                                                                                     #
# Date: 27-06-2016                                                                    #
#                                                                                     #
#######################################################################################

# Feed parameters related to heat generation, digestion (Chilibroste et al, 1997) and fill units (Jarrige, 1986)
# Chilibroste P, Aguilar C and Garcia F 1997. Nutritional evaluation of diets. Simulation model of digestion and passage of nutrients through the rumen-reticulum. Animal Feed Science and Technology 68, 259-275.
# Jarrige R, Demarquilly C, Dulphy JP, Hoden A, Robelin J, Beranger C, Geay Y, Journet M, Malterre C, Micol D and Petit M 1986. The INRA fill unit system for predicting the voluntary intake of forage-based 
# diets in ruminants - a review. Journal of Animal Science 63, 1737-1758.

# Abbreviations
# HIF = Heat Increment of feeding (MJ MJ-1 metabolisable energy, see Table S4 of the supplementary material)
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

#                        HIF    FU    SNSC INSC DNDF     SCP    DCP        kdINSC kdPNDF kdDCP kdPASS  UNDF            pef  CP    GE                

BARLEY             <- c(0.245, 0.573, 389, 214, 156.00,  34.50, 82.80, NA, 0.242, 0.145, 0.125, 0.040,  21.00, NA, 0.34, 138, 18.4) #            # Kolver,2000
CONCENTRATE        <- c(0.249, 0.619, 262, 175, 243.10,  72.80, 87.36, NA, 0.150, 0.060, 0.100, 0.040,  42.90, NA, 0.69, 182, 18.5) #            # Chilibroste et al, 1997 Last value is eNDF for barley, Mertens, 1997, Fill unit estimated  
HAY                <- c(0.318, 1.120, 100, 150, 345.80,  48.16, 74.30, NA, 0.300, 0.040, 0.085, 0.035, 148.20, NA, 1.00, 172, 18.5) #            # Chilibroste et al, 1997 Last value is eNDF, Mertens, 1997, Fill unit from Jarrige, 1989                      
HAYPOOR            <- c(0.420, 1.370,  73,  73, 462.00,  20.30,149.10, NA, 0.300, 0.040, 0.085, 0.035, 198.00, NA, 1.00,  70, 18.2) #            # Kolver,2000
GRASSSPRING        <- c(0.304, 0.960, 130,  30, 360.00,  66.25, 97.40, NA, 0.300, 0.040, 0.085, 0.035, 120.00, NA, 0.40, 265, 18.6) #            # Kolver,2000
GRASSSUMMER        <- c(0.356, 1.120, 100,  60, 376.00,  49.50, 76.50, NA, 0.300, 0.040, 0.085, 0.035, 141.00, NA, 0.50, 180, 18.4) #            # Kolver,2000
GRASSSUMMERDRY     <- c(0.447, 1.280,  50,  60, 409.50,  23.00, 69.00, NA, 0.300, 0.040, 0.085, 0.035, 175.50, NA, 1.00, 115, 18.1) #            # Kolver,2000 
MAIZE              <- c(0.237, 0.438, 202, 532, 101.70,  20.10, 86.56, NA, 0.040, 0.051, 0.035, 0.050,  11.30, NA, 0.40, 134, 17.0) #            # Chilibroste et al, 1997 Last value is eNDF, Mertens, 1997, Fill unit estimated
MOLASSE            <- c(0.050, 0.200, 828,   0,      0,    3.8,   0.2, NA,     0,     0, 0.125, 0.040,      0, NA,    0,   4, 17.0) #
SBM                <- c(0.242, 0.526, 107,   0, 138.60, 202.80,243.40, NA, 0.242, 0.145, 0.125, 0.040,   0.00, NA, 0.34, 507, 19.7)
STRAWCER           <- c(0.557, 1.800,  14,  78, 401.00,  10.00,  5.00, NA, 0.300, 0.040, 0.085, 0.035,   0.00, NA, 0.34,  40, 18.3) #            # MAFF, 1986 UK table book, p. 24 
WHEAT              <- c(0.239, 0.475, 475, 212,  80.00,  39.90, 69.80, NA, 0.182, 0.150, 0.080, 0.040,  34.20, NA, 0.00, 133, 18.2)              # UK table book, p. 24 
MAIZESILAGE        <- c(0.290, 1.000, 100, 351, 239.00,  54.94, 23.00, NA, 0.250, 0.040, 0.040, 0.030, 239.00, NA, 0.93,  82, 18.5)

SUNFLOWERHULLS     <- c(0.000, 1.000)

FEEDS <- rbind(BARLEY,CONCENTRATE, HAY, HAYPOOR, GRASSSPRING, GRASSSUMMER, GRASSSUMMERDRY, MAIZE, MOLASSE, SBM, STRAWCER, WHEAT, MAIZESILAGE)
FEEDS <- rbind(FEEDS,FEEDS)

# The lowest passage rate is 55% of the standard passage rate;
# and the highest passage rate is 100% of the standard passage rate
PASSREDFRAC <- c(rep(0.55,13), rep(1.00,13)) 
  
MECONTENTS <- c(NA)

# The feed intake and digestion sub-model is can handle a maximum of four feed types. Only one feed type (FEED1) is used for this model comparison 
# against independent data for metabolisable energy content.Strictly speaking, this programme code only simulates feed digestion, because the energy
# and protein requirements are unknown if one of the three sub-models of LiGAPS-Beef is not included. Feed intake is part of the feed intake and 
# digestion model, however, in the version of LiGAPS-Beef where the three sub-models are integrated, and where animal growth is simulated at animal 
# and herd level.
  
  for(s in 1:26){
  
  FEED1 = FEEDS[s,]
  FEED2 = HAY
  FEED3 = GRASSSPRING
  FEED4 = GRASSSUMMERDRY
  
  # minimum feed quantities based on rumen digestive capacity, feed quantity offered and feed fractions
  FEED1QNTY <- 1    # kg feed
  FEED2QNTY <- 0
  FEED3QNTY <- 0
  FEED4QNTY <- 0
  
  FEEDQNTY <- FEED1QNTY + FEED2QNTY + FEED3QNTY + FEED4QNTY    
  
  if(FEEDQNTY == 0) CPAVG <- 0 else CPAVG <- (FEED1QNTY*FEED1[16] + FEED2QNTY*FEED2[16] + FEED3QNTY*FEED3[16] + FEED4QNTY*FEED1[16]) / FEEDQNTY # Crude protein (g kg DM-1 feed)
  
  PASSRED <- PASSREDFRAC[s] # 1-0.55
  
  # CH digestion (INSC = insoluble, non-structural carbohydrates, which is assumed to be mainly starch) 
  
  INSC <-       FEED1QNTY * FEED1[4] * FEED1[9]  / (FEED1[9]  + FEED1[12]*PASSRED) + # Digestion insoluble, non-structural carbohydrates (g day-1)
                FEED2QNTY * FEED2[4] * FEED2[9]  / (FEED2[9]  + FEED2[12]*PASSRED) +
                FEED3QNTY * FEED3[4] * FEED3[9]  / (FEED3[9]  + FEED3[12]*PASSRED) +
                FEED4QNTY * FEED4[4] * FEED4[9]  / (FEED4[9]  + FEED4[12]*PASSRED) 
  
  INSCTOTAL <-  FEED1QNTY * FEED1[4] + FEED2QNTY * FEED2[4] + FEED3QNTY * FEED3[4] + FEED4QNTY * FEED4[4]  # Total intake insoluble, non-structural carbohydrates (g day-1)  
  INSCDIG   <-  INSC/INSCTOTAL # Fraction insoluble, non-structural carbohydrates digested in rumen (compare to Owens, 1986) 
  
  INSCINT   <-  max(0,INSCTOTAL*0.97-INSC) # Total tract digestibility assumed to be 97% for all feeds (Moharrery et al, 2014) 
    
  INSCINTDIG <- INSCINT/INSCTOTAL 
  
  
  
  NDF <-        FEED1QNTY * FEED1[5] * FEED1[10] / (FEED1[10] + FEED1[12]*PASSRED) +  # Digestion potentially degradable NDF (g day-1)
                FEED2QNTY * FEED2[5] * FEED2[10] / (FEED2[10] + FEED2[12]*PASSRED) +
                FEED3QNTY * FEED3[5] * FEED3[10] / (FEED3[10] + FEED3[12]*PASSRED) +
                FEED4QNTY * FEED4[5] * FEED4[10] / (FEED4[10] + FEED4[12]*PASSRED)  
  
  NDFTOTAL <-   FEED1QNTY * FEED1[5] + FEED2QNTY * FEED2[5] + FEED3QNTY * FEED3[5] + FEED4QNTY * FEED4[5] # Total intake digestible NDF (g day-1)   
  NDFDIG   <-   NDF/NDFTOTAL # Fraction NDF digested in the rumen
  
  NDFINT   <-   FEED1QNTY * FEED1[5] * (1- FEED1[10] / (FEED1[10] + FEED1[12]*PASSRED)) * (FEED1[10]*0.9) / (FEED1[10]*0.9 + 0.125) +  # Cabral et al., 2011 (http://www.scielo.br/pdf/rbz/v40n9/a20v40n9.pdf)
                FEED2QNTY * FEED2[5] * (1- FEED2[10] / (FEED2[10] + FEED2[12]*PASSRED)) * (FEED2[10]*0.9) / (FEED2[10]*0.9 + 0.125) +  # This concerns digestion of degradable NDF in the intestines 
                FEED3QNTY * FEED3[5] * (1- FEED3[10] / (FEED3[10] + FEED3[12]*PASSRED)) * (FEED3[10]*0.9) / (FEED3[10]*0.9 + 0.125) +  # Volative fatty acids released are assumed not to be taken up by the animal
                FEED4QNTY * FEED4[5] * (1- FEED4[10] / (FEED4[10] + FEED4[12]*PASSRED)) * (FEED4[10]*0.9) / (FEED4[10]*0.9 + 0.125) 
  
  NDFINTDIG <-  NDFINT/NDFTOTAL # Fraction degradable NDF digested in intestines
  NDFINTDIGTOT <- NDFINT/(FEEDQNTY*1000)  # Fraction degradable NDF digested in intestines (on DM basis)
  
  #Protein digestion
  PICP <-       FEED1QNTY * FEED1[7] * FEED1[11] / (FEED1[11] + FEED1[12]*PASSRED) +  # Protein digestion in the rumen (g day-1)
                FEED2QNTY * FEED2[7] * FEED2[11] / (FEED2[11] + FEED2[12]*PASSRED) +
                FEED3QNTY * FEED3[7] * FEED3[11] / (FEED3[11] + FEED3[12]*PASSRED) +
                FEED4QNTY * FEED4[7] * FEED4[11] / (FEED4[11] + FEED4[12]*PASSRED)  
  
  PROTTOTAL  <- (FEED1QNTY * FEED1[16] + FEED2QNTY * FEED2[16] + FEED3QNTY * FEED3[16] + FEED4QNTY * FEED4[16]) # Total crude protein intake (g day-1) 
  PROTINT    <- PROTTOTAL - (FEED1QNTY * FEED1[6] + FEED2QNTY * FEED2[6] + FEED3QNTY * FEED3[6] + FEED4QNTY * FEED4[6]) - PICP   # Protein to intestines (g day-1)                     
  PROTUPT    <- 0.9 * PROTTOTAL - 32 * FEEDQNTY # Lucas equation, empirical formula (g protein day-1), whole digestive tract  
  PROTEXCR   <- PROTTOTAL - PROTUPT # Protein excreted (g protein day-1)  
  
  PROTDIGRU  <- (PROTTOTAL-PROTINT)/ PROTTOTAL # Fraction protein digested in rumen
  PROTDIGWT  <- PROTUPT / PROTTOTAL # Fraction protein digested in the whole digestive tract
  
  # Digestion and excretion
  if(FEED1[16] >500) PROTTOTAL <- 0
  
  DIGFRAC <-    FEED1QNTY * (FEED1[3]) +           # Feed digested (g day-1)
                FEED2QNTY * (FEED2[3]+FEED2[6]) +
                FEED3QNTY * (FEED3[3]+FEED3[6]) +
                FEED4QNTY * (FEED4[3]+FEED4[6]) +
                INSC + INSCINT + NDF + NDFINT + PROTUPT + PROTTOTAL * (121.7 - 12.01*(FEED1[16]/10) + 0.3235*(FEED1[16]/10)^2)/100     
  
  CHEXCR   <- FEEDQNTY*1000-DIGFRAC-PROTEXCR        # Carbohydrates excreted (g day-1)                                                                            # Carbohydrates (CHs) present minus CH's digested (g kg-1) 
  
  EXCRFRAC <- FEEDQNTY*1000-DIGFRAC                      # Feed excreted (g day-1) 
  
  GEEXCR   <- (PROTEXCR * 23.8 + CHEXCR * 17.4) / (PROTEXCR + CHEXCR) # GE content excreted feed (MJ GE kg-1 DM) 
  
  GEUPTAKE <- (PROTUPT * 23.8 + (DIGFRAC-PROTUPT) * 17.4) / (DIGFRAC) # GE content digested feed (MJ GE kg-1 DM)  
  
  if(EXCRFRAC == 0) MEUPTAKE <-0 else MEUPTAKE <- DIGFRAC/1000 * GEUPTAKE * 0.82      # ME uptake (MJ kg-1 DM feed); 0.82 is conversion DE --> ME  
  
  if(EXCRFRAC == 0) Q <-0 else Q = DIGFRAC/(FEEDQNTY*1000)  # Digestibility (g g-1)
  
  MECONTENTS <- c(MECONTENTS, MEUPTAKE) # ME contents under low and high rumen fill of the 13 feed types
  
  }
  
  MEpred1 <- matrix(nrow=13, ncol=2, MECONTENTS[2:length(MECONTENTS)]) # ME contents under low and high rumen fill of the 13 feed types
  colnames(MEpred1) <- c("low rumen fill", "high rumen fill")
  rownames(MEpred1) <- c("BARLEY", "CONCENTRATE", "HAY", "HAYPOOR", "GRASSSPRING", "GRASSSUMMER", "GRASSSUMMERDRY", "MAIZE", "MOLASSE", "SBM", "STRAWCER", "WHEAT", "MAIZESILAGE")

  MEpred <- rowMeans(MEpred1)

  BARS <- MEpred1[,1]-MEpred
  
  # Maize silage is excluded from the analysis for the dataset of Kolver (2000), as this feed type is not part of the measure data. 
  
  MEMAFF    <- c(12.8, 13.8, 9.6, 8.1, 13.1, 10.7, 8.4, 13.8, 12.6, 13.4, 6.4, 13.6, 11.2) # ME contents of feed types given by MAFF (1986)
  MEKolver  <- c(13, 13.6, 9.7, 7.3, 11.75, 10, 8.75, 13.6, 12.0, NA, 6.4, 12.6, 10.3) # ME contents of feed types given by Kolver (2000)
  # MAFF 1986. Feed composition: UK table of feed composition and nutritive value for ruminants. Chalcombe Publications, Marlow, UK.  
  # Kolver E 2000. Nutrition guidelines for the high producing dairy cow. Proceedings of the Ruakura Farmers Conference 52, 17-28.
  MINMAFF <- c(12.1,NA, 8.7,7.5,12.9, 9.7,7.0,12.2,NA,12.6, 3.4,12.3,10.2)
  MAXMAFF <- c(13.7,NA,10.3,8.7,13.5,12.3,9.3,16.4,NA,14.3,10.4,14.7,11.7)
  
  DIG <- data.frame(MEpred,MEMAFF,MEKolver) 
  DIG1 <- data.frame(rep(MEpred,2), c(MEMAFF,MEKolver))
              
  # Statistics for MAFF, 1986
  stats <- summary(lm(MEpred ~ MEMAFF, data = DIG))
  
  slope <- stats$coefficients[2,1]
  se.slope <- stats$coefficients[2,2]
  t.value<-(slope-1)/se.slope        # reduces slope by 1, should subsequently be equal to zero.
  1-pt(t.value,df=13-2)
  
  RMSE <- (sum((MEMAFF-MEpred)^2)/13)^0.5
  RMSEmean <- mean(MEMAFF)
  RMSErel <- RMSE/RMSEmean
  
  meanerror <- sum(abs(MEMAFF-MEpred))/13
  meanrel <- meanerror/RMSEmean 
  
  # Statistics for Kolver, 2000
  sm<-summary(lm(MEKolver ~ MEpred, data = DIG)) 
  sm$coefficients 
  slope<-sm$coefficients[2,1] 
  se.slope<-sm$coefficients[2,2] 
  
  # the last steps: calculating t-value and probability of type I error 
  t.value<-(slope-1)/se.slope 
  1-pt(t.value,df=12-2) 
  
  
  RMSE <- (sum((c(MEKolver[1:9],MEKolver[11:13])-c(MEpred[1:9],MEpred[11:13]))^2)/12)^0.5
  RMSEmean <- mean(c(MEKolver[1:9],MEKolver[11:13]))
  RMSErel <- RMSE/RMSEmean
  
  meanerror <- sum(abs(c(MEKolver[1:9],MEKolver[11:13])-c(MEpred[1:9],MEpred[11:13])))/12
  meanrel <- meanerror/RMSEmean 
  
# Again statistics for both MAFF (1986) and Kolver (2000)   
SUMMAFF <-summary(lm(MEMAFF ~ MEpred, data = DIG))
SUMMAFF$coefficients 
slope<-SUMMAFF$coefficients[2,1] 
se.slope<-SUMMAFF$coefficients[2,2]  
t.value<-(slope-1)/se.slope 
1-pt(t.value,df=13-2)*2 

SUMKOL <-summary(lm(MEKolver ~ MEpred, data = DIG))
SUMKOL$coefficients 
slope<-SUMKOL$coefficients[2,1] 
se.slope<-SUMKOL$coefficients[2,2]  
t.value<-(slope-1)/se.slope 
1-pt(t.value,df=12-2)*2
  
# confint(summary(lm(MEpred ~ MEMAFF, data = DIG)))
  
# Root mean square error (RMSE) and mean absolute error (MAE)
PRED <- c(MEpred,MEpred)
PRED <- c(PRED[1:22],PRED[24:26])
MEAS <- c(MEMAFF,MEKolver) 
MEAS <- c(MEAS[1:22],MEAS[24:26])

RMSE <- (sum((PRED-MEAS)^2)/25)^0.5
RMSEmean <- mean(MEAS)
RMSErel <- RMSE/RMSEmean
  
MAE <- sum(abs(PRED-MEAS))/25
MAErel <- meanerror/RMSEmean 

# Create a plot with simulated versus measured ME contents
# Figure 4 in the main paper of Van der Linden et al. 
tiff("D:/Plot4.tiff", width = 5.0, height = 5.0, units = 'in', res = 200)
  
one <- c(0,16)

par(mar=c(5, 5, 1, 1))
plot(one~c(0,16), type = "l", lty="dashed", xlim = c(0,16), ylim = c(0,16),xaxs="i",yaxs="i", las=1,
     ylab = expression(paste("Measured ME content (MJ kg"^" -1"*")")),
     xlab = expression(paste("Simulated ME content (MJ kg"^"-1"*")")))

lines(c(SUMMAFF$coefficients[1,1], SUMMAFF$coefficients[1,1]+ 16*SUMMAFF$coefficients[2,1])~c(0, 16), lty="solid")
lines(c(SUMKOL$coefficients[1,1], SUMKOL$coefficients[1,1]+ 16*SUMKOL$coefficients[2,1])~c(0, 16), lty="dotted")

arrows(DIG1[,1]-BARS, DIG1[,2], DIG1[,1]+BARS,DIG1[,2], length=0.05, angle=90, code=3)

arrows(DIG$MEpred, MINMAFF, DIG$MEpred,MAXMAFF, length=0.05, angle=90, code=3)

points(MEMAFF~MEpred, pch = 19  , xlim = c(0,16), ylim = c(0,16))
points(MEKolver~MEpred, pch = 24, bg="white", xlim = c(0,16), ylim = c(0,16))


legend("topleft",
       legend=c("MAFF (1986)","Kolver (2000)", "x = y"),
       col = c("black", "black", "black"),
       pch=c(19, 24, NA),
       pt.bg = c(NA,"white",NA),
       lty= c("solid","dotted","dashed"),
       cex=0.9,
       bty = "n")

text(9.5,2.0, bquote(MAFF~(1986)~":"~y~"="~.(round(SUMMAFF$coefficients[1,1], digits = 2))~+~.(round(SUMMAFF$coefficients[2,1], digits = 2))~"x;"~R^{2}~adj.~"= 0.90"), cex = 0.8)  
text(9.5,0.7, bquote(Kolver~(2000)~":"~y~"="~.(round(SUMKOL$coefficients[1,1], digits = 2))~+~.(round(SUMKOL$coefficients[2,1], digits = 2))~"x;"~R^{2}~adj.~"="~.(round(SUMKOL$adj.r.squared, digits = 2))), cex = 0.8)

dev.off()



