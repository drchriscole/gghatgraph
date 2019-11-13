## This example code taken from appendix in https://doi.org/10.31234/osf.io/sg37q

dt <- data.frame(subjectID = seq(1,30),DV = rnorm(30,50,20),IV1 = rep(c(1,2),15), IV2 = c(rep(1,15),rep(2,15)))
#dt <- read.csv("YourDataHere.csv",header=T)
#### [1] - HOW TO IMPLEMENT WITH YOUR DATA ####
# Replace DV with your dependent measure (or name your dependent measure "DV")
# Replace IV1 with your independent measure that you want paired in your hats (or name this measure "IV1")
# Assumes only two levels of IV1
# Replace IV2 with your independent measure that you want spaced across the x-axis (or name this measure "IV2")
#
# Also replace label names here to match how you want your axes labeled:
xLab <- "Independent Variable 2"
yLab <- "Dependent Variable"
legendLab <- c("IV1: Level 1","IV1: Level 2")
upsideDownColor <- "gray" #[3] Color to fill hat crown when second part is less than first part - set to "white" if you do not want shading
## Computes means for plotting
# Also computes SD for setting y-axis range and for error bars
# If your design involves within-subjects, see Loftus & Masson (1994) for computing SEM within- subjects
dtMeans <- aggregate(DV ~ IV1 + IV2, dt, mean)
dtSEM <- aggregate(DV ~ IV1 + IV2, dt, sd) #assumes between-subjects effects only 
dtMeans$SEM <- dtSEM$DV / sqrt(length(unique(dt$subjectID)))
dtMeans$hatPos <- as.integer(as.factor(dtMeans$IV1))
dtMeans$xPos <- as.integer(as.factor(dtMeans$IV2))
## Values for setting the range of the y-axis
# Based on recommendations from Witt (in press)
# If data do not fit in graph with this value, increase .75 to greater number
# Should not decrease .75
grandMean <- mean(dtMeans$DV)
grandSD <- mean(dtSEM$DV) * .75 # produces y-axis range of 1.5 SDs (see Witt (in press) for details)
hatWidth <- .25
plot(dtMeans$IV1,dtMeans$DV,ylim=c(grandMean - grandSD,grandMean + grandSD),xlim=c(min(dtMeans$xPos) - (2 * hatWidth),max(dtMeans$xPos) + (2 * hatWidth)),xaxt="n",col="white",bty="l",xlab=xLab,ylab=yLab)
axis(side = 1, at=dtMeans$xPos,labels = dtMeans$IV2)
for (i in unique(dtMeans$xPos)) {
  m1 <- which(dtMeans$xPos == i & dtMeans$hatPos == 1)
  m2 <- which(dtMeans$xPos == i & dtMeans$hatPos == 2) 
  segments(i-hatWidth,dtMeans$DV[m1],i,dtMeans$DV[m1],lwd=3) # Brim of the hat 
  if (dtMeans$DV[m2] > dtMeans$DV[m1]) {
    rect(i,dtMeans$DV[m1],i+hatWidth,dtMeans$DV[m2],lwd=3) # Crown of the hat 
  } else {
    rect(i,dtMeans$DV[m1],i+hatWidth,dtMeans$DV[m2],lwd=3, col=upsideDownColor)
} }
# Crown of
# [2] - Add error bars
dtMeans$semPos <- ifelse(dtMeans$hatPos == 1, dtMeans$xPos - (hatWidth/2), dtMeans$xPos + (hatWidth/2))
for (i in 1:length(dtMeans$semPos)) {
  segments(dtMeans$semPos[i],dtMeans$DV[i] - dtMeans$SEM[i],dtMeans$semPos[i],dtMeans$DV[i] + dtMeans$SEM[i])
}
# Add Legend 
legend("topleft",pch=c(NA,0),lwd=c(3,NA),pt.cex=c(NA,3),cex=1.5,legend=legendLab,bty="n")

