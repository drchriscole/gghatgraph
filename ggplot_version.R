
library(ggplot2)

# TODO - make it work with other data
dt <- data.frame(subjectID = seq(1,30),DV = rnorm(30,50,20),IV1 = rep(c(1,2),15), IV2 = c(rep(1,15),rep(2,15)))
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
grandMean <- mean(dtMeans$DV)
# TODO - if the inter-group means are quite different and
#      - and the intra-group variance is large +/- the grandSD
#      - may not be a enough of a range to represent the data
grandSD <- mean(dtSEM$DV) * .75 # produces y-axis range of 1.5 SDs (see Witt (in press) for details)

ggplot(dtMeans, aes(x = factor(IV2), y = DV, group=hatPos )) + 
  # first, plot means. Using geom_errorbar() as couldn't see another way to draw a line
  geom_errorbar(aes(ymax = DV, ymin = DV), position = position_dodge(0.4), width = 0.4, size = 0.8) +
  # next, add the rectangle representing the within-group difference
  # TODO - make this generic
  geom_rect( xmin = c(NA,1,NA,2) + 0,
             xmax = c(NA,1,NA,2) + 0.2,
             ymin = c(rep(dtMeans[1,3],2),rep(dtMeans[3,3],2)), 
             ymax = c(dtMeans[1:2,3],dtMeans[3:4,3]),
             size = 0.8,
             colour = 'black') +
  # finally, add the actual error bar
  geom_errorbar(aes(ymax = DV-SEM, ymin = DV+SEM), position = position_dodge(0.4), width = 0.03, colour = "darkgrey") +
  # set the y-axis range to 
  ylim(c(grandMean - grandSD, grandMean + grandSD)) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.line = element_line())

