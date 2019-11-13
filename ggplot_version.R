
library(ggplot2)

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
grandSD <- mean(dtSEM$DV) * .75 # produces y-axis range of 1.5 SDs (see Witt (in press) for details)


ggplot(dtMeans, aes(x = factor(IV2), y = DV, group=DV )) + 
  geom_errorbar(aes(ymax = DV, ymin = DV), position = position_dodge(0.4), width = 0.4) +
  geom_rect( xmin = c(NA,1,NA,2) + 0,
             xmax = c(NA,1,NA,2) + 0.3,
             ymin = c(rep(dtMeans[1,3],2),rep(dtMeans[3,3],2)), 
             ymax = c(dtMeans[1:2,3],dtMeans[3:4,3]),
             colour = 'black') +
  ylim(c(grandMean - grandSD, grandMean + grandSD))


  
#  scratch space #
#
#   geom_rect( xmin = as.numeric(dtMeans$IV2) - 0.3,
#             xmax = as.numeric(dtMeans$IV2) + 0.3,
#             ymin = c(rep(dtMeans[1,3],2),rep(dtMeans[3,3],2)), 
#             ymax = c(dtMeans[1:2,3],dtMeans[3:4,3]),
#             linetype = 'dashed'
#             )
#   
#   geom_point(shape = '-', size = 4)
#   layer(geom = 'point', stat = 'identity', params = list(shape = '-', size = 4))
# 
# 
# df <- data.frame(x = c("a","b","c"), y = c(1,2,3))
# ggplot(data = df, aes(x=x, y=y, fill = x)) +
#   geom_col() +
#   geom_rect(data = df, aes(x = x, y=y), xmin = as.numeric(df$x[[2]]) - 0.2,
#             xmax = as.numeric(df$x[[3]]) + 0.3,
#             ymin = 1.5, ymax = 2,
#             linetype = 'dashed')
  