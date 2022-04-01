#####################################################################################################################################
####Investigate the correlation between cooling and wetting conditions in Aug & Sept between AEZ's in West Africa
#####################################################################################################################################

TmaxTmin.join= full_join(TmaxWasigdf.long, TminWasigdf.long, by = 'mergeID', copy=F)
TmaxTminchirps.join= full_join(TmaxTmin.join, chirps.wa.signdf.long, by = 'mergeID', copy=F)
write.csv(TmaxTminchirps.join, file='E:/Francis_IITA/GIS_RS/GIS/Analysis/WA_TED/TmaxTminchirpsfulljoin.csv')
TmaxTminchirpsFulljoin= read.csv('TmaxTminchirpsfulljoin.csv', header=T,sep=',')

####Subset zone with significant increase of rain and cooling temperatures####

TmaxTminchirpsFulljoin.cool = TmaxTminchirpsFulljoin%>%
                              filter(Month.x == "Aug" | Month.x == "Sept")%>%
                              filter(Trend.x < 0.00000000000000001) %>%
                              filter(Trend.y < 0.00000000000000001) %>%
                              filter(Trend > 0.00000000000000001) %>%
                              filter(!is.na(Trend.x))%>%
                              filter(!is.na(Trend.y))%>%
                              filter(!is.na(Trend))
write.csv(TmaxTminchirpsFulljoin.cool, file='E:/Francis_IITA/GIS_RS/GIS/Analysis/WA_TED/TmaxTminchirpsFulljoincool.csv')
TmaxTminchirpsFulljoin.cool =read.csv('TmaxTminchirpsFulljoincool.csv', header=T, sep=',')

##Investigate relationship between cooling and wetting conditions in Aug & Sept months##
Boxplot(Trend.x~Month.x, data=TmaxTminchirpsFulljoin.cool, id=list(method="y"))
Boxplot(Trend.y~Month.y, data=TmaxTminchirpsFulljoin.cool, id=list(method="y"))
Boxplot(Trend~Month, data=TmaxTminchirpsFulljoin.cool, id=list(method="y"))
scatterplot(Trend.y~Trend.x | Month.y, regLine=FALSE, smooth=FALSE, boxplots=FALSE, by.groups=TRUE, data=TmaxTminchirpsFulljoin.cool)
scatterplot(Trend.x~Trend | Month.y, regLine=FALSE, smooth=FALSE, boxplots=FALSE, by.groups=TRUE, data=TmaxTminchirpsFulljoin.cool)
scatterplot(Trend.y~Trend | Month.y, regLine=FALSE, smooth=FALSE, boxplots=FALSE, by.groups=TRUE, data=TmaxTminchirpsFulljoin.cool)


##test correlations with Kendal Tau
with(TmaxTminchirpsFulljoin.cool, cor.test(Trend.x, Trend,alternative="two.sided", method="kendall"))
with(TmaxTminchirpsFulljoin.cool, cor.test(Trend.y, Trend,alternative="two.sided", method="kendall"))
with(TmaxTminchirpsFulljoin.cool, cor.test(Trend.x, Trend.y,alternative="two.sided", method="kendall"))

##Plot ggscatter for Warming and cooling trends
# Rainfall vs Tmax
TmaxChirps.Cooling = ggscatter(TmaxTminchirpsFulljoin.cool, x = "Trend", y = "Trend.x",
                xlab = expression(Rainfall~(mm~Month^{-1}~year^{-1})), ylab =expression(Tmax~(C^{o}~Month^{-1}~year^{-1})), 
                add = "reg.line", 
                add.params = list(color = "mergeID2.x", fill='black'), # Add regression line
                conf.int = TRUE,                # Add confidence interval
                color = "mergeID2.x", palette = "heat.colors", # Color by groups 
                shape =  19, size = 2, point = T, 
                legend='top', legend.title = "Month+AEZ" # Change point shape by groups 
                )+
  stat_cor(aes(color = mergeID2.x), method = "kendall", label.x = -0.048, label.x.npc = "right", label.y.npc = "top")      # Add correlation coefficient
TmaxChirps.Cooling

# Rainfall vs Tmin
TminChirps.Cooling = ggscatter(TmaxTminchirpsFulljoin.cool, x = "Trend", y = "Trend.y",
                               xlab = expression(Rainfall~(mm~Month^{-1}~year^{-1})), ylab = expression(Tmin~(C^{o}~Month^{-1}~year^{-1})), 
                               add = "reg.line", 
                               add.params = list(color = "mergeID2.x", fill='black'), # Add regression line
                               conf.int = TRUE,                # Add confidence interval
                               color = "mergeID2.x", palette = "heat.colors", # Color by groups 
                               shape =  19, size = 2, point = T, 
                               legend='top', legend.title = "Month+AEZ"                  # Change point shape by groups
)+
  stat_cor(aes(color = mergeID2.x), method = "kendall", label.x = -0.038, label.x.npc = "centre", label.y.npc = "top")      # Add correlation coefficient
TminChirps.Cooling

##ggarange in multi-panel

####Arrange many panels####
ggarrange(TminChirps.Cooling,TmaxChirps.Cooling,labels = c("a", "b"),ncol = 2, nrow = 1)


##Bubble plot
# Scatterplot
theme_set(theme_bw())  # pre-set the bw theme.
TminTmaxChirps.bublep <- ggplot(TmaxTminchirpsFulljoin.cool, aes(Trend.x, Trend.y)) + 
  labs(subtitle="Tmax vs Tmin",
       title="Bubble chart")

TminTmaxChirps.bublep + geom_jitter(aes(col=mergeID2.x, size=Trend)) + 
  geom_smooth(aes(col=mergeID2.x), method="lm", se=T, fill='black')
