## ------------------------------------------------------------------------
# the location
MyFile='https://github.com/eScienceUW-WinterSchool-2020/RSession/raw/master/hdidemocia.RDS'


# if location is website:
MyFile=url(MyFile)

# get the data:
fromPy=readRDS(file = MyFile) # no need for a library
row.names(fromPy)=NULL   # reset indexes from Python.
# View(fromPy)


## ------------------------------------------------------------------------
str(fromPy)  # less space: width = 70,strict.width='cut'


## ------------------------------------------------------------------------
# you could get more than one
fromPy[fromPy$Humandevelopmentindex==max(fromPy$Humandevelopmentindex),]


## ------------------------------------------------------------------------
#or
fromPy[fromPy$Humandevelopmentindex==max(fromPy$Humandevelopmentindex),'Country']


## ------------------------------------------------------------------------
#or
fromPy[which.max(fromPy$Humandevelopmentindex),'Country']


## ------------------------------------------------------------------------
# from AMERICA:
AMERICA=c('South America','North America')
subAmerica=fromPy[fromPy$Continent %in% AMERICA,]
subAmerica[which.max(subAmerica$Humandevelopmentindex),'Country']


## ------------------------------------------------------------------------
library(magrittr)
library(dplyr)

fromPy%>%
    filter(Continent %in% AMERICA)%>%
    filter(Humandevelopmentindex==max(Humandevelopmentindex))%>%
    select(Country)


## ------------------------------------------------------------------------
# from AMERICA:
AMERICA=c('South America','North America')
subNotAmerica=fromPy[!fromPy$Continent %in% AMERICA,]
subNotAmerica[which.max(subNotAmerica$Humandevelopmentindex),'Country']


## ------------------------------------------------------------------------
fromPy%>%
    filter(!Continent %in% AMERICA)%>%
    filter(Humandevelopmentindex==max(Humandevelopmentindex))%>%
    select(Country)


## ------------------------------------------------------------------------
aggregate(data=fromPy,Humandevelopmentindex ~ Continent,FUN=mean)

## ------------------------------------------------------------------------
fromPy%>%
    group_by(Continent) %>% 
    summarise(meanHDI = mean(Humandevelopmentindex))


## ------------------------------------------------------------------------
aggregate(data=fromPy,
          cbind(Electoralprocessandpluralism,Functioningofgovernment,
                Politicalparticipation,Politicalculture,
                Civilliberties)~Continent,
          FUN=median)


## ------------------------------------------------------------------------
aggregate(data=fromPy[,c(8:12,14)],.~Continent,FUN=median)


## ------------------------------------------------------------------------
fromPy[,c(8:12,14)]%>%
    group_by(Continent) %>% 
    summarise_all(list(median, sd))


## ------------------------------------------------------------------------
fromPy$HDIdico=ifelse(fromPy$Humandevelopmentindex>
                          median(fromPy$Humandevelopmentindex),
                      1,0)


## ------------------------------------------------------------------------
fromPy%>%
    mutate(HDIdico = ifelse(Humandevelopmentindex>median(Humandevelopmentindex),
                            1, 0))%>%
    select(Country,HDIdico)


## ---- eval=TRUE----------------------------------------------------------
subDemo=fromPy[,c(8:12)]


## ------------------------------------------------------------------------
names(subDemo)


## ---- eval=TRUE----------------------------------------------------------
library(lavaan)

model='
democracy=~Electoralprocessandpluralism + Functioningofgovernment + Politicalparticipation + Politicalculture + Civilliberties
'

fitNUM<-cfa(model, data = subDemo,std.lv=TRUE)
indexCFA=lavPredict(fitNUM)


## ------------------------------------------------------------------------
head(indexCFA,20)


## ---- eval=TRUE----------------------------------------------------------
library(BBmisc)
indexCFANorm=normalize(indexCFA, 
                       method = "range", 
                       margin=2, # by column
                       range = c(0, 10))


## ---- eval=TRUE----------------------------------------------------------
fromPy$dem_FA=as.vector(indexCFANorm)
# class(indexCFANorm)
# dim(indexCFANorm)
# class(c(indexCFANorm))
# dim(as.vector(indexCFANorm))


## ---- eval=TRUE----------------------------------------------------------
library(ggplot2)

base=ggplot(data=fromPy)
base + geom_histogram(aes(x=dem_FA))


## ------------------------------------------------------------------------
base + geom_boxplot(aes(y=dem_FA))


## ------------------------------------------------------------------------
evalCFA1=parameterEstimates(fitNUM, standardized =TRUE)


## ----echo=FALSE----------------------------------------------------------
evalCFA1[evalCFA1$op=="=~",c('rhs','std.all','pvalue')]


## ---- echo=FALSE---------------------------------------------------------
evalCFA2=as.list(fitMeasures(fitNUM))


## ------------------------------------------------------------------------
evalCFA2[c("chisq", "df", "pvalue")] 


## ----echo=FALSE----------------------------------------------------------
evalCFA2$tli # > 0.90


## ----echo=FALSE----------------------------------------------------------
evalCFA2[c( 'rmsea.ci.lower','rmsea','rmsea.ci.upper')] 


## ---- echo=FALSE---------------------------------------------------------
library(semPlot)
semPaths(fitNUM, what='std', nCharNodes=10, sizeMan=8,
         edge.label.cex=1.5, fade=T,residuals = F)


## ------------------------------------------------------------------------
dfClus=fromPy[,c(2,13,16)]


## ------------------------------------------------------------------------
#from
head(dfClus)


## ------------------------------------------------------------------------
#to
row.names(dfClus)=fromPy$Country
head(dfClus)


## ------------------------------------------------------------------------
dfClus=dfClus[complete.cases(dfClus),]


## ------------------------------------------------------------------------
library(cluster)
dfClus_D=cluster::daisy(x=dfClus,metric="gower")


## ------------------------------------------------------------------------
set.seed(123)
numberOfClusters=4
res.pam = pam(x=dfClus_D,k = numberOfClusters,cluster.only = F)


## ------------------------------------------------------------------------
fromPy$pam=as.factor(res.pam$clustering)


## ------------------------------------------------------------------------
fromPy[fromPy$pam==1,'Country']


## ------------------------------------------------------------------------
fromPy[fromPy$Country=="Peru",'pam']


## ------------------------------------------------------------------------
pamEval=as.data.frame.array(silhouette(res.pam))
pamEval$country=row.names(pamEval)
row.names(pamEval)=NULL
aggregate(data=pamEval,sil_width~cluster,FUN=mean)


## ------------------------------------------------------------------------
head(pamEval)


## ------------------------------------------------------------------------
pamEval[pamEval$sil_width<0,]


## ------------------------------------------------------------------------
pamEval_O=pamEval[order(pamEval$cluster,pamEval$sil_width),]


## ------------------------------------------------------------------------
clusterVal=1
pamEval=pamEval_O[pamEval_O$cluster==clusterVal,]

base=ggplot(data=pamEval,
             aes(x=country,
                 y=sil_width))
sil1= base + geom_bar(stat='identity')
sil1= sil1 + scale_x_discrete(limits=pamEval$country)
sil1= sil1 + theme(axis.text.x = element_text(angle = 80,
                                              size = 6,
                                              hjust = 1))
sil1

## ------------------------------------------------------------------------
clusterVal=2
pamEval=pamEval_O[pamEval_O$cluster==clusterVal,]

base=ggplot(data=pamEval,
             aes(x=country,
                 y=sil_width))
sil2= base + geom_bar(stat='identity',fill='grey')
sil2= sil2 + scale_x_discrete(limits=pamEval$country)
sil2= sil2 + theme(axis.text.x = element_text(angle = 80,
                                              size = 6,
                                              hjust = 1))
sil2= sil2 + labs(x=NULL)
sil2


## ------------------------------------------------------------------------
library(ggpubr)

ggarrange(sil1,sil2,ncol = 1)


## ------------------------------------------------------------------------
# installed?
#library(sp)
#library(geojsonio)
library(rgdal)

fromGit="https://github.com/eScienceUW-WinterSchool-2020/RSession/raw/master/map/world_map.json" # link desde github


mapWorld <- rgdal::readOGR(fromGit,stringsAsFactors = FALSE)


## ---- eval=TRUE----------------------------------------------------------
plot(mapWorld)


## ---- eval=TRUE----------------------------------------------------------
# see data in map
head(mapWorld@data)


## ---- eval=TRUE----------------------------------------------------------
mapWorldAll=merge(mapWorld, #map first
                   fromPy, 
                   by.x='NAME', # common column
                   by.y='Country', # common column
                   all.x=F) # reduced map.



## ---- eval=TRUE----------------------------------------------------------
# what:
varToPlot=mapWorldAll$pam

#which colors:
library(RColorBrewer)
colorForScale='YlOrRd'
palette = brewer.pal(numberOfClusters, colorForScale)

# plotting:

## base layer - coloring missing data
plot(mapWorld,col='grey',border=0) 

## top layer
plot(mapWorldAll, col = palette[varToPlot],border=F,add=T)


legend('left', legend = c("TOP","GOOD","BAD","POOR"), 
       fill = palette,
       cex = 0.6, 
       bty = "n",
       title="Clusters")


## ------------------------------------------------------------------------
# hypothesis 1:

# The more democratic and the better HDI 
# the less contaminated a country is, 
# controlling continent
#row.names(fromPy)=fromPy$Country
hypo1=formula(co2_in_MT~ dem_FA + Humandevelopmentindex + Continent)
regre1=glm(hypo1,data = fromPy,family = 'gaussian')


## ------------------------------------------------------------------------
summary(regre1)


## ------------------------------------------------------------------------
library(sjPlot)

plot_models(regre1,vline.color = "grey")


## ------------------------------------------------------------------------
# hypothesis 2:

# The democracy and level of contamination
# affect  the leve level of human development,
# controlling continent

hypo2=formula(HDIdico~ dem_FA + co2_in_MT + Continent)
regre2=glm(hypo2,data = fromPy,family = "binomial")


## ------------------------------------------------------------------------
summary(regre2)


## ------------------------------------------------------------------------
# interpracion usando marginal effects:
library(margins)
# 
(model = margins(regre2))

## ------------------------------------------------------------------------
(margins=summary(model))


## ------------------------------------------------------------------------

base= ggplot(margins,aes(x=factor, y=AME)) + geom_point()
plot2 = base + theme(axis.text.x = element_text(angle = 80,
                                              size = 6,
                                              hjust = 1))
plot2    

## ------------------------------------------------------------------------
plot2 +  geom_errorbar(aes(ymin=lower, ymax=upper))

