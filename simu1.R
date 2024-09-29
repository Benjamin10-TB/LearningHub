```{r include=FALSE}
#######Install packages
library(maps)
library(ggplot2)
library(MASS)
library(foreign)
library(shapefiles)
library(sp)
library(gstat)
library(spdep)
library(splines)
library(spdep)
library(geos)
library(RColorBrewer)
library(raster)
library(dismo)
library(dplyr)
library(ggmap)
library(grid)
library(gridGeometry)
library(ape)
library(fields)
library(rmarkdown)

```
```{r}
# unconditional simulations on a 100 x 100 grid using gstat
# create structure

set.seed(88888, kind = NULL, normal.kind = NULL, sample.kind = NULL)

bay=expand.grid(1:100, 1:100)
names(bay)=c("x","y")

# define the gstat object (spatial model)
g.dummy=gstat(formula=z~1, locations=~x+y, dummy=T, beta=1,model=vgm(psill=0.5,model="Gau",nugget=0.05, range=12))

# make a number of simulations
baye1=predict(g.dummy, newdata=bay,nsim=1000)
 #[using unconditional Gaussian simulation]

#calculate rowmeans of the realizations
######################################################################################
#abu=baye1[,3:12]
#baye1$


##export to csv
#write.csv(baye1,"C:\\Users\\User\\Desktop\\hello\\ben.csv", row.names = T)


```
```{r}
##import data bact to r
mydata=read.csv(file="C:\\Users\\BENJAMIN TOMMY BAVUG\\Desktop\\BenThesis\\Documents_sss\\hello\\datanew.csv", header=TRUE, sep=",")

simulated_data=mydata[,3]
hist(simulated_data)
gridded(mydata) = ~x+y

spplot(mydata,main="Random Fields From Spherical  Model")
##################################################################################################################
#fitting Spherical variogram to simav data
kk=variogram(simav~1,data=mydata)

kk1=fit.variogram(kk, vgm("Sph"))
kk1
plot(kk, model=kk1, main="Spherical Model on Referenced Data", col="green")
#################################################################################################################
#fiting Gaussian variogram to sim2 data
hh=variogram(simav~1,data=mydata)

hh1=fit.variogram(hh, vgm("Gau"))
hh1
plot(hh, model=hh1, main="Gaussian Model on Referenced data", col="red")
#################################################################################################################

```

```{r}

##Random sampling scheme
#set.seed(2000, kind = NULL, normal.kind = NULL, sample.kind = NULL)

#dk=spsample(mydata, n=50, type='random',pretty=TRUE)

#export dk sample into excel sheet

#write.csv(dk,"C:\\Users\\BENJAMIN TOMMY BAVUG\\Desktop\\BenThesis\\Documents_sss\\hello\\ss50.csv", row.names = T)

#srs.w=read.csv(file="C:\\Users\\User\\Desktop\\hello\\radom\\SRS50.csv", header=TRUE, sep=",")

#plot(mydata,main="Random Sampling");points(dk, col='black', pch=3, cex=0.9)

#######################################################################################
#1. fiting Gaussian variogram to srs data
sys.w1=read.csv(file="C:\\Users\\BENJAMIN TOMMY BAVUG\\Desktop\\BenThesis\\Documents_sss\\hello\\radom\\SRS50.csv", header=TRUE, sep=",")
################################################
#Histogram
SRS50=sys.w1[,3]
hist(SRS50)
summary(SRS50)
################################################
gridded(sys.w1)=~x+y
bb=variogram(SRS1~1,data=sys.w1)
bb1=fit.variogram(bb,vgm("Sph"))
print(bb1)
plot(bb, model=bb1, main="Spherical Model(On SRS sampling of size 50)", col="red")

bb1=fit.variogram(bb,vgm("Gau"))
print(bb1)
plot(bb, model=bb1, main="Gaussian Model(On SRS sampling of size 50)", col="green")

bb1=fit.variogram(bb,vgm("Exp"))
print(bb1)
plot(bb, model=bb1, main="Exponential Model(On SRS sampling of size 50)", col="blue")

bb1=fit.variogram(bb,vgm("Mat"))
print(bb1)
plot(bb, model=bb1, main="Matern Model(On SRS sampling of size 50)", col="purple")

#KRIGING
mydata=read.csv(file="C:\\Users\\BENJAMIN TOMMY BAVUG\\Desktop\\BenThesis\\Documents_sss\\hello\\datanew.csv", header=TRUE, sep=",")
gridded(mydata) = ~x+y
m=vgm(.0004, "Sph", 19, 0)
jj=krige(simav~1,mydata,sys.w1, m)
spplot(jj["var1.pred"],colorkey=TRUE, main = "ordinary kriging predictions")
spplot(jj["var1.var"],  main = "ordinary kriging variance")


```


```{r}
#2. fitting Gaussian variogram to srs data
sys.w2=read.csv(file="C:\\Users\\BENJAMIN TOMMY BAVUG\\Desktop\\BenThesis\\Documents_sss\\hello\\radom\\SRS150.csv", header=TRUE, sep=",")
################################################
#Histogram
SRS150=sys.w2[,3]
hist(SRS150)
summary(SRS150)
################################################

gridded(sys.w2)=~x+y
cc=variogram(SRS2~1,data=sys.w2)
cc1=fit.variogram(cc,vgm("Sph"))
print(cc1)
plot(cc, model=cc1, main="Spherical Model(On SRS sampling of size 150)", col="red")

cc1=fit.variogram(cc,vgm("Gau"))
print(cc1)
plot(cc, model=cc1, main="Gaussian Model(On SRS sampling of size 150)", col="red")
###############################################################

#KRIGING
mydata=read.csv(file="C:\\Users\\BENJAMIN TOMMY BAVUG\\Desktop\\BenThesis\\Documents_sss\\hello\\datanew.csv", header=TRUE, sep=",")
gridded(mydata) = ~x+y
m=vgm(.00024, "Sph", 11, .000087)
jj=krige(simav~x+y,mydata, sys.w2, m)
spplot(jj["var1.pred"],colorkey=TRUE, main = "universal kriging predictions")
spplot(jj["var1.var"],  main = "universal kriging variance")

```
```{r}
################################################################
#3. fiting Gaussian variogram to srs data
sys.w3=read.csv(file="C:\\Users\\BENJAMIN TOMMY BAVUG\\Desktop\\BenThesis\\Documents_sss\\hello\\radom\\SRS500.csv", header=TRUE, sep=",")
################################################
#Histogram
SRS500=sys.w3[,3]
hist(SRS500)
summary(SRS500)
###############################################

gridded(sys.w3)=~x+y
dd=variogram(SRS3~1,data=sys.w3)
dd1=fit.variogram(dd,vgm("Sph"))
dd1
plot(dd, model=dd1, main="Spherical Model(On SRS sampling of size 500)", col="red")

#KRIGING
mydata=read.csv(file="C:\\Users\\BENJAMIN TOMMY BAVUG\\Desktop\\BenThesis\\Documents_sss\\hello\\datanew.csv", header=TRUE, sep=",")
gridded(mydata) = ~x+y
m=vgm(.00025, "Sph", 23, .0001)
jj=krige(simav~x+y,mydata, sys.w3, m)
spplot(jj["var1.pred"],colorkey=TRUE, main = "universal kriging predictions")
spplot(jj["var1.var"],  main = "universal kriging variance")


```
```{r}
################################################################
#4. fiting Gaussian variogram to srs data
sys.w4=read.csv(file="C:\\Users\\BENJAMIN TOMMY BAVUG\\Desktop\\BenThesis\\Documents_sss\\hello\\radom\\SRS800.csv", header=TRUE, sep=",")
################################################
#Histogram
SRS800=sys.w4[,3]
hist(SRS800)
summary(SRS800)
###############################################

gridded(sys.w4)=~x+y
dd=variogram(SRS4~1,data=sys.w4)
dd1=fit.variogram(dd,vgm("Sph"))
dd1
plot(dd, model=dd1, main="Spherical Model(On SRS sampling of size 800)", col="red")

dd1=fit.variogram(dd,vgm("Gau"))
dd1
plot(dd, model=dd1, main="Gaussian Model(On SRS sampling of size 800)", col="green")

#KRIGING
mydata=read.csv(file="C:\\Users\\BENJAMIN TOMMY BAVUG\\Desktop\\BenThesis\\Documents_sss\\hello\\datanew.csv", header=TRUE, sep=",")
gridded(mydata) = ~x+y
m=vgm(.0003, "Sph", 20, .000037)
jj=krige(simav~x+y,mydata, sys.w4, m)
spplot(jj["var1.pred"],colorkey=TRUE, main = "universal kriging predictions")
spplot(jj["var1.var"],  main = "universal kriging variance")

```
```{r}
################################################################
#5. fiting Gaussian variogram to srs data
sys.w5=read.csv(file="C:\\Users\\BENJAMIN TOMMY BAVUG\\Desktop\\BenThesis\\Documents_sss\\hello\\radom\\SRS1000.csv", header=TRUE, sep=",")
################################################
#Histogram
SRS1000=sys.w5[,3]
hist(SRS1000)
summary(SRS1000)
###############################################

gridded(sys.w5)=~x+y
dd=variogram(SRS5~1,data=sys.w5)
dd1=fit.variogram(dd,vgm("Sph"))
dd1
plot(dd, model=dd1, main="Spherical Model(On SRS sampling of size 1000)", col="red")

dd1=fit.variogram(dd,vgm("Gau"))
dd1
plot(dd, model=dd1, main="Gaussian Model(On SRS sampling of size 1000)", col="green")

#KRIGING
mydata=read.csv(file="C:\\Users\\BENJAMIN TOMMY BAVUG\\Desktop\\BenThesis\\Documents_sss\\hello\\datanew.csv", header=TRUE, sep=",")
gridded(mydata) = ~x+y
m=vgm(.0003, "Sph", 21, .000056)
jj=krige(simav~x+y,mydata, sys.w5, m)
spplot(jj["var1.pred"],colorkey=TRUE, main = "universal kriging predictions")
spplot(jj["var1.var"],  main = "universal kriging variance")

```
```{r}
################################################################
#6. fiting Gaussian variogram to srs data
sys.w6=read.csv(file="C:\\Users\\BENJAMIN TOMMY BAVUG\\Desktop\\BenThesis\\Documents_sss\\hello\\radom\\SRS1500.csv", header=TRUE, sep=",")
################################################
#Histogram
SRS1500=sys.w6[,3]
hist(SRS1500)
summary(SRS1500)
###############################################

gridded(sys.w6)=~x+y
dd=variogram(SRS6~1,data=sys.w6)
dd1=fit.variogram(dd,vgm("Sph"))
dd1
plot(dd, model=dd1, main="Spherical Model(On SRS sampling of size 1500)", col="red")

dd1=fit.variogram(dd,vgm("Gau"))
dd1
plot(dd, model=dd1, main="Gaussian Model(On SRS sampling of size 1500)", col="green")

#KRIGING
mydata=read.csv(file="C:\\Users\\BENJAMIN TOMMY BAVUG\\Desktop\\BenThesis\\Documents_sss\\hello\\datanew.csv", header=TRUE, sep=",")
gridded(mydata) = ~x+y
m=vgm(.00034, "Sph", 22, .000045)
jj=krige(simav~x+y,mydata, sys.w6, m)
spplot(jj["var1.pred"],colorkey=TRUE, main = "universal kriging predictions")
spplot(jj["var1.var"],  main = "universal kriging variance")

```
```{r}
################################################################
#7. fiting Gaussian variogram to srs data
sys.w7=read.csv(file="C:\\Users\\BENJAMIN TOMMY BAVUG\\Desktop\\BenThesis\\Documents_sss\\hello\\radom\\SRS2000.csv", header=TRUE, sep=",")
################################################
#Histogram
SRS2000=sys.w7[,3]
hist(SRS2000)
summary(SRS2000)
###############################################

gridded(sys.w7)=~x+y
dd=variogram(SRS7~1,data=sys.w7)
dd1=fit.variogram(dd,vgm("Sph"))
dd1
plot(dd, model=dd1, main="Spherical Model(On SRS sampling of size 2000)", col="red")

dd1=fit.variogram(dd,vgm("Gau"))
dd1
plot(dd, model=dd1, main="Gaussian Model(On SRS sampling of size 2000)", col="green")

#KRIGING
mydata=read.csv(file="C:\\Users\\BENJAMIN TOMMY BAVUG\\Desktop\\BenThesis\\Documents_sss\\hello\\datanew.csv", header=TRUE, sep=",")
gridded(mydata) = ~x+y
m=vgm(.00034, "Sph", 21, .000043)
jj=krige(simav~x+y,mydata, sys.w7, m)
spplot(jj["var1.pred"],colorkey=TRUE, main = "universal kriging predictions")
spplot(jj["var1.var"],  main = "universal kriging variance")


```
```{r}
################################################################
#8. fiting Gaussian variogram to srs data
sys.w8=read.csv(file="C:\\Users\\BENJAMIN TOMMY BAVUG\\Desktop\\BenThesis\\Documents_sss\\hello\\radom\\SRS2500.csv", header=TRUE, sep=",")
################################################
#Histogram
SRS2500=sys.w8[,3]
hist(SRS2500)
summary(SRS2500)
###############################################

gridded(sys.w8)=~x+y
dd=variogram(SRS8~1,data=sys.w8)
dd1=fit.variogram(dd,vgm("Sph"))
dd1
plot(dd, model=dd1, main="Spherical Model(On SRS sampling of size 2500)", col="red")

dd1=fit.variogram(dd,vgm("Gau"))
dd1
plot(dd, model=dd1, main="Gaussian Model(On SRS sampling of size 2500)", col="green")

dd1=fit.variogram(bb,vgm("Exp"))
print(dd1)
plot(dd, model=dd1, main="Exponential Model(On SRS sampling of size 2500)", col="blue")

bb1=fit.variogram(dd,vgm("Mat"))
print(dd1)
plot(dd, model=dd1, main="Matern Model(On SRS sampling of size 2500)", col="purple")

#KRIGING
mydata=read.csv(file="C:\\Users\\BENJAMIN TOMMY BAVUG\\Desktop\\BenThesis\\Documents_sss\\hello\\datanew.csv", header=TRUE, sep=",")
gridded(mydata) = ~x+y
m=vgm(.00034, "Sph", 21, .000049)
jj=krige(simav~x+y,mydata, sys.w8, m)
spplot(jj["var1.pred"],colorkey=TRUE, main = "universal kriging predictions")
spplot(jj["var1.var"],  main = "universal kriging variance")




# Non-stationarity
dd1 = fit.variogram(dd, vgm("Sph", trend = ~x+y))
plot(dd, model = dd1, main = "Spherical Model with Trend (On SRS sampling of size 2500)", col = "red")

# Anisotropy
dd1 = fit.variogram(dd, vgm("Sph", anis = c(0.5, 0.3, 20)))
plot(dd, model = dd1, main = "Spherical Model with Anisotropy (On SRS sampling of size 2500)", col = "green")

# Kriging with non-stationarity and anisotropy
m = vgm(0.00034, "Sph", 21, 0.000049, anis = c(0.5,0.3, 20))
jj = krige(simav ~ x+y, mydata, sys.w8, m)
spplot(jj["var1.pred"], colorkey = TRUE, main = "Universal Kriging Predictions with Non-stationarity and Anisotropy")
spplot(jj["var1.var"], main = "Universal Kriging Variance with Non-stationarity and Anisotropy")


```

```{r}

################################################################
##Random sampling scheme
#set.seed(8000, kind = NULL, normal.kind = NULL, sample.kind = NULL)

#dk=spsample(mydata, n=2500, type='random',pretty=TRUE)

#export dk sample into excel sheet

#write.csv(dk,"C:\\Users\\BENJAMIN TOMMY BAVUG\\Desktop\\BenThesis\\Documents_sss\\hello\\ss2500.csv", row.names = T)
#4. fiting Gaussian variogram to srs data
sys.w4=read.csv(file="C:\\Users\\BENJAMIN TOMMY BAVUG\\Desktop\\BenThesis\\Documents_sss\\hello\\radom\\SRS800.csv", header=TRUE, sep=",")
################################################
#Histogram
SRS800=sys.w4[,3]
hist(SRS800)
summary(SRS800)
###############################################

gridded(sys.w4)=~x+y
dd=variogram(SRS4~1,data=sys.w4)
dd1=fit.variogram(dd,vgm("Sph"))
dd1
plot(dd, model=dd1, main="Spherical Model(On SRS sampling of size 800)", col="red")

#KRIGING
mydata=read.csv(file="C:\\Users\\BENJAMIN TOMMY BAVUG\\Desktop\\BenThesis\\Documents_sss\\hello\\datanew.csv", header=TRUE, sep=",")
gridded(mydata) = ~x+y
m=vgm(.00031, "Sph", 20, .000037)
jj=krige(simav~x+y,mydata, sys.w4, m)
spplot(jj["var1.pred"],colorkey=TRUE, main = "universal kriging predictions")
spplot(jj["var1.var"],  main = "universal kriging variance")

```
```{r}
################################################################
##Systematic sampling scheme
#set.seed(8000, kind = NULL, normal.kind = NULL, sample.kind = NULL)

#dk=spsample(mydata, n=2500, type='regular',pretty=TRUE)

#export dk sample into excel sheet

#write.csv(dk,"C:\\Users\\BENJAMIN TOMMY BAVUG\\Desktop\\BenThesis\\Documents_sss\\hello\\sys2500.csv", row.names = T)

##################################################################################
# Read in the data
sys.w1 = read.csv(file="C:\\Users\\BENJAMIN TOMMY BAVUG\\Desktop\\BenThesis\\Documents_sss\\hello\\systematic\\syste50.csv", header=TRUE, sep=",")

# Histogram of the third column
syste50 = sys.w1[, 3]
hist(syste50)
summary(syste50)

# Convert the data to a spatial object
coordinates(sys.w1) <- ~x+y

# Compute the experimental variogram
cc <- variogram(SYS1 ~ 1, sys.w1)

# Fit a variogram model to the experimental variogram
cc1 <- fit.variogram(cc, model = vgm("Sph"))
cc1
# Plot the experimental and fitted variogram
plot(cc, model = cc1, main="Spherical Model(Systematic sampling with size 50)", col="red")

# Fit a variogram model to the experimental variogram
cc1 <- fit.variogram(cc, model = vgm("Gau"))
cc1
# Plot the experimental and fitted variogram
plot(cc, model = cc1, main="Gaussian Model(Systematic sampling with size 50)", col="green")

#KRIGING
mydata=read.csv(file="C:\\Users\\BENJAMIN TOMMY BAVUG\\Desktop\\BenThesis\\Documents_sss\\hello\\datanew.csv", header=TRUE, sep=",")

coordinates(mydata) = ~x+y
m=vgm(0.00034, "Sph", 13, 0.00038)
jj=krige(simav~x+y,mydata, sys.w1, m)
spplot(jj["var1.pred"],colorkey=TRUE, main = "Universal kriging predictions")
spplot(jj["var1.var"],  main = "Universal kriging variance")

```


```{r}

##################################################################################
# Read in the data
sys.w2 = read.csv(file="C:\\Users\\BENJAMIN TOMMY BAVUG\\Desktop\\BenThesis\\Documents_sss\\hello\\systematic\\syste150.csv", header=TRUE, sep=",")

# Histogram of the third column
syste150 = sys.w2[, 3]
hist(syste150)
summary(syste150)

# Convert the data to a spatial object
coordinates(sys.w2) <- ~x+y

# Compute the experimental variogram
cc <- variogram(SYS2 ~ 1, sys.w2)

# Fit a variogram model to the experimental variogram
cc1 <- fit.variogram(cc, model = vgm("Sph"))
cc1
# Plot the experimental and fitted variogram
plot(cc, model = cc1, main="Spherical Model(Systematic sampling with size 150)", col="red")

# Fit a variogram model to the experimental variogram
cc1 <- fit.variogram(cc, model = vgm("Gau"))
cc1
# Plot the experimental and fitted variogram
plot(cc, model = cc1, main="Gaussian Model(Systematic sampling with size 150)", col="green")

#KRIGING
mydata=read.csv(file="C:\\Users\\BENJAMIN TOMMY BAVUG\\Desktop\\BenThesis\\Documents_sss\\hello\\datanew.csv", header=TRUE, sep=",")

coordinates(mydata) = ~x+y
m=vgm(0.00035, "Sph", 20, 0.0000)
jj=krige(simav~x+y,mydata, sys.w2, m)
spplot(jj["var1.pred"],colorkey=TRUE, main = "Universal kriging predictions")
spplot(jj["var1.var"],  main = "Universal kriging variance")

```


```{r}

##################################################################################
# Read in the data
sys.w3 = read.csv(file="C:\\Users\\BENJAMIN TOMMY BAVUG\\Desktop\\BenThesis\\Documents_sss\\hello\\systematic\\syste500.csv", header=TRUE, sep=",")

# Histogram of the third column
syste500 = sys.w3[, 3]
hist(syste500)
summary(syste500)

# Convert the data to a spatial object
coordinates(sys.w3) <- ~x+y

# Compute the experimental variogram
cc <- variogram(SYS3 ~ 1, sys.w3)

# Fit a variogram model to the experimental variogram
cc1 <- fit.variogram(cc, model = vgm("Sph"))
cc1
# Plot the experimental and fitted variogram
plot(cc, model = cc1, main="Spherical Model(Systematic sampling with size 500)", col="red")

# Fit a variogram model to the experimental variogram
cc1 <- fit.variogram(cc, model = vgm("Gau"))
cc1
# Plot the experimental and fitted variogram
plot(cc, model = cc1, main="Gaussian Model(Systematic sampling with size 500)", col="green")

#KRIGING
mydata=read.csv(file="C:\\Users\\BENJAMIN TOMMY BAVUG\\Desktop\\BenThesis\\Documents_sss\\hello\\datanew.csv", header=TRUE, sep=",")

coordinates(mydata) = ~x+y
m=vgm(0.00038, "Sph", 19, 0.00004)
jj=krige(simav~x+y,mydata, sys.w3, m)
spplot(jj["var1.pred"],colorkey=TRUE, main = "Universal kriging predictions")
spplot(jj["var1.var"],  main = "Universal kriging variance")

```

```{r}

##################################################################################
# Read in the data
sys.w4 = read.csv(file="C:\\Users\\BENJAMIN TOMMY BAVUG\\Desktop\\BenThesis\\Documents_sss\\hello\\systematic\\syste800.csv", header=TRUE, sep=",")

# Histogram of the third column
syste800 = sys.w4[, 3]
hist(syste800)
summary(syste800)

# Convert the data to a spatial object
coordinates(sys.w4) <- ~x+y

# Compute the experimental variogram
cc <- variogram(SYS4 ~ 1, sys.w4)

# Fit a variogram model to the experimental variogram
cc1 <- fit.variogram(cc, model = vgm("Sph"))
cc1
# Plot the experimental and fitted variogram
plot(cc, model = cc1, main="Spherical Model(Systematic sampling with size 800)", col="red")

# Fit a variogram model to the experimental variogram
cc1 <- fit.variogram(cc, model = vgm("Gau"))
cc1
# Plot the experimental and fitted variogram
plot(cc, model = cc1, main="Gaussian Model(Systematic sampling with size 800)", col="green")

#KRIGING
mydata=read.csv(file="C:\\Users\\BENJAMIN TOMMY BAVUG\\Desktop\\BenThesis\\Documents_sss\\hello\\datanew.csv", header=TRUE, sep=",")

coordinates(mydata) = ~x+y
m=vgm(0.00035, "Sph", 21, 0.00001)
jj=krige(simav~x+y,mydata, sys.w4, m)
spplot(jj["var1.pred"],colorkey=TRUE, main = "Universal kriging predictions")
spplot(jj["var1.var"],  main = "Universal kriging variance")

```
```{r}
##################################################################################
# Read in the data
sys.w5 = read.csv(file="C:\\Users\\BENJAMIN TOMMY BAVUG\\Desktop\\BenThesis\\Documents_sss\\hello\\systematic\\syste1000.csv", header=TRUE, sep=",")

# Histogram of the third column
syste1000 = sys.w5[, 3]
hist(syste1000)
summary(syste1000)

# Convert the data to a spatial object
coordinates(sys.w5) <- ~x+y

# Compute the experimental variogram
cc <- variogram(SYS5 ~ 1, sys.w5)

# Fit a variogram model to the experimental variogram
cc1 <- fit.variogram(cc, model = vgm("Sph"))
cc1
# Plot the experimental and fitted variogram
plot(cc, model = cc1, main="Spherical Model(Systematic sampling with size 1000)", col="red")

# Fit a variogram model to the experimental variogram
cc1 <- fit.variogram(cc, model = vgm("Gau"))
cc1
# Plot the experimental and fitted variogram
plot(cc, model = cc1, main="Gaussian Model(Systematic sampling with size 1000)", col="green")

#KRIGING
mydata=read.csv(file="C:\\Users\\BENJAMIN TOMMY BAVUG\\Desktop\\BenThesis\\Documents_sss\\hello\\datanew.csv", header=TRUE, sep=",")

coordinates(mydata) = ~x+y
m=vgm(0.00042, "Sph", 20, 0.0000)
jj=krige(simav~x+y,mydata, sys.w5, m)
spplot(jj["var1.pred"],colorkey=TRUE, main = "Universal kriging predictions")
spplot(jj["var1.var"],  main = "Universal kriging variance")

```
```{r}
##################################################################################
# Read in the data
sys.w6 = read.csv(file="C:\\Users\\BENJAMIN TOMMY BAVUG\\Desktop\\BenThesis\\Documents_sss\\hello\\systematic\\syste1500.csv", header=TRUE, sep=",")

# Histogram of the third column
syste1500 = sys.w6[, 3]
hist(syste1500)
summary(syste1500)

# Convert the data to a spatial object
coordinates(sys.w6) <- ~x+y

# Compute the experimental variogram
cc <- variogram(SYS6 ~ 1, sys.w6)

# Fit a variogram model to the experimental variogram
cc1 <- fit.variogram(cc, model = vgm("Sph"))
cc1
# Plot the experimental and fitted variogram
plot(cc, model = cc1, main="Spherical Model(Systematic sampling with size 1500)", col="red")

# Fit a variogram model to the experimental variogram
cc1 <- fit.variogram(cc, model = vgm("Gau"))
cc1
# Plot the experimental and fitted variogram
plot(cc, model = cc1, main="Gaussian Model(Systematic sampling with size 1500)", col="green")

#KRIGING
mydata=read.csv(file="C:\\Users\\BENJAMIN TOMMY BAVUG\\Desktop\\BenThesis\\Documents_sss\\hello\\datanew.csv", header=TRUE, sep=",")

coordinates(mydata) = ~x+y
m=vgm(0.00033, "Sph", 21, 0.000037)
jj=krige(simav~x+y,mydata, sys.w6, m)
spplot(jj["var1.pred"],colorkey=TRUE, main = "Universal kriging predictions")
spplot(jj["var1.var"],  main = "Universal kriging variance")

```
```{r}
##################################################################################
# Read in the data
sys.w7 = read.csv(file="C:\\Users\\BENJAMIN TOMMY BAVUG\\Desktop\\BenThesis\\Documents_sss\\hello\\systematic\\syste2000.csv", header=TRUE, sep=",")

# Histogram of the third column
syste2000 = sys.w7[, 3]
hist(syste2000)
summary(syste2000)

# Convert the data to a spatial object
coordinates(sys.w7) <- ~x+y

# Compute the experimental variogram
cc <- variogram(SYS7 ~ 1, sys.w7)

# Fit a variogram model to the experimental variogram
cc1 <- fit.variogram(cc, model = vgm("Sph"))
cc1
# Plot the experimental and fitted variogram
plot(cc, model = cc1, main="Spherical Model(Systematic sampling with size 2000)", col="red")

# Fit a variogram model to the experimental variogram
cc1 <- fit.variogram(cc, model = vgm("Gau"))
cc1
# Plot the experimental and fitted variogram
plot(cc, model = cc1, main="Gaussian Model(Systematic sampling with size 2000)", col="green")

#KRIGING
mydata=read.csv(file="C:\\Users\\BENJAMIN TOMMY BAVUG\\Desktop\\BenThesis\\Documents_sss\\hello\\datanew.csv", header=TRUE, sep=",")

coordinates(mydata) = ~x+y
m=vgm(0.00034, "Sph", 21, 0.000059)
jj=krige(simav~x+y,mydata, sys.w7, m)
spplot(jj["var1.pred"],colorkey=TRUE, main = "Universal kriging predictions")
spplot(jj["var1.var"],  main = "Universal kriging variance")

```
```{r}
##################################################################################
# Read in the data
sys.w8 = read.csv(file="C:\\Users\\BENJAMIN TOMMY BAVUG\\Desktop\\BenThesis\\Documents_sss\\hello\\systematic\\syste2500.csv", header=TRUE, sep=",")

# Histogram of the third column
syste2500 = sys.w8[, 3]
hist(syste2500)
summary(syste2500)

# Convert the data to a spatial object
coordinates(sys.w8) <- ~x+y

# Compute the experimental variogram
cc <- variogram(SYS8 ~ 1, sys.w8)

# Fit a variogram model to the experimental variogram
cc1 <- fit.variogram(cc, model = vgm("Sph"))
cc1
# Plot the experimental and fitted variogram
plot(cc, model = cc1, main="Spherical Model(Systematic sampling with size 2500)", col="red")

# Fit a variogram model to the experimental variogram
cc1 <- fit.variogram(cc, model = vgm("Gau"))
cc1
# Plot the experimental and fitted variogram
plot(cc, model = cc1, main="Gaussian Model(Systematic sampling with size 2500)", col="green")

#KRIGING
mydata=read.csv(file="C:\\Users\\BENJAMIN TOMMY BAVUG\\Desktop\\BenThesis\\Documents_sss\\hello\\datanew.csv", header=TRUE, sep=",")

coordinates(mydata) = ~x+y
m=vgm(0.00034, "Sph", 22, 0.000021)
jj=krige(simav~x+y,mydata, sys.w8, m)
spplot(jj["var1.pred"],colorkey=TRUE, main = "Universal kriging predictions")
spplot(jj["var1.var"],  main = "Universal kriging variance")

```

```{r}
################################################################
##Cluster sampling scheme
#set.seed(8000, kind = NULL, normal.kind = NULL, sample.kind = NULL)

#dk=spsample(mydata, n=1000, type='nonaligned',pretty=TRUE)

#export dk sampled data into excel sheet

#write.csv(dk,"C:\\Users\\BENJAMIN TOMMY BAVUG\\Desktop\\BenThesis\\Documents_sss\\hello\\clu1000.csv", row.names = T)

#1. fiting Gaussian variogram to srs data
sys.w1=read.csv(file="C:\\Users\\BENJAMIN TOMMY BAVUG\\Desktop\\BenThesis\\Documents_sss\\hello\\cluster\\clus50.csv", header=TRUE, sep=",")

################################################
#Histogram
clus50=sys.w1[,3]
hist(clus50)
summary(clus50)
#################################################

coordinates(sys.w1)=~x+y
bb=variogram(CLUS1~1,data=sys.w1)
bb1=fit.variogram(bb,vgm("Sph"))
bb1
plot(bb, model=bb1, main="Spherical Model(On Cluster sampling of size 50)", col="red")

bb1=fit.variogram(bb,vgm("Gau"))
bb1
plot(bb, model=bb1, main="Gaussian Model(On Cluster sampling of size 50)", col="green")

#KRIGING
mydata=read.csv(file="C:\\Users\\BENJAMIN TOMMY BAVUG\\Desktop\\BenThesis\\Documents_sss\\hello\\datanew.csv", header=TRUE, sep=",")

coordinates(mydata) = ~x+y
m=vgm(.00036, "Sph", 14, .000)
jj=krige(simav~x+y,mydata, sys.w1, m)
spplot(jj["var1.pred"],colorkey=TRUE, main = "Universal kriging predictions")
spplot(jj["var1.var"],  main = "Universal kriging variance")

```
```{r}
#2. fiting Gaussian variogram to srs data
sys.w2=read.csv(file="C:\\Users\\BENJAMIN TOMMY BAVUG\\Desktop\\BenThesis\\Documents_sss\\hello\\cluster\\clus150.csv", header=TRUE, sep=",")

################################################
#Histogram
clus150=sys.w2[,3]
hist(clus150)
summary(clus150)
#################################################

gridded(sys.w2)=~x+y
bb=variogram(CLUS2~1,data=sys.w2)
bb1=fit.variogram(bb,vgm("Sph"))
bb1
plot(bb, model=bb1, main="Spherical Model(On Cluster sampling of size 150)", col="red")

bb1=fit.variogram(bb,vgm("Gau"))
bb1
plot(bb, model=bb1, main="Gaussian Model(On Cluster sampling of size 150)", col="green")

#KRIGING
mydata=read.csv(file="C:\\Users\\BENJAMIN TOMMY BAVUG\\Desktop\\BenThesis\\Documents_sss\\hello\\datanew.csv", header=TRUE, sep=",")

gridded(mydata) = ~x+y
m=vgm(.00033, "Sph", 21, .000029)
jj=krige(simav~x+y,mydata, sys.w2, m)
spplot(jj["var1.pred"],colorkey=TRUE, main = "Universal kriging predictions")
spplot(jj["var1.var"],  main = "Universal kriging variance")


```
```{r}
#3. fiting Gaussian variogram to srs data
sys.w3=read.csv(file="C:\\Users\\BENJAMIN TOMMY BAVUG\\Desktop\\BenThesis\\Documents_sss\\hello\\cluster\\clus500.csv", header=TRUE, sep=",")

################################################
#Histogram
clus500=sys.w3[,3]
hist(clus500)
summary(clus500)
#################################################

gridded(sys.w3)=~x+y
bb=variogram(CLUS3~1,data=sys.w3)
bb1=fit.variogram(bb,vgm("Sph"))
bb1
plot(bb, model=bb1, main="Spherical Model(On Cluster sampling of size 500)", col="red")

bb1=fit.variogram(bb,vgm("Gau"))
bb1
plot(bb, model=bb1, main="Gaussian Model(On Cluster sampling of size 500)", col="green")

#KRIGING
mydata=read.csv(file="C:\\Users\\BENJAMIN TOMMY BAVUG\\Desktop\\BenThesis\\Documents_sss\\hello\\datanew.csv", header=TRUE, sep=",")

gridded(mydata) = ~x+y
m=vgm(.00035, "Sph", 20, .000021)
jj=krige(simav~x+y,mydata, sys.w3, m)
spplot(jj["var1.pred"],colorkey=TRUE, main = "Universal kriging predictions")
spplot(jj["var1.var"],  main = "Universal kriging variance")


```
```{r}
#4. fiting Gaussian variogram to srs data
sys.w4=read.csv(file="C:\\Users\\BENJAMIN TOMMY BAVUG\\Desktop\\BenThesis\\Documents_sss\\hello\\cluster\\clus800.csv", header=TRUE, sep=",")

################################################
#Histogram
clus800=sys.w4[,3]
hist(clus800)
summary(clus800)
#################################################

gridded(sys.w4)=~x+y
bb=variogram(CLUS4~1,data=sys.w4)
bb1=fit.variogram(bb,vgm("Sph"))
bb1
plot(bb, model=bb1, main="Spherical Model(On Cluster sampling of size 800)", col="red")

bb1=fit.variogram(bb,vgm("Gau"))
bb1
plot(bb, model=bb1, main="Gaussian Model(On Cluster sampling of size 800)", col="green")

#KRIGING
mydata=read.csv(file="C:\\Users\\BENJAMIN TOMMY BAVUG\\Desktop\\BenThesis\\Documents_sss\\hello\\datanew.csv", header=TRUE, sep=",")

gridded(mydata) = ~x+y
m=vgm(.00035, "Sph", 21, .000034)
jj=krige(simav~x+y,mydata, sys.w4, m)
spplot(jj["var1.pred"],colorkey=TRUE, main = "Universal kriging predictions")
spplot(jj["var1.var"], colorkey=TRUE, main = "Universal kriging variance")


```
```{r}
#5. fiting Gaussian variogram to srs data
sys.w5=read.csv(file="C:\\Users\\BENJAMIN TOMMY BAVUG\\Desktop\\BenThesis\\Documents_sss\\hello\\cluster\\clus1000.csv", header=TRUE, sep=",")

################################################
#Histogram
clus1000=sys.w5[,3]
hist(clus1000)
summary(clus1000)
#################################################

gridded(sys.w5)=~x+y
bb=variogram(CLUS5~1,data=sys.w5)
bb1=fit.variogram(bb,vgm("Sph"))
bb1
plot(bb, model=bb1, main="Spherical Model(On Cluster sampling of size 1000)", col="red")

bb1=fit.variogram(bb,vgm("Gau"))
bb1
plot(bb, model=bb1, main="Gaussian Model(On Cluster sampling of size 1000)", col="green")

#KRIGING
mydata=read.csv(file="C:\\Users\\BENJAMIN TOMMY BAVUG\\Desktop\\BenThesis\\Documents_sss\\hello\\datanew.csv", header=TRUE, sep=",")

gridded(mydata) = ~x+y
m=vgm(.00035, "Sph", 20, .000029)
jj=krige(simav~x+y,mydata, sys.w5, m)
spplot(jj["var1.pred"],colorkey=TRUE, main = "Universal kriging predictions")
spplot(jj["var1.var"], colorkey=TRUE, main = "Universal kriging variance")


```
```{r}
#6. fiting Gaussian variogram to srs data
sys.w6=read.csv(file="C:\\Users\\BENJAMIN TOMMY BAVUG\\Desktop\\BenThesis\\Documents_sss\\hello\\cluster\\clus1500.csv", header=TRUE, sep=",")

################################################
#Histogram
clus1500=sys.w6[,3]
hist(clus1500)
summary(clus1500)
#################################################

gridded(sys.w6)=~x+y
bb=variogram(CLUS6~1,data=sys.w6)
bb1=fit.variogram(bb,vgm("Sph"))
bb1
plot(bb, model=bb1, main="Spherical Model(On Cluster sampling of size 1500)", col="red")

bb1=fit.variogram(bb,vgm("Gau"))
bb1
plot(bb, model=bb1, main="Gaussian Model(On Cluster sampling of size 1500)", col="green")

#KRIGING
mydata=read.csv(file="C:\\Users\\BENJAMIN TOMMY BAVUG\\Desktop\\BenThesis\\Documents_sss\\hello\\datanew.csv", header=TRUE, sep=",")

gridded(mydata) = ~x+y
m=vgm(.00034, "Sph", 20, .000034)
jj=krige(simav~x+y,mydata, sys.w6, m)
spplot(jj["var1.pred"],colorkey=TRUE, main = "Universal kriging predictions")
spplot(jj["var1.var"], colorkey=TRUE, main = "Universal kriging variance")



```
```{r}
#7. fiting Gaussian variogram to srs data
sys.w7=read.csv(file="C:\\Users\\BENJAMIN TOMMY BAVUG\\Desktop\\BenThesis\\Documents_sss\\hello\\cluster\\clus2000.csv", header=TRUE, sep=",")

################################################
#Histogram
clus2000=sys.w7[,3]
hist(clus2000)
summary(clus2000)
#################################################

gridded(sys.w7)=~x+y
bb=variogram(CLUS7~1,data=sys.w7)
bb1=fit.variogram(bb,vgm("Sph"))
bb1
plot(bb, model=bb1, main="Spherical Model(On Cluster sampling of size 2000)", col="red")

bb1=fit.variogram(bb,vgm("Gau"))
bb1
plot(bb, model=bb1, main="Gaussian Model(On Cluster sampling of size 2000)", col="green")

#KRIGING
mydata=read.csv(file="C:\\Users\\BENJAMIN TOMMY BAVUG\\Desktop\\BenThesis\\Documents_sss\\hello\\datanew.csv", header=TRUE, sep=",")

gridded(mydata) = ~x+y
m=vgm(.00033, "Sph", 21, .000038)
jj=krige(simav~x+y,mydata, sys.w7, m)
spplot(jj["var1.pred"],colorkey=TRUE, main = "Universal kriging predictions")
spplot(jj["var1.var"], colorkey=TRUE, main = "Universal kriging variance")


```

```{r}
#8. fiting Gaussian variogram to srs data
sys.w8=read.csv(file="C:\\Users\\BENJAMIN TOMMY BAVUG\\Desktop\\BenThesis\\Documents_sss\\hello\\cluster\\clus2500.csv", header=TRUE, sep=",")

################################################
#Histogram
clus2500=sys.w8[,3]
hist(clus2500)
summary(clus2500)
#################################################

gridded(sys.w8)=~x+y
bb=variogram(CLUS8~1,data=sys.w8)
bb1=fit.variogram(bb,vgm("Sph"))
bb1
plot(bb, model=bb1, main="Spherical Model(On Cluster sampling of size 2500)", col="red")

bb1=fit.variogram(bb,vgm("Gau"))
bb1
plot(bb, model=bb1, main="Gaussian Model(On Cluster sampling of size 2500)", col="green")

#KRIGING
mydata=read.csv(file="C:\\Users\\BENJAMIN TOMMY BAVUG\\Desktop\\BenThesis\\Documents_sss\\hello\\datanew.csv", header=TRUE, sep=",")

gridded(mydata) = ~x+y
m=vgm(.00033, "Sph", 21, .000038)
jj=krige(simav~x+y,mydata, sys.w8, m)
spplot(jj["var1.pred"],colorkey=TRUE, main = "Universal kriging predictions")
spplot(jj["var1.var"], colorkey=TRUE, main = "Universal kriging variance")

```

```{r}
##Stratified sampling scheme
#set.seed(8000, kind = NULL, normal.kind = NULL, sample.kind = NULL)

#dk=spsample(mydata, n=2500, type='stratified',pretty=TRUE)

#export dk sample into excel sheet

#write.csv(dk,"C:\\Users\\BENJAMIN TOMMY BAVUG\\Desktop\\BenThesis\\Documents_sss\\hello\\stra2500.csv", row.names = T)

#1. fiting Gaussian variogram to strs data
sys.w1=read.csv(file="C:\\Users\\BENJAMIN TOMMY BAVUG\\Desktop\\BenThesis\\Documents_sss\\hello\\stratified\\str50.csv", header=TRUE, sep=",")

################################################
#Histogram
STR50=sys.w1[,3]
hist(STR50)
summary(STR50)
############################################
gridded(sys.w1)=~x+y
bb=variogram(STR1~1,data=sys.w1)
rr1=fit.variogram(bb,vgm("Sph"))
rr1
plot(bb, model=bb1, main="Spherical Model(On stratified sampling of size 50)", col="red")

rr1=fit.variogram(bb,vgm("Gau"))
rr1
plot(bb, model=bb1, main="Gaussian Model(On stratified sampling of size 50)", col="green")

#KRIGING
mydata=read.csv(file="C:\\Users\\BENJAMIN TOMMY BAVUG\\Desktop\\BenThesis\\Documents_sss\\hello\\datanew.csv", header=TRUE, sep=",")
gridded(mydata) = ~x+y
m=vgm(.00027, "Sph", 16, .000092)
jj=krige(simav~x+y,mydata, sys.w1, m)
spplot(jj["var1.pred"],colorkey=TRUE, main = "universal kriging predictions")
spplot(jj["var1.var"],  main = "universal kriging variance")

```

```{r}
#2. fiting Gaussian variogram to strs data
sys.w2=read.csv(file="C:\\Users\\BENJAMIN TOMMY BAVUG\\Desktop\\BenThesis\\Documents_sss\\hello\\stratified\\str150.csv", header=TRUE, sep=",")

################################################
#Histogram
STR150=sys.w2[,3]
hist(STR150)
summary(STR150)
############################################
gridded(sys.w2)=~x+y
bb=variogram(STR2~1,data=sys.w2)
rr1=fit.variogram(bb,vgm("Sph"))
rr1
plot(bb, model=bb1, main="Spherical Model(On stratified sampling of size 150)", col="red")

rr1=fit.variogram(bb,vgm("Gau"))
rr1
plot(bb, model=bb1, main="Gaussian Model(On stratified sampling of size 150)", col="green")

#KRIGING
mydata=read.csv(file="C:\\Users\\BENJAMIN TOMMY BAVUG\\Desktop\\BenThesis\\Documents_sss\\hello\\datanew.csv", header=TRUE, sep=",")
gridded(mydata) = ~x+y
m=vgm(.00029, "Sph", 24, .000049)
jj=krige(simav~x+y,mydata, sys.w2, m)
spplot(jj["var1.pred"],colorkey=TRUE, main = "universal kriging predictions")
spplot(jj["var1.var"],  main = "universal kriging variance")

```
```{r}
#3. fiting Gaussian variogram to strs data
sys.w3=read.csv(file="C:\\Users\\BENJAMIN TOMMY BAVUG\\Desktop\\BenThesis\\Documents_sss\\hello\\stratified\\str500.csv", header=TRUE, sep=",")

################################################
#Histogram
STR500=sys.w3[,3]
hist(STR500)
summary(STR500)
############################################
gridded(sys.w3)=~x+y
bb=variogram(STR3~1,data=sys.w3)
rr1=fit.variogram(bb,vgm("Sph"))
rr1
plot(bb, model=bb1, main="Spherical Model(On stratified sampling of size 500)", col="red")

rr1=fit.variogram(bb,vgm("Gau"))
rr1
plot(bb, model=bb1, main="Gaussian Model(On stratified sampling of size 500)", col="green")

#KRIGING
mydata=read.csv(file="C:\\Users\\BENJAMIN TOMMY BAVUG\\Desktop\\BenThesis\\Documents_sss\\hello\\datanew.csv", header=TRUE, sep=",")
gridded(mydata) = ~x+y
m=vgm(.00029, "Sph", 21, .000074)
jj=krige(simav~x+y,mydata, sys.w3, m)
spplot(jj["var1.pred"],colorkey=TRUE, main = "universal kriging predictions")
spplot(jj["var1.var"],  main = "universal kriging variance")

```
```{r}
#4. fiting Gaussian variogram to strs data
sys.w4=read.csv(file="C:\\Users\\BENJAMIN TOMMY BAVUG\\Desktop\\BenThesis\\Documents_sss\\hello\\stratified\\str800.csv", header=TRUE, sep=",")

################################################
#Histogram
STR800=sys.w4[,3]
hist(STR800)
summary(STR800)
############################################
gridded(sys.w4)=~x+y
bb=variogram(STR4~1,data=sys.w4)
rr1=fit.variogram(bb,vgm("Sph"))
rr1
plot(bb, model=bb1, main="Spherical Model(On stratified sampling of size 800)", col="red")

rr1=fit.variogram(bb,vgm("Gau"))
rr1
plot(bb, model=bb1, main="Gaussian Model(On stratified sampling of size 800)", col="green")

#KRIGING
mydata=read.csv(file="C:\\Users\\BENJAMIN TOMMY BAVUG\\Desktop\\BenThesis\\Documents_sss\\hello\\datanew.csv", header=TRUE, sep=",")
gridded(mydata) = ~x+y
m=vgm(.00033, "Sph", 21, .000034)
jj=krige(simav~x+y,mydata, sys.w4, m)
spplot(jj["var1.pred"],colorkey=TRUE, main = "universal kriging predictions")
spplot(jj["var1.var"],  main = "universal kriging variance")

```
```{r}
#5. fiting Gaussian variogram to strs data
sys.w5=read.csv(file="C:\\Users\\BENJAMIN TOMMY BAVUG\\Desktop\\BenThesis\\Documents_sss\\hello\\stratified\\str1000.csv", header=TRUE, sep=",")

################################################
#Histogram
STR1000=sys.w5[,3]
hist(STR1000)
summary(STR1000)
############################################
gridded(sys.w5)=~x+y
bb=variogram(STR5~1,data=sys.w5)
rr1=fit.variogram(bb,vgm("Sph"))
rr1
plot(bb, model=bb1, main="Spherical Model(On stratified sampling of size 1000)", col="red")

rr1=fit.variogram(bb,vgm("Gau"))
rr1
plot(bb, model=bb1, main="Gaussian Model(On stratified sampling of size 1000)", col="green")

#KRIGING
mydata=read.csv(file="C:\\Users\\BENJAMIN TOMMY BAVUG\\Desktop\\BenThesis\\Documents_sss\\hello\\datanew.csv", header=TRUE, sep=",")
gridded(mydata) = ~x+y
m=vgm(.00036, "Sph", 21, .000029)
jj=krige(simav~x+y,mydata, sys.w5, m)
spplot(jj["var1.pred"],colorkey=TRUE, main = "universal kriging predictions")
spplot(jj["var1.var"],  main = "universal kriging variance")

```
```{r}
#6. fiting Gaussian variogram to strs data
sys.w6=read.csv(file="C:\\Users\\BENJAMIN TOMMY BAVUG\\Desktop\\BenThesis\\Documents_sss\\hello\\stratified\\str1500.csv", header=TRUE, sep=",")

################################################
#Histogram
STR1500=sys.w6[,3]
hist(STR1500)
summary(STR1500)
############################################
gridded(sys.w6)=~x+y
bb=variogram(STR6~1,data=sys.w6)
rr1=fit.variogram(bb,vgm("Sph"))
rr1
plot(bb, model=bb1, main="Spherical Model(On stratified sampling of size 1500)", col="red")

rr1=fit.variogram(bb,vgm("Gau"))
rr1
plot(bb, model=bb1, main="Gaussian Model(On stratified sampling of size 1500)", col="green")

#KRIGING
mydata=read.csv(file="C:\\Users\\BENJAMIN TOMMY BAVUG\\Desktop\\BenThesis\\Documents_sss\\hello\\datanew.csv", header=TRUE, sep=",")
gridded(mydata) = ~x+y
m=vgm(.00034, "Sph", 21, .000029)
jj=krige(simav~x+y,mydata, sys.w6, m)
spplot(jj["var1.pred"],colorkey=TRUE, main = "universal kriging predictions")
spplot(jj["var1.var"],  main = "universal kriging variance")

```
```{r}
#7. fiting Gaussian variogram to strs data
sys.w7=read.csv(file="C:\\Users\\BENJAMIN TOMMY BAVUG\\Desktop\\BenThesis\\Documents_sss\\hello\\stratified\\str2000.csv", header=TRUE, sep=",")

################################################
#Histogram
STR2000=sys.w7[,3]
hist(STR2000)
summary(STR2000)
############################################
gridded(sys.w7)=~x+y
bb=variogram(STR7~1,data=sys.w7)
rr1=fit.variogram(bb,vgm("Sph"))
rr1
plot(bb, model=bb1, main="Spherical Model(On stratified sampling of size 2000)", col="red")

rr1=fit.variogram(bb,vgm("Gau"))
rr1
plot(bb, model=bb1, main="Gaussian Model(On stratified sampling of size 2000)", col="green")

#KRIGING
mydata=read.csv(file="C:\\Users\\BENJAMIN TOMMY BAVUG\\Desktop\\BenThesis\\Documents_sss\\hello\\datanew.csv", header=TRUE, sep=",")
gridded(mydata) = ~x+y
m=vgm(.00034, "Sph", 21, .000035)
jj=krige(simav~x+y,mydata, sys.w7, m)
spplot(jj["var1.pred"],colorkey=TRUE, main = "universal kriging predictions")
spplot(jj["var1.var"],  main = "universal kriging variance")

```

```{r}
#8. fiting Gaussian variogram to strs data
sys.w8=read.csv(file="C:\\Users\\BENJAMIN TOMMY BAVUG\\Desktop\\BenThesis\\Documents_sss\\hello\\stratified\\str2500.csv", header=TRUE, sep=",")

################################################
#Histogram
STR2500=sys.w8[,3]
hist(STR2500)
summary(STR2500)
############################################
gridded(sys.w8)=~x+y
bb=variogram(STR8~1,data=sys.w8)
rr1=fit.variogram(bb,vgm("Sph"))
rr1
plot(bb, model=bb1, main="Spherical Model(On stratified sampling of size 2500)", col="red")

rr1=fit.variogram(bb,vgm("Gau"))
rr1
plot(bb, model=bb1, main="Gaussian Model(On stratified sampling of size 2500)", col="green")

#KRIGING
mydata=read.csv(file="C:\\Users\\BENJAMIN TOMMY BAVUG\\Desktop\\BenThesis\\Documents_sss\\hello\\datanew.csv", header=TRUE, sep=",")
gridded(mydata) = ~x+y
m=vgm(.00034, "Sph", 21, .000039)
jj=krige(simav~x+y,mydata, sys.w8, m)
spplot(jj["var1.pred"],colorkey=TRUE, main = "universal kriging predictions")
spplot(jj["var1.var"],  main = "universal kriging variance")

```

```{r}
## systematic sampling scheme
spsample(mydata, n=50, type='regular')
plot(mydata, main="Systematic Sampling of size 50");points(spsample(mydata, n=50, type='regular'), col='black', pch=3, cex=0.9,)
##stratified sampling scheme
spsample(mydata, n=50, type='stratified')
plot(mydata, main="Stratified Sampling of size 50");points(spsample(mydata, n=50, type='stratified'), col='black', pch=3, cex=0.9,)
##cluster sampling scheme
spsample(mydata, n=50, type='nonaligned')
plot(mydata, main="Cluster Sampling of size 50");points(spsample(mydata, n=50, type='nonaligned'), col='black', pch=3, cex=0.9,)

## Random sampling scheme
spsample(mydata, n=50, type='random')
plot(mydata, main="Random Sampling of size 50");points(spsample(mydata, n=50, type='random'), col='black', pch=3, cex=0.9,)

###########################################################################################

## systematic sampling scheme
spsample(mydata, n=150, type='regular')
plot(mydata, main="Systematic Sampling of size 150");points(spsample(mydata, n=50, type='regular'), col='black', pch=3, cex=0.9,)
##stratified sampling scheme
spsample(mydata, n=150, type='stratified')
plot(mydata, main="Stratified Sampling of size 150");points(spsample(mydata, n=50, type='stratified'), col='black', pch=3, cex=0.9,)
##cluster sampling scheme
spsample(mydata, n=150, type='nonaligned')
plot(mydata, main="Cluster Sampling of size 150");points(spsample(mydata, n=50, type='nonaligned'), col='black', pch=3, cex=0.9,)

## Random sampling scheme
##########################################################################################
spsample(mydata, n=150, type='random')
plot(mydata, main="Random Sampling of size 150");points(spsample(mydata, n=50, type='random'), col='black', pch=3, cex=0.9,)
## systematic sampling scheme
spsample(mydata, n=500, type='regular')
plot(mydata, main="Systematic Sampling of size 500");points(spsample(mydata, n=50, type='regular'), col='black', pch=3, cex=0.9,)
##stratified sampling scheme
spsample(mydata, n=500, type='stratified')
plot(mydata, main="Stratified Sampling of size 500");points(spsample(mydata, n=50, type='stratified'), col='black', pch=3, cex=0.9,)
##cluster sampling scheme
spsample(mydata, n=500, type='nonaligned')
plot(mydata, main="Cluster Sampling of size 500");points(spsample(mydata, n=50, type='nonaligned'), col='black', pch=3, cex=0.9,)

## Random sampling scheme
spsample(mydata, n=500, type='random')
plot(mydata, main="Random Sampling of size 500");points(spsample(mydata, n=50, type='random'), col='black', pch=3, cex=0.9,)

```


