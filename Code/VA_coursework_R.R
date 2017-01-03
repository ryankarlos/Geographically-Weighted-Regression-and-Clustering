
# For spatial data handling and working with shape files 
library(rgdal) # provides the function for reading vector-based spatial data : readOGR 
library(spdep) #  enables transformations and projections of the data and provides functions for working with the loaded spatial polygon objects.
library(rgeos) # Interface to Geometry Engine - Open Source (GEOS)

# For visualisation 
library(tmap) # good for plotting cholorpeth maps which works with spatial data frames 
library(cartogram)# allows plotting cartograms 
library(ggplot2) # the godfather of plotting libraries - however need to convert spatial data frames to dataframe (using fortify()) function so ggplot can recognise 
library(ggvis)# extension of ggplot but allows more interactive visualisation 
library(GGally)#extension of ggplot
library(gridExtra) # for multiple scatter plots along same row 
library(colorspace)# Carries out mapping between assorted color spaces. Qualitative, sequential, and diverging color palettes based on HCL colors are provided along with an interactive palette picker 
library(RColorBrewer)# Provides color schemes for maps (and other graphics)
library(scales) # Graphical scales map data to aesthetics, and provide methods for automatically determining breaks and labels for axes and legends.

#### For data loding and munging ######
library(readr) # reading tabular data in csv (read_csv) and tables (read_table)
library(readxl)#reading tabular data in excel (read_excel)
library(tidyr) # tidying the data - gather(), spread(), unite(), seperate() functions 
library(dplyr)# data munging and transformations with mutate, summarise, filter, arrange, select 
library(magrittr)# for using piping operator %>% to make the code cleaner 
library(data.table) #faster than dplyr with same functionality 
library(reshape2) # allows use of dcast and melt functions for converting to wide format and long format respectively. Analogous to the spread() and gather() functions in tidyr 

# For spatial stats
library(GWmodel) # geographically weighted regression but can also be carried our using spgwr or gwrr, although this is the latest package 
library(spgwr)# geographically weighted regression 
gpclibPermit()
library(lm.beta)# Package lm.beta provides the option of standardizing the coef???cients after estimating them using the standard deviations or similar measures of the used variables. So there are unstandardized and standardized coef???cients available simultaneously. 
library(ggmap) # A collection of functions to visualize spatial data and models on top of static maps from various online sources (e.g Google Maps and Stamen Maps). It includes tools common to those tasks, including functions for geolocation and routing.

# For regression analysis 
library(car) # for computing VIF scores 

# For cluster analysis and machine learning 
library(cluster) # cluster analysis : kmeans, heirarchial clustering 
library(caret) # R's machine learning package 

## Loading  and cleaning the data  
#An excel spreadsheet with all explanatory variables are loaded and saved in factors_wards and factors_boroughs respectively, the outcome variable (wellbeing)for wards and borough is saved as wellbeing_wards and wellbeing_borough respectively, census data will also be used as explanatory variables to be merged with wellbeing_factors  and shapefiles containing details of London_ward boundaries for mapping. In summary, the following variables are produced :

#an R DataFrame ("factors_wards") where rows are wards and columns are wellbeing variables

#an R DataFrame ("wellbeing__wards") where rows are wards and columns describe wellbeing probability scores from 2009 to 2013 

#an R DataFrame ("factors_boroughs") where rows are wards and columns are wellbeing variables

#an R DataFrame ("wellbeing_boroughs") where rows are wards and columns describe wellbeing probability scores from 2009 to 2013 

#an R DataFrame ("census 2011") with 610 variables of census data 

#an R SpatialDataFrame ("london.wards"), with a DataFrame (accessed by @data) where the rows are LAs and columns are variables (as above), as well as coordinates describing the geographical extent of each LA (@polygons). This is produced using the readShapePoly function which also requires the coordinate system to be used in the data. Epsg:27700 is the code for British National Grid. 

#To get the shapefiles into a format that can be plotted we have to use the fortify() function. Spatial objects in R have a number of slots containing the various items of data (polygon geometry, projection, attribute information) associated with a shapefile. Slots can be thought of as shelves within the data object that contain the different attributes. The "polygons" slot contains the geometry of the polygons in the form of the XY coordinates used to draw the polygon outline. The generic plot function can work out what to do with these, ggplot2 cannot. We therefore need to extract them as a data frame. The fortify function was written specifically for this purpose. For this to work, either gpclib or rgeos packages must be installed.

#We then merge the wards.fort dataframe (containing the coordinates) with the factors (independent variables), wellbeing dataframes(outcome variable) and census dataframe (which contains some more independent variables), to create a new dataframe - plotdata. We will only do this for the ward dataframes for the time being.

#Rename the column names in plotdata- replacing spaces with underscores using gsub function. Also rename the last five columns to something meaningful for subsequent analysis 

#source: https://rpubs.com/m_dev/Intro-to-Spatial-Data-and-ggplot2




rm(list= ls())  # clears the workspace
capture.output(dev.off(), file ="NULL")    # clears all the plots. capture.output suppreses the output 

setwd("C:/Users/ryank/Documents/City_University/Visual_Analytics/Coursework/Individual_coursework_visualanalytics/London_wellbeing_scores/Data/")

#capture.output suppreses the output of excel files which have been imported

capture.output(factors_wards <- read_excel("wellbeing.xls", sheet ="Factors_wards"), file ="NUL");

capture.output( wellbeing_wards <- read_excel("wellbeing.xls", sheet ="Wellbeing_wards"),file ="NUL"); # importing the well being probaility scores for wards 

census2011 <- read_csv("population.csv"); # importing the census datac containing the area, density and population variables for boroughs 
names(census2011) <- gsub(" ", "_", names(census2011))

census2011 <- census2011 %>%
  group_by(id) %>%
  mutate(goodhealth = Good_health + Very_good_health)%>%
  mutate(badhealth = Bad_health + Very_bad_health) %>%
  mutate(Maual_occupations = Semi_skilled_and_unskilled_manual_occupations+ Skilled_manual_occupations)%>%
  mutate(Professionals = Clerical_and_junior_managerial + Higher_managerial_occupations) 

#names(wellbeing_wards)[2:6] <-c("Wellbeing_2009","Wellbeing_2010","Wellbeing_2011","Wellbeing_2012","Wellbeing_2013")
#names(factors_wards) <- gsub(" ", "_", names(factors_wards))
gb_boundaries <- readOGR(dsn = "shapefiles", layer ="london_wards_2011_wgs84")
#proj4string(gb_boundaries) <- CRS("+init=epsg:27700")

factors_wards$WardCode <- as.character(factors_wards$id)

gb_boundaries@data <- inner_join(gb_boundaries@data, census2011, by =c("CODE"= "id"))
gb_boundaries@data <- left_join(gb_boundaries@data, wellbeing_wards, by =c("CODE"= "id"))
gb_boundaries@data <- left_join(gb_boundaries@data, factors_wards, by = c("CODE" = "id"))
plotdata <- gb_boundaries
plotdata <- plotdata[26:nrow(plotdata),]
names(plotdata@data) <- gsub(" ", "_", names(plotdata@data))


plotdata@data <- plotdata@data %>%    # calculating % change wellbeing scores for one/two year ranges from 2011
  mutate(wellbeing_change_2011_2012 = Wellbeing2012 - Wellbeing2011) %>%
  mutate(wellbeing_change_2011_2013 = Wellbeing2013 - Wellbeing2011) %>%
  mutate(wellbeing_change_2011_2010 = Wellbeing2010 - Wellbeing2011) %>%
  mutate(wellbeing_change_2011_2009 = Wellbeing2009 - Wellbeing2011) %>%
  mutate(Crime2011 = Crime2011 + DeliberateFires2011)%>%
  within(rm(NAME))%>%   #removing redundant variable for ward 
  unite(Name, Name, Borough, sep = "_",remove =TRUE ) #using unite function in tidyr to merge two column  values together: ward name and borough name for easier identification of ward 



plotdata_tableau  <- plotdata@data %>%
  gather(Wellbeing, Wellbeing_value, Wellbeing2009:Wellbeing2013)  # using the gather function from tidyr to convert wellbeing to long format for analysis in tableau 


#Exploring spatial variation in well being scores 

#The code block below creates a spatial map of variation in wellbeing scores using the ggplot2 package and tm_shape. 
#For ggplot, we need to convert our spatial data frame to a dataframe which can be read by R. First we append to the data an extra column called "id". This column will contain the rownames so that we define an explicit relationship between the data and the polygons associated with that data. The we use the fortify function. Basically, fortify take two arguments: model, which will consist of the SpatialPolygonsDataFrame object we wish to convert and region, the name of the variable by which to split regions. If you inspect this data.frame, you'll notice it appears to be missing some critical information. We then merge it with the original dataframe to recover the missing variables. We can then plot using the ggplot tool. 

#source: https://rpubs.com/m_dev/Intro-to-Spatial-Data-and-ggplot2


plotdata@data$id <- rownames(plotdata@data)
plotdatafortify = fortify(plotdata, region = "id")
plotdatafortify <- merge(plotdatafortify, plotdata@data, by = "id")


#plotting all wellbeing output variables for all 5 years 

b <- which(names(plotdatafortify)%in%c("Wellbeing2009", "Wellbeing2010", "Wellbeing2011","Wellbeing2012", "Wellbeing2013"))


# Creating a loop for displaying all the wellbeing plots from 2009 to 2013. if in loops need to change aes to aes_string  
for (j in  b){
for(i in plotdatafortify[j]){     
plt <- ggplot(data = plotdatafortify, aes_string(x = "long", y = "lat", group = "group",fill = i))  + 
geom_polygon(color = "black",size = 0.1) +
labs(x = "Longitude",y ="latitude", fill = colnames(plotdatafortify[j])) + 
scale_fill_gradientn(colours=c("darkred","red" ,"yellow","green","darkgreen"), guide = "colorbar") +theme(aspect.ratio = 0.8) # + coord_equal()

print(plt)
   }
}


#plotting all changes in wellbeing scores from 2011 to previous or future years 

b <- which(names(plotdatafortify)%in%c("wellbeing_change_2011_2012", "wellbeing_change_2011_2013", "wellbeing_change_2011_2010", "wellbeing_change_2011_2009"))


# Creating a loop for displaying all the wellbeing plots from 2009 to 2013. if in loops need to change aes to aes_string  
for (j in  b){
for(i in plotdatafortify[j]){     
plt <- ggplot(data = plotdatafortify, aes_string(x = "long", y = "lat", group = "group",fill = i))  + 
geom_polygon(color = "black",size = 0.1) +
labs(x = "Longitude",y ="latitude", fill = colnames(plotdatafortify[j])) + 
scale_fill_gradientn(colours=c("darkblue","blue" ,"white","orange","brown"), guide = "colorbar") +theme(aspect.ratio = 0.8) # + coord_equal()

print(plt)
}
}

tm_shape(plotdata) +  
tm_fill(col="Wellbeing2011",style="cont",palette="Spectral", size=0.2, id="id", title="") + 
tm_borders(col="#bdbdbd", lwd=0.5)+
tm_layout(
title="wellbeing scores_2011 ",
title.snap.to.legend=TRUE,
title.size=0.8,
legend.text.size=0.6,
title.position = c("right", "center"),
legend.position = c("right","center"),
frame=FALSE,
legend.outside=TRUE)


#Exploring correlation between wellbeing scores and variables  
#Scatter plots that display relationships between each explanatory variable and our outcome.


plotdata@data %>%
ggplot(aes(x = Unemployment2011,y=Wellbeing2011))+ 
geom_point(colour="#525252",pch=21) +
stat_smooth(method=lm, se=FALSE, size=1, colour="#525252")+
scale_fill_distiller("BrBG", type="div", direction=1, guide="colourbar", limits=c(-0.29,0.29))+
theme_bw()+
theme(legend.position="none") +
ggtitle(paste("correlation:",round(cor.test(plotdata@data$Unemployment2011,plotdata@data$Wellbeing2011)$estimate,2)))

plotdata@data %>%
ggplot(aes(x = Crime2011,y=Wellbeing2011))+ 
geom_point(colour="#525252",pch=21) +
stat_smooth(method=lm, se=FALSE, size=1, colour="#525252")+
scale_fill_distiller("BrBG", type="div", direction=1, guide="colourbar", limits=c(-0.29,0.29))+
theme_bw()+
theme(legend.position="none") +
ggtitle(paste("correlation:",round(cor.test(plotdata@data$Crime2011,plotdata@data$Wellbeing2011)$estimate,2)))

plotdata@data %>%
ggplot(aes(x = Good_access_to_nature,y=Wellbeing2011))+ 
geom_point(colour="#525252",pch=21) +
stat_smooth(method=lm, se=FALSE, size=1, colour="#525252")+
scale_fill_distiller("BrBG", type="div", direction=1, guide="colourbar", limits=c(-0.29,0.29))+
theme_bw()+
theme(legend.position="none") +
ggtitle(paste("correlation:",round(cor.test(plotdata@data$Good_access_to_nature,plotdata@data$Wellbeing2011)$estimate,2)))

plotdata@data %>%
ggplot(aes(x = goodhealth ,y=Wellbeing2011))+ 
geom_point(colour="#525252",pch=21) +
stat_smooth(method=lm, se=FALSE, size=1, colour="#525252")+
scale_fill_distiller("BrBG", type="div", direction=1, guide="colourbar", limits=c(-0.29,0.29))+
theme_bw()+
theme(legend.position="none") +
ggtitle(paste("correlation:",round(cor.test(plotdata@data$goodhealth,plotdata@data$Wellbeing2011)$estimate,2)))


# Boxplot of wellbeing scores 2011 

boxplot(plotdata@data$Wellbeing2011, ylab = "Wellbeing_2011",boxwex = 0.25, col = "bisque")


# Correlation coefficient matrix and vif scores 

#Here we generate a correlation coefficient matrix so we can choose to include the variables which produce the higher correlation coefficients (above 0.5) in the results 

plotdatafortify %>% 
select(Wellbeing2011, LifeExpectancy2011, ChildhoodObesity2011, Unemployment2011,Crime2011,goodhealth, Population_Density, SchoolAbsence2011, Younger_adults, Composite_Happiness_Score_2011, GCSEscores2011,Professionals, Transport2011, dependentchildren2011, Avg_dist_to_work, Work_from_home,Carer_for_family) %>%
ggcorr(label = TRUE, nbreaks=5, hjust = 1, size = 3, palette = "RdYlBu",layout.exp = 10, geom ="text", label_alpha=0.4)

#plotdata.scaled <- data.frame(scale(plotdatafortify[,14:ncol(plotdatafortify)]))

model <- lm(Wellbeing2011 ~ LifeExpectancy2011 + ChildhoodObesity2011 +  Unemployment2011 + Crime2011  +  goodhealth + Population_Density +  SchoolAbsence2011 + Composite_Happiness_Score_2011 + Professionals + Younger_adults + dependentchildren2011+ Avg_dist_to_work+ Work_from_home +  Carer_for_family +GCSEscores2011, data = plotdata) 

# Calculate VIF scores
vif(model)

plotdata@data$resids <- resid(model)

# New Correlation coefficient matrix with added/removed variables and corresponding vif scores  

plotdatafortify %>% 
select(Wellbeing2011, ChildhoodObesity2011, Unemployment2011,Crime2011,goodhealth, Population_Density, Younger_adults, GCSEscores2011, Composite_Happiness_Score_2011, Work_from_home, Carer_for_family) %>%
ggcorr(label = TRUE, nbreaks=5, hjust = 1, size = 3, geom = "text", palette = "RdYlBu",layout.exp = 10, label_alpha=0.4)

#plotdata.scaled <- data.frame(scale(plotdatafortify[,14:ncol(plotdatafortify)]))

model_refined <- lm(Wellbeing2011 ~ ChildhoodObesity2011 +  Unemployment2011 + Crime2011  +  goodhealth + Population_Density + GCSEscores2011 + Younger_adults+ Composite_Happiness_Score_2011 +Work_from_home + Carer_for_family, data = plotdata) 

# Calculate VIF scores
vif(model_refined)

plotdata@data$resids_refined <- resid(model_refined)


# Computing gwss, and gw.model
#Auto bw tuning used for gw regression. For gw_ss we haveadjusted bw manually. 


#plot(plotdata)

gw_ss <- gwss(plotdata,vars = c("Wellbeing2011", "LifeExpectancy2011", "ChildhoodObesity2011", "Unemployment2011", "Crime2011", "goodhealth", "Population_Density", "SchoolAbsence2011", "GCSEscores2011", "Younger_adults", "Composite_Happiness_Score_2011", "Carer_for_family", "Work_from_home"),kernel ="bisquare", adaptive = TRUE, bw = 50)

bw.gwr.1 <- bw.gwr( Wellbeing2011 ~ LifeExpectancy2011 + ChildhoodObesity2011 +  Unemployment2011 + Crime2011  +  goodhealth + Population_Density +  SchoolAbsence2011 +   GCSEscores2011 + Younger_adults+ Composite_Happiness_Score_2011 +Work_from_home + Carer_for_family, data = plotdata,kernel = "bisquare",adaptive = TRUE)

gw.modelman <- gwr.basic(Wellbeing2011 ~ LifeExpectancy2011 + ChildhoodObesity2011 +  Unemployment2011 + Crime2011  +  goodhealth + Population_Density +  SchoolAbsence2011 +   GCSEscores2011 + Younger_adults+ Composite_Happiness_Score_2011 + +Work_from_home + Carer_for_family, data = plotdata,bw = 40, kernel = "bisquare", adaptive = TRUE)

gw.modelauto <- gwr.basic(Wellbeing2011 ~ LifeExpectancy2011 + ChildhoodObesity2011 +  Unemployment2011 + Crime2011  +  goodhealth + Population_Density +  SchoolAbsence2011 +   GCSEscores2011 + Younger_adults+ Composite_Happiness_Score_2011 +Work_from_home + Carer_for_family, data = plotdata,bw = bw.gwr.1, kernel = "bisquare", adaptive = TRUE)


# Mapping the gwss results 

quick.map <- function(plotdata,var,legend.title,main.title) {
x <- plotdata@data[,var]
cut.vals <- pretty(x)
x.cut <- cut(x,cut.vals)
cut.levels <- levels(x.cut)
cut.band <- match(x.cut,cut.levels)
colors <- rev(brewer.pal(length(cut.levels),'YlOrRd'))
par(mar=c(1,1,1,1))
plot(plotdata, col=colors,bg='white')
title(main.title)
plot(plotdata,add=TRUE,col=colors[cut.band],pch=16)
legend('bottomright',cut.levels,col=colors,pch=16,bty='n',title=legend.title)
}

#gwss stats - mean and SD

quick.map(gw_ss$SDF,"Wellbeing2011_LM","Wellbeing_2011","Geographically Weighted Mean")
quick.map(gw_ss$SDF,"Wellbeing2011_LSD","Wellbeing_2011","Geographically Weighted SD")
quick.map(gw_ss$SDF,"Corr_Wellbeing2011.Unemployment2011","Unemployment","GW Pearson Correlation Wellbeing-Unemployment")
quick.map(gw_ss$SDF,"Corr_Wellbeing2011.goodhealth","Good health","GW Pearson Correlation Wellbeing-good health")

# lm_mdodel_residuals

tm_shape(plotdata) + 
tm_fill(col=c("resids_refined"),style="fixed", breaks=c(min(plotdata$resids_refined), -1.5, -0.25, 0.25, 1.5, max(plotdata$resids_refined)),palette="RdBu", id="geo_label",size=0.2, title="") + 
tm_borders(col="#bdbdbd", lwd=0.5)+
tm_facets(free.scales = FALSE)+
tm_layout(
title=" global model residuals",
frame=FALSE,
title.size=1,
title.position = c("left", "top"),
inner.margins = c(0,0,0.15,0),
legend.title.size=1,
legend.text.size=0.6,
legend.outside=TRUE)

# residuals for gwr with manually applied bandwidth 40

tm_shape(gw.modelman$SDF) +
tm_fill(col=colnames(gw.modelman$SDF@data["residual"]), title="gwr residuals", style="fixed",breaks=c(min(gw.modelman$SDF@data["residual"]), -1.5, -0.25, 0.25, 1.5, max(gw.modelman$SDF@data["residual"])), palette="RdBu", size=0.2) + 
tm_borders(col="#bdbdbd", lwd=0.5) +
tm_facets(free.scales = FALSE) +
tm_layout(
frame=FALSE,
title=" residuals for gwr with manually applied bandwidth 40",
title.snap.to.legend=FALSE,
title.size=1,
title.position = c("left", "top"),
inner.margins = c(0,0,0.15,0),
legend.title.size=1,
legend.text.size=0.6,
legend.outside=TRUE)


# residuals for gwr with auto tuned bandwidth : 175


tm_shape(gw.modelauto$SDF) +
tm_fill(col=colnames(gw.modelauto$SDF@data["residual"]), title="gwr residuals", style="fixed",breaks=c(min(gw.modelauto$SDF@data["residual"]), -1.5, -0.25, 0.25, 1.5, max(gw.modelauto$SDF@data["residual"])),palette="RdBu", size=0.2) + 
tm_borders(col="#bdbdbd", lwd=0.5) +
tm_facets(free.scales = FALSE) +
tm_layout(
title=" residuals for gwr with auto tuned bandwidth : 175",
frame=FALSE,
title.snap.to.legend=FALSE,
title.size=1,
title.position = c("left", "top"),
inner.margins = c(0,0,0.15,0),
legend.title.size=1,
legend.text.size=0.6,
legend.outside=TRUE)

# kmeans Clustering on gwr coeffecients and showing results on chloropeth map 

set.seed(20)

dist_matrix <- gw.modelman$SDF@data[,2:13] %>%
select( LifeExpectancy2011,ChildhoodObesity2011,Unemployment2011, Crime2011, goodhealth, Population_Density, SchoolAbsence2011, GCSEscores2011, Younger_adults, Composite_Happiness_Score_2011, Work_from_home, Carer_for_family) %>%
mutate_each(funs(z = .-mean(.)/sd(.)))

dist_matrix <- dist(dist_matrix[,13:24])


#5 clusters 
gwcluster <- kmeans(dist_matrix,5)

#plot(silhouette(gwcluster,dist_matrix))

gwcluster$cluster <- as.factor(gwcluster$cluster)
gw.modelman$SDF@data$cluster_groups <- gwcluster$cluster 

tm_shape(gw.modelman$SDF) +
tm_fill(col="cluster_groups", style="cat",id="geo_label", palette="Accent", size=0.2) + 
tm_borders(col="#636363", lwd=0.2) +
tm_facets(free.scales = FALSE) +
tm_layout(
frame = FALSE,
title.size=2,
title.position = c("left","top"),
inner.margins = c(0,0,0.15,0),
legend.title.size=1,
legend.text.size=0.6,
legend.outside=TRUE)

# 4 clusters 

gwcluster <- kmeans(dist_matrix,4)

#plot(silhouette(gwcluster,dist_matrix))

gwcluster$cluster <- as.factor(gwcluster$cluster)
gw.modelman$SDF@data$cluster_groups <- gwcluster$cluster 

tm_shape(gw.modelman$SDF) +
  tm_fill(col="cluster_groups", style="cat",id="geo_label", palette="Accent", size=0.2) + 
  tm_borders(col="#636363", lwd=0.2) +
  tm_facets(free.scales = FALSE) +
  tm_layout(
    frame = FALSE,
    title.size=2,
    title.position = c("left","top"),
    inner.margins = c(0,0,0.15,0),
    legend.title.size=1,
    legend.text.size=0.6,
    legend.outside=TRUE)

# 3 clusters 

gwcluster <- kmeans(dist_matrix,3)

#plot(silhouette(gwcluster,dist_matrix))

gwcluster$cluster <- as.factor(gwcluster$cluster)
gw.modelman$SDF@data$cluster_groups <- gwcluster$cluster 

tm_shape(gw.modelman$SDF) +
  tm_fill(col="cluster_groups", style="cat",id="geo_label", palette="Accent", size=0.2) + 
  tm_borders(col="#636363", lwd=0.2) +
  tm_facets(free.scales = FALSE) +
  tm_layout(
    frame = FALSE,
    title.size=2,
    title.position = c("left","top"),
    inner.margins = c(0,0,0.15,0),
    legend.title.size=1,
    legend.text.size=0.6,
    legend.outside=TRUE)
