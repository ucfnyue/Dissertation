#This Rscript is for the study of spatial characteristic 
#and factors influencing violent crime rates in London
#First, please set to the working directory with data used
getwd()
setwd("D:/Dissertation/Dissertation")
library(fs)
library(sf)
library(sp)
library(dplyr)
library(tidyverse)
library(janitor)
library(tmap)
library(tmaptools)
library(geojsonio)
library(plotly)
library(ggplot2)
library(ggthemes)
library(reshape2)
library(raster)
library(rgdal)
library(broom)
library(mapview)
library(crosstalk)
library(highcharter)
library(downloader)
library(spdep)
library(car)
library(sjPlot)
library(dbscan)

#Part1 load the data processed by Python
#Load London MSOA shapefile data
londonshp <- st_read("shp/MSOA_2011_London_gen_MHW.shp")
summary(londonshp)
#Check the shapefile
qtm(londonshp)

#Load attribute data and clean up
#Check the columns type
csv <- read_csv("data_R/xy_final_violent.csv") %>% 
  clean_names()  
csv
summary(csv)

#Merge londonshp and csv using a common ID
crimefactor <- londonshp %>%
  left_join(.,
            csv, 
            by = c("MSOA11CD" = "mso_acode"))

#Plot the spatial distribution of violent crime rates
tmap_mode("plot")
png("violent_rate.png",width=1300,height=963,res=216)
tm_shape(crimefactor) + 
  tm_polygons("violent_rate", 
              style="jenks",
              palette="RdBu",
              midpoint=NA,
              title="Violent Crime Rate",
              alpha = 1) + 
  tm_compass(position = c("left", "bottom"),type = "arrow",size = 1.5) + 
  tm_scale_bar(position = c("left", "bottom"),text.size =0.3) +
  tm_layout(frame=FALSE,legend.position = c("right", "bottom"),legend.title.size = 1,legend.text.size = 0.5)
dev.off()

------#Jenks Natural Break Classification----------
library(classInt)
classIntervals(crimefactor$violent_rate, n = 5, style = 'jenks',
               warnLargeN = F)

------#Variables distribution---------
tmap_mode("plot")
png("pop_density.png",width=1300,height=963,res=216)
tm_shape(crimefactor) + 
  tm_polygons("pop_density", 
              style="jenks",
              midpoint=NA,
              title="Population Density",
              alpha = 0.6) + 
  tm_compass(position = c("left", "bottom"),type = "arrow",size = 1.5) + 
  tm_scale_bar(position = c("left", "bottom"),text.size =0.3) +
  tm_layout(frame=FALSE,legend.position = c("right", "bottom"),legend.title.size = 1,legend.text.size = 0.5)
dev.off()

tmap_mode("plot")
png("age16_29_percent.png",width=1300,height=963,res=216)
tm_shape(crimefactor) + 
  tm_polygons("age16_29_percent", 
              style="jenks",
              midpoint=NA,
              title="Age16-29 %",
              alpha = 0.6) + 
  tm_compass(position = c("left", "bottom"),type = "arrow",size = 1.5) + 
  tm_scale_bar(position = c("left", "bottom"),text.size =0.3) +
  tm_layout(frame=FALSE,legend.position = c("right", "bottom"),legend.title.size = 1,legend.text.size = 0.5)
dev.off()

tmap_mode("plot")
png("bame_percent.png",width=1300,height=963,res=216)
tm_shape(crimefactor) + 
  tm_polygons("bame_percent", 
              style="jenks",
              midpoint=NA,
              title="BAME %",
              alpha = 0.6) + 
  tm_compass(position = c("left", "bottom"),type = "arrow",size = 1.5) + 
  tm_scale_bar(position = c("left", "bottom"),text.size =0.3) +
  tm_layout(frame=FALSE,legend.position = c("right", "bottom"),legend.title.size = 1,legend.text.size = 0.5)
dev.off()

tmap_mode("plot")
png("householdspaces_no_usualresidents.png",width=1300,height=963,res=216)
tm_shape(crimefactor) + 
  tm_polygons("householdspaces_no_usualresidents", 
              style="jenks",
              midpoint=NA,
              title="(log)No Usual Residents%",
              alpha = 0.6) + 
  tm_compass(position = c("left", "bottom"),type = "arrow",size = 1.5) + 
  tm_scale_bar(position = c("left", "bottom"),text.size =0.3) +
  tm_layout(frame=FALSE,legend.position = c("right", "bottom"),legend.title.size = 1,legend.text.size = 0.5)
dev.off()

tmap_mode("plot")
png("house_price.png",width=1300,height=963,res=216)
tm_shape(crimefactor) + 
  tm_polygons("house_price", 
              style="jenks",
              midpoint=NA,
              title="(log)House Price",
              alpha = 0.6) + 
  tm_compass(position = c("left", "bottom"),type = "arrow",size = 1.5) + 
  tm_scale_bar(position = c("left", "bottom"),text.size =0.3) +
  tm_layout(frame=FALSE,legend.position = c("right", "bottom"),legend.title.size = 1,legend.text.size = 0.5)
dev.off()

tmap_mode("plot")
png("employment_deprivation.png",width=1300,height=963,res=216)
tm_shape(crimefactor) + 
  tm_polygons("employment_deprivation", 
              style="jenks",
              midpoint=NA,
              title="Employment Deprivation",
              alpha = 0.6) + 
  tm_compass(position = c("left", "bottom"),type = "arrow",size = 1.5) + 
  tm_scale_bar(position = c("left", "bottom"),text.size =0.3) +
  tm_layout(frame=FALSE,legend.position = c("right", "bottom"),legend.title.size = 1,legend.text.size = 0.5)
dev.off()

tmap_mode("plot")
png("education_skills_deprivation.png",width=1300,height=963,res=216)
tm_shape(crimefactor) + 
  tm_polygons("education_skills_deprivation", 
              style="jenks",
              midpoint=NA,
              title="Education Skills Deprivation",
              alpha = 0.6) + 
  tm_compass(position = c("left", "bottom"),type = "arrow",size = 1.5) + 
  tm_scale_bar(position = c("left", "bottom"),text.size =0.3) +
  tm_layout(frame=FALSE,legend.position = c("right", "bottom"),legend.title.size = 1,legend.text.size = 0.5)
dev.off()

tmap_mode("plot")
png("living_environment_deprivation.png",width=1300,height=963,res=216)
tm_shape(crimefactor) + 
  tm_polygons("living_environment_deprivation", 
              style="jenks",
              midpoint=NA,
              title="Living Environment Deprivation",
              alpha = 0.6) + 
  tm_compass(position = c("left", "bottom"),type = "arrow",size = 1.5) + 
  tm_scale_bar(position = c("left", "bottom"),text.size =0.3) +
  tm_layout(frame=FALSE,legend.position = c("right", "bottom"),legend.title.size = 1,legend.text.size = 0.5)
dev.off()
-----------------------------
  
#Part2 Stablish OLS regression model
OLSdata <- crimefactor %>%
  dplyr::select(log_violent,
                pop_density,
                age16_29_percent,
                bame_percent,
                householdspaces_no_usualresidents,
                house_price,employment_deprivation,
                education_skills_deprivation,living_environment_deprivation)
view(OLSdata)

OLSmodel <- OLSdata %>%
  lm(log_violent ~
       pop_density + age16_29_percent + bame_percent + 
       householdspaces_no_usualresidents + house_price +
       employment_deprivation + education_skills_deprivation + 
       living_environment_deprivation,
     data=.)

#Output the summary of those parameters and save the generated tables
tidy(OLSmodel)
glance(OLSmodel)
tab_df(tidy(OLSmodel),digits = 6)
tab_df(glance(OLSmodel),digits = 4)

#To observe if multicollinearity exist in the independent variables
tab_df(vif(OLSmodel),digits = 4)

#Now save the residuals into crimefactor
crimefactor <- crimefactor %>%
  mutate(modelresiduals = residuals(OLSmodel))
plot(OLSmodel)

#Plot the fitting graph
q <- qplot(x =`pop_density` +
             `age16_29_percent` +
             `bame_percent` +
             `householdspaces_no_usualresidents` +
             `house_price` + `employment_deprivation` +
             `education_skills_deprivation` + `living_environment_deprivation`,
           y = `log_violent`, 
           data= crimefactor)

#Plot with a regression line
q + stat_smooth(method="lm", size=1) + 
  geom_jitter()

#Plot model residuals to see if there are any apparent patterns
png("OLS_Residuals_violent.png",width=1300,height=963,res=216)
tm_shape(crimefactor) + 
  tm_polygons("modelresiduals", 
              style="jenks",
              midpoint=NA,
              title="Model Residuals",
              alpha = 0.6) + 
  tm_compass(position = c("left", "bottom"),type = "arrow",size = 1.5) + 
  tm_scale_bar(position = c("left", "bottom"),text.size =0.3) +
  tm_layout(title="Violent Crime",title.size=1,frame=FALSE,legend.position = c("right", "bottom"),legend.title.size = 1,legend.text.size = 0.5)
dev.off()

#Then test Moran's I to check for spatial autocorrelation
#Calculate the centroids of all MSOAs in London
coordsW <- crimefactor%>%
  st_centroid()%>%
  st_geometry()

#Using the nearest neighbours method
knn_msoas <-coordsW %>%
  knearneigh(., k=4)
msoa_knn <- knn_msoas %>%
  knn2nb()
knn_msoa_weight <- msoa_knn %>%
  nb2listw(., style="C")

#Run a moran's I test on the residuals
OLSMoran <- crimefactor %>%
  st_drop_geometry()%>%
  dplyr::select(modelresiduals)%>%
  pull()%>%
  moran.test(., knn_msoa_weight)%>%
  tidy()

tab_df(OLSMoran,digits = 4)
---------------------------------------------
  
#Part3 GWR -- Spatial Regression Model
#Considering some spatial autocorrelations could be leading to biased estimates
library(spgwr)

st_crs(crimefactor) = 27700
crimefactor_SP <- crimefactor %>%
  as(., "Spatial")
st_crs(coordsW) = 27700
coordsW_SP <- coordsW %>%
  as(., "Spatial")
coordsW_SP

#Then calculate the kernel bandwidth
GWRbandwidth <- gwr.sel(log_violent ~
                          pop_density + age16_29_percent + bame_percent + 
                          householdspaces_no_usualresidents + employment_deprivation +
                          house_price + education_skills_deprivation + 
                          living_environment_deprivation,
                        data = crimefactor_SP, 
                        coords=coordsW_SP,
                        adapt=T)
GWRbandwidth

#Run GWR model
gwr.model = gwr(log_violent ~
                  pop_density + age16_29_percent + bame_percent + 
                  householdspaces_no_usualresidents + employment_deprivation +
                  house_price + education_skills_deprivation + 
                  living_environment_deprivation,
                data = crimefactor_SP, 
                coords=coordsW_SP,
                adapt=GWRbandwidth, 
                hatmatrix=TRUE, 
                se.fit=TRUE)

#Compare the similarity and difference between GWR model and OLS model
#To see how the coefficients/significance vary across the MSOAs
gwr.model
OLSmodel

#Attach the coefficients to the crimefactor dataframe
#And save the file to the project

results <- as.data.frame(gwr.model$SDF)
names(results)
write.csv(x = results, file="D:/Dissertation/Dissertation/results_violent.csv")

crimefactor2 <- crimefactor %>%
  mutate(coef_pop_density = results$pop_density,
         coef_age16_29_percent = results$age16_29_percent,
         coef_bame_percent = results$bame_percent,
         coef_householdspaces_no_usualresidents = results$householdspaces_no_usualresidents,
         coef_house_price = results$house_price,
         coef_employment_deprivation = results$employment_deprivation,
         coef_education_skills_deprivation = results$education_skills_deprivation,
         coef_living_environment_deprivation = results$living_environment_deprivation)

#save the coefficents in order to perform clustering later in Python
coef_csv <- csv %>%
  mutate(coef_pop_density = results$pop_density,
         coef_age16_29_percent = results$age16_29_percent,
         coef_bame_percent = results$bame_percent,
         coef_householdspaces_no_usualresidents = results$householdspaces_no_usualresidents,
         coef_house_price = results$house_price,
         coef_employment_deprivation = results$employment_deprivation,
         coef_education_skills_deprivation = results$education_skills_deprivation,
         coef_living_environment_deprivation = results$living_environment_deprivation)

write.csv(x = coef_csv, file="D:/Dissertation/Dissertation/coef_violent.csv")

#Visualization on the coefficents
png("coef_pop_density.png",width=1085,height=963,res=216)
tm_shape(crimefactor2) + 
  tm_polygons("coef_pop_density", 
              style="jenks",
              palette="GnBu",
              midpoint=NA,
              title="Coef Population Density",
              alpha = 0.7) + 
  tm_compass(position = c("left", "bottom"),type = "arrow",size = 1.5) + 
  tm_scale_bar(position = c("left", "bottom"),text.size =0.3) +
  tm_layout(title="Violent Crime",title.size=1,frame=FALSE,legend.position = c("right", "bottom"),legend.title.size = 1,legend.text.size = 0.5)
dev.off()

png("coef_age16_29_percent.png",width=1085,height=963,res=216)
tm_shape(crimefactor2) + 
  tm_polygons("coef_age16_29_percent", 
              style="jenks",
              palette="GnBu",
              midpoint=NA,
              title="Coef Age16-29%",
              alpha = 0.7) + 
  tm_compass(position = c("left", "bottom"),type = "arrow",size = 1.5) + 
  tm_scale_bar(position = c("left", "bottom"),text.size =0.3) +
  tm_layout(title="Violent Crime",title.size=1,frame=FALSE,legend.position = c("right", "bottom"),legend.title.size = 1,legend.text.size = 0.5)
dev.off()

png("coef_bame_percent.png",width=1085,height=963,res=216)
tm_shape(crimefactor2) + 
  tm_polygons("coef_bame_percent", 
              style="jenks",
              palette="GnBu",
              midpoint=NA,
              title="Coef BAME%",
              alpha = 0.7) + 
  tm_compass(position = c("left", "bottom"),type = "arrow",size = 1.5) + 
  tm_scale_bar(position = c("left", "bottom"),text.size =0.3) +
  tm_layout(title="Violent Crime",title.size=1,frame=FALSE,legend.position = c("right", "bottom"),legend.title.size = 1,legend.text.size = 0.5)
dev.off()

png("coef_householdspaces_no_usualresidents.png",width=1085,height=963,res=216)
tm_shape(crimefactor2) + 
  tm_polygons("coef_householdspaces_no_usualresidents", 
              style="jenks",
              palette="GnBu",
              midpoint=NA,
              title="Coef No Usual Residents(log)",
              alpha = 0.7) + 
  tm_compass(position = c("left", "bottom"),type = "arrow",size = 1.5) + 
  tm_scale_bar(position = c("left", "bottom"),text.size =0.3) +
  tm_layout(title="Violent Crime",title.size=1,frame=FALSE,legend.position = c("right", "bottom"),legend.title.size = 1,legend.text.size = 0.5)
dev.off()

png("coef_house_price.png",width=1085,height=963,res=216)
tm_shape(crimefactor2) + 
  tm_polygons("coef_house_price", 
              style="jenks",
              palette="GnBu",
              midpoint=NA,
              title="Coef House Price",
              alpha = 0.6) + 
  tm_compass(position = c("left", "bottom"),type = "arrow",size = 1.5) + 
  tm_scale_bar(position = c("left", "bottom"),text.size =0.3) +
  tm_layout(title="Violent Crime",title.size=1,frame=FALSE,legend.position = c("right", "bottom"),legend.title.size = 1,legend.text.size = 0.5)
dev.off()

png("coef_employment_deprivation.png",width=1085,height=963,res=216)
tm_shape(crimefactor2) + 
  tm_polygons("coef_employment_deprivation", 
              style="jenks",
              palette="GnBu",
              midpoint=NA,
              title="Coef Employment Deprivation",
              alpha = 0.6) + 
  tm_compass(position = c("left", "bottom"),type = "arrow",size = 1.5) + 
  tm_scale_bar(position = c("left", "bottom"),text.size =0.3) +
  tm_layout(title="Violent Crime",title.size=1,frame=FALSE,legend.position = c("right", "bottom"),legend.title.size = 1,legend.text.size = 0.5)
dev.off()

png("coef_education_skills_deprivation.png",width=1085,height=963,res=216)
tm_shape(crimefactor2) + 
  tm_polygons("coef_education_skills_deprivation", 
              style="jenks",
              palette="GnBu",
              midpoint=NA,
              title="Coef EducationSkills Deprivation",
              alpha = 0.6) + 
  tm_compass(position = c("left", "bottom"),type = "arrow",size = 1.5) + 
  tm_scale_bar(position = c("left", "bottom"),text.size =0.3) +
  tm_layout(title="Violent Crime",title.size=1,frame=FALSE,legend.position = c("right", "bottom"),legend.title.size = 1,legend.text.size = 0.5)
dev.off()

png("coef_living_environment_deprivation.png",width=1085,height=963,res=216)
tm_shape(crimefactor2) + 
  tm_polygons("coef_living_environment_deprivation", 
              style="jenks",
              palette="GnBu",
              midpoint=NA,
              title="Coef LivingEnvironment Deprivation",
              alpha = 0.6) + 
  tm_compass(position = c("left", "bottom"),type = "arrow",size = 1.5) + 
  tm_scale_bar(position = c("left", "bottom"),text.size =0.3) +
  tm_layout(title="Violent Crime",title.size=1,frame=FALSE,legend.position = c("right", "bottom"),legend.title.size = 1,legend.text.size = 0.5)
dev.off()
-------------------------------
#Visualisation on the local R2
crimefactor2 <- crimefactor2 %>%
  mutate(localR2 = results$localR2)

png("local_R2_violent.png",width=1085,height=963,res=216)
tm_shape(crimefactor2) + 
  tm_polygons("localR2", 
              style="jenks",
              palette="RdBu",
              midpoint=NA,
              title="Local R2",
              alpha = 0.6) + 
  tm_compass(position = c("left", "bottom"),type = "arrow",size = 1.5) + 
  tm_scale_bar(position = c("left", "bottom"),text.size =0.3) +
  tm_layout(title="Violent Crime",title.size=1,frame=FALSE,legend.position = c("right", "bottom"),legend.title.size = 1,legend.text.size = 0.5)
dev.off()

#Run the significance test
sigTest_pop_density = abs(gwr.model$SDF$"pop_density")-2 * gwr.model$SDF$"pop_density_se"
sigTest_age16_29_percent = abs(gwr.model$SDF$"age16_29_percent")-2 * gwr.model$SDF$"age16_29_percent_se"
sigTest_bame_percent = abs(gwr.model$SDF$"bame_percent")-2 * gwr.model$SDF$"bame_percent_se"
sigTest_householdspaces_no_usualresidents = abs(gwr.model$SDF$"householdspaces_no_usualresidents")-2 * gwr.model$SDF$"householdspaces_no_usualresidents_se"
sigTest_house_price = abs(gwr.model$SDF$"house_price")-2 * gwr.model$SDF$"house_price_se"
sigTest_employment_deprivation = abs(gwr.model$SDF$"employment_deprivation")-2 * gwr.model$SDF$"employment_deprivation_se"
sigTest_education_skills_deprivation = abs(gwr.model$SDF$"education_skills_deprivation")-2 * gwr.model$SDF$"education_skills_deprivation_se"
sigTest_living_environment_deprivation = abs(gwr.model$SDF$"living_environment_deprivation")-2 * gwr.model$SDF$"living_environment_deprivation_se"

crimefactor2 <- crimefactor2 %>%
  mutate(GWRsigTest_pop_density = sigTest_pop_density,
         GWRsigTest_age16_29_percent = sigTest_age16_29_percent,
         GWRsigTest_bame_percent = sigTest_bame_percent,
         GWRsigTest_householdspaces_no_usualresidents = sigTest_householdspaces_no_usualresidents,
         GWRsigTest_house_price = sigTest_house_price,
         GWRsigTest_employment_deprivation = sigTest_employment_deprivation,
         GWRsigTest_education_skills_deprivation = sigTest_education_skills_deprivation,
         GWRsigTest_living_environment_deprivation = sigTest_living_environment_deprivation)

#Calculate the t-value to test significance
t_value_pop_density = gwr.model$SDF$"pop_density"/gwr.model$SDF$"pop_density_se"
t_value_age16_29_percent = gwr.model$SDF$"age16_29_percent"/ gwr.model$SDF$"age16_29_percent_se"
t_value_bame_percent = gwr.model$SDF$"bame_percent"/gwr.model$SDF$"bame_percent_se"
t_value_householdspaces_no_usualresidents = gwr.model$SDF$"householdspaces_no_usualresidents"/gwr.model$SDF$"householdspaces_no_usualresidents_se"
t_value_house_price = gwr.model$SDF$"house_price"/gwr.model$SDF$"house_price_se"
t_value_employment_deprivation = gwr.model$SDF$"employment_deprivation"/gwr.model$SDF$"employment_deprivation_se"
t_value_education_skills_deprivation = gwr.model$SDF$"education_skills_deprivation"/gwr.model$SDF$"education_skills_deprivation_se"
t_value_living_environment_deprivation = gwr.model$SDF$"living_environment_deprivation"/gwr.model$SDF$"living_environment_deprivation_se"

crimefactor2 <- crimefactor2 %>%
  mutate(GWR_t_value_pop_density = t_value_pop_density,
         GWR_t_value_age16_29_percent = t_value_age16_29_percent,
         GWR_t_value_bame_percent = t_value_bame_percent,
         GWR_t_value_householdspaces_no_usualresidents = t_value_householdspaces_no_usualresidents,
         GWR_t_value_house_price = t_value_house_price,
         GWR_t_value_employment_deprivation = t_value_employment_deprivation,
         GWR_t_value_education_skills_deprivation = t_value_education_skills_deprivation,
         GWR_t_value_living_environment_deprivation = t_value_living_environment_deprivation) 

#Visualisation on the t-value
png("t_value_employment.png",width=1085,height=963,res=216)
tm_shape(crimefactor2) + 
  tm_polygons("GWR_t_value_employment_deprivation", 
              style="fixed",
              breaks = c(1.96, 3.92, 5.88,7.84,9.8,11.15),
              palette="Blues",
              midpoint=NA,
              title="t-value Employment Deprivation",
              alpha = 0.6) + 
  tm_compass(position = c("left", "bottom"),type = "arrow",size = 1.5) + 
  tm_scale_bar(position = c("left", "bottom"),text.size =0.3) +
  tm_layout(title="Violent Crime",title.size=1,frame=FALSE,legend.position = c("right", "bottom"),legend.title.size = 0.6,legend.text.size = 0.5)
dev.off()

png("t_value_education.png",width=1085,height=963,res=216)
tm_shape(crimefactor2) + 
  tm_polygons("GWR_t_value_education_skills_deprivation", 
              style="fixed",
              palette="RdBu",
              breaks = c(-3.2, -2.5, -1.96, 1.96, 2.5, 3.2),
              midpoint=NA,
              title="t-value EducationSkills Deprivation",
              alpha = 0.6) + 
  tm_compass(position = c("left", "bottom"),type = "arrow",size = 1.5) + 
  tm_scale_bar(position = c("left", "bottom"),text.size =0.3) +
  tm_layout(title="Violent Crime",title.size=1,frame=FALSE,legend.position = c("right", "bottom"),legend.title.size = 0.6,legend.text.size = 0.5)
dev.off()

png("t_value_popdensity.png",width=1085,height=963,res=216)
tm_shape(crimefactor2) + 
  tm_polygons("GWR_t_value_pop_density", 
              style="fixed",
              palette="Reds",
              breaks = c(-10.2, -7.84, -3.92, -1.96, 1.96),
              midpoint=NA,
              title="t-value Population Density",
              alpha = 0.6) + 
  tm_compass(position = c("left", "bottom"),type = "arrow",size = 1.5) + 
  tm_scale_bar(position = c("left", "bottom"),text.size =0.3) +
  tm_layout(title="Violent Crime",title.size=1,frame=FALSE,legend.position = c("right", "bottom"),legend.title.size = 0.6,legend.text.size = 0.5)
dev.off()

png("t_value_age16-19.png",width=1085,height=963,res=216)
tm_shape(crimefactor2) + 
  tm_polygons("GWR_t_value_age16_29_percent", 
              style="fixed",
              palette="Reds",
              breaks = c(0, 1.96, 3.92, 5.88, 6.1),
              midpoint=NA,
              title="t-value Age16-29%",
              alpha = 0.5) + 
  tm_compass(position = c("left", "bottom"),type = "arrow",size = 1.5) + 
  tm_scale_bar(position = c("left", "bottom"),text.size =0.3) +
  tm_layout(title="Violent Crime",title.size=1,frame=FALSE,legend.position = c("right", "bottom"),legend.title.size = 0.6,legend.text.size = 0.5)
dev.off()

png("t_value_bame.png",width=1085,height=963,res=216)
tm_shape(crimefactor2) + 
  tm_polygons("GWR_t_value_bame_percent", 
              style="fixed",
              palette="Blues",
              breaks = c(-1.96, 1.96, 4),
              midpoint=NA,
              title="t-value BAME%",
              alpha = 0.5) + 
  tm_compass(position = c("left", "bottom"),type = "arrow",size = 1.5) + 
  tm_scale_bar(position = c("left", "bottom"),text.size =0.3) +
  tm_layout(title="Violent Crime",title.size=1,frame=FALSE,legend.position = c("right", "bottom"),legend.title.size = 0.6,legend.text.size = 0.5)
dev.off()

png("t_value_household.png",width=1085,height=963,res=216)
tm_shape(crimefactor2) + 
  tm_polygons("GWR_t_value_householdspaces_no_usualresidents", 
              style="fixed",
              palette="RdBu",
              breaks = c(-2.5, -1.96, 1.96, 5.8),
              midpoint=NA,
              title="t-value No UsualResidents(log)",
              alpha = 0.5) + 
  tm_compass(position = c("left", "bottom"),type = "arrow",size = 1.5) + 
  tm_scale_bar(position = c("left", "bottom"),text.size =0.3) +
  tm_layout(title="Violent Crime",title.size=1,frame=FALSE,legend.position = c("right", "bottom"),legend.title.size = 0.6,legend.text.size = 0.5)
dev.off()

png("t_value_house_price.png",width=1085,height=963,res=216)
tm_shape(crimefactor2) + 
  tm_polygons("GWR_t_value_house_price", 
              style="fixed",
              palette="RdBu",
              breaks = c(-5.8, -1.96, 1.96, 4.4),
              midpoint=NA,
              title="t-value House Price(log)",
              alpha = 0.5) + 
  tm_compass(position = c("left", "bottom"),type = "arrow",size = 1.5) + 
  tm_scale_bar(position = c("left", "bottom"),text.size =0.3) +
  tm_layout(title="Violent Crime",title.size=1,frame=FALSE,legend.position = c("right", "bottom"),legend.title.size = 0.6,legend.text.size = 0.5)
dev.off()

png("t_value_living.png",width=1085,height=963,res=216)
tm_shape(crimefactor2) + 
  tm_polygons("GWR_t_value_living_environment_deprivation", 
              style="fixed",
              palette="Blues",
              breaks = c(4.6, 6.56, 8.52, 10.48, 12.1),
              midpoint=NA,
              title="t-value LivingEnvironment Deprivation",
              alpha = 0.5) + 
  tm_compass(position = c("left", "bottom"),type = "arrow",size = 1.5) + 
  tm_scale_bar(position = c("left", "bottom"),text.size =0.3) +
  tm_layout(title="Violent Crime",title.size=1,frame=FALSE,legend.position = c("right", "bottom"),legend.title.size = 0.6,legend.text.size = 0.5)
dev.off()