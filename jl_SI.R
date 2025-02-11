library(tidyverse) #Calling in tidyverse for data management
library(readxl) #Calling in readxl to read in excel files
library(sf)     # Spatial Package
library(terra) #spatial package
library(gstat) #geospatial package
library(tmap) #mapping package
library(dataRetrieval) #Used to retrieve data from USGS
library(viridis)
library('parallel')
library(sp)
#### Calling in isotope data ####
all.iso<-read_csv("~/Documents/Data/Chapter.3/Isotope.Data/isotope.data") %>% 
  filter(SITE != "JL3.7" & SITE != "JL8.7" & Setting.Type == "Lake") %>% 
  subset(select = c(SITE, Event, d18O, d2H, dxs)) %>% 
  group_by(SITE) %>% 
  reframe(d18O = mean(d18O),
          d2H = mean(d2H),
          dxs = mean(dxs),
          Event = Event) %>% 
  distinct()

# Calling Depth data#
#JL.depth.202308 <- read_csv('~/Documents/Data/Jackson_Lake_Bathy/2023.08.23.depths.csv',show_col_types = FALSE)
#Calling GRTE LiDar
grte <- rast("~/Documents/Data/Chapter.1/Watershed.Delineation/dem_fill.tif")
tm_shape(grte)+
  tm_raster(col.scale = tmap::tm_scale_continuous(
    values = "brewer.rd_yl_gn",
    midpoint = NA))

blavet_bbox <- st_bbox(c(xmin = 230000, xmax = 258000, ymax = 545000, ymin = 515000), 
                       crs = st_crs(grte))
blavet_loc <- st_as_sfc(blavet_bbox)|> st_sf()
blavet_loc <- st_transform(blavet_loc, crs = st_crs(grte))


plot(grte)
plot(JL, add = TRUE)
test <- crop(grte, blavet_loc)
plot(test)
plot(JL2, add = TRUE)
#### Calling in the spatial information & setting up Jackson Lake Grid ####
JL_SP <- read_csv('~/Documents/Data/Lake_YSI/Coords.csv',show_col_types = FALSE) %>% #Calling the latitude and longitude of each sampling point
  filter(grepl('JL', SITE))
JL_SP10 <- read_csv('~/Documents/Data/Lake_YSI/Coord_10.csv',show_col_types = FALSE)  #Calling the latitude and longitude of each sampling point


JL <- read_sf(dsn = "~/Documents/Data/Jackson_Lake_Shapefiles/", layer = "Jack_Lake_Final") #Calling the Jackson Lake shapefile
JL2 <- vect("~/Documents/Data/Jackson_Lake_Shapefiles/", layer = "Jack_Lake_Final") #Calling the Jackson Lake shapefile
JL_SP <- st_as_sf(JL_SP, coords = c('Long', 'Lat'),crs = 4326) #taking the lat and long coordinates in dd and creating a geometric feature of the long lat
JL_SP10 <- st_as_sf(JL_SP10, coords = c('Long', 'Lat'),crs = 4326) #taking the lat and long coordinates in dd and creating a geometric feature of the long lat

JL_SP <- st_transform(JL_SP, crs = st_crs(JL)) %>%
  dplyr::mutate(x = sf::st_coordinates(.)[,1],
                y = sf::st_coordinates(.)[,2])

JL_SP10 <- st_transform(JL_SP10, crs = st_crs(JL)) %>%
  dplyr::mutate(x = sf::st_coordinates(.)[,1],
                y = sf::st_coordinates(.)[,2])

# event8_sp <- merge(JL_SP, event8, by = "SITE")
# event8_sp <- merge(event8_sp, JL.depth.202308, by = "SITE")
# 
# event9_sp <- merge(JL_SP, event9, by = "SITE")
# event9_sp <- merge(event9_sp, JL.depth.202308, by = "SITE")


grid <- terra::rast(JL, nrows = 1000, ncols = 1000) #Creates a raster grid of 1000 X 1000  
# coordinates of all cells
xy <- terra::xyFromCell(grid, 1:ncell(grid)) #This is gives coordinates to each one of the points that have been created


coop <- st_as_sf(as.data.frame(xy), coords = c("x", "y"),
                 crs = st_crs(JL)) #this creates a dataframe with the points from the recently created grid giving the crs as JL
coop <- st_filter(coop, JL) #This essentially clips the grid that was created to the shapefile of Jackson Lake.

qtm(coop)
####Voronoi####
#Calling Isotope data for each sampling event, this will allow me to form 
#voronoi polygons around points sampled during each event
#Calling isotopic data to know what points I sampled during each event
e7 <- all.iso %>% 
  filter(Event == 7) 
  
e8 <- all.iso %>% 
  filter(Event == 8)

e9 <- all.iso %>% 
  filter(Event == 9)

e10 <- all.iso %>% 
  filter(Event == 10)

e11 <- all.iso %>% 
  filter(Event == 11)

e12 <- all.iso %>% 
  filter(Event == 12)
#Merging points actual sampled points.
JLe7 <- JL_SP %>% 
  merge(e7)
JLe8 <- JL_SP %>% 
  merge(e8)
JLe9 <- JL_SP %>% 
  merge(e9)
JLe10 <- JL_SP10 %>% 
  merge(e10)
JLe11 <- JL_SP %>% 
  merge(e11)
JLe12 <- JL_SP %>% 
  merge(e12)
#Making voronoi polygons based on the sampling locations
v7 <- terra::voronoi(x = vect(JLe7), bnd = JL)
v8 <- terra::voronoi(x = vect(JLe8), bnd = JL)
v9 <- terra::voronoi(x = vect(JLe9), bnd = JL)
v10 <- terra::voronoi(x = vect(JLe10), bnd = JL)
v11 <- terra::voronoi(x = vect(JLe11), bnd = JL)
v12 <- terra::voronoi(x = vect(JLe12), bnd = JL)
#Cropping the polygons based on the jackson lake shapefile
cv7 <- crop(v7, JL2) 
cv8 <- crop(v8, JL2)
cv9 <- crop(v9, JL2)
cv10 <- crop(v10, JL2)
cv11 <- crop(v11, JL2)
cv12 <- crop(v12, JL2)
#calculating the area of each polygon
cv7$v.area.m2 <- expanse(cv7)
cv8$v.area.m2 <- expanse(cv8)
cv9$v.area.m2 <- expanse(cv9)
cv10$v.area.m2 <- expanse(cv10)
cv11$v.area.m2 <- expanse(cv11)
cv12$v.area.m2 <- expanse(cv12)
#creating dataframes of site name and area
v7.a <- data.frame(SITE = cv7[["SITE"]],
                   v.area.m2 = cv7[["v.area.m2"]],
                   Event = 7)
v8.a <- data.frame(SITE = cv8[["SITE"]],
                   v.area.m2 = cv8[["v.area.m2"]],
                   Event = 8)
v9.a <- data.frame(SITE = cv9[["SITE"]],
                   v.area.m2 = cv9[["v.area.m2"]],
                   Event = 9)
v10.a <- data.frame(SITE = cv10[["SITE"]],
                    v.area.m2 = cv10[["v.area.m2"]],
                    Event = 10)
v11.a <- data.frame(SITE = cv11[["SITE"]],
                    v.area.m2 = cv11[["v.area.m2"]],
                    Event = 11)
v12.a <- data.frame(SITE = cv12[["SITE"]],
                    v.area.m2 = cv12[["v.area.m2"]],
                    Event = 12)

#Writing to disk, Has been written
# write_csv(v7.a, "~/Documents/Data/Chapter.3/IMB/variables.data.tables/voronoi_areas/v7a")
# write_csv(v8.a, "~/Documents/Data/Chapter.3/IMB/variables.data.tables/voronoi_areas/v8a")
# write_csv(v9.a, "~/Documents/Data/Chapter.3/IMB/variables.data.tables/voronoi_areas/v9a")
# write_csv(v10.a, "~/Documents/Data/Chapter.3/IMB/variables.data.tables/voronoi_areas/v10a")
# write_csv(v11.a, "~/Documents/Data/Chapter.3/IMB/variables.data.tables/voronoi_areas/v11a")
# write_csv(v12.a, "~/Documents/Data/Chapter.3/IMB/variables.data.tables/voronoi_areas/v12a")
rm(e7,e8,e9,e10,e11,e12)
#### Functions for Inverse Distance Weighting and Nearest Neighbor. ####
#Inverse Distance Weighting:This interpolates values between points by considering all points that have been sampled.Sampling points are weighted with the weights inversely proporational to the distance between the unsampled and sampled locations.

idw.function <- function(variable, dataframe, idw.b){
  gstat(formula = variable ~ 1, locations = dataframe,
        nmax = nrow(dataframe), # use all the neighbors locations
        #nmax = 3,
        set = list(idp = idw.b))} # beta = 1, This is the idw function,using a variable in the formula with an intercept only model, locations are taken from the sp object JL_2023_1, nmax is the number of sites is uses, idp is the beta having weights of 1

#This is the prediction function for Hydrogen. I have created one for hydrogen, oxygen, and dxs
pred.function.idw.h <- function(idw_res, pred.dataframe.points, pred.grid, idw.b, num.classes, points){
  resp <- predict(idw_res, pred.dataframe.points) #this is what creates the predictions on top of the grid that has been constructed
  resp$x <- st_coordinates(resp)[,1] #retrieves the x coordinates
  resp$y <- st_coordinates(resp)[,2] #retrieves the y coordinates
  resp$pred <- resp$var1.pred #creates a prediction column
  
  pred <- terra::rasterize(resp, pred.grid, field = "pred", fun = "mean") #Creating the raster prediction surface
  tm_shape(test)+
    tm_raster(col.scale = tmap::tm_scale_continuous(
      values = "grey",
      midpoint = NA),
      col.legend = tmap::tm_legend_hide()) + 
    tm_shape(pred) + tm_raster(col.scale = tmap::tm_scale_continuous(
    values = "viridis",
    midpoint = NA),
    col.legend = tmap::tm_legend(
      title = expression(paste(delta^2, "H (\u2030)")),
      title.size = 1,
      reverse = TRUE,
      text.size = 1,
      bg.color = "white",
      bg.alpha = 0.7,
      position = tmap::tm_pos_in("right", "top"),
      frame = TRUE))+
    tm_options(component.autoscale = (FALSE))+ 
    tm_shape(points)+ tm_dots(col = "d2H", size = 0.4, palette = "black",legend.show = FALSE)}


#Creating the hydrogen spatial interpolations, inverse weight is 3. most common
#is 2 visually 3 is how I would expect the water isotopes to behave.
JL_idw_H.7 <- idw.function(JLe7$d2H, JLe7, 3) 
JL_idw.H.7 <- pred.function.idw.h(JL_idw_H.7, coop, grid,1, 5, JLe7)
JL_idw.H.7
JL_idw_H.8 <- idw.function(JLe8$d2H, JLe8, 3) 
JL_idw.H.8 <- pred.function.idw.h(JL_idw_H.8, coop, grid,1, 10, JLe8)
JL_idw.H.8
JL_idw_H.9 <- idw.function(JLe9$d2H, JLe9, 3) 
JL_idw.H.9 <- pred.function.idw.h(JL_idw_H.9, coop, grid,1, 10, JLe9)
JL_idw.H.9
JL_idw_H.10 <- idw.function(JLe10$d2H, JLe10, 3) 
JL_idw.H.10 <- pred.function.idw.h(JL_idw_H.10, coop, grid,1, 10, JLe10)
JL_idw.H.10
JL_idw_H.11 <- idw.function(JLe11$d2H, JLe11, 3) 
JL_idw.H.11 <- pred.function.idw.h(JL_idw_H.11, coop, grid,1, 10, JLe11)
JL_idw.H.11
JL_idw_H.12 <- idw.function(JLe12$d2H, JLe12, 3) 
JL_idw.H.12 <- pred.function.idw.h(JL_idw_H.12, coop, grid,1, 10, JLe12)
JL_idw.H.12


#This is the prediction function for oxygen.
pred.function.idw.o <- function(idw_res, pred.dataframe.points, pred.grid, idw.b, num.classes, points){
  resp <- predict(idw_res, pred.dataframe.points) #this is what creates the predictions on top of the grid that has been constructed
  resp$x <- st_coordinates(resp)[,1] #retrieves the x coordinates
  resp$y <- st_coordinates(resp)[,2] #retrieves the y coordinates
  resp$pred <- resp$var1.pred #creates a prediction column
  
  pred <- terra::rasterize(resp, pred.grid, field = "pred", fun = "mean") #Creating the raster prediction surface
  #plot.title <- paste("Inverse Distance Weighting, beta =", idw.b, sep = " ")
  plot.title <- paste("September Interpolation 2023 (IDW)")
  tm_shape(pred) + tm_raster(alpha = 0.6, palette = "viridis", n = num.classes, title = expression(paste(delta^18, "O (\u2030)"))) + tm_shape(points)+
    tm_dots(col = "d18O", size = 0.2, palette = "viridis",legend.show = FALSE) +     
    tm_layout(main.title = plot.title,main.title.size = 1,legend.outside = TRUE, legend.title.size = 2)} #plotting the raster
# legend.position = c("right", "top"),

JL_idw_O.8 <- idw.function(JLe7$d18O, JLe7, 8)
JL_idw.O.8 <- pred.function.idw.o(JL_idw_O.8, coop, grid,1, 10, JLe7)
JL_idw.O.8

#This is the prediction function for dxs.
pred.function.idw.dxs <- function(idw_res, pred.dataframe.points, pred.grid, idw.b, num.classes, points){
  resp <- predict(idw_res, pred.dataframe.points) #this is what creates the predictions on top of the grid that has been constructed
  resp$x <- st_coordinates(resp)[,1] #retrieves the x coordinates
  resp$y <- st_coordinates(resp)[,2] #retrieves the y coordinates
  resp$pred <- resp$var1.pred #creates a prediction column
  
  pred <- terra::rasterize(resp, pred.grid, field = "pred", fun = "mean") #Creating the raster prediction surface
  #plot.title <- paste("Inverse Distance Weighting, beta =", idw.b, sep = " ")
  plot.title <- paste("September Interpolation 2023 (IDW)")
  tm_shape(pred) + tm_raster(alpha = 0.6, palette = "-viridis", n = num.classes, title = "d-excess") + tm_shape(points)+
    tm_dots(col = "dxs", size = 0.2, palette = "-viridis",legend.show = FALSE) +     
    tm_layout(main.title = plot.title,main.title.size = 1,legend.outside = TRUE, legend.title.size = 1)} #plotting the raster

JL_idw_dxs.8 <- idw.function(JLe7$dxs, JLe7, 8)
JL_idw.dxs.8 <- pred.function.idw.dxs(JL_idw_dxs.8, coop, grid,1, 10, JLe7)
JL_idw.dxs.8

JL_idw_H.9 <- idw.function(event9_sp$d2H, event9_sp, 2) 
JL_idw.H.9 <- pred.function.idw.h(JL_idw_H.9, coop, grid,2, 6)
JL_idw.H.9

JL_idw_O.9 <- idw.function(event9_sp$d18O, event9_sp, 2)
JL_idw.O.9 <- pred.function.idw.o(JL_idw_O.9, coop, grid,2, 5)
JL_idw.O.9

JL_idw_dxs.9 <- idw.function(event9_sp$dxs, event9_sp, 2)
JL_idw.dxs.9 <- pred.function.idw.dxs(JL_idw_dxs.9, coop, grid,2, 8)
JL_idw.dxs.9


####Nearest Neighbor #####
res <- gstat(formula = d18O ~ 1, locations = event9_sp, nmax = 55,
             set = list(idp = 0))

resp <- predict(res, coop)
resp$x <- st_coordinates(resp)[,1]
resp$y <- st_coordinates(resp)[,2]
resp$pred <- resp$var1.pred

plot.title <- paste("September Interpolation 2023 (NN)")

pred <- terra::rasterize(resp, grid, field = "pred", fun = "mean")
tm_shape(pred) + tm_raster(alpha = 0.6, palette = "viridis", n = 7, title = expression(paste(delta^18, "O (\u2030)")))+ tm_shape(event9_sp)+
  tm_dots(col = "d18O", size = 0.2, palette = "viridis",legend.show = FALSE) +     
  tm_layout(main.title = plot.title,main.title.size = 1,legend.outside = TRUE, legend.title.size = 1) #plotting the raster


#### Variogram ####
## Constructing a variogram for dxs from Sampling event 8 ##

hist(event8_sp$dxs)
dxsvc <- variogram((dxs) ~ 1, event8_sp, cloud = TRUE)
plot(dxsvc) #This is a variogram cloud.

dxsv <- variogram((dxs) ~ 1, data = event8_sp)
plot(dxsv) #This is a sample variogram

dxsvinitial <- vgm(psill = 0.6, model = "Wav",
                   range = 2000, nugget = 0.0)
plot(dxsv, dxsvinitial, cutoff = 1000, cex = 1.5)

dxsfv <- fit.variogram(object = dxsv,
                       model = vgm(psill =0.6, model = "Gau",
                                   range = 2000, nugget = 0.0001))
dxsfv
plot(dxsv, dxsfv, cex = 1.5)

####
dxsv <- variogram((dxs) ~ 1, data = event9_sp, alpha = c(0, 45, + 90, 135))
plot(dxsv) #This is a sample variogram

dxsv <- variogram((dxs) ~ 1, data = event8_sp)
plot(dxsv) #This is a sample variogram


dxsvinitial <- vgm(psill = 1, model = "Nug", nugget = 0.2)
plot(dxsv, dxsvinitial, cutoff = 1000, cex = 1.5)

dxsfv <- fit.variogram(object = dxsv,
                       model = vgm("Nug"))
dxsfv
plot(dxsv, dxsfv, cex = 1.5)



#### Kriging Interpolation plots ####
## dxs interpolation ##
k <- gstat(formula = dxs ~ 1, data = event8_sp, model = dxsfv)

kpred <- predict(k, coop)
kpred$dxs <- exp(kpred$var1.pred + 0.5 * (kpred$var1.var))

ggplot() + geom_sf(data = kpred, aes(color = var1.pred)) +
  geom_sf(data = event8_sp) +
  scale_color_viridis(name = "dxs", direction = -1) + theme_bw() +
  ggtitle("August Interpolation (Kriging)")+ guides(color=guide_colorbar(title='d-excess')) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        legend.text=element_text(size=14),
        legend.title = element_text(size=14))

ggplot() + geom_sf(data = kpred, aes(color = var1.var)) +
  geom_sf(data = event8_sp) +
  scale_color_viridis(name = "variance", direction = -1) + theme_bw()

## Constructing a variogram for d18O from Sampling event 8 ##
hist(event8_sp$d18O)
Ovc <- variogram((d18O) ~ 1, event8_sp, cloud = TRUE)
plot(Ovc) #This is a variogram cloud.

Ov <- variogram(d18O ~ 1, data = event8_sp)
plot(Ov) #This is a sample variogram

Ovinitial <- vgm(psill = 0.015, model = "Sph",
                 range = 3000, nugget = 0.005)
plot(Ov, Ovinitial, cutoff = 1000, cex = 1.5)


Ofv <- fit.variogram(object = Ov,
                     model = vgm(psill = 0.015, model = "Sph",
                                 range = 3000, nugget = 0.005))
Ofv
plot(Ov, Ofv, cex = 1.5)


## d18O interpolation ##
k <- gstat(formula = d18O ~ 1, data = event8_sp, model = Ofv)

kpred <- predict(k, coop)
kpred$d18O <- exp(kpred$var1.pred + 0.5 * (kpred$var1.var))

ggplot() + geom_sf(data = kpred, aes(color = var1.pred)) +
  geom_sf(data = event8_sp) +
  scale_color_viridis(name = "d180") + theme_bw()+
  ggtitle("August Interpolation (Kriging)")+ guides(color=guide_colorbar(title=expression(paste(delta^18, "O (\u2030)")))) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        legend.text=element_text(size=14),
        legend.title = element_text(size=14))

ggplot() + geom_sf(data = kpred, aes(color = var1.var)) +
  geom_sf(data = event8_sp) +
  scale_color_viridis(name = "variance") + theme_bw()

## Constructing a variogram for d2h from Sampling event 8 ##
hist(event8_sp$d2H)
Hvc <- variogram((d2H) ~ 1, event8_sp, cloud = TRUE)
plot(Hvc) #This is a variogram cloud.

Hv <- variogram(d2H ~ 1, data = event8_sp)
plot(Hv) #This is a sample variogram

Hvinitial <- vgm(psill = 0.15, model = "Exp",
                 range = 2500, nugget = 0.0)
plot(Hv, Hvinitial, cutoff = 1000, cex = 1.5)


Hfv <- fit.variogram(object = Hv,
                     model = vgm(psill = .2, model = "Exp",
                                 range = 4000, nugget = 0.0))
Hfv
plot(Hv, Hfv, cex = 1.5)


## d2h interpolation ##
k <- gstat(formula = d2H ~ 1, data = event8_sp, model = Hfv)

kpred <- predict(k, coop)
kpred$d2H <- exp(kpred$var1.pred + 0.5 * (kpred$var1.var))

ggplot() + geom_sf(data = kpred, aes(color = var1.pred)) +
  geom_sf(data = event8_sp) +
  scale_color_viridis(name = "d2H") + theme_bw()+
  ggtitle("August Interpolation (Kriging)")+ guides(color=guide_colorbar(title=expression(paste(delta^2, "H (\u2030)")))) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        legend.text=element_text(size=14),
        legend.title = element_text(size=14))

ggplot() + geom_sf(data = kpred, aes(color = var1.var)) +
  geom_sf(data = event8_sp) +
  scale_color_viridis(name = "variance") + theme_bw()

## Constructing a variogram for dxs from Sampling event 9 ##
hist(event9_sp$dxs)
dxsvc <- variogram((dxs) ~ 1, event9_sp, cloud = TRUE)
plot(dxsvc) #This is a variogram cloud.

dxsv <- variogram(dxs ~ 1, data = event9_sp)
plot(dxsv) #This is a sample variogram

dxsv1 <- variogram((dxs) ~ 1, data = event9_sp)
plot(dxsv1)

dxsvinitial <- vgm(psill = 0.25, model = "Nug",
                   range = 2000, nugget = 0.25)
plot(dxsv, dxsvinitial, cutoff = 1000, cex = 1.5)


dxsfv <- fit.variogram(object = dxsv,
                       model = vgm(psill = 0.25, model = "Wav",
                                   range = 2500, nugget = 0.25))
dxsfv
plot(dxsv, dxsfv, cex = 1.5)

## Constructing a variogram for d18O from Sampling event 8 ##
hist(event9_sp$d18O)
Ovc <- variogram((d18O) ~ 1, event9_sp, cloud = TRUE)
plot(Ovc) #This is a variogram cloud.

Ov <- variogram(d18O ~ 1, data = event9_sp)
plot(Ov) #This is a sample variogram

Ovinitial <- vgm(psill = 0.01, model = "Sph",
                 range = 2000, nugget = 0.00)
plot(Ov, Ovinitial, cutoff = 1000, cex = 1.5)


Ofv <- fit.variogram(object = Ov,
                     model = vgm(psill = 0.01, model = "Sph",
                                 range = 2000, nugget = 0.))
Ofv
plot(Ov, Ofv, cex = 1.5)

#### Kriging Interpolation plots ####
## dxs interpolation ##
k <- gstat(formula = dxs ~ 1, data = event9_sp, model = dxsfv)

kpred <- predict(k, coop)
kpred$dxs <- exp(kpred$var1.pred + 0.5 * (kpred$var1.var))

ggplot() + geom_sf(data = kpred, aes(color = var1.pred)) +
  geom_sf(data = event8_sp) +
  scale_color_viridis(name = "dxs") + theme_bw()

ggplot() + geom_sf(data = kpred, aes(color = var1.var)) +
  geom_sf(data = event8_sp) +
  scale_color_viridis(name = "variance") + theme_bw()

## d18O interpolation ##
k <- gstat(formula = d18O ~ 1, data = event8_sp, model = Ofv)

kpred <- predict(k, coop)
kpred$d18O <- exp(kpred$var1.pred + 0.5 * (kpred$var1.var))

ggplot() + geom_sf(data = kpred, aes(color = var1.pred)) +
  geom_sf(data = event8_sp) +
  scale_color_viridis(name = "dxs") + theme_bw()

ggplot() + geom_sf(data = kpred, aes(color = var1.var)) +
  geom_sf(data = event8_sp) +
  scale_color_viridis(name = "variance") + theme_bw()
#### Dual Isotope Plots ####

ggplot() +
  geom_point(data = run1, aes(x = d18O, y = d2H, col = Setting.Type)) +
  geom_point(data = run2, aes(x = d18O, y = d2H, col = Setting.Type)) +      
  geom_point(data = run3, aes(x = d18O, y = d2H, col = Setting.Type)) +
  geom_point(data = run4, aes(x = d18O, y = d2H, col = Setting.Type)) +
  geom_point(data = run5, aes(x = d18O, y = d2H, col = Setting.Type)) +
  geom_point(data = run6, aes(x = d18O, y = d2H, col = Setting.Type)) +
  geom_point(data = run7, aes(x = d18O, y = d2H, col = Setting.Type)) +
  geom_point(data = run8, aes(x = d18O, y = d2H, col = Setting.Type)) +
  geom_point(data = run9, aes(x = d18O, y = d2H, col = Setting.Type)) +
  geom_point(data = run10, aes(x = d18O, y = d2H, col = Setting.Type)) +
  geom_point(data = run11, aes(x = d18O, y = d2H, col = Setting.Type)) +
  geom_point(data = run12, aes(x = d18O, y = d2H, col = Setting.Type)) +
  geom_point(data = run13, aes(x = d18O, y = d2H, col = Setting.Type)) +
  geom_point(data = run14, aes(x = d18O, y = d2H, col = Setting.Type)) +
  geom_abline(aes(intercept = 8.09, slope = 7.95, col = "LMWL")) + #LMWL from Benjamin (2005)
  geom_abline(aes(intercept = 3.12, slope = 7.94, col = "Idaho Falls LMWL")) #From the 1976-1977 Idaho Fall LMWL

ggplot() +
  geom_point(data = all.iso, aes(x = d18O, y = d2H, col = as.factor(Event))) + 
  geom_abline(aes(intercept = 8.09, slope = 7.95)) + #LMWL from Benjamin (2005)
  geom_abline(aes(intercept = 3.12, slope = 7.94)) #From the 1976-1977 Idaho Fall LMWL


ggplot() +
  geom_point(data = run1, aes(x = d18O, y = d2H, col = "Run 1")) +
  geom_point(data = run2, aes(x = d18O, y = d2H, col = "Run 2")) +      
  geom_point(data = run3, aes(x = d18O, y = d2H, col = "Run 3")) +
  geom_point(data = run4, aes(x = d18O, y = d2H, col = "Run 4")) +
  geom_point(data = run5, aes(x = d18O, y = d2H, col = "Run 5")) +
  geom_point(data = run6, aes(x = d18O, y = d2H, col = "Run 6")) +
  geom_point(data = run7, aes(x = d18O, y = d2H, col = "Run 7")) +
  geom_point(data = run8, aes(x = d18O, y = d2H, col = "Run 8")) +
  geom_point(data = run9, aes(x = d18O, y = d2H, col = "Run 9")) +
  geom_point(data = run10, aes(x = d18O, y = d2H, col = "Run 10")) +
  geom_point(data = run11, aes(x = d18O, y = d2H, col = "Run 11")) +
  geom_point(data = run12, aes(x = d18O, y = d2H, col = "Run 12")) +
  geom_point(data = run13, aes(x = d18O, y = d2H, col = "Run 13")) +
  geom_point(data = run14, aes(x = d18O, y = d2H, col = "Run 14")) +
  #scale_color_viridis_d()+
  geom_abline(aes(intercept = 8.09, slope = 7.95)) + #LMWL from Benjamin (2005)
  geom_abline(aes(intercept = 3.12, slope = 7.94)) 


ggplot() +
  geom_boxplot(data = all.iso, aes(x = SITE, y = dxs))+ 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))




res <- gstat(formula = event8_sp$dxs ~ 1, locations = event8_sp, nmax = 5,
             set = list(idp = 0))

resp <- predict(res, coop)
resp$x <- st_coordinates(resp)[,1]
resp$y <- st_coordinates(resp)[,2]
resp$pred <- resp$var1.pred

pred <- terra::rasterize(resp, grid, field = "pred", fun = "mean")
tm_shape(pred) + tm_raster(alpha = 0.6, palette = "viridis")


####Lake Interpolation####
JL_YSI_sur <- read_csv('~/Documents/Data/Lake_YSI/2023_YSI.csv',show_col_types = FALSE) %>%  
  mutate(DATE = as.Date(DATE, "%m/%d/%Y")) %>% 
  filter(sample.event.normal.scheme == 7 |sample.event.normal.scheme == 8|sample.event.normal.scheme == 9| depth.m.2 == 0.5)

ysi.7 <- JL_YSI_sur %>% 
  filter(sample.event.normal.scheme == 7 & depth.m.2 == 0.5)
ysi.8 <- JL_YSI_sur %>% 
  filter(sample.event.normal.scheme == 8 & depth.m.2 == 0.5)
ysi.9 <- JL_YSI_sur %>% 
  filter(sample.event.normal.scheme == 9 & depth.m.2 == 0.5)

####Kriging####
hist(event9_sp$d18O)
Ovc <- variogram((d18O) ~ 1, event9_sp, cloud = TRUE)
plot(Ovc) #This is a variogram cloud.

Ov <- variogram(d18O ~ 1, data = event9_sp)
plot(Ov) #This is a sample variogram

Ovinitial <- vgm(psill = 0.015, model = "Sph",
                 range = 3000, nugget = 0.005)
plot(Ov, Ovinitial, cutoff = 1000, cex = 1.5)


Ofv <- fit.variogram(object = Ov,
                     model = vgm(psill = 0.015, model = "Sph",
                                 range = 3000, nugget = 0.005))
Ofv
plot(Ov, Ofv, cex = 1.5)


## d18O interpolation ##
k <- gstat(formula = d18O ~ 1, data = event9_sp, model = Ofv)

kpred <- predict(k, coop)
kpred$d18O <- exp(kpred$var1.pred + 0.5 * (kpred$var1.var))

ggplot() + geom_sf(data = kpred, aes(color = var1.pred)) +
  geom_sf(data = event8_sp) +
  scale_color_viridis(name = "d180") + theme_bw()+
  ggtitle("July Interpolation (Kriging)")+ guides(color=guide_colorbar(title=expression(paste(delta^18, "O (\u2030)")))) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        legend.text=element_text(size=14),
        legend.title = element_text(size=14))

ggplot() + geom_sf(data = kpred, aes(color = var1.var)) +
  geom_sf(data = event8_sp) +
  scale_color_viridis(name = "variance") + theme_bw()


