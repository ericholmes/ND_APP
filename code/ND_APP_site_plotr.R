
library(tidyverse)
library(leaflet)
library(sf)

load(file = "data/Zooper_and_EDSM_EDI_download.Rdata")
load("C:/Users/eholmes/Documents/R/Projects/Calneva2025_Rworkshop/data/Calneva2025_workshop_data.Rdata")

# Prep Zooper data --------------------------------------------------------

##filter out total counts
zoop <- MyZoops[!(grepl(MyZoops$Taxname, pattern = "all_Meso")),]
zoopsums <- MyZoops[MyZoops$Taxatype == "Summed group",]

unique(zoopsums$Taxlifestage)

##Add julien day, year, month and water year
zoop$jday <- as.numeric(format(as.Date(zoop$Date), format = "%j"))
zoop$year <- as.integer(format(zoop$Date, format = "%Y"))
zoop$month <- as.integer(format(zoop$Date, format = "%m"))
zoop$wy <- ifelse(zoop$month %in% c(10:12), zoop$year + 1, zoop$year)

zoop$Longitude <- ifelse(zoop$Longitude >0, -zoop$Longitude, zoop$Longitude)
zoop_sp <- zoop %>% group_by(Source, Station, Latitude, Longitude, Datetime) %>% summarise() %>% data.frame()
zoopsf <- st_as_sf(zoop_sp[is.na(zoop_sp$Latitude) == F,], coords = c("Longitude", "Latitude"), crs = 4269)
##transform CRS to match deltamapr projection
zoopsf <- st_transform(zoopsf, st_crs(R_EDSM_Regions_1718P1))
# Prep EDSM data ----------------------------------------------------------

#dt1 is 20mm survey
#dt2 is kodiak trawl
#dt3 is fish taxonomy

# Merge fish metadata -----------------------------------------------------
colnames(dt1)
dt1sp <- dt1 %>% group_by(RegionCode, Subregion, StationCode, Latitude, Longitude, SampleDate, MethodCode) %>% 
                          #MethodCode, CommonName, OrganismCode, ForkLength, Count) %>% 
  summarize() %>% data.frame()
dt2sp <- dt2 %>% group_by(RegionCode, Subregion, StationCode, Latitude, Longitude, SampleDate, MethodCode) %>%
                          #MethodCode, CommonName, OrganismCode, ForkLength, Count) %>% 
  summarize() %>% data.frame()

dtsp <- rbind(dt1sp, dt2sp)

##Convert sites dataframe to sf object
dtsf <- st_as_sf(dtsp, coords = c("Longitude", "Latitude"), crs = 4269)

##transform CRS to match deltamapr projection
dtsf <- st_transform(dtsf, st_crs(R_EDSM_Regions_1718P1))

# leaflet(dtsp) %>% addCircles(lng = ~Longitude, lat = ~Latitude,
#                                            label = ~StationCode,
#                                            labelOptions = labelOptions(noHide = T, textOnly = F)) %>%
#   addTiles(options = providerTileOptions(noWrap = TRUE), group="Base") %>%
#   addProviderTiles("Esri.WorldImagery", group="Imagery") %>%
#   addLayersControl(baseGroups = c("Base","Imagery"), options = layersControlOptions(collapsed = FALSE))

(sfemap <- ggplot() + 
    geom_sf(data = WW_Delta, fill = "blue", color = "blue") + 
    geom_sf(data = R_EDSM_Regions_1718P1$geometry, fill = 1:4, alpha = .3) +
    geom_sf(data = R_EDSM_Regions_1718P1$geometry, fill = NA, linewidth = 1, color = 1) +
    geom_sf(data = dtsf$geometry, size = 2) +
    geom_sf(data = zoopsf$geometry, size = 2, color = "red") +
    theme_bw())


# Spatial join EDSM regions -----------------------------------------------

dtsf_join <- st_join(dtsf, R_EDSM_Regions_1718P1, left = T)
zoopsf_join <- st_join(zoopsf, R_EDSM_Regions_1718P1, left = T)   

dtsf_join[dtsf_join$Region == "North",]
if(saveoutput == T){tiff("output/NDAPP_zooper+edsm_map_%02da.tif",
                         height = 6, width = 4, units = "in", res = 1000, family = "serif", compression = "lzw")}

(sfemap <- ggplot() + 
    geom_sf(data = WW_Delta, fill = "blue", color = "blue") + 
    geom_sf(data = R_EDSM_Regions_1718P1$geometry, fill = 1:4, alpha = .3) +
    geom_sf(data = R_EDSM_Regions_1718P1$geometry, fill = NA, linewidth = 1, color = 1) +
    geom_sf(data = dtsf_join[dtsf_join$Region == "North",], size = 2, alpha = .5) +
    geom_sf(data = zoopsf_join[zoopsf_join$Region == "North",], size = 2, color = "red", alpha = .5) +
    coord_sf(xlim = c(-121.86, -121.48), ylim = c(38.06, 38.61), expand = FALSE) +
    theme_bw())

if(saveoutput == T){dev.off()}

# Summarize sampling time frame -------------------------------------------
zoopsf_join$Date <- as.Date(zoopsf_join$Datetime)
unique(zoopsf_join$Region)

if(saveoutput == T){png("output/NDAPP_zooper+edsm_ts_%02da.png",
                         height = 6, width = 4, units = "in", res = 1000, family = "serif")}
cowplot::plot_grid(
  ggplot(zoopsf_join[zoopsf_join$Region %in% "North",], aes(x = Date)) + geom_bar() + 
    facet_grid(Source ~ .) + theme_bw() + 
    labs(title = "Zoop sampling in N. Delta by program"),
  
  ggplot(dtsf[dtsf_join$Region %in% "North",], aes(x = SampleDate)) + geom_bar() + 
    facet_grid(MethodCode ~ .) + theme_bw() + 
    labs(title = "EDSM sampling in N. Delta"),
  nrow = 2)
if(saveoutput == T){dev.off()}