# MoveShape creates videos out of time referenced shapefiles
# Raumanalysen - Christian Mueller
# raumanalysen@mailbox.org

# data input
shp_dir <- "C:/Raumanalysen_ChristianMueller/Projekte/Bergheim_Animation/Arbeitsdaten"
shps_names <- c("Uraufnahme_Staedte",
                "1891-1912_Staedte",
                "1936-1945_Staedte", 
                "1988-1994_Staedte",
                "2015_Staedte")
shps_show_names <- c("Uraufnahme",
                     "1891-1912",
                     "1936-1945",
                     "1988-1994",
                     "2015")
lg_name_show <- "Städte im Laufe der Zeit"
bg_shp <- "Stadtgebiet"
bg_shp_name_show <- "heutiges Stadtgebiet"
save_dir <- "C:/Raumanalysen_ChristianMueller/Projekte/Bergheim_Animation/Arbeitsdaten"
video_name <- "StadtentwicklungBergheim.avi"

# load packages
library(rgdal)
library(sp)
library(animation)
library(tweenr)
if (Sys.which('ffmpeg') == ""){
  source("install_ffmpeg.r")
  install_ffmpeg()
}

# load background data
bg <- readOGR(shp_dir, bg_shp)

# define coordinate system
WGSproj <- CRS("+init=epsg:4326")

# reproject background map
bg <- spTransform(bg, WGSproj)


shp1@polygons$coords
tween_elements(data = rbind(shp1, shp2), n = 10)

# set animation options
ani.options(
  nmax = 500,
  interval = 2,
  ani.width = 1920,
  ani.height = 1440
  )

# create animation
saveVideo({

  # create plots
  for (s in 1:length(shps_names)){
    
    # save additionally as png
    png(filename = paste0(save_dir, "/animationFrame_", s, ".png"),
        width = 1920, height = 1440)
    
    # load spatial data
    shp <- readOGR(shp_dir, shps_names[s])
    
    # reproject shapefile
    shp <- spTransform(shp, WGSproj)
    
    # plot backgroud
    plot(bg, col = "lightblue", border = "lightblue")
    
    # add time step as title
    title(main = shps_show_names[s], cex.main = 3)
  
    # plot shape
    plot(shp, col = "coral", border = "coral", add = T)
  
    # add legend
    legend("bottomright", cex = 3,
           fill = c("lightblue", "coral"),
           legend = c(bg_shp_name_show, lg_name_show))
    
    dev.off()
    
  }
  
},
  video.name = paste0(save_dir, "/", video_name))

