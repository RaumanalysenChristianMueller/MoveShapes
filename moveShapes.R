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
pics <- T

# load packages
library(rgdal)
library(sp)
library(animation)
library(png)
if (Sys.which('ffmpeg') == ""){
  source("install_ffmpeg.r")
  install_ffmpeg()
}

# define coordinate system
WGSproj <- CRS("+init=epsg:4326")

# load all shapefiles
for (s in 1:length(shps_names)){
  
  # load data
  assign(paste0("shp_", s), readOGR(shp_dir, shps_names[s]))
  
  # reproject data
  assign(paste0("shp_", s), spTransform(get(paste0("shp_", s)), WGSproj))
  
}

# load background data
bg <- readOGR(shp_dir, bg_shp)

# reproject background map
bg <- spTransform(bg, WGSproj)

# create output folder for individual images
if (pics){
  im_dir <- paste0(save_dir, "/moveShapes_images")
  if (!dir.exists(im_dir)) dir.create(im_dir)
}
  
# define count variable for time bar
t <- 1

# set number of frames for transitions
ntrans <- 90

# calculate number of frames
nt <- length(shps_names) * ntrans


# create progress bar
pr_bar <- winProgressBar(title = "moveShapes - Raumanalysen_Christian_Müller", label = "moveShapes erzeugt einzelne Bilder",
                         min = 1, max = nt, width = 500)

# create individual frames as png
# create plots
for (s in 1:length(shps_names)){
    
  # define starting and end state of current transition 
  this_1 <- get(paste0("shp_", s))
  if (s < length(shps_names)) this_2 <- get(paste0("shp_", s + 1)) else this_2 <- this_1
    
  # iterate over each transition
  for (f in 1:ntrans){
  
    # save additionally as png
    png(filename = paste0(im_dir, "/animationFrame_", t, ".png"), width = 1920, height = 1440)
      
    # set plot options
    layout(matrix(c(1,2), 2, 1), heights = c(1,7))
      
    # update time bar
    par(mar = c(1, 2, 0, 2))
    plot(1:nt, rep(1, nt), axes = F, type = "n", xlab = "", ylab = "")
    axis(1, at = seq(1, nt, length.out = length(shps_names)),
         labels = shps_show_names, las = 1, cex.axis = 2)
    segments(x0 = t, x1 = t, y0 = 0, y1 = 1, col = "coral")
      
    # plot backgroud
    par(mar = c(5, 3, 3, 3))
    plot(bg, col = "lightblue", border = "lightblue")
      
    # plot shape 1 with decreasing opacity
    col1 <- rgb(255, 127, 80, seq(0, 255, length.out = ntrans)[ntrans - f], maxColorValue = 255)
    plot(this_1, col = col1, border = col1, add = T)
      
    # plot shape 1 with increasing opacity
    col2 <- rgb(255, 127, 80, seq(0, 255, length.out = ntrans)[f], maxColorValue = 255)
    plot(this_2, col = col2, border = col2, add = T)
    
    # add legend
    legend("bottomright", cex = 2,
           fill = c("lightblue", "coral"),
           legend = c(bg_shp_name_show, lg_name_show))
      
    # close image file
    dev.off()
      
    
    # increase count variable and progress bar
    setWinProgressBar(pr_bar, t,
                      label = paste0("Einen Moment Geduld...", round(t / nt * 100, 0), " %"))
    t <- t + 1
      
      
    }
    
}
  
# close progress bar
close(pr_bar)

# create new progress bar
pr_bar <- winProgressBar(title = "moveShapes - Raumanalysen_Christian_Müller", label = "moveShapes fügt einzelne Bilder zu einem Video zusammen",
                         min = 1, max = 100, width = 500)
setWinProgressBar(pr_bar, 80,
                  label = paste0("Einen Moment Geduld...80 %"))

# building a system command line
sys_com <- paste0(
  "ffmpeg ",
  "-y -framerate 30 ",
  "-i ",
  im_dir, "/animationFrame_%d.png ",
  "-s:v 1920x1440 -c:v libx264 -profile:v high -crf 20 -pix_fmt yuv420p ",
  save_dir, "/", video_name
)

# creating a video from individual png files
system(sys_com)

# close progress bar
close(pr_bar)
