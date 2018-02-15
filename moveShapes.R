# MoveShape creates 4k (UHD) videos with smooth transitions out of time referenced shapefiles
# Raumanalysen - Christian Mueller
# raumanalysen@mailbox.org

options(encoding = "UTF-8")

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
for (i in 1:2){
  if (!require("rgdal")) install.packages("rgdal", dependencies = T)
  if (!require("sp")) install.packages("sp", dependencies = T)
  if (!require("animation")) install.packages("animation", dependencies = T)
  if (!require("png")) install.packages("png", dependencies = T)
  if (!require("extrafont")) install.packages("extrafont", dependencies = T)
  if (!require("tcltk")) install.packages("tcltk", dependencies = T)
  if (!require("tcltk2")) install.packages("tcltk2", dependencies = T)
}

# install ffmpeg codec if necessary
if (Sys.which('ffmpeg') == ""){
  source("install_ffmpeg.r")
  install_ffmpeg()
}

# create progress bar
try(tk <- tktoplevel(), silent = T)
try(tk2ico.setFromFile(win = tk, iconfile =  paste0(getwd(), "/Logo.ico")), silent = T)
try(tktitle(tk) <- "Raumanalysen - Christian Mueller - moveShapes", silent = T)
try(tk_lab <- tk2label(tk), silent = T)
try(tk_pb <- tk2progress(tk, length = 500), silent = T)
try(tkgrid(tk_lab, row = 0), silent = T)
try(tkgrid(tk_pb, row = 1), silent = T)

# load fonts if necessary (with progress bar)
try(tkconfigure(tk_lab, text = "Schriften werden geladen (dieser Prozess wird nur beim ersten AusfÃ¼hren durchgeführt)..."), silent = T)
try(tkconfigure(tk_pb, value = 1, maximum = 100), silent = T)
if (is.null(fonts())){
  font_import(prompt = F)
  loadfonts()
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

# load logo
logo <- readPNG(paste0(getwd(), "/Logo.png"))


# create output folder for individual images
im_dir <- paste0(save_dir, "/moveShapes_images")
if (!dir.exists(im_dir)) dir.create(im_dir)

# define count variable for time bar
t <- 1

# set number of frames for transitions
ntrans <- 200

# calculate number of frames
nt <- length(shps_names) * ntrans


# update progress bar
try(tkconfigure(tk_lab, text = "moveShapes erzeugt einzelne Bilder..."), silent = T)
try(tkconfigure(tk_pb, value = 1, maximum = nt + 5), silent = T)


# create individual frames as png
# create plots
for (s in 1:length(shps_names)){
    
  # define starting and end state of current transition 
  this_1 <- get(paste0("shp_", s))
  if (s < length(shps_names)) this_2 <- get(paste0("shp_", s + 1)) else this_2 <- this_1
    
  # iterate over each transition
  for (f in 1:ntrans){
  
    # save additionally as png
    png(filename = paste0(im_dir, "/animationFrame_", t, ".png"),
        width = 3840, height = 2160, family = "Ebrima", antialias = "default")
      
    # set plot options
    layout(matrix(c(1,2), 2, 1), heights = c(1,10))
      
    # update time bar
    par(mar = c(1, 4, 0, 4), family = "Ebrima", font = 2)
    plot(1:nt, rep(1, nt), axes = F, type = "n", xlab = "", ylab = "")
    axis(1, at = seq(1, nt, length.out = length(shps_names)), mgp = c(3, 3, 0), font = 2,
         labels = shps_show_names, las = 1, cex.axis = 5, lwd = 3, lwd.ticks = 3)
    segments(x0 = t, x1 = t, y0 = 0, y1 = 1, lwd = 8, col = rgb(244, 100, 48, maxColorValue = 255))
      
    # plot backgroud
    par(mar = c(5, 3, 3, 3), family = "Ebrima", font = 2)
    bgcol <- rgb(86, 170, 179, maxColorValue = 255)
    plot(bg, col = bgcol, border = bgcol)
      
    # plot shape 1 with decreasing opacity
    col1 <- rgb(244, 100, 48, seq(0, 255, length.out = ntrans)[ntrans - f], maxColorValue = 255)
    plot(this_1, col = col1, border = col1, add = T)
      
    # plot shape 2 with increasing opacity
    col2 <- rgb(244, 100, 48, seq(0, 255, length.out = ntrans)[f], maxColorValue = 255)
    if (s == length(shps_names)) col2 <- rgb(244, 100, 48, maxColorValue = 255)
    plot(this_2, col = col2, border = col2, add = T)
    
    # add legend
    legend("bottomright", cex = 5,
           fill = c("lightblue", "coral"),
           legend = paste0(c(bg_shp_name_show, lg_name_show), "     "))
    
    # add logo
    dif1 <- (par("usr")[2] - par("usr")[1])
    dif2 <- (par("usr")[4] - par("usr")[3])
    rat <- 3840/2160
    rasterImage(logo, par("usr")[1], par("usr")[3], par("usr")[1] + dif1/30, par("usr")[3] + (dif2/30 * rat))
    
    # add contact
    text(x = par("usr")[1] + dif1/10, y = par("usr")[3] + ((dif2/30 * rat)/2), cex = 3, xpd = T,
         labels = "raumanalysen@mailbox.org", col = rgb(244, 100, 48, maxColorValue = 255))
    
    # close image file
    dev.off()
      
    
    # increase count variable and progress bar
    try(tkconfigure(tk_lab, text = paste0("moveShapes erzeugt einzelne Bilder...", round(t / nt * 100, 0), " %")), silent = T)
    try(tkconfigure(tk_pb, value = t, maximum = nt + 5), silent = T)
    t <- t + 1
      
      
    }
    
}
  

# create new progress bar
try(tkconfigure(tk_lab, text = "moveShapes fügt einzelne Bilder zu einem Video zusammen"), silent = T)
try(tkconfigure(tk_pb, value = 99, maximum = 100), silent = T)


# building a system command line
sys_com <- paste0(
  "ffmpeg ",
  "-y -framerate 30 ",
  "-i ",
  im_dir, "/animationFrame_%d.png ",
  "-s:v 3840x2160 -c:v libx264 -profile:v high -crf 20 -pix_fmt yuv420p ",
  save_dir, "/", video_name
)

# creating a video from individual png files
system(sys_com)

# close progress bar
tkdestroy(tk)
