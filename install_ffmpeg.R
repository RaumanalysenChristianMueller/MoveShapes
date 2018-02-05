# Installs the ffmpeg codec on a windows machine
# Raumanalysen - Christian Mueller
# raumanalysen@mailbox.org

install_ffmpeg <- function(){

  # download codec
  if (!file.exists(paste0(getwd(), "/ffmpeg.zip"))){
    download.file("https://ffmpeg.zeranoe.com/builds/win64/static/ffmpeg-20180205-118e1b0-win64-static.zip",
                  paste0(getwd(), "/ffmpeg.zip"))
  }
  
  # unzip codec files
  if (!file.exists(paste0(getwd(), "/ffmpeg"))){
    unzip(paste0(getwd(), "/ffmpeg.zip"),
          exdir = paste0(getwd(), "/ffmpeg"))
  }
  
  # update system environment path
  if ((Sys.which('ffmpeg') == "")){
    temp_path <- Sys.getenv("PATH")
    temp_path <- paste0(temp_path, ";", paste0(getwd(), "/ffmpeg/ffmpeg-20180205-118e1b0-win64-static/bin"))
    Sys.setenv(PATH = temp_path)
  }
  
  # print feedback to console
  if (Sys.which('ffmpeg') != "") cat("Successfully installed ffmpeg") else warning("Unable to install ffmpeg")
  
}