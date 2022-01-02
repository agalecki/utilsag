Dropbox.path <- function () {
## see https://stackoverflow.com/questions/35985167/determining-the-dropbox-path-in-r
## https://help.dropbox.com/installs-integrations/desktop/locate-dropbox-folder

appath <-  file.path(Sys.getenv(x = "LOCALAPPDATA"), "Dropbox", "info.json")
if (length(appath) == 0) appath <- file.path( Sys.getenv(x = "APPDATA"), "Dropbox", "info.json")
file_content <- jsonlite::fromJSON(txt= appath)
nms <- names(file_content)  # personal | business

dropbox_pth <- sapply(nms, FUN= function(x){
     ex <-file_content[[x]]
     file.path(ex$path)
     })
if (length(dropbox_pth) == 0) then {
   message ("Dropbox not found")    
     } else {
assign(".Dropbox.path", dropbox_pth, envir = .GlobalEnv)
message(".Dropbox.path: ", .Dropbox.path)
}
invisible(dropbox_pth)
}
# Dropbox.path()   # Vector with paths to Dropbox (both personal and business) 
# .Dropbox.path
