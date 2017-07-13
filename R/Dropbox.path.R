Dropbox.path <- function () {
## see https://stackoverflow.com/questions/35985167/determining-the-dropbox-path-in-r
# By default returns path to Dropbox
appath <-  file.path( Sys.getenv(x = "LOCALAPPDATA"), "Dropbox", "info.json")
if (length(appath) == 0) appath <- file.path( Sys.getenv(x = "APPDATA"), "Dropbox", "info.json")
file_content <- jsonlite::fromJSON(txt= appath)$personal
dropbox_pth <- file.path(file_content$path)

if (dir.exists(dropbox_pth)){
     assign(".Dropbox.path", dropbox_pth, envir = .GlobalEnv)
     message(".Dropbox.path: ", .Dropbox.path)
}
invisible(dropbox_pth)
}
