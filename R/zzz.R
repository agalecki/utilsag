.onAttach <-function(libname,pckname){
packageStartupMessage("Version: ", utils::packageVersion("utilsag"),". Built on: ",  utils::packageDate("utilsag"))
}
