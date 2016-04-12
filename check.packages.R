#Check if needed libraries are available and install if available
install_package<-function(package){
  message(paste("Checking if",package,"is available"))
  if(!(pack %in% row.names(installed.packages())))
  {
    message(paste(package, "not found, installing..."))
    #update all packages
    #update.packages(ask=F)
    #install missing package
    message(paste("Loading",package))
    install.packages(pack,dependencies=T)
    require(package,character.only=TRUE)
  }
  message(paste(package,"found"))
  require(package,character.only=TRUE)
}

