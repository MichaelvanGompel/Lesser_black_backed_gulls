already.installed <- installed.packages()
required_packages = c("dplyr","geosphere", "lubridate", "modelr", "ggplot2", "gridExtra",
                 "cowplot", "rnaturalearth")

exist <- required_packages %in% already.installed

packages_to_download <- required_packages[which(exist == F)]

if(length(packages_to_download) >= 1){
	install.packages(packages_to_download, dependencies=TRUE)
}
