library(rvest)
library(ggplot2)
#if data from BAG or RSalzer
weeklyBAG <- FALSE
if (weeklyBAG) {
  # get data: download, unzip, read
  baseuRL <- "https://www.covid19.admin.ch"
  filename <- "data/COVID19Hosp_geoRegion_AKL10_w.csv"
  pg <- read_html(paste(baseuRL, "/de/overview", sep = ""))
  hrefs <- html_attr(html_nodes(pg, "a"), "href")
  currentcsv <- hrefs[grepl("csv", hrefs)]
  download.file(paste(baseuRL, currentcsv, sep = ""), "currentdata.zip")
  hospdata <- read.table(unz("currentdata.zip", filename), header = T, quote = "\"", sep = ",")
  
  
  # format date nicely
  hospdata$titledate <- paste0(substr(hospdata$datum, 1, 4), " Woche ", as.numeric(substr(hospdata$datum, 5, 6)))
  hospdata <- subset(hospdata, geoRegion == "CH")
  # determine extremal values for display
  maxy <- ceiling(max(hospdata$entries/100, na.rm = T)) * 100
  miny <- -50
} else {
  # download data from github
  salzerdata <- read.csv(url("https://raw.githubusercontent.com/rsalzer/COVID_19_BAG/master/data/hospitalised.csv"))
  nobs <- nrow(salzerdata)
  #reshape data to long format
  hospdata <- data.frame(date = salzerdata$date)
  # sum up male and female
  for (i in 1:9) {
    hospdata <- cbind(hospdata, c(NA, diff(salzerdata[, i + 1] + salzerdata[, i + 10])))
    names(hospdata)[ncol(hospdata)] <- paste((i - 1) * 10, "-", i * 10 - 1)
  }
  #manually label last column
  names(hospdata)[ncol(hospdata)] <- "80+"
  # reshape
  hospdata <- reshape(hospdata, varying = list(2:10), direction = "long", times = names(hospdata)[2:10], timevar = "altersklasse_covid19", 
                      v.names = "entries")
  
  hospdata$titledate <- hospdata$date
  
  # determine extremal values for display
  maxy <- ceiling(max(hospdata$entries/10, na.rm = T)) * 10
  miny <- -5
  
  
}

# open png device
png("barchart%003d.png", width = 1200, height = 800, res = 200)
for (tdate in sort(unique(hospdata$titledate))) {
  p <- ggplot(subset(hospdata, titledate == tdate), aes(x = altersklasse_covid19, y = entries, fill = entries)) + geom_bar(stat = "identity") + 
    ylim(c(miny, maxy)) + coord_flip() + labs(x = "Altersklasse", y = "Anzahl", fill = NULL, subtitle = tdate, title = "Hospitalisierungen COVID nach BAG") + 
    scale_fill_gradient(limits = c(miny, maxy), low = "green", high = "red")
  print(p)
}
dev.off()
framerate <- ifelse(weeklyBAG, 4, 15)
system(paste0("ffmpeg -framerate ", framerate, " -i barchart%003d.png -c:v libx264 -r 30 -vcodec libx264 -vb 1024k -minrate 1024k -maxrate 1024k -bufsize 1024k outhosd.mp4"))
