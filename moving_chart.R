library(rvest)
library(ggplot2)
# get data: download, unzip, read
baseuRL <- "https://www.covid19.admin.ch"
filename <- "data/COVID19Hosp_geoRegion_AKL10_w.csv"
pg <- read_html(paste(baseuRL, "/de/overview", sep = ""))
hrefs <- html_attr(html_nodes(pg, "a"), "href")
currentcsv <- hrefs[grepl("csv", hrefs)]
download.file(paste(baseuRL, currentcsv, sep = ""), "currentdata.zip")
hospdata <- read.table(unz("currentdata.zip", filename), header = T, quote = "\"", sep = ",")

# determine maximal value
maxy <- ceiling(max(hospdata$entries/100)) * 100

# format date nicely
hospdata$titledate <- paste0(substr(hospdata$datum, 1, 4), " Woche ", as.numeric(substr(hospdata$datum, 5, 6)))

# open png device
png("barchart%003d.png", width = 1200, height = 800, res = 200)
for (tdate in sort(unique(hospdata$titledate))) {
  p <- ggplot(subset(hospdata, titledate == tdate & geoRegion == "CH"), aes(x = altersklasse_covid19, y = entries, fill = entries)) + geom_bar(stat = "identity") + 
    ylim(c(0, maxy)) + coord_flip() + labs(x = "Altersklasse", y = "Anzahl", fill = NULL, subtitle = tdate, title = "Hospitalisierungen COVID nach BAG") + 
    scale_fill_gradient(limits = c(-100, maxy), low = "green", high = "red")
  print(p)
}
dev.off()

system("ffmpeg -framerate 4 -i barchart%003d.png -c:v libx264 -r 30 -vcodec libx264 -vb 1024k -minrate 1024k -maxrate 1024k -bufsize 1024k outhos.mp4")