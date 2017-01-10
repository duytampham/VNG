cheatListen <- function(fileName) {
  library(stringr)
  library(ggplot2)
  setwd("/home/tampd/VNG")
  # Read Data
  raw.datas <- read.table(fileName, header = TRUE, fill = TRUE)
  colnames(raw.datas) <- c("Date", "Time", "Label", "VisitorID", "MediaID", "MediaID_Copy")
  # Standardized Data
  datas <- raw.datas[, c("Date", "Time", "VisitorID", "MediaID")]
  datas <- data.frame(lapply(datas, as.character), stringsAsFactors = FALSE)
  datas["Time"] <- (str_split_fixed(datas$Time, ":", 2))[, 1]
  # Descriptive
  fre.MediaID <- table(datas$MediaID)
  fre.MediaID <- data.frame(fre.MediaID)
  summary(fre.MediaID)
  quantile(fre.MediaID$Freq, c(.90, .95, .99, .999))
  # 1%
  sel.fre.MediaID <- subset(fre.MediaID, fre.MediaID$Freq > quantile(fre.MediaID$Freq, .99))
  summary(sel.fre.MediaID)
  quantile(sel.fre.MediaID$Freq, c(.90, .95, .99, .999))
  
  # 2% of 1%
  sel.fre.MediaID <- subset(sel.fre.MediaID, sel.fre.MediaID$Freq > quantile(sel.fre.MediaID$Freq, 0))
  summary(sel.fre.MediaID)
  quantile(sel.fre.MediaID$Freq, c(.90, .95, .99))
  
  # Data with MediaID 2% of 1%
  datas.sel.MediaID = subset(datas, is.element(datas$MediaID, sel.fre.MediaID$Var1))
  
  dir.create(file.path("/home/tampd/VNG", substr(fileName, 1, 10)))
  setwd(file.path("/home/tampd/VNG", substr(fileName, 1, 10)))
  for(media.id in unique(datas.sel.MediaID$MediaID)) {
    datas.byMediaID = subset(datas, is.element(datas$MediaID, media.id))
    t1 <- data.frame(table(datas.byMediaID$Time), stringsAsFactors=FALSE)
    t2 <- data.frame(table((unique((datas.byMediaID)[, c("Time", "VisitorID")]))$Time), stringsAsFactors=FALSE)
    t1.sort <- data.frame()
    t2.sort <- data.frame()
    for (i in c("00", "01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23")) {
      if (!(i %in% t1$Var1))
        t1.sort <- rbind(t1.sort, data.frame(Var1 = i, Freq = 0))
      else
        t1.sort <- rbind(t1.sort, data.frame(Var1 = i, Freq = (t1[t1$Var1 == i, ])$Freq))
      if (!(i %in% t2$Var1))
        t2.sort <- rbind(t2.sort, data.frame(Var1 = i, Freq = 0))
      else
        t2.sort <- rbind(t2.sort, data.frame(Var1 = i, Freq = (t2[t2$Var1 == i, ])$Freq))
    }
    t1.sort['Key'] <- rep("Frequency of MediaID", 24)
    t2.sort['Key'] <- rep("MediaID with some VisitorID", 24)
    t = rbind(t1.sort, t2.sort)
    ggplot(data = t, aes(x = t$Var1, y = t$Freq, fill = t$Key)) + geom_bar(stat="identity",position=position_dodge()) + xlab("Time") + ylab("Frequency") + scale_fill_discrete(name = media.id)
    ggsave(media.id, device = "png", width = 20, height = 20, units = "cm")
  }
  print(table(datas.sel.MediaID$MediaID))
}