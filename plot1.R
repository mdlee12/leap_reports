folders <- test$school_name
parent.dir <- "C:/Users/mlee/Documents/Leap/Figures/Fig1"
for (i in 1:length(folders))  { 
  dir.create(paste(parent.dir,folders[i], sep="/")) 
} 

library(showtext)
dest <- file.path(tempdir(), "open-sans.semibold.ttf")
download.file("http://ff.static.1001fonts.net/o/p/open-sans.semibold.ttf", dest, mode = "wb")
font.add("semibold", regular = dest)
font.families()
showtext.auto()

draw1 <- function(y1,y2,y3, school1, school2, school3, k=0.8, h=0.6) {
  # remove the border
  wd <- paste("C:/Users/mlee/Documents/Leap/Figures/Fig1/",school,sep="")
  setwd(wd)
  file=paste(school1,"_plot1.png",sep="")
  png(file=file,width=5040,height=720,res=500)
  par(mai=c(0,0,0,0))
  # make a blank canvas
  plot(x=c(0,5040),y=c(0,720), type="n")
  # plot.new()
#   pWidth = 6
#   pHeight = 1
#   plot.window(c(0,pWidth),
#             c(0,pHeight))
  # add grid lines
  for(i in c(1008,2016,3024,4032)) { lines(c(i,i),c(0,720), col="#d8d5d6",lwd=2) }
  for(i in c(0,5040)) { lines(c(i,i),c(0,720), col="#d8d5d6",lwd=2) }
  # s2 is the hight of any given bar
  s2 <- 720/(4*k+3)
  # s1 is the height of the space between bars
  s1 <- k*s2
  
  # drawing rectangle for top box
  rect(0, 3*s1+2*s2, 5040*(y1/100), 3*s1+3*s2, fg="#01AFDC", border="#01AFDC", col="#01AFDC")
  # draw circle that will have the number in it
  symbols(x=5040*(y1/100),y=3*s1+2.5*s2,circles=s2/2, add=TRUE, inches=FALSE, fg="#01AFDC", lwd=1.5, bg="white")
  # draw number in circle
  w2 <- strwidth(school1,cex=4.3)
  #adj gives how the text is centered, 0.5, 0.5 centers it

  xmid <- (5040*(y1/100)-(s2/2))/2

  if(w2 > xmid*2) {
    text(x=(xmid+(s2/2))*2 + w2/2 +100,y=3*s1+2.5*s2, adj=c(0.5,0.5), col="#01AFDC", family="semibold", labels=school1, cex=4)
  } else {
    text(x=xmid,y=3*s1+2.5*s2, adj=c(0.5,0.5), col="#FFFFFF", family="semibold", labels=school1, cex=4)
  }

  text(x=5040*(y1/100),y=3*s1+2.5*s2, adj=c(0.5,0.5), col="#58565a", family="semibold", labels=y1, cex=4)

  # draw line for the CPS value
  lines(x=c(0, 5040*(y2/100)), y=rep(2*s1+1.5*s2,2), col="#ecb91e", lty=3, lwd=1.5)
  symbols(x=5040*(y2/100),y=2*s1+1.5*s2,circles=s2/2, add=TRUE, inches=FALSE, fg="#ecb91e", lwd=1.5, bg="white")
  # draw number in circle
  # w1 <- strwidth(y1, cex=1)
  # h1 <- strheight(y1, cex=1)
  w2 <- strwidth(school2, cex=4.4)
  # h2 <- strheight(school2, cex=1)
  # size <- max(w1,h1)
  # cexi <- h*s2/size
  # cexi2 <- w2/s2
  xmid <- (5040*(y2/100)-(s2/2))-450
  #adj gives how the text is centered, 0.5, 0.5 centers it
  rect(xmid - w2/2, 2*s1+1.5*s2-50, xmid + w2/2, 2*s1+1.5*s2+50, fg="white", border="white", col="white")
  # rect(5040*(y2/125)-110, 2*s1+1.5*s2-50, 5040*(y2/125)+110, 2*s1+1.5*s2+50, fg="white", border="white", col="white")
  text(x=xmid,y=2*s1+1.5*s2, adj=c(0.5,0.5), col="#58565a", family="semibold", labels=school2,bg="white", cex=4)
  text(x=5040*(y2/100),y=2*s1+1.5*s2, adj=c(0.5,0.5), col="#58565a", family="semibold", labels=y2, cex=4)
  
  # draw line for the CPS value
  lines(x=c(0, 5040*(y3/100)), y=rep(1*s1+0.5*s2,2), col="#99999A", lty=3, lwd=1.5)
  symbols(x=5040*(y3/100),y=1*s1+0.5*s2,circles=s2/2, add=TRUE, inches=FALSE, fg="#99999A", lwd=1.5, bg="white")
  # draw number in circle
  # w1 <- strwidth(y3, cex=1)
  # h1 <- strheight(y3, cex=1)
  w2 <- strwidth(school3, cex = 4.4)
  # h2 <- strheight(school3, cex=1)
  # size <- max(w1,h1)
  # size2 <- max(w2,h2)
  # cexi <- h*s2/size
  # cexi2 <- h*s2/size2
  xmid <- (5040*(y3/100)-(s2/2))-275
  #adj gives how the text is centered, 0.5, 0.5 centers it
  rect(xmid - w2/2, 1*s1+0.5*s2-50, xmid + w2/2, 1*s1+0.5*s2+50, fg="white", border="white", col="white")
  # rect(5040*(y3/140)-360, 1*s1+0.5*s2-50, 5040*(y3/140)+360, 1*s1+0.5*s2+50, fg="white", border="white", col="white")
  text(x=xmid,y=1*s1+0.5*s2, adj=c(0.5,0.5), col="#58565a", family="semibold", labels=school3,bg="white", cex=4)
  text(x=5040*(y3/100),y=1*s1+0.5*s2, adj=c(0.5,0.5), col="#58565a", family="semibold", labels=y3, cex=4)

  lines(c(720,0), col="#d8d5d6",lwd=2)
  for(i in c(0,720)) { lines(c(0,5040),c(i,i), col="#d8d5d6",lwd=2) }
  lines(c(0,0), c(0,720), col="#d8d5d6",lwd=2)
	dev.copy(png, filename=file)
	dev.off()
  dev.off()
}

for (i in 1:length(test$school_name)) { draw1(test$Score[i],test$CPS[i],test$National.Average[i],test$school_name[i],"Chicago Area Schools","All Schools")}

# draw1(26,68,65,"Chavez Teacher Survey Results","Chicago Area Schools","All Schools")