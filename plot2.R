# create folders for each school code to store plots
folders <- leap$schoolname
parent.dir <- "C:/Users/mlee/Documents/Leap/Figures/Fig2"
for (i in 1:length(folders))  { 
  dir.create(paste(parent.dir,folders[i], sep="/")) 
} 

# download open sans semibold and add it to R using showtext package

library(showtext)
dest <- file.path(tempdir(), "open-sans.semibold.ttf")
download.file("http://ff.static.1001fonts.net/o/p/open-sans.semibold.ttf", dest, mode = "wb")
font.add("semibold", regular = dest)

# confirm semibold has been added
font.families()
showtext.auto()





draw2 <- function(x1, dash1, school, question, k=0.125, barcol=c("#ecb91e", "#f8d87f", "#5ebbe2", "#3a7e98")) { # x1 and dash1 are vectors
  if(length(x1) != 16) { stop("x1 must have 16 elements.")}
  if(length(dash1) != 16) { stop("dash1 must have 16 elements.")}

  # First, the percentages should add up to near 100 -  maybe 96 to 104 are credible.
  # Second, they may pass a bunch of 0s, then you should not plot the national average. - e.g. they said that they had 14 things to plot. 14/4 doesn't come out even
  # remove the border
  wd <- paste("C:/Users/mlee/Documents/Leap/Figures/Fig2/",school,sep="")
  setwd(wd)
  file=paste(question,"_fig2.png",sep="")
  png(file=file,width=4680,height=1260,res=500)
  par(mai=c(0,0,0,0))
  # make a blank canvas
  plot(x=c(0,4680),y=c(0,1260), type="n")

  # old grid lines 315,630,945)
  for(i in c(252,504,756,1008)) { lines(c(0,4680),c(i,i), col="#d8d5d6",lwd=2) }

  lines(c(0,4680),c(1260,1260), col="#d8d5d6",lwd=2)

  # s1 is the height of the space between bars
  s1 <- 4680/41
  s3 <- s1/4
  
  # 
  col <- rep(barcol, 4)

  ###rectangles
  for(ri in 1:16) {
    xi1 <- ((ri-1)*2 + 1 + ((ri-1) %/% 4))*s1+(ri-1) + (ri-1)*s3
   # xi2 <- 5*s1+1*s3
    xi2 <- ((ri-1)*2 + 3 + ((ri-1) %/% 4))*s1+(ri-1) + (ri-1)*s3
    xmid <- mean(c(xi1, xi2))
    ytop <- 1260*(x1[ri]/100)
    rect(xi1, 0, xi2, ytop, fg=col[ri], border=col[ri], col=col[ri])
    # the rest need to be fixed to reference ri
    lines(x=rep(xmid,2), y=c(0, 1260*(dash1[ri]/100)), col="#99999A", lty=2)
    symbols(x=xmid,y=1260*(dash1[ri]/100), circles=s1/3, add=TRUE, inches=FALSE, fg="#989898", lwd=1, bg="white")
    w2 <- paste0(x1[ri],"%")
    text(x=xmid,y=ytop+65, adj=c(0.45,0.5), col="#58565a", family="semibold", labels=w2, cex=4)
  }

	for(i in c(0,4680)) { lines(c(i,i),c(0,1260), col="#d8d5d6",lwd=2) }
  lines(c(0,4680),c(0,0), col="#d8d5d6",lwd=2)

	dev.copy(png, filename=file)
	dev.off()
  dev.off()
}