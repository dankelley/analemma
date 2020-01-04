## Analemma for Barrow Strait Observatory
require(oce)
require(lubridate)

skyaxis <- function(col="gray", cex=0.6)
{
    offset <- 0.07
    ## Set up plot area
    plot(c(-1, 1), c(-1, 1), asp=1, type='n', xlab="", ylab="", axes=FALSE)
    ## grid
    alt <- seq(0, 90, 10)
    for (i in seq_along(alt)) {
        t <- seq(0, 2*pi, pi/64)
        r <- (alt[i] - 90) / 90
        lines(r*cos(t), r*sin(t), col=col)
        text(0, alt[i]/90, 90-alt[i], cex=cex)
    }
    azi <- seq(10, 360, 10)
    for (i in seq_along(azi)) {
        theta <- 90 - azi[i]
        px <- cos(theta*pi/180)
        py <- sin(theta*pi/180)
        lines(px*c(10/90, 1), py*c(10/90, 1), col=col, lty=3)
        text(px*(1+offset), py*(1+offset), azi[i], cex=cex)
    }
    ## Points of compass
    text(0, 1+offset, "N", cex=1.2*cex, pos=3, font=2)
    text(1+offset, 0, "E", cex=1.2*cex, pos=4, font=2)
    text(0, -1-offset, "S", cex=1.2*cex, pos=1, font=2)
    text(-1-offset, 0, "W", cex=1.2*cex, pos=2, font=2)
}

skypoints <- function(Azimuth, Altitude, col=1, pch=20, cex=1)
{
    r <- (90 - Altitude) / 90
    theta <- 90 - Azimuth
    x <- r * cos(theta * pi / 180)
    y <- r * sin(theta * pi / 180)
    points(x, y, col=col, pch=pch, cex=cex)
}
skylines <- function(Azimuth, Altitude, col=1, lwd=par("lwd"))
{
    r <- (90 - Altitude) / 90
    theta <- 90 - Azimuth
    x <- r * cos(theta * pi / 180)
    y <- r * sin(theta * pi / 180)
    lines(x, y, col=col, lwd=lwd)
}
skytext <- function(Azimuth, Altitude, text, col=1, cex=1, pos=1)
{
    r <- (90 - Altitude) / 90
    theta <- 90 - Azimuth
    x <- r * cos(theta * pi / 180)
    y <- r * sin(theta * pi / 180)
    text(x, y, text, col=col, cex=cex, pos=pos)
}

place <- "Barrow St. Observatory"
zone <- -6                             # central time
lat <- 74.5                            # by DK eye on map
lon <- -92                             # by DK eye on map

## Noon today, and weekly for a year thereafter
today <- as.POSIXct(paste(Sys.Date(), sprintf("%d:00:00", 12 - zone)), tz="UTC")
days <- seq.POSIXt(today, by="1 week", length.out=53, tz="UTC")
Azimuth <- NULL
Altitude <- NULL
for (day in days) {
    noon <- numberAsPOSIXct(day)
    noonAngles <- sunAngle(noon, lon, lat)
    Azimuth <- c(Azimuth, noonAngles$azimuth)
    Altitude <- c(Altitude, noonAngles$altitude)
}

## Plot
filename <- "analemma_bso.png"

if (!interactive()) png(filename, width=6, height=6, res=100, unit="in", pointsize=20)
par(mar=c(1.0, 0.5, 1.0, 0.5), xpd=TRUE)
skyaxis()
skylines(Azimuth, Altitude, col="red", lwd=2)
skypoints(Azimuth[1], Altitude[1], pch=20, col=2)

## today's hourly trace
hours <- seq(as.POSIXct(paste(Sys.Date(), "00:00:00"), tz="UTC"), by="hour", length=24)
hoursLocal <- hours + zone * 3600
hourAngles <- sunAngle(hours, lon, lat)
hazi <- hourAngles$azimuth
halt <- hourAngles$altitude
visible <- halt > 0
if (any(visible)) {
    hazi <- hazi[visible]
    halt <- halt[visible]
    hours <- hours[visible]
    hoursLocal <- hoursLocal[visible]
    skylines(hazi, halt, col="orange")
    skypoints(hazi, halt, col="orange")
    noon <- which.max(halt)
    skypoints(hazi[noon], halt[noon], col="red")
    skytext(hazi, halt, format(hoursLocal, "%Hh"), cex=2/3, pos=1, col="red")
}

## Place/time legend at top
mtext(place, side=3, line=0, adj=0, cex=3/4)
mtext(sprintf("%gW %gN", -lon, lat), side=3, line=0, adj=1, cex=3/4)
mtext(sprintf("%s", format(today, "%b %d")), side=3, line=-1, adj=1, cex=3/4)

if (!interactive()) dev.off()

