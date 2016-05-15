# creates roughly 790 frames to make up an animation of tourism spend in New Zealand 

library(RODBC)
library(dplyr)
library(mbiemaps)
library(ggplot2)
library(maptools)
library(maps)
library(extrafont)

my_font <- "Calibri"
# download RTI spend data from database
TRED <- odbcConnect("TRED_Prod")

SQL <- "Select          a.Year,
                        a.MonthNumber as Month,
                        b.Merchant as TA_NAME,
                        sum(SpendAmount) as SPEND 
                  from  common.vw_RTISurveyMainHeader a,
                        common.vw_RTISpend b
                  where (a.SurveyResponseID = b.SurveyResponseID)
                    and (a.RecordType = 'International')
                  group by a.Year, a.MonthNumber, a.RecordType, b.Merchant 
                  order by a.RecordType, a.Year, a.MonthNumber"

Int_orig <- sqlQuery(TRED, SQL)

# merge with concordance to create real TA, not the made up TA
load("data/TA_RTO_R2.rda")
Int <- Int_orig %>%
    left_join(TA_RTO_r2[ , c("TA_NAME", "SNZ_TA")]) %>%
    group_by(Year, Month, SNZ_TA) %>%
    summarise(Spend = sum(SPEND))

# get the centre of each TA
data(TA)
cent <- TA@data[ , c("NAME", "long", "lat")]
names(cent)[names(cent) == "long"] <- "x"
names(cent)[names(cent) == "lat"] <- "y"


# Merge the monthly spend data with the x and y of the centroids of TAs
INT2 <- merge(Int, cent, by.x="SNZ_TA", by.y="NAME", all.x=FALSE) # so can drop Chatham Islands


# create a single variable with period to be used for later looping
INT2$Period <- with(INT2, Year + (Month-1)/12)

# create a vector of unique values of period
ps <- sort(unique(INT2$Period))

# create a vector of months to be used for later plot annotation
Months <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")


#####################
# 
# Section 2 - a bit of prep
#

tit <- "\n\nInternational tourist spend"



# Create labs1 for use in annotation if drawing absolute spend
labs <- data.frame(City=c("Auckland", "Wellington", "Christchurch", "Queenstown", "Rotorua"),
                   x=c(175, 174.25, 172.76, 168.55, 176.33),
                   y=c(-36.5, -41.26, -43.68, 501-44.90, -38.22))

# use wickham's fortify() to turn the built in map of NZ into a data frame
border <- fortify(map('nz', plot=FALSE))

# Set a vector to hold possible camera angles
thetas <- rep(seq(from=-180, to=180, length.out=240),200) # rotate every 24 months, 10 repeates per month

########################
##
# Section 3.
#
#  

# Define square frame overwhich to draw map
fr <- 50 # 35 seems ok but 50 a bit better; could experiment with this (and also the span argument in loess, below)
frx <- with(INT2, seq(from=min(x)*.96, to=max(x)*1.04, length.out=fr))
fry <- with(INT2, seq(from=min(y)*1.04, to=max(y)*0.96, length.out=fr))
xyframe <- expand.grid(frx, fry)
names(xyframe) <- c("x", "y")

# Which of these points is in the sea, and so should be added to our data as a zero spend?
# A horribly clumsy way to go about it below; must be a better way, but it only needs to be done once.
# Takes about 60 seconds
#

dists <- numeric(fr^2)

for(i in 1:(fr^2)){
  dists[i] <- min(sqrt(
    apply(
      (matrix(unlist(rep(xyframe[i,], nrow(INT2))), nrow = nrow(INT2), byrow = TRUE) -
         as.matrix(INT2[, c("x", "y")])) ^ 2
      ,1,sum)
  ))
}

AtSea <- dists > quantile(dists, probs=.18)

# define the number of rows and columns in the matrix we're drawing for future reference
nrz <- fr
ncz <- fr

# Generate a nice set of colors
color <- c(
  colorRampPalette(c("white", tourism.cols(2)))(10), 
  colorRampPalette(c(tourism.cols(c(2,11))))(20),
  colorRampPalette(c(tourism.cols(c(11,8))))(70))


#####

# Define the limit of the z axis, also used to help set colours.  

mx <- max(INT2$Spend)/5 # manual choice of the limit of the z axis and how to break the colors - depends
# very much on how big the frame is and the loess span

# for testing purposes is often useful to set i and j equal to something and just draw one plot
# within the loop rather than running the whole loop.
# i <- j <-1

######
# Set index of which element in the vector of camera angles to start with
theta.ind <- 0

##
# Main sequence starts here and draws 620 png images - 10 for each month, with a 
# slowly rotating camera angle.

plot_count <- 0

for (i in 1:length(ps)){
  # get the subset of the data applicable for just one month
  INT3 <- subset(INT2, INT2$Period==ps[i])
  
  # merge this with all the zeros of the points in the frame in the sea so the later smoothing knows they are zeros...
  dat <- rbind(
    data.frame(xyframe[dists > quantile(dists, probs = .18),], z = 0), 
    data.frame(x = INT3$x, y = INT3$y, z = INT3$Spend))
  
  # fit a predictive model to predict a smoothed surface
  mod <- loess(z ~ x * y, data = dat, span = 0.05) # Can play with span, but .1 or .05 seems good
  z <- matrix(predict(mod, newdata = xyframe), ncol = fr, byrow = F)
  z[matrix(AtSea, nrow = fr, byrow = FALSE)] <- 0
  # Compute the z-value at the facet centres
  zfacet <- z[-1, -1] + z[-1, -ncz] + z[-nrz, -1] + z[-nrz, -ncz]
  # Recode facet z-values into color indices
  facetcol <- cut(zfacet, breaks = seq(from = 0, to = mx * 4, length.out = 100))
  
  # Secondary loop starts here - draws 10 versions of the plot for the current map
  for (j in 1:10){
    # move forward by one the index in the vector of camera angles to tak
    theta.ind <- theta.ind + 1
    plot_count <- plot_count + 1
    
    # Start a graphics device, incorporating both i and j into the name.  Note use of sprint()
    # to force it to use 001, 002 etc so alphabetical order makes sense for the application that
    # later turns the sequence of images into a movie.
    png(paste0("anim1/spend_persp_", plot_count,".png"), 1200, 750, res = 100)
      
      # set margins, leaving plenty of space on the right for the date
      par(mar = c(1, 0, 1, 8), family = my_font)
      
      # Draw the plot, and store its key data in res for later use
      res <- persp(frx, fry, z, col = color[facetcol], border = "grey90",
                   box = FALSE, theta = thetas[theta.ind], phi = 30, zlim = c(0, mx))
      
      
      # draw borders of New Zealand
      with(subset(border, group==1), lines(trans3d(long, lat, 0, pmat=res), col="grey50"))
      with(subset(border, group==2), lines(trans3d(long, lat ,0,pmat=res), col="grey50"))
      
      # add labels for cities.  Note use of trans3d to project onto the persp plot.
      text(trans3d(labs$x, labs$y, 6, pmat = res), labels = labs$City)
      
      # Draw year in grey and bold
      grid.text(trunc(ps[i]), .75, .65, just = "center", 
                gp = gpar(col = "grey80", cex = 6, fontface = "bold", fontfamily = my_font))
      
      # Draw month
      grid.text(Months[round((ps[i]-trunc(ps[i]))*12+1)], .75, .65, just = "center", 
                gp=gpar(col = "black", cex = 1.5, fontfamily = my_font))
      
      # Draw title
      title(main =	tit, cex.main = 2)
    dev.off()
  }
}

