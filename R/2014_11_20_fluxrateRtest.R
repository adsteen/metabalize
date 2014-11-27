##################
# Drew's script around which to build the analysis functions
##################

#############
# Load necessary packages
#############
require(ggplot2)
require(plyr)
require(reshape2)

#source("R/safe_NLS_metabalize.R")
source("R/read_long.R")
source("R/read_MAVEN.R")
source("R/calc_12C.R")


############
# Read in rawa data
###########

raw_list <- read_MAVEN(data_fn="data/140114_AET_13C_glucose_flux.csv",
                  key_fn="data/curacao_samplekey.csv")

raw <- raw_list$raw_data

# OK, so this is long-format but need to calculate percent_12C
system.time({
  raw_12C <- calc_12C(raw) #Hmm: relative.ion.count doesnt work, always returns 1
}) # likr 5 seconds on my macbook air

hist(raw_12C$relative.ion.count)
## Calc number of peaks for each metabolite
n_peaks <- ddply(raw_12C, c("sample", "compound"), summarise,
                 n.peaks=length(medMz))




# Pick out just the 12C parts
C12_only <- subset(raw_12C, is.12C==TRUE)

# Make a plot of %C
few_metab <- subset(C12_only, compound %in% unique(C12_only$compound)[1:3])
p_timecourse <- plot_timecourse(C12_only, exp_var=raw_list$exp_var)
print(p_timecourse) # Generates a plot, but there's a problem with teh talk


atp <- subset(C12, compound=="ATP")
ggplot(raw_12C), aes(x=time, y=relative.ion.count))




# <Later when I make plots: facet by arbitrary number of variables with
# as.formula(paste("~", response), nrow=length(response) or something)







#############
# Read in raw data
#############

# Assumes raw data will be in tidy .csv files, in a known directory
# Define the path in which our data sit
pth <- "data/glucose flux csv"
#all_data <- read_long(pth)


### Read in data

# All the glucose flux files are in the "glucose flux csv" folder

# # Create a vector containing the relative path of each .csv file in the appropriate folder
# glu_folder <- "glucose flux csv"
# glu_to_read <- paste(glu_folder, "/", dir(glu_folder, pattern=".csv$"), sep="")
# 
# # Read all the .csv files in the "glucose flux csv" file and put them into one giant data frame
# all_data <- adply(glu_to_read, 1, read.csv)
# all_data$Replicate <- as.factor(all_data$Replicate)

# It seems that some files have TOTAL.AREA and some have total.area
# But we won't fix that for now

# Create a single metabolite to test the basic timeseries plot
test_subset <- subset(all_data, metabolite=="1-methyl-histidine")



# Make one test plot
test_p <- make_basic_timeseries_plot(test_subset)
print(test_p)

# Make *all* of the plots
d_ply(all_data, c("metabolite"), make_basic_timeseries_plot)


# Now calculate an exponential fit for all of the metabolites at once

all_fits <- dlply(all_data, c("metabolite"), safe_NLS_curacao) # Amazingly, seems to calculate a fit for all the data

# Put the coefficients of the fits into a data frame
all_fit_coefs <- ldply(all_fits, coef)

## CHeck: can we get an exponential fit on any of the data
#test1 <- subset(all_data, metabolite=="4-pyridoxic acid")

#nls(formula(I(percent.12C ~ A * exp(-k*Time))), test1, start=list(A=100, k=0.05))




# Initialize a master list in which to store all your raw data
all_data_list <- list()

x2_keto_isovalerate <- read.xlsx("AET_glucose_flux_fixed.xlsx", sheetName="2-keto-isovalerate_t")
x2_keto_isovalerate$metabolite <- "2-keto_isovalerate"
all_data_list[["2-keto_isovalerate"]] <- x2_keto_isovalerate

# repeat this process for each metabolite

# Combine all the raw data from the giant raw data list into a data frame
all_data_df <- ldply(all_data_list, identity)
all_data_df <- rename(all_data_df, c(".id" = "metabolite"))

read_spreadsheets <- function()

p_raw <- ggplot(x2_keto_isovalerate, aes(x=Time, y=X12C, colour=as.factor(Replicate)))  + 
  geom_point() + 
  geom_line() + 
  facet_wrap(~sample.type)
print(p_raw)


graph <- structure(list(temp[,3], temp[,5], .Names = c("Time", "area"), class = "data.frame"))
graph<-t(cbind(temp[,3],temp[,5]))

for( i in 2:49){
  glucose2<-read.xlsx("AET_glucose_flux_fixed.xlsx",i)
  dat <- glucose2[-c(5,6,24:91),-c(56:66)]
  samps <- names(dat)[2:55]
  colnames <- dat[,1]
  temp <- t(dat[,2:55])
  temp <- data.frame(temp)
  names(temp)<-colnames
  metabolite <- rep(fakeName[i],54)
  dat1 <- cbind(metabolite, samps, temp)
  output <- rbind(output, dat1)
}

# Load necessary packages - install them once
library(ggplot2)
library(reshape2)
library(plyr)

# Load necessary functions
source("lm_stats.R")


# set graphical theme
theme_set(theme_bw() + theme(text=element_text(size=8)))

#not using
d <- read.csv("~/Dropbox/Drew/misc/Enzyme data for DS.csv", na.strings="OVRFLW")
d <- rename(d, c("Time..days."="time"))

#make data table
dm <- melt(output, id.vars=c(output[,2], output[,3], output[,1], output[,5]), variable.name=output[,4], value.name=output[,6])
head(output)
dm<-output
head(dm)

# Make a plot of the raw data
#worked
p_raw <- ggplot(dm, aes(x="time", y="area")) + geom_point() + 
  geom_smooth(method="lm", se=FALSE) + 
  facet_wrap(~sample) + 
  aes(ymin=0) +
  ggtitle("curacao test") 

#error
slopes <- ddply(dm, "samps", function(x) lm_stats(x, xvar="Time", yvar="area"))
head(slopes)
dim(dm)
# Calculate NLS & return
# To do: import model as a parameter

safe_NLS <- function(df, xvar="conc", yvar="v0", form=NULL, guesses=NULL) {
  #modbrowser()
  # Create a default model based on Shane & Katherine's data
  if(is.null(form)) {
    mod <- formula(I(v0 ~ (Vmax * conc)/(Km + conc)))
  }
  
  xvals <- df[ , xvar]
  yvals <- df[ , yvar]
  
  if(is.null(guesses)) {
    # Need to include some code to ensure that the guesses have the same variables as the formula
    # warning("Dude you should really make a starting guess, but I'll try to make one for you")
    Km_guess <- mean(xvals, na.rm=TRUE)
    Vmax_guess <- max(xvals, na.rm=TRUE)
    guesses <- list(Km=Km_guess, Vmax=Vmax_guess)
  }
  
  mod <- tryCatch(
    nls(mod, df, start=guesses),
    # Note: on warning, the function executes and the warning is issued
    error=function(err) {
      warning("This model threw an error")
      NULL
    })
  
  
mod
}


#worked?
ggplot(slopes, aes(x=sample, y=slope)) + 
  geom_point() +
  geom_errorbar(aes(ymin=slope-slope.se, ymax=slope+slope.se)) +
  ylab("slope, fl unit hr-1") +
  theme(axis.text.x=element_text(angle=-45, hjust=0))

#
ggsave("curacaotest1.pdf", height=8, width=10, units="in")

ggplot(output17, aes(output17[[4]],output17[[6]]))+geom_point(aes(colour=output17[[5]]))
ggplot(output, aes(output[,4],output[,6], colour=output[,5]))+geom_point()
ggplot(output)+geom_point(aes(output[,4],output[,6], colour=output[,5]))
write.csv(temp, file="~/Desktop/temp.csv")

# print(p_raw) # Drawing this plot is really slow
ggsave("curacaotest.pdf", height=12, width=12, units="in")



source("lm_stats.R")
slopes <- ddply(output, function(x) lm_stats(x, xvar=output[,4], yvar=output[,6]))
#worked
dm <- melt(output, id.vars=c(output[,2], output[,3], output[,1], output[,5]), variable.name=output[,4], value.name=output[,6])
          

ggplot(slopes, aes(x=sample, y=slope)) + 
  geom_point() +
  geom_errorbar(aes(ymin=slope-slope.se, ymax=slope+slope.se)) +
  ylab("slope, fl unit hr-1") +
  theme(axis.text.x=element_text(angle=-45, hjust=0))

ggsave("slope_dotplot.pdf", height=8, width=10, units="in")

write.csv(slopes, "KB_slopes.csv")
  