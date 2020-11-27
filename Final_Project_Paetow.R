##Final Project

##====Section 1: Set Up====##
##Load dependencies 

library(lmtest)
library(ggplot2)
library(quantmod)
library(xts)
library(gridExtra)
library(stargazer)
library(xlsx)
library(rJava)

write.xlsx(DF, "dataf.xlsx")

##Set global parameters 
FRED_SERIES_PCE <- "PCEPILFE" 
TITLE_PCE <- "U.S. PCE Excluding Food and Energy" 
VAR_UNITS_PCE <- "Index 2012=100,
Seasonally Adjusted"
FREQ_PCE <- "Monthly"
FRED_SERIES_UNRATE <- "UNRATE"
TITLE_UNRATE <- "U.S. Unemployment Rate"
VAR_UNITS_UNRATE <- "Percent,
Seasonally Adjusted"
FREQ_UNRATE <- "Monthly"

##====Section 2: Getting/Cleaning the Data====##
##Get data from FRED
quantmod::getSymbols.FRED(Symbols = FRED_SERIES_PCE, env = parent.frame(), src = 'FRED')
quantmod::getSymbols.FRED(Symbols = FRED_SERIES_UNRATE, env = parent.frame(), src = 'FRED')

UNRATE_cutoff <- UNRATE["1961-12-01/2019-12-01"]
PCE <- PCEPILFE["1960-12-01/2019-12-01"]

##Change the frequency for unemployment from monthly to annual
##IGNORE THIS FOR NOW
years1 <- endpoints(UNRATE_cutoff, on = "years")
unrate_yrly_avg <- xts::period.apply(UNRATE_cutoff, INDEX = years1, FUN =  mean)

##Change units for PCE to percent change from year ago
PCE_pca_calc <- (diff(PCE, lag=12, differences=1, arithmetic = TRUE, log = FALSE)/PCE)*100

##Creating one xts object using cbind 
DF <- cbind(UNRATE_cutoff, PCE_pca_calc)

##Subsetting the data to delete any rows with NA values (only one row)
PCE_UNRATE_data_xts <- DF[complete.cases(DF), ]

##====Section 3: Statistical Analysis====##
##Summary Statistics
str(PCE_UNRATE_data_xts)
summary(PCE_UNRATE_data_xts)
xts::nyears(PCE_UNRATE_data_xts)
xts::nmonths(PCE_UNRATE_data_xts)

##Plotting individual variables curves (trendlines)
xts::plot.xts(UNRATE_cutoff,
              col = 1:8,
              main = "U.S. Monthly Unemployment Rate",
              )
xts::plot.xts(PCE_pca_calc,
              col = 1:8,
              main = "U.S. Monthly PCE Exlcuding Food and Energy",
              )

xts::plot.xts(PCE_UNRATE_data_xts,
              main = "Unemployment and PCE")

##Durbin Watson Test for Autocorrelation 
lmtest::dwtest(formula = PCEPILFE ~ UNRATE, data = PCE_UNRATE_data_xts)

##OLS Regression
model_phillips_curve <- lm(PCEPILFE ~ UNRATE, data = PCE_UNRATE_data_xts)

#Model Summary
summary(model_phillips_curve)

#Formatting Results 
stargazer(model_phillips_curve, title="Results", type="text", align=TRUE)

##====Section 3: Graphical Visualization====##
# QQ plot (are the residuals normal?)
plot(model_phillips_curve, which = 2)

##Changing data object from xts to dataframe using fortify 
PCE_UNRATE_data_frame = ggplot2::fortify(model = PCE_UNRATE_data_xts)

##Plotting Trendlines
ggplot2::ggplot(data = PCE_UNRATE_data_frame, mapping = aes(x=Index, y=UNRATE))+
  geom_path()+
  xlab("Year")+
  ylab("Unemployment Rate (%)")
ggplot2::ggplot(data = PCE_UNRATE_data_frame, mapping = aes(x=Index, y=PCEPILFE))+
  geom_path()+
  xlab("Year")+
  ylab("PCE exlcuding food and energy (% change from year ago)")

##Plotting the Phillip's Curve
ggplot2::ggplot(data = PCE_UNRATE_data_frame, mapping = aes(x=UNRATE, y=PCEPILFE))+
  geom_point(col="lightblue")+
  geom_smooth(method='lm')+
  labs(title = "The Phillip's Curve 1961-2019")+
  labs(caption = "based on data from Federal Reserve Bank of St. Louis (FRED)")+
  xlab("Unemployment Rate (%)")+
  ylab("PCE excluding food and energy (% change from year ago)")

##Plotting the Phillip's Curve by Decade

#First, I subset the data by decade
subset_1960s <- PCE_UNRATE_data_frame[PCE_UNRATE_data_frame$Index<("1970-01-01"),] 
subset_1970s <- PCE_UNRATE_data_frame[PCE_UNRATE_data_frame$Index<("1980-01-01") & 
                                        PCE_UNRATE_data_frame$Index>("1969-12-01"),] 
subset_1980s <- PCE_UNRATE_data_frame[PCE_UNRATE_data_frame$Index<("1990-01-01")  & 
                                        PCE_UNRATE_data_frame$Index>("1979-12-01"),] 
subset_1990s <- PCE_UNRATE_data_frame[PCE_UNRATE_data_frame$Index<("2000-01-01")  & 
                                        PCE_UNRATE_data_frame$Index>("1989-12-01"),] 
subset_2000s <- PCE_UNRATE_data_frame[PCE_UNRATE_data_frame$Index<("2010-01-01")  & 
                                        PCE_UNRATE_data_frame$Index>("1999-12-01"),] 
subset_2010s <- PCE_UNRATE_data_frame[PCE_UNRATE_data_frame$Index<("2019-01-01")  & 
                                        PCE_UNRATE_data_frame$Index>("2009-12-01"),] 
##Setting axis parameters 
My_Theme = theme(
  axis.title.x = element_text(size = 7),
  axis.text.x = element_text(size = 5),
  axis.title.y = element_text(size = 5),
  axis.text.y = element_text(size = 5),
  plot.title = element_text(size = 7))

#1960s
Plot_1960s = ggplot2::ggplot(data = subset_1960s, mapping = aes(x=UNRATE, y=PCEPILFE))+
  geom_point(col="red")+
  geom_smooth(method='lm')+
  labs(title = "The Phillip's Curve 1960s")+
  xlab("Unemployment Rate (%)")+
  ylab("PCE excluding food and energy (% change from year ago)")+
  My_Theme

#1970s
Plot_1970s = ggplot2::ggplot(data = subset_1970s, mapping = aes(x=UNRATE, y=PCEPILFE))+
  geom_point(col="purple")+
  geom_smooth(method='lm')+
  labs(title = "The Phillip's Curve 1970s")+
  xlab("Unemployment Rate (%)")+
  ylab("PCE excluding food and energy (% change from year ago)")+
  My_Theme

#1980s
Plot_1980s = ggplot2::ggplot(data = subset_1980s, mapping = aes(x=UNRATE, y=PCEPILFE))+
  geom_point(col="coral")+
  geom_smooth(method='lm')+
  labs(title = "The Phillip's Curve 1980s")+
  xlab("Unemployment Rate (%)")+
  ylab("PCE excluding food and energy (% change from year ago)")+
  My_Theme

#1990s
Plot_1990s = ggplot2::ggplot(data = subset_1990s, mapping = aes(x=UNRATE, y=PCEPILFE))+
  geom_point(col="gray")+
  geom_smooth(method='lm')+
  labs(title = "The Phillip's Curve 1990s")+
  xlab("Unemployment Rate (%)")+
  ylab("PCE excluding food and energy (% change from year ago)")+
  My_Theme

#2000s
Plot_2000s = ggplot2::ggplot(data = subset_2000s, mapping = aes(x=UNRATE, y=PCEPILFE))+
  geom_point(col="darkblue")+
  geom_smooth(method='lm')+
  labs(title = "The Phillip's Curve 2000s")+
  xlab("Unemployment Rate (%)")+
  ylab("PCE excluding food and energy (% change from year ago)")+
  My_Theme

#2010s
Plot_2010s = ggplot2::ggplot(data = subset_2010s, mapping = aes(x=UNRATE, y=PCEPILFE))+
  geom_point(col="firebrick")+
  geom_smooth(method='lm')+
  labs(title = "The Phillip's Curve 2010s")+
  xlab("Unemployment Rate (%)")+
  ylab("PCE excluding food and energy (% change from year ago)")+
  My_Theme

##Combine each decade graph into a single page
gridExtra::grid.arrange(Plot_1960s, Plot_1970s, Plot_1980s, Plot_1990s, Plot_2000s, Plot_2010s, nrow=2, ncol=3)

##Does the relationship change when looking at a subset of data?
##Run the same regression on 1960s data

##OLS Regression
model_phillips_curve_1960s <- lm(PCEPILFE ~ UNRATE, data = subset_1960s)

#Model Summary
summary(model_phillips_curve_1960s)


date <- as.Date(as.character(pc_data$DATE), "%d-%m-%Y")




















