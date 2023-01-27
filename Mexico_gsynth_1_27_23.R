#------------------------------------------------------------------------------#
#--------Wildfire smoke and cardio-respiratory hospitalizations----------------#  
#--------San Diego Tijuana border region: Tijuana code-------------------------#
#-------------------------R code-----------------------------------------------#
#------------------------Date:1/27/23------------------------------------------#
#-----------------------------Lara Schwarz-------------------------------------#
#------------------------------------------------------------------------------#

setwd("D:/Lara/Border/Code/Smoke_cardio_resp_hosp_analysis/codes_clean/GitHub")
### uploading and processing  smoke data
#Mex_smk_intsct_mun_2007 <- read.csv("D:/Lara/Border/Data/smoke/Smoke_data/Mex_smoke_estimates/2007.csv")
#Mex_smk_intsct_mun_2007 <- read.csv("Mex_smoke_2007.csv")
#save(Mex_smk_intsct_mun_2007, file="Mex_smoke_2007.Rdata")
load(file="Mex_smoke_2007.Rdata")

# Adding a new variable 'year' with the value 2007
Mex_smk_intsct_mun_2007$year<- 2007

## uploading Baja hospitalization data for 2007
Mex_hosp <- read.csv("Mex_hosp_circ_resp_2007.csv")

# Padding the 'munic' and 'entidad' variables with leading zeroes
Mex_hosp$munic<-str_pad(Mex_hosp$munic, 3, pad = "0")
Mex_hosp$entidad<-str_pad(Mex_hosp$entidad, 2, pad = "0")

# Creating a new variable 'ADM2_PCODE' by concatenating 'MX', 'entidad', and 'munic'
Mex_hosp$ADM2_PCODE <-paste0("MX", Mex_hosp$entidad,  Mex_hosp$munic)

# Converting the 'date' variable to a date format
Mex_hosp$date<-as.Date(Mex_hosp$date, format="%Y-%m-%d")

##############################################
########### October 2007 wildfire ############
##############################################

# Assign the dataframe Mex_smk_intsct_mun_2007 to a new variable Smokebymunic07
Smokebymunic07 <- Mex_smk_intsct_mun_2007

# Adding a new binary variable 'Smkbin' based on the value of 'coverage_pct'
# If coverage_pct is greater than 30, Smkbin is 1, otherwise it is 0
Smokebymunic07$Smkbin <- ifelse(Smokebymunic07$coverage_pct > 30, 1, 0)

# Converting the 'dates' variable to character format
Smokebymunic07$dates <- as.character(Smokebymunic07$dates)

# Converting the 'dates' variable to date format, using the format "%Y%m%d"
Smokebymunic07$date <- as.Date(Smokebymunic07$dates, format = "%Y%m%d")

# Filtering the data to only include Tijuana
TJ_smoke_2007 <- Smokebymunic07[which(Smokebymunic07$ADM2_PCODE == "MX02004"),]

## Looking at smoke data

# Plotting a line graph of date vs coverage_pct for Tijuana smoke data
TJ2007 <- ggplot(TJ_smoke_2007, aes(x = date, y = coverage_pct)) +
  geom_line() + 
  xlab("") +
  ggtitle("Tijuana Smoke")
TJ2007

## Creating month and day variables

# Creating a new variable for month
TJ_smoke_2007$month <- month(TJ_smoke_2007$date) 

# Creating a new variable for day
TJ_smoke_2007$day <- day(TJ_smoke_2007$date) 

# Filtering the data to only include October
TJ_smoke_2007_oct <- TJ_smoke_2007[which(TJ_smoke_2007$month == 10),]

# Find which days had wildfire smoke in October 2007
# Grouping the data by day and calculating the mean coverage_pct
TJ_smoke_2007_oct_days <- TJ_smoke_2007_oct %>%
  group_by(day) %>%
  summarise(pct_smoke = mean(coverage_pct))

# Joining the Smokebymunic07 and Mex_hosp dataframes on ADM2_PCODE and date
Mexbymunic_smoke_hosp_07 <- left_join(Smokebymunic07, Mex_hosp,
                                      by = c("ADM2_PCODE", "date"))

# Creating new variables for day, month and year
Mexbymunic_smoke_hosp_07$day <- day(Mexbymunic_smoke_hosp_07$date) 
Mexbymunic_smoke_hosp_07$month <- month(Mexbymunic_smoke_hosp_07$date) 
Mexbymunic_smoke_hosp_07$year <- year(Mexbymunic_smoke_hosp_07$date) 


##############################################
########### 2007 wildfire event ############
##############################################

# Filter data for October 2007
oct2007_data <- Mexbymunic_smoke_hosp_07[which(Mexbymunic_smoke_hosp_07$month==10),]

# Create new column for combined circulatory and respiratory values
oct2007_data$circresp <- oct2007_data$circ+oct2007_data$resp

# Rename existing columns
oct2007_data$circ <- oct2007_data$circulatory
oct2007_data$resp <- oct2007_data$respiratory

# Replace NA values in new column with 0
oct2007_data$circresp[is.na(oct2007_data$circresp)] <- 0

# Create function to calculate rolling mean of 2 days
fnrollmean <- function (x) {
  if (length(x) < 2) {
    rep(NA,length(x)) 
  } else {
    rollmean(x,2,align="right",na.pad=TRUE)
  }
}

# Group data by ADM2_PCODE and calculate rolling mean
oct2007_data <- oct2007_data %>%
  dplyr::group_by(ADM2_PCODE) %>% 
  dplyr::arrange( ADM2_PCODE, dates, coverage_pct) %>% 
  dplyr::mutate(circresp2da=fnrollmean(circresp))

# Filter data for October 11th - 31st 2007
oct_11_31_data <- oct2007_data[which(oct2007_data$day>10 & oct2007_data$day<=26),]

# Group data by ADM2_PCODE and calculate mean of smoke and coverage percentage
oct_11_31_mean <- oct_11_31_data %>%
  dplyr::group_by(ADM2_PCODE) %>%
  dplyr::summarize(Smk = mean(Smkbin), coverage_pct = mean(coverage_pct))

# Identify exposed counties
exposed_counties <- oct_11_31_mean[oct_11_31_mean$coverage_pct > 0,]
exposed_counties$Ineligible_control <- 1

# Identify non-exposed counties
non_exposed_counties <- oct_11_31_mean[oct_11_31_mean$Smk == 0,]

# Create a new dataframe with necessary columns
oct_11_31_data <- subset(oct_11_31_data, select = c(ADM2_PCODE, date, circ, resp, circresp, circresp2da, day, month, Smkbin))

# Join exposed counties information to data
MX_smoke_scm <- left_join(oct_11_31_data, exposed_counties, by = c("ADM2_PCODE"))

# Create a new column for exposure
MX_smoke_scm$Exp <- ifelse(MX_smoke_scm$day < 21, 0, ifelse(MX_smoke_scm$day >= 21, 1, NA))

# Filter data for only Tijuana and set unexposed cases to 0
MX_smoke_scm_TJ <- MX_smoke_scm %>% filter(ADM2_PCODE == "MX02004")
MX_smoke_scm$Ineligible_control[is.na(MX_smoke_scm$Ineligible_control)] <- 0

# Group data by municipality and filter out cases with no circulatory or respiratory hospitalizations
MX_smoke_scm$circresp[is.na(MX_smoke_scm$circresp)] <- 0
MX_smoke_scm$circresp2da[is.na(MX_smoke_scm$circresp2da)] <- 0
MX_smoke_scm$munic <- MX_smoke_scm$ADM2_PCODE
MX_smoke_scm <- MX_smoke_scm %>% group_by(ADM2_PCODE) %>% filter(prod(circresp2da) > 0)

# Create final dataset for analysis
df <- MX_smoke_scm

#---------------------------------Generalized synthetic control------------------------------

# Set seed for reproducibility
set.seed(100)

# Create new exposure indicator
# 0 for all weeks prior to fire week and 1 only if exposed and during fire week
df <- df[which(df$munic=="MX02004" | df$Ineligible_control==0),]
df$exp_daily <- ifelse(df$Smkbin ==1 & df$day>=11,1,0)

# Remove missing values
missing <- df[!complete.cases(df),]

# Factor municipality id
df$munic <- as.factor(df$munic)

# Implement generalized synthetic control
gsynth_tj.out <- gsynth(circresp2da ~ exp_daily, data = df, index = c("munic","day"), 
                        nboots = 500, inference = "parametric", se = TRUE, parallel = TRUE)

# Create rate per 100,000 persons
coeff_TJ <- (1000000/1603955)

# Plot gap
plot_gsynth_TJ<-plot(gsynth_tj.out, type = "gap" , xlab = "day", ylab="Difference Between Treated and Counterfactual")

# Plot treated average and estimated counterfactual
plot_gsynth_TJ_scm<-plot(gsynth_tj.out, type = "counterfactual", xlab = "day", ylab="Hospitalizations")

# Add second axis with rate
plot_Tijuana<-plot_gsynth_TJ_scm +scale_y_continuous(name = "Hospitalizations", sec.axis = sec_axis(~.*coeff_TJ, name="Hospitalization rate (per million)")) 

# Save plot
ggsave("D:/Lara/Border/results/Smoke_cardio_resp_hosp/SCM_results/Tijuana_smoke30.svg", width = 12, height = 10, units = "cm")

# Save weights
TJ_weights<-gsynth_tj.out$wgt.implied
write.csv(TJ_weights, "D:/Lara/Border/results/Smoke_cardio_resp_hosp/SCM_results/weights_TJ_smoke30.csv")

#---------------------------------Combining figures with San Diego data-----------------------------#
# San Diego hospitalization data cannot be shared for confidentiality reasons 
# data came from the California Office of Statewide Health Planning and Development (OSHPD)
# Same methodology was applied 

# Create a plot of hospitalizations of results from San Diego
plot_SD <- plot(gsynth1_sd.out, 
                                 type = "counterfactual",  
                                 xlab = "Day of October 2007", 
                                 ylab = "Hospitalizations", 
                                 shade.post = TRUE, 
                                 raw = "none", 
                                 main = "")

# Add a second y-axis to the plot and specify its features
plot_SD <- plot_SD + 
  scale_y_continuous(name = "Hospitalizations", 
                     sec.axis = sec_axis(~.*coeff_SD, name="Hospitalization rate (per million)"))

# Add a title to the plot
plot_SD <- annotate_figure(plot_SD, top = text_grob("San Diego", face = "bold", size = 14))


# Use the same theme options for both plots
apply_common_theme <- function(plot) {
  plot + 
    theme(panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank())
}

plot_SD <- apply_common_theme(plot_SD)
plot_Tijuana <- apply_common_theme(plot_Tijuana)

# Create a list of the two plots
plots_list <- list(plot_Tijuana, plot_SD)

# Create a single plot containing both plots
combined_plots <- wrap_plots(plots_list, nrow = 2)

# Create a single plot containing both plots with a common legend
SCM_TJ_SD <- ggarrange(plot_Tijuana, plot_SD, 
                       nrow = 2, common.legend = T)
