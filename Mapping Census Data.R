################################################################################ 
#################### R Script to Download & Maps Census Data on ################
################# the Population of People Living in Correctional ##############
####################### facilities in PNW Counties in 2020 #####################
################################################################################ 



# 1st clear the working memory, (down)load the libraries needed, & set the working 
# directory.
  rm(list = ls())
  # install.packages("tidyverse")
  # install.packages("tidycensus")
  # install.packages("tigris")
  # install.packages("tmap")
  library(tidyverse) # Contains a suite of helpful packages (readr, ggplot2, dplyr, tidyr, purrr, tibble, stringr, forcats)
  library(tidycensus) # For accessing ACS data via API key
  library(tigris) # For accessing shapefiles from the Census Bureau
  library(tmap) # For mapping functions
  options(scipen = 100)    # To turn off scientific notation
  setwd("your directory here")

# 1st, load in a table of the full list of variable names in the Decennial 
# Census data. Then download the data for total population, total adult  
# correctional population, and total juvenile correctional population.  
# NOTE - In order for this code to run, you will need to request an API key 
# from the Census Bureau at https://api.census.gov/data/key_signup.html
  # census_api_key("your key here",overwrite = TRUE,install = TRUE)
  readRenviron("~/.Renviron")
  acs5 <- load_variables(2020,"pl")
  View(acs5)
  
  corrpop2020 <- get_decennial(geography="county",year=2020,sumfile = "pl",
                     variables = c(totalpop="H1_001N",
                                   adultcorpop="P5_003N",
                                   juvcorpop="P5_004N"))
  
# Now pivot the data to wide format & compute a measure for the percentage of the 
# population that is institutionalized.
  corrpop2020w <- corrpop2020 %>%
    pivot_wider(names_from = variable, values_from = c(value)) 
  corrpop2020w <- corrpop2020w %>%
    mutate(percinst=(adultcorpop+juvcorpop)/totalpop)

# Now use the tigris pacakge to laod in the shapefile data for PNW counties
  pnwcounties <- counties(state=c("ID","OR","WA"), cb=TRUE)
  qtm(pnwcounties)

# Now merge the Census data with the map data & summarize the correctional 
# population data.
  pnwmap<- left_join(pnwcounties, corrpop2020w, by = "GEOID")
  sumstats <- pnwmap %>%
    summarise(min=min(adultcorpop),
              max=max(adultcorpop),
              med=median(adultcorpop),
              quant=quantile(adultcorpop))

# Now create a figure with the total adult correctional population in each county,
# and save it as a PNG in the working directory.
  corpopfig <- tm_shape(pnwmap) + 
    tm_fill("adultcorpop", breaks = c(0,10,50,100,300,500,1000,3000,5000,6400),
            palette="YlOrRd", contrast = c(.1,1),
            labels = c("<10","10-50","50-100","100-300","300-500","500-1k","1k-3k","3k-5k",">5k"),
            title=" ",colorNA="gray100",textNA = "Missing") + 
    tm_borders(alpha =.6,col="gray80") + 
    tm_layout(inner.margins = 0.03,frame = FALSE) + 
    tm_legend(show=TRUE, width = .15, position = c("right", "top"), 
              frame = FALSE,text.fontfamily="Helvetica")  + 
    tm_shape(pnwmap) +  
    tm_text("NAME.x", size =0.4,fontfamily="Helvetica")
  
  png(file ="Correctional Population.png", width = 2400, height = 1600, units = "px", res = 300)
    corpopfig
  dev.off()


# Now do the same, but for the proportion of the population in correctional 
# facilities
  sumstats <- pnwmap %>%
    summarise(min=min(percinst),
              max=max(percinst),
              med=median(percinst),
              quant=quantile(percinst))
  pcorpopfig <- tm_shape(pnwmap) + 
    tm_fill("percinst", breaks = c(0,.01,.10,.25,.3),
            palette="YlOrRd", contrast = c(.1,1),
            labels = c("<1%","1-10%","10-25%",">25%"),
            title=" ",colorNA="gray100",textNA = "Missing") + 
    tm_borders(alpha =.6,col="gray80") + 
    tm_layout(inner.margins = 0.03,frame = FALSE) + 
    tm_legend(show=TRUE, width = .15, position = c("right", "top"), 
              frame = FALSE,text.fontfamily="Helvetica")  + 
    tm_shape(pnwmap) +  
    tm_text("NAME.x", size =0.4,fontfamily="Helvetica")
  
  png(file ="Percent Correctional Population.png", width = 2400, height = 1600, units = "px", res = 300)
    pcorpopfig
  dev.off()