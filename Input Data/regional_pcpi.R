library(tidyverse)

# Regional CPI factors for 1997:2014 rescaled to 2013 base.
south_13 <- tibble(year = c(1997:2014), Region = rep("south", 18), cpi_factor = c(0.692039996,	0.700861411,	0.714534604,	0.737470283,
              0.754672042,	0.764375598,	0.782018428,	0.801866611,
              0.83053621,	0.858764737,	0.883733752,	0.920430838,
              0.916743486,	0.932150088,	0.964260038,	0.9846551491, 1,
              1.01689742))

west_13 <- tibble(year = c(1997:2014), Region = rep("west", 18), cpi_factor = c(0.684408712,	0.697130063,	0.71621209,	0.741230748,	
             0.768369632,	0.783211208,	0.799748965,	0.818406948,	
             0.843425606,	0.872260669,	0.899950811,	0.931397992,	
             0.927903861,	0.938000373,	0.964638883,	0.985378927,	1,
             1.018619818))

midwest_13 <- tibble(year = c(1997:2014), Region = rep("midwest", 18), cpi_factor = c(0.705315749,	0.717018499,	0.732322096,	0.757528019,
                0.777782779,	0.787235,	0.802538597,	0.821893145,
                0.84799928,	0.868704145,	0.891763064,	0.924436243,
                0.918503848,	0.93642706,	0.966570644,	0.986181753,
                1, 1.014650943))

northeast_13 <- tibble(year = c(1997:2014), Region = rep("northeast", 18), cpi_factor = c(0.672989664,	0.682626748,	0.696680828,	0.720371991,	
                  0.740449249,	0.755707964,	0.776989857,	0.803893382,	
                  0.833206177,	0.863322063,	0.885455232,	0.920767112,	
                  0.920915684,	0.939085601,	0.967711755,	0.986588392,	
                  1,	1.013752921))

regional_cpi <- rbind(south_13, west_13, midwest_13, northeast_13) %>% 
  mutate(year = as.character(year))
  
nominal_pcpi <- readxl::read_xlsx("Input Data/TANF_ind-variables.xlsx", 
                                  sheet = "PCPI Nominal")

real_pcpi <-gather(nominal_pcpi, key = "year", value = "nominal_pcpi", -Region, -GeoName)

real_pcpi <- left_join(regional_cpi, real_pcpi, by = c("year", "Region"))

real_pcpi <- real_pcpi %>% 
  mutate(real_pcpi = nominal_pcpi / cpi_factor) %>% 
  select(GeoName, year, real_pcpi)

real_pcpi <- spread(real_pcpi, key = "year", value = "real_pcpi")

write_csv(real_pcpi, "Input Data/real_regional_pcpi.csv")



