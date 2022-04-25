library(readr)
library(readxl)
library(psych)
library(dplyr)
supplychain_data <- read.csv("food-emissions-supply-chain.csv")
View(supplychain_data)
dim(supplychain_data)
supplychain1 <- supplychain_data[,-c(1,2)]
View(supplychain1)
library(ggridges)
supplychainlong1 <- supplychain1 %>% pivot_longer(
  c('food_emissions_land_use', 'food_emissions_farm', 
    'food_emissions_animal_feed', 'food_emissions_processing', 
    'food_emissions_transport', 'food_emissions_retail', 
    'food_emissions_packaging'), names_to = "supply_chain_stage",
                                 values_to = "GHG_emissions")
View(supplychainlong1)
str(supplychainlong1)
supplychainlong1$supply_chain_stage <- as.factor(supplychainlong1$supply_chain_stage)
data1 <- supplychainlong1 %>% filter(supply_chain_stage == "food_emissions_farm"|
    supply_chain_stage == "food_emissions_processing"|supply_chain_stage == "food_emissions_transport"| supply_chain_stage == "food_emissions_transport"| supply_chain_stage == "food_emissions_retail"| supply_chain_stage == "food_emissions_packaging" ) 
View(data1)
ggplot(data1, aes(x= GHG_emissions, 
           y = as_factor(supply_chain_stage))) + 
               geom_density_ridges(scale = 6) +
  scale_fill_viridis_c(
    option = "plasma",
    name = "") +
  labs(x = "GHG emissions",
       y = "stage") +
  theme_ridges() + scale_x_continuous(breaks = seq(0,20,2))

ggplot(data1, aes(x= GHG_emissions, 
                  y = as_factor(supply_chain_stage))) + 
  stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = FALSE, quantiles = 4, quantile_lines = FALSE) +
  scale_fill_viridis_c(name = "Quartiles") + scale_x_continuous(breaks = seq(0,20,2)) + lims(x = c(-1,10))
  

ggplot(data1, aes(x= GHG_emissions, 
                  y = as_factor(supply_chain_stage), fill = factor(stat(quantile)))) + 
  stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE, quantiles = 2, quantile_lines = FALSE) +
  scale_fill_viridis_d(name = "Quartiles") + scale_x_continuous(breaks = seq(0,20,2)) + lims(x = c(-1,10)) + labs(x = "GHG emissions(kgCO2eq per kg of food)",
                                                                                                                  y = "stage", title = "GHG impact in different stages.", subtitle = "Per kilogram of food products")

supplychain2 <- supplychain_data[,-c(2,3)]
View(supplychain2)
quantile(supplychain2$food_emissions_transport, seq(0,1,0.25))
supplychain_trnasport_top <- supplychain2 %>% 
                        filter(food_emissions_transport > 0.1)
head(supplychain_trnasport_top)  
top_transport_emission_sorted <- arrange(supplychain_trnasport_top, desc(food_emissions_transport))  
ggplot(top_transport_emission_sorted, aes(fct_reorder(Entity, food_emissions_transport), food_emissions_transport)) + 
  geom_col(aes(fill = food_emissions_transport)) + coord_flip() + 
  labs(x = "Food Item", y = "GHG emissions(kgCO2eq per kg of food)", title = "Emissions during transport by various food" ) + 
  theme_bw()  + geom_hline(yintercept = 0.4, linetype = 2)
View(supplychain2)  
supplychain2$total_emissions <- supplychain2$food_emissions_land_use +
                        supplychain2$food_emissions_farm + supplychain2$food_emissions_retail + supplychain2$food_emissions_packaging +
                        supplychain2$food_emissions_animal_feed +
                        supplychain2$food_emissions_processing +
                        supplychain2$food_emissions_transport 
  
lowest_emission_sorted <- arrange(supplychain2, desc(total_emissions)) 
lowest_emissions_food <- tail(lowest_emission_sorted, n = 16)  
ggplot(lowest_emissions_food, aes(fct_reorder(Entity, total_emissions), total_emissions))  +
  geom_col(aes(fill = total_emissions)) + coord_flip() +
            labs(x = "Food Item", y = "GHG emissions(kgCO2eq per kg of food)", title = "Environment friendly foods") 
  
quantile(supplychain2$total_emissions)




