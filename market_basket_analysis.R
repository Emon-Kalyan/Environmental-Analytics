library(tidyr)

orders <- read.csv("orders.csv")
dim(orders)
View(orders)
order_products_prior <- read.csv("order_products__prior.csv")
View(order_products_prior)
products <- read.csv("products.csv")
depts <- read.csv("departments.csv")
aisles <- read.csv("aisles.csv")
View(products)
View(depts)
View(aisles)
library(dplyr)

productswithdeptnames <- inner_join(products, depts, by = "department_id")
dim(productswithdeptnames)
View(productswithdeptnames)
max(orders$order_id)
max(order_products_prior$order_id)
productsfull <- inner_join(productswithdeptnames, aisles, by = "aisle_id")
View(productsfull)
orders1 <- orders %>% filter(eval_set == "prior")
max(orders1$order_id)
orders2 <- inner_join(productsfull, order_products_prior, by = "product_id")
View(orders2)
orders2 %>% filter(orders2$order_id == 2)
orders3 <- inner_join(orders2, orders, by = "order_id")
dim(orders3)
dim(order_products_prior)
View(orders3)
orders3 <- orders3 %>% drop_na()
dim(orders3)
###------Starting with EDA of the data and the products------###
str(orders3)
orders3 %>% group_by(product_name) %>% summarise(count = n())
orders3[c('product_id', 'product_name',
          'aisle_id', 'aisle', 'department_id', 'department', 'order_id', 'reordered', 'user_id')] <- lapply(orders3[c('product_id', 'product_name',
                                                                                                                       'aisle_id', 'aisle', 'department_id', 'department', 'order_id', 'reordered', 'user_id')], factor)
str(orders3)
ggplot(orders3, aes('product_name'))
?chisq.test
##Let us check the associations
chisq.test(orders3$product_name, orders3$reordered)
##From the chisq.test we get to that there is strong association between
##the product_names and the reorders.
levs <- levels(orders3$department)
length(levs)
##There are 21  departments
#Let us see which are the highest ordered departments
deptsordered <- orders3 %>% group_by(department) %>% summarise(count = n())
depts_sorted <- arrange(deptsordered, desc(count))
head(depts_sorted, n = 10)
library(tidyverse)
ggplot(depts_sorted, aes(fct_reorder(department, count), count)) + 
  geom_col(aes(fill = count)) + coord_flip() + 
  labs(x = "grocery departments", y = "count of orders", title = "Volume of orders by grocery deptments" ) + 
  geom_hline( aes(yintercept = 60000) ) + theme_bw()
##Thus we have seen that the highest volume of orders come from the 
##departments of - produce, dairy eggs, snacks, beevrages and frozen
chisq.test(orders3$department, orders3$reordered)
##The chi-square test between the reorders show that the reorders
##are significantly associated with the departments
fit1 <- glm(reordered ~ department, data = orders3, family = binomial(logit))
summary(fit1)


##----***Let us extract the data only for produce and see which 
## products are contributing to the highest orders
produce_data <- orders3 %>% filter(department == "produce")
View(produce_data)
dim(produce_data)
produceordered <- produce_data %>% group_by(product_name) %>%
  summarise(count = n())
produceordered <- arrange(produceordered, desc(count))
dim(produceordered)
head(produceordered, n =10)
reorderedproduce <- produce_data %>% filter(reordered == 1) %>%
group_by(product_name)  %>% summarise(count = n())
highest_ordered_produce <- head(arrange(reorderedproduce, desc(count)), n = 10)
##The top reordered produces are - banana, organic strawberries, organic
##baby spinach, organic hass avocado, organic avocado,
##large lemon, organic avocado, organic raspberries, limes.
ggplot(highest_ordered_produce, aes(fct_reorder(product_name, count), count)) + 
  geom_col(aes(fill = count)) + coord_flip() + 
  labs(x = "grocery departments", y = "count of reorders", title = "Volume of reorders by produce." ) + 
  theme_bw()
##From the above analysis and visualizations, we can see the top
##in demand fresh produce

###The same analysis as above can be repeated for dairy and eggs
dairy_eggs_data <- orders3 %>% filter(department == "dairy eggs")
View(dairy_eggs_data)
dim(dairy_eggs_data)
dairyeggsordered <- dairy_eggs_data %>% group_by(product_name) %>%
  summarise(count = n())
dairyeggsordered <- arrange(dairyeggsordered, desc(count))
dim(dairyeggsordered)
head(dairyeggsordered, n =10)
reordereddairyeggs <- dairy_eggs_data %>% filter(reordered == 1) %>%
  group_by(product_name)  %>% summarise(count = n())
highest_ordered_dairyeggs <- head(arrange(reordereddairyeggs, desc(count)), n = 10)
##The top reordered produces are - banana, organic strawberries, organic
##baby spinach, organic hass avocado, organic avocado,
##large lemon, organic avocado, organic raspberries, limes.
ggplot(highest_ordered_dairyeggs, aes(fct_reorder(product_name, count), count)) + 
  geom_col(aes(fill = count)) + coord_flip() + 
  labs(x = "dairy & egg products", y = "count of reorders", title = "Volume of reorders by dairy & egg products." ) + 
  theme_bw()


###The same analysis as above can be repeated for snacks
snacks_data <- orders3 %>% filter(department == "snacks")
View(snacks_data)
dim(snacks_data)
snacksordered <- snacks_data %>% group_by(product_name) %>%
  summarise(count = n())
snacksordered <- arrange(snacksordered, desc(count))
dim(snacksordered)
head(snacksordered, n =10)
reorderedsnacks <- snacks_data %>% filter(reordered == 1) %>%
  group_by(product_name)  %>% summarise(count = n())
highest_ordered_snacks <- head(arrange(reorderedsnacks, desc(count)), n = 10)
##The top reordered produces are - banana, organic strawberries, organic
##baby spinach, organic hass avocado, organic avocado,
##large lemon, organic avocado, organic raspberries, limes.
ggplot(highest_ordered_snacks, aes(fct_reorder(product_name, count), count)) + 
  geom_col(aes(fill = count)) + coord_flip() + 
  labs(x = "snacks products", y = "count of reorders", title = "Volume of reorders by snacks products." ) + 
  theme_bw()

###The same analysis as above can be repeated for frozen
frozen_data <- orders3 %>% filter(department == "frozen")
View(frozen_data)
dim(frozen_data)
frozenordered <- frozen_data %>% group_by(product_name) %>%
  summarise(count = n())
frozenordered <- arrange(frozenordered, desc(count))
dim(frozenordered)
head(frozenordered, n =10)
reorderedfrozen <- frozen_data %>% filter(reordered == 1) %>%
  group_by(product_name)  %>% summarise(count = n())
highest_ordered_frozen <- head(arrange(reorderedfrozen, desc(count)), n = 10)
##The top reordered produces are - banana, organic strawberries, organic
##baby spinach, organic hass avocado, organic avocado,
##large lemon, organic avocado, organic raspberries, limes.
ggplot(highest_ordered_frozen, aes(fct_reorder(product_name, count), count)) + 
  geom_col(aes(fill = count)) + coord_flip() + 
  labs(x = "frozen products", y = "count of reorders", title = "Volume of reorders by frozen products." ) + 
  theme_bw()

###The same analysis as above can be repeated for beverages
bev_data <- orders3 %>% filter(department == "beverages")
View(bev_data)
dim(bev_data)
bevordered <- bev_data %>% group_by(product_name) %>%
  summarise(count = n())
bevordered <- arrange(bevordered, desc(count))
dim(bevordered)
head(bevordered, n =10)
reorderedbev <- bev_data %>% filter(reordered == 1) %>%
  group_by(product_name)  %>% summarise(count = n())
highest_ordered_bev <- head(arrange(reorderedbev, desc(count)), n = 10)
##The top reordered produces are - banana, organic strawberries, organic
##baby spinach, organic hass avocado, organic avocado,
##large lemon, organic avocado, organic raspberries, limes.
ggplot(highest_ordered_bev, aes(fct_reorder(product_name, count), count)) + 
  geom_col(aes(fill = count)) + coord_flip() + 
  labs(x = "Beverage products", y = "count of reorders", title = "Volume of reorders by beverage products." ) + 
  theme_bw()


pantry_data <- orders3 %>% filter(department == "pantry")
View(pantry_data)
dim(pantry_data)
pantryordered <- pantry_data %>% group_by(product_name) %>%
  summarise(count = n())
pantryordered <- arrange(pantryordered, desc(count))
dim(pantryordered)
head(pantryordered, n =10)
pantryreordered <- pantry_data %>% filter(reordered == 1) %>%
  group_by(product_name)  %>% summarise(count = n())
pantry_highest_ordered <- head(arrange(pantryreordered, desc(count)), n = 10)
##The top reordered produces are - banana, organic strawberries, organic
##baby spinach, organic hass avocado, organic avocado,
##large lemon, organic avocado, organic raspberries, limes.
ggplot(pantry_highest_ordered, aes(fct_reorder(product_name, count), count)) + 
  geom_col(aes(fill = count)) + coord_flip() + 
  labs(x = "pantry", y = "count of reorders", title = "Volume of reorders by pantry." ) + 
  theme_bw()





