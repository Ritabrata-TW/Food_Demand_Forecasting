######## Read data ############
demand_data <- read_csv("/Users/in-rmoitra/Projects/AnalyticsVidya/FoodDemand/train/train.csv")
fulfilment_data <- read_csv("/Users/in-rmoitra/Projects/AnalyticsVidya/FoodDemand/train/fulfilment_center_info.csv")
meal_data <- read_csv("/Users/in-rmoitra/Projects/AnalyticsVidya/FoodDemand/train/meal_info.csv")
attach(demand_data)
attach(fulfilment_data)
attach(meal_data)

###### EDA on demand_data #######
sum(is.na(demand_data))
summary(demand_data)

#### How does overall demand vary by time? ######
overall_demand_by_week <- demand_data %>%
  group_by(week) %>%
  summarise('overall_demand' = sum(num_orders))

ggplot(data=overall_demand_by_week, aes(x=week,y=overall_demand,group=1)) + 
  geom_line() +
  scale_x_continuous(breaks = round(seq(min(overall_demand_by_week$week), max(overall_demand_by_week$week), by = 1),1))

#### How does individual meal types demand vary over time? #####
demand_with_meal_data <- demand_data %>%
  left_join(meal_data, by='meal_id')

unique(demand_with_meal_data$cuisine)

overall_demand_by_meal_type <- demand_with_meal_data %>%
  group_by(cuisine, week) %>%
  summarise('overall_demand' = sum(num_orders))


ggplot(data=overall_demand_by_meal_type, aes(x=week,y=overall_demand,group=cuisine, color=cuisine)) + 
  geom_line() +
  scale_x_continuous(breaks = round(seq(min(overall_demand_by_week$week), max(overall_demand_by_week$week), by = 1),1))

#### How does demand data vary with respect to regions? ##########
demand_with_center_data <- demand_data %>%
  left_join(fulfilment_data, by='center_id')

unique(demand_with_center_data$region_code)

overall_demand_by_region<- demand_with_center_data %>%
  group_by(region_code, week) %>%
  summarise('overall_demand' = sum(num_orders))

ggplot(data=overall_demand_by_region, aes(x=week,y=overall_demand,group=region_code, color=region_code)) + 
  geom_line() +
  scale_x_continuous(breaks = round(seq(min(overall_demand_by_week$week), max(overall_demand_by_week$week), by = 1),1))
