library(tidyverse)
library(ggplot2)
library(deSolve)
library(tidyr)

# With Policy OFF
time <- seq(0,20,by = 0.125)
state <- c(Geo_Political_Tensions = 1, Inventory =1, Production_Capacity = 1) # General Initial States of Stocks

parameters <- c(Effect_Of_Us_Arm_Sales = 0.02, 
                Initial_Price =  1, 
                Initial_Cost = 0.7, 
                Inventory_Capacity = 1, 
                Production_Capacity =1 )

TSMC_Policy_Off<- function(t,state,parameters){ with(as.list(c(state,parameters)),{
  Export_Restrictions <- Geo_Political_Tensions
  Tax_Rate <- Geo_Political_Tensions
  Prices <- Initial_Price * Tax_Rate * Export_Restrictions
  Demand <- 0.1/Prices
  Rate_Of_Investment <- Demand
  
  Revenue <- Prices*Inventory
  Resources <- 1/(Tax_Rate * Export_Restrictions)
  Research_And_Development <- Revenue * Resources

  dGeo_Political_Tensions <- if (Research_And_Development*Inventory > 10) 0.01 else Effect_Of_Us_Arm_Sales*Inventory*Research_And_Development

  Sales_Factor <- if (t>=1) Demand+1 else Demand
  Sales_Rate <- Sales_Factor*1
  Delivery_Rate <- Production_Capacity
  dInventory <- Delivery_Rate - Sales_Rate

  Inventory_Gap <- Inventory_Capacity - Inventory 
  dProduction_Capacity <- Rate_Of_Investment*Inventory_Gap
  
  print(c(Geo_Political_Tensions,dGeo_Political_Tensions,Inventory,Production_Capacity,t))
  list(c(dGeo_Political_Tensions,dInventory,dProduction_Capacity),Demand = Demand,Prices=Prices,Research_And_Development = Research_And_Development,
       Inventory_Gap = Inventory_Gap, Revenue = Revenue, Tax_Rate=Tax_Rate, Export_Restrictions = Export_Restrictions,
       Resources = Resources, Sales_Rate = Sales_Rate, Delivery_Rate = Delivery_Rate)
})
}

result <- ode(y = state, times = time, func = TSMC_Policy_Off, parms = parameters,method =euler)
result <- as.data.frame(result)
result %>% ggplot() +
  geom_line(aes(x = time, y =Inventory,colour = 'red'))

#TSMC with Policy ON
parameters <- c(Effect_Of_Us_Arm_Sales = 0.01, Initial_Price =  1, Initial_Cost = 0.7, Inventory_Capacity = 1, Production_Capacity =1 )
TSMC_Policy_On<- function(t,state,parameters){ with(as.list(c(state,parameters)),{
  Export_Restrictions <- Geo_Political_Tensions
  Tax_Rate <- Geo_Political_Tensions
  Prices <- Initial_Price * Tax_Rate * Export_Restrictions
  Demand <- 0.1/Prices
  Rate_Of_Investment <- Demand
  
  Revenue <- Prices*Inventory
  Resources <- 1/(Tax_Rate * Export_Restrictions)
  Research_And_Development <- Revenue * Resources
  
  dGeo_Political_Tensions <- if (Research_And_Development*Inventory > 10) 0.01 else Effect_Of_Us_Arm_Sales*Inventory*Research_And_Development
  
  Sales_Factor <- if (t>=1) Demand+1 else Demand
  Sales_Rate <- Sales_Factor*1
  Delivery_Rate <- Production_Capacity
  dInventory <- Delivery_Rate - Sales_Rate

  Inventory_Gap <- Inventory_Capacity - Inventory 
  dProduction_Capacity <- Rate_Of_Investment*Inventory_Gap
  
  
  list(c(dGeo_Political_Tensions,dInventory,dProduction_Capacity),Demand = Demand,Prices=Prices,Research_And_Development = Research_And_Development,
       Inventory_Gap = Inventory_Gap, Revenue = Revenue, Tax_Rate=Tax_Rate, Export_Restrictions = Export_Restrictions,
       Resources = Resources, Sales_Rate = Sales_Rate, Delivery_Rate = Delivery_Rate)
})  
}

result2 <- ode(y = state, times = time, func = TSMC_Policy_On, parms = parameters,method =euler)
result2 <- as.data.frame(result2)
result2 %>% ggplot() +
  geom_line(aes(x = time, y =Geo_Political_Tensions,colour = 'red'))

# Combining both the results

ggplot()+
  geom_line(data = result,aes(x=time,y=Geo_Political_Tensions,colour="Geo_Political_Tensions_Policy_OFF")) +
  geom_line(data= result2,aes(x=time,y=Geo_Political_Tensions,colour="Geo_Political_Tensions_Policy_ON"))+
  geom_line(data = result,aes(x=time,y=Inventory,colour="Inventory_OFF")) +
  geom_line(data= result2,aes(x=time,y=Inventory,colour="Inventory_ON"))+
  geom_line(data = result,aes(x=time,y=Production_Capacity,colour="Production_Capacity_OFF")) +
  geom_line(data= result2,aes(x=time,y=Production_Capacity,colour="Production_Capacity_ON"))


# Mutating two Data frames
Geo_Political_Tensions_Policy_Off <- result$Geo_Political_Tensions

result2 <- result2 %>% cbind(Geo_Political_Tensions_Policy_Off)

result2 %>% ggplot(colour = ) +
  geom_line(aes(x = time, y =Geo_Political_Tensions,colour = "Policy On",linewidth=0.1)) +
  geom_line(aes(x = time, y =Geo_Political_Tensions_Policy_Off,colour = "Policy Off"))+
  labs(title = "Geo political tensions")

# Bitcoin activity -- BOX PLOT -- [Demonstration of R only]
btc <- read.csv(file = "E:\\System thinking with R\\BTC-USD.csv")

btc %>% filter(Open != "null") %>% 
  mutate(Month=format(as.Date(Date),"%m"),Open=as.numeric(Open)) %>% 
  select(Month,Open,Close) %>%
  ggplot(aes(x=Month,y=Open))+
  geom_boxplot(fill='green')

btc %>% filter(Open != "null") %>% 
  mutate(Month=as.Date(Date),Open=as.numeric(Open),High=as.numeric(High),Low=as.numeric(Low)) %>% 
  select(Month,Open,Low,High) %>%
  ggplot(aes(x = Month,y=Open,color='Open'))+
  geom_line()+
  geom_line(aes(y=High,color='High'))+
  geom_line(aes(y=Low,color='Low'))+
  labs(title ="Comparing Open Balances")

result %>% ggplot()+
  geom_boxplot(aes(y=Geo_Political_Tensions,x=Inventory,fill='Geo_Political_Tensions'))

# Filter function
result %>% 
  filter(time > 5 & Demand < 1)  %>% 
  slice(1:5) %>% select(Sales_Rate,Delivery_Rate) %>%
  summarise(avg_sr = mean(Sales_Rate),avg_dr=mean(Delivery_Rate),
            sd_sr=sd(Sales_Rate))

# Slice function
result %>% slice(1:20) %>% arrange(desc(Inventory)) %>% select(Inventory,time)

# SET OPERATIONS
intersect(result,result2)

# Classifying the Tensions into classes
result <- result %>% mutate(Class = case_when(
  Geo_Political_Tensions <= 1.1 ~ "Low",
  1.1 < Geo_Political_Tensions & Geo_Political_Tensions <= 1.2 ~ "Medium",
  1.2 < Geo_Political_Tensions & Geo_Political_Tensions <= 1.29 ~ "High",
  Geo_Political_Tensions > 1.29 ~ "Very High"
))

# Fill and Drop_na
result %>% ggplot() +
  geom_point(aes(x=time,y=Revenue,colour = Class))

result %>% drop_na(Class) %>% 
  group_by(Class) %>% summarise(mean_Inv = mean(Inventory),
                                median_Inv = median(Inventory),
                                sd_Inv=sd(Inventory), max_Invetory = max(Inventory),
                                max_revenue= max(Revenue))
  

