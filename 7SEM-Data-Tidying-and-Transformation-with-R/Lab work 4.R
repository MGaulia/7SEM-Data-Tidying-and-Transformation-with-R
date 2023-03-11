library ( lubridate )
library ( tidyverse )

## 1
# a
levels(diamonds$cut)
# rusiuojamas pagal kirpimo geruma didejanciai
levels(diamonds$color)
# pagal abeceles tvarke


# b
diamonds$color %>% fct_collapse(
  jager = c("D"),
  river = c("E", "F"),
  top_wesselton = c("G"),
  wesselton = c("H"),
  top_crystal = c("I"),
  crystal = c("J")
)

# c
# c1
cone <- diamonds$clarity %>% factor() %>% fct_infreq(ordered = F)
levels(cone)
diamonds$clarity %>% as_tibble() %>% count(value) %>% arrange(n,value)

# c2
diamonds %>% as_tibble() %>% 
  group_by(clarity) %>%
  mutate(imties_dydis = max(price) - min(price)) %>%
  select(clarity, imties_dydis) %>% 
  distinct() %>% 
  arrange(imties_dydis)

# d
diamonds$clarity %>% fct_lump(n = 2,other_level = "kitos")

# e
levels(diamonds$color) # turime  7 kategorijas
levels(diamonds$clarity) # turime 8 kategorijas
fct_cross(diamonds$clarity, diamonds$color) # turime 56 kategorijas, nes susijunge faktoriai


## 2
# a
today()
# a1
aone <- today() + seconds(365*24*60*60)
aone
# a2
atwo <- today() %m+% years(1)
atwo

# b
weekdays(aone)
day(aone)

# c
whichyearhalf <- function(possible_date) {
    ifelse(month(possible_date) < 7,1,2)
}
whichyearhalf(today())
whichyearhalf(today() + months(6))


## 3
path <- "https://www.mif.vu.lt/~visk/Duomenu_tvarkyba_R/NorthWind/"

categories <- read.csv2(file = paste0(path,"Categories.txt"),header = T,sep = ";",dec = ",")
customers <- read.csv2(file = paste0(path,"Customers.txt"),header = T,sep = ";",dec = ",")
employees <- read.csv2(file = paste0(path,"Employees.txt"),header = T,sep = ";",dec = ",")
order_details <- read.csv2(file = paste0(path,"Order_Details.txt"),header = T,sep = ";",dec = ",")
orders <- read.csv2(file = paste0(path,"Orders.txt"),header = T,sep = ";",dec = ",")
products <- read.csv2(file = paste0(path,"Products.txt"),header = T,sep = ";",dec = ",")
shippers <- read.csv2(file = paste0(path,"Shippers.txt"),header = T,sep = ";",dec = ",")
suppliers <- read.csv2(file = paste0(path,"Suppliers.txt"),header = T,sep = ";",dec = ",")

orders <- orders %>% mutate(OrderDate = ymd_hms(orders$OrderDate),
                            RequiredDate = ymd_hms(orders$RequiredDate),
                            ShippedDate = ymd_hms(orders$ShippedDate))

# a
threea <- orders %>%
    filter(year(OrderDate) == 1997) %>% 
    select(OrderID) %>% 
    inner_join(order_details) %>% 
    inner_join(products) %>% 
    count(ProductName) %>%
    select(ProductName, n) %>% 
    top_n(1, n)
threea

# b
preke <- threea$ProductName[1]
preke

pop_category <- orders %>%
    filter(year(OrderDate) == 1998) %>% 
    inner_join(order_details) %>% 
    inner_join(products) %>% 
    inner_join(categories) %>%
    select(OrderID, CategoryName) %>%
    distinct(.keep_all = TRUE ) %>%
    count(CategoryName) %>%
    top_n(1, n) 
pop_category

top_one_in_1997_category <- products %>%
    inner_join(categories) %>% 
    filter(ProductName == preke) %>%
    select(CategoryName)

pop_category$CategoryName[1] == top_one_in_1997_category$CategoryName[1]
# Ats: nera

# c
orders %>% 
    mutate(metai = year(OrderDate)) %>%
    count(metai, EmployeeID) %>%    
    arrange(desc(n)) %>% 
    group_by(metai) %>%
    slice(seq_len(1)) %>% 
    select(-n) %>% 
    inner_join(
        orders %>% 
            mutate(metai = year(OrderDate)) %>%
            select(metai, EmployeeID, CustomerID) %>%
            distinct() %>%
            count(metai, EmployeeID) %>%
            rename(klientuSk = n),
            by = c("metai", "EmployeeID")
    ) %>% 
    inner_join(
        orders %>% 
            mutate(metai = year(OrderDate)) %>%
            select(metai, EmployeeID, ShipCountry) %>%
            count(metai, EmployeeID, ShipCountry) %>%
            group_by(metai, EmployeeID) %>% 
            filter(n == max(n)) %>% 
            mutate(topSalis = paste0(ShipCountry, collapse = ",")) %>% 
            select(metai, EmployeeID, topSalis),
    ) %>% 
    rename(topDarb = EmployeeID)  %>% 
    select(metai, topDarb, klientuSk, topSalis)



