library(tidyverse)
library(lubridate)

bike_orderlines_tbl <- read_rds("00_data/01_bike_sales/02_wrangled_data/bike_orderlines.rds")

# 1.0 Anatomy of a ggplot ----

# 1.1 How ggplot works ----

# Step 1: Format data ----

sales_by_year_tbl <- bike_orderlines_tbl %>%
  
  # Selecting columns to focus on and adding a year column
  select(order_date, total_price) %>%
  mutate(year = year(order_date)) %>%
  
  # Grouping by year, and summarizing sales
  group_by(year) %>%
  summarize(sales = sum(total_price)) %>%
  ungroup() %>%
  
  # € Format Text
  mutate(sales_text = scales::dollar(sales, 
                                     big.mark     = ".", 
                                     decimal.mark = ",", 
                                     prefix       = "", 
                                     suffix       = " €"))

sales_by_year_tbl

# Step 2: Plot ----

sales_by_year_tbl %>%
  
  # Canvas
  ggplot(aes(x = year, y = sales, color = sales)) +
  
  # Geometries 
  geom_line(size = 1) +
  geom_point(size = 5) +
  geom_smooth(method = "lm", se = FALSE)

# Data Manipulation
order_value_tbl <- bike_orderlines_tbl %>%
  
  select(order_id, order_line, total_price, quantity) %>%
  
  group_by(order_id) %>%
  summarize(
    total_quantity = sum(quantity),
    total_price    = sum(total_price)
  ) %>%
  ungroup()

# Scatter Plot
order_value_tbl %>%
  
  ggplot(aes(x = total_quantity, y = total_price)) +
  
  geom_point(alpha = 0.5, size = 2) +
  geom_smooth(method = "lm", se = FALSE)

# Data Manipulation
revenue_by_month_tbl <- bike_orderlines_tbl %>%
  
  select(order_date, total_price) %>%
  
  mutate(year_month = floor_date(order_date, "months") %>% ymd()) %>%
  
  group_by(year_month) %>%
  summarize(revenue = sum(total_price)) %>%
  ungroup()

# Line Plot
revenue_by_month_tbl %>%
  
  ggplot(aes(year_month, revenue)) +
  
  geom_line(size = 0.5, linetype = 1) +
  geom_smooth(method = "loess", span = 0.2)

# Data Manipulation
revenue_by_category_2_tbl <- bike_orderlines_tbl %>%
  
  select(category_2, total_price) %>%
  
  group_by(category_2) %>%
  summarize(revenue = sum(total_price)) %>%
  ungroup()

# Bar Plot
revenue_by_category_2_tbl %>%
  
  mutate(category_2 = category_2 %>% as_factor() %>% fct_reorder(revenue)) %>%
  
  ggplot(aes(category_2, revenue)) +
  
  geom_col(fill = "#2c3e50") + 
  coord_flip()

# Histogram

bike_orderlines_tbl %>%
  
  distinct(model, price) %>%
  
  ggplot(aes(price)) +
  
  geom_histogram(bins = 25, fill = "blue", color = "white")

# Histogram
bike_orderlines_tbl %>%
  
  distinct(price, model, frame_material) %>%
  
  ggplot(aes(price, fill = frame_material)) +
  
  geom_histogram() +
  
  facet_wrap(~ frame_material, ncol = 1)

# Density
bike_orderlines_tbl %>%
  
  distinct(price, model, frame_material) %>%
  
  ggplot(aes(price, fill = frame_material)) +
  
  geom_density(alpha = 0.5) +
  # facet_wrap(~ frame_material, ncol = 1) +
  
  theme(legend.position = "bottom")

# Data Manipulation
unit_price_by_cat_2_tbl <- bike_orderlines_tbl %>%
  
  select(category_2, model, price) %>%
  distinct() %>%
  
  mutate(category_2 = as_factor(category_2) %>% fct_reorder(price))

# Box Plot
unit_price_by_cat_2_tbl %>%
  
  ggplot(aes(category_2, price)) +
  
  geom_boxplot() +
  coord_flip()

# Violin Plot & Jitter Plot

unit_price_by_cat_2_tbl %>%
  
  ggplot(aes(category_2, price)) +
  
  geom_jitter(width = 0.15, color = "#2c3e50") +
  geom_violin(alpha = 0.5) +
  
  coord_flip()

# Data Manipulation

revenue_by_year_tbl <- bike_orderlines_tbl %>%
  
  select(order_date, total_price) %>%
  
  mutate(year = year(order_date)) %>%
  
  group_by(year) %>%
  summarize(revenue = sum(total_price)) %>%
  ungroup()

# Adding text to bar chart
# Filtering labels to highlight a point

revenue_by_year_tbl %>%
  
  ggplot(aes(year, revenue)) +
  
  geom_col(fill = "#2c3e50") +
  geom_smooth(method = "lm", se = FALSE) +
  
  geom_text(aes(label =  scales::dollar(revenue, 
                                        scale  = 1e-6, 
                                        prefix = "",
                                        suffix = "M")), 
            vjust = 1.5, color = "white") +
  
  geom_label(label =  "Major Demand This Year",
             vjust = -0.5, 
             size  = 5,
             fill  = "#1f78b4",
             color = "white",
             fontface = "italic",
             data = revenue_by_year_tbl %>%
               filter(year %in% c(2019))) + 
  
  expand_limits(y = 2e7)

sales_by_year_tbl %>%
  
  # Canvas
  ggplot(aes(x = year, y = sales, color = sales)) +
  
  # Geometries 
  geom_line(size = 1) +
  geom_point(size = 5) +
  geom_smooth(method = "lm", se = FALSE) +
  
  # same as above, with explicit scales
  scale_x_continuous() +
  scale_y_continuous() +
  scale_colour_continuous()

sales_by_year_tbl %>%
  
  # Canvas
  ggplot(aes(x = year, y = sales, color = sales)) +
  
  # Geometries 
  geom_line(size = 1) +
  geom_point(size = 5) +
  geom_smooth(method = "lm", se = FALSE, color = "#d62dc6") +
  
  # Formatting
  expand_limits(y = 0) +
  # You can also type "red", "black" etc. for the colors
  scale_color_continuous(low    = "#95E1EA", high = "#2097A3", 
                         labels = scales::dollar_format(scale  = 1/1e6, 
                                                        prefix = "", 
                                                        suffix = "M €")) +
  scale_y_continuous(labels = scales::dollar_format(scale  = 1/1e6, 
                                                    prefix = "", 
                                                    suffix = "M €")) +
  
  labs(
    title = "Revenue",
    subtitle = "Sales are trending up and to the right!",
    x = "",
    y = "Sales (Millions)",
    color = "Rev (M €)",
    caption = "What's happening?\nSales numbers showing year-over-year growth."
  )

library(ggthemes)
## DATA PREPARATION
sales_by_month_2015 <- bike_orderlines_tbl %>% 
  
  # Selecting columns to focus on and adding a month column
  select(order_date, total_price) %>% 
  mutate(year = year(order_date)) %>% 
  mutate(month = month(order_date)) %>% 
  
  filter(year == "2015") %>% 
  
  group_by(month) %>% 
  summarize(sales = sum(total_price)) %>% 
  ungroup() %>% 
  
  # $ Format Text
  mutate(sales_text = scales::dollar(sales, big.mark = ".",
                                     decimal.mark    = ",",
                                     prefix          = "",
                                     suffix          = " €"))

## PLOTTING
# Canvas
sales_by_month_2015 %>% 
  ggplot(aes(x = month, y = sales, color = sales)) +
  
  # Geometries
  geom_line(size = 1) +
  geom_point(size = 5) +
  geom_smooth(method = "lm", se = FALSE) +
  
  # Formatting
  expand_limits(y = 0) +
  scale_color_continuous(low = "red", high = "black",
                         labels = scales::dollar_format(scale = 1/1e6,
                                                        prefix = "",
                                                        suffix = "M €")) +
  scale_x_continuous(breaks = sales_by_month_2015$month,
                     labels = month(sales_by_month_2015$month, label = T)) +
  scale_y_continuous(labels = scales::dollar_format(scale = 1/1e6,
                                                    prefix = "",
                                                    suffix = "M")) +
  labs(
    title = "Monthly sales (2015)",
    subtitle = "April is the strongest month!",
    x = "",
    y = "Sales (Millions)",
    color = "Rev (M €)",
    caption = "What'd happening?\nSales numbers are dropping towards the end of the year"
  ) +
  theme_economist() +
  theme(legend.position = "right",
        legend.direction = "vertical",
        axis.text.x = element_text(angle = 45))

# Data Manipulation

sales_by_year_category_1_tbl <- bike_orderlines_tbl %>%
  select(order_date, category_1, total_price) %>%
  
  mutate(order_date = ymd(order_date)) %>%
  mutate(year = year(order_date)) %>%
  
  group_by(category_1, year) %>%
  summarize(revenue = sum(total_price)) %>%
  ungroup() %>%
  
  # Convert character vectors to factors
  # Arrange by year and revenue
  mutate(category_1 = fct_reorder2(category_1, year, revenue))

sales_by_year_category_1_tbl

# Uncover the factor levels (just for demonstration)
# sorted by years and the highest revenues
sales_by_year_category_1_tbl %>%
  mutate(category_1_num = as.numeric(category_1)) %>%
  arrange(category_1_num)