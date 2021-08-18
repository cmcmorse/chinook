setwd("~/Desktop/School/R stuff/r_sql")
library(RSQLite)
library(DBI)

run_query <- function(q) {
  conn <- dbConnect(SQLite(), 'chinook.db')
  result <- dbGetQuery(conn, q)
  dbDisconnect(conn)
  return(result)
}

show_tables <- function() {
  q = "SELECT name, type FROM sqlite_master WHERE type IN ('table','view')"
  return(run_query(q))
}

show_tables()

# want to see which genres sell the most tracks in the USA
query <- "WITH purchased_genres AS
          (
          SELECT il.invoice_line_id, g.name
          FROM invoice_line AS il 
          LEFT JOIN track AS t ON il.track_id = t.track_id
          LEFT JOIN genre AS g ON t.genre_id = g.genre_id
          LEFT JOIN invoice AS i ON i.invoice_id = il.invoice_id
          WHERE billing_country = 'USA'
          ),
          genres_with_pct AS
          (
          SELECT 
            name, 
            COUNT(*) AS num_purchases
          FROM purchased_genres
          GROUP BY name
          ORDER BY 2 DESC
          )
          SELECT 
            name, 
            num_purchases,
            ROUND(CAST(num_purchases AS FLOAT) / 
              (SELECT COUNT(*)
               FROM purchased_genres)*100) AS pct
          FROM genres_with_pct
          GROUP BY name
          ORDER BY 2 DESC
          LIMIT 10;"

top_10_g <- run_query(query)
top_10_g
# Want to choose 3 out of the 4 options for a new album.
# Options are: Hip-Hop, Punk, Pop, and Blues.
# All options except Hip-Hop are present in the top-10 genres list.
g_opts <- c('Hip-Hop','Alternative & Punk','Pop','Blues')
g_opts[g_opts %in% top_10_g$name]
# The three genres to choose should be: Punk, Pop, and Blues.

library(tidyverse)
library(ggplot2)

top_10_g %>% ggplot(aes(x=name, y=num_purchases)) + 
  geom_bar(stat='identity')

# Want total dollars sold per employee
query <- "SELECT
            e.first_name || ' ' || e.last_name AS employee_name,
            e.title,
            e.birthdate,
            e.hire_date,
            SUM(i.total) AS total_sold
          FROM employee AS e
          LEFT JOIN customer AS c ON e.employee_id = c.support_rep_id
          LEFT JOIN invoice AS i ON c.customer_id = i.customer_id
          GROUP BY e.employee_id
          ORDER BY 2 DESC;"
employee_sales <- run_query(query)
employee_sales
# The employee hired most recently has sold the least.
# Only sales support agents have made sales.

# Analyze sales data from customers from each country
# Want: total num customers, total sales, avg sale per customer, and avg order value per country.
query <- "WITH country_sales AS
          (
          SELECT 
            c.country,
            COUNT(DISTINCT(c.customer_id)) AS num_customers,
            COUNT(DISTINCT(i.invoice_id)) AS total_orders,
            SUM(i.total) AS total_sales
          FROM customer AS c
          LEFT JOIN invoice AS i ON i.customer_id = c.customer_id
          GROUP BY c.country
          HAVING num_customers > 1
          ORDER BY 2 DESC
          )
          SELECT
            country,
            num_customers,
            total_sales,
            ROUND(total_sales / num_customers, 2) AS avg_sale_per_customer,
            ROUND(total_sales / total_orders, 2) AS avg_sale_per_order
          FROM country_sales"
sales_by_country <- run_query(query)
sales_by_country

highest_sales_per_customer <- sales_by_country %>%
  filter(avg_sale_per_customer >= 0.6*max(avg_sale_per_customer)) %>%
  arrange(-avg_sale_per_customer)
highest_sales_per_customer

highest_sales_per_order <- sales_by_country %>%
  filter(avg_sale_per_order >= 0.9*max(avg_sale_per_order)) %>%
  arrange(-avg_sale_per_order)
highest_sales_per_order

highest_sales_per_customer$country[highest_sales_per_customer$country %in% highest_sales_per_order$country]
# Both the Czech Republic and India have some of the highest
# sales per customer and highest sales per order. These countries
# may benefit from additional marketing.

# countries with only 1 customer:
query_lonely <- "SELECT 
                  c.country,
                  COUNT(DISTINCT(i.invoice_id)) AS total_orders,
                  SUM(i.total) AS total_sales
                FROM customer AS c
                LEFT JOIN invoice AS i ON i.customer_id = c.customer_id
                GROUP BY c.country
                HAVING COUNT(DISTINCT(c.customer_id)) = 1;"
lonely_countries <- run_query(query_lonely)
lonely_countries

others <- lonely_countries %>%
  mutate(country = "other",
         num_customers = nrow(lonely_countries),
         total_orders = sum(total_orders),
         total_sales = sum(total_sales),
         avg_sale_per_customer = total_sales/num_customers,
         avg_sale_per_order = total_sales/total_orders) %>%
  select(country, num_customers, total_sales, avg_sale_per_customer, avg_sale_per_order)
others <- others[1,]

countries <- rbind(sales_by_country, others)
countries <- countries %>% arrange(-total_sales)
countries

countries %>% ggplot(aes(x=reorder(country, -avg_sale_per_customer),
                         y=avg_sale_per_customer)) + 
  geom_bar(stat='identity', fill='blue') + 
  theme(axis.text.x = element_text(angle=90)) + 
  ylab('Sales') + 
  xlab('Country') + 
  ggtitle('Sales per Customer')

countries %>% ggplot(aes(x=reorder(country, -avg_sale_per_order),
                         y=avg_sale_per_order)) + 
  geom_point(color='orange') + 
  theme(axis.text.x = element_text(angle=90)) + 
  scale_y_continuous(breaks=c(7, 9), limits = c(6, 10)) + 
  ylab('Sales') + 
  xlab('Country') + 
  ggtitle('Sales per Order')

# Czech Republic and India are clearly seen as top sales in both categories.


# Which artist is used the most in playlists?
query <- "SELECT a.name, COUNT(DISTINCT(pt.track_id)) AS num_tracks
          FROM playlist_track AS pt
          LEFT JOIN track AS t ON pt.track_id = t.track_id
          LEFT JOIN album AS al ON t.album_id = al.album_id
          LEFT JOIN artist AS a ON al.artist_id = a.artist_id
          GROUP BY a.name
          ORDER BY 2 DESC
          LIMIT 10;"

top_playlist_artists <- run_query(query)
top_playlist_artists

# How many tracks have been purchased? not purchased?
query <- "WITH purch_info AS
          (
          SELECT
            t.track_id,
            il.quantity
          FROM track AS t
          LEFT JOIN invoice_line AS il ON il.track_id = t.track_id
          )
          SELECT 
            COUNT(quantity) AS purchased,
            COUNT(quantity IS NULL) AS not_purchased
          FROM purch_info;"
run_query(query)
# 4757 tracks were purchased compared to 
# 6454 tracks not purchased.

# Do "protected" vs "non-protected" media types have 
# an effect on popularity?

query <- "WITH invoice_to_media_type AS (
          SELECT 
            il.invoice_line_id,
            il.track_id,
            m.name AS media_type_name
          FROM invoice_line AS il
          LEFT JOIN track AS t ON il.track_id = t.track_id
          LEFT JOIN media_type AS m ON t.media_type_id = m.media_type_id
          )
          SELECT 
            media_type_name, 
            COUNT(*) AS num_purchases
          FROM invoice_to_media_type
          GROUP BY 1
          ORDER BY 2 DESC;"

media_types <- run_query(query)
media_types <- media_types %>%
  mutate(type = ifelse(grepl('^Protected', media_type_name),
                "Protected",
                "Not protected")) %>%
  select(type, num_purchases) %>%
  group_by(type) %>%
  summarise(num_purchases = sum(num_purchases))
media_types
# Protected files (442 sold) seem to sell 
# much less than non-protected (4315 sold).




