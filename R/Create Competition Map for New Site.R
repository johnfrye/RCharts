
# Environment -------------------------------------------------------------

rm(list=ls())
pacman::p_load(stats, devtools, purrr, httr, readxl, openxlsx,
               writexl, tidyr, feather, sp, ggmap, labelled, 
               dplyr, leaflet,
               WorksheetFunctions, smutilities)

setoptions()
wd <- here::here()
ad <- here::here("analysis")
dd <- here::here("data")
gd <- here::here("graphics")
fd <- here::here("feather")
compdir <- '~/TDLinx_Processing/data'

ses.info <- sessionInfo()
time <- time_stamp()
pryr::mem_used()

# load results list -----------------------------------------------------

res_name <- "104_Manteca.RDS"
res_path <- fp(dd, res_name)
res <- readRDS(res_path)
names(res)

# Load Data ---------------------------------------------------------------

files <- fileTable(compdir, search.pattern = "TDLinx_Data") %>% 
  mutate(fn2 = gsub("TDLinx_Data", "", file_name),
         fn2 = gsub(".rds", "", fn2),
         fn3 = str_sub(fn2, 1, -6),
         fn3 = gsub("_", "/", fn3, fixed = TRUE),
         file_created = as.Date(fn3)) %>% 
  select(-fn2, -fn3) %>% 
  arrange(desc(file_created))

res_comp <- readRDS(files$file_path[1])
comp_geocoded <- res_comp$TDLinx_Competition_Data_Geocoded

create_complist <- function(df, bannername, storename, latitude, longitude, range_miles){
  
  # df <- comp_geocoded
  # bannername <- "Save Mart"
  # storename ="104"
  # latitude = "37.778543"
  # longitude = "-121.216972"
  # range_miles = "5"
  
  names(df) <- fixnames(df)
  
  dates <- df %>% 
    select(table_date) %>% 
    distinct() %>% 
    arrange(desc(table_date))
  
  date_sel <- max(dates$table_date)
  statuses <- unique(df$store_status)
  # [1] "O" "F" "C" "R" "D"
  
  # Code Description
  # O Open
  # C Closed
  # F Future
  # D Deleted
  # R Reclassified
  
  keep <- c("O", "F", "R")
  
  ourbanners <- c("Food Maxx", "Lucky", "Save Mart")
  
  comp <- df %>% 
    filter(table_date==date_sel, store_status %in% keep) %>% 
    select(table_date, banner = store_name, store = store_number, latitude, longitude) %>% 
    mutate(banner = gsub("Food Maxx Store", "Food Maxx", banner),
           banner = gsub("Lucky Store", "Lucky", banner),
           banner = gsub("S Mart Foods", "Save Mart", banner),
           store = ifelse(store == "", NA, store),
           banner = ifelse(banner %in% ourbanners, paste(banner, store, sep = " "), banner)) %>% 
    arrange(banner) %>% 
    select(banner, latitude, longitude)
  
  dest <- dff(ourbanner = paste(bannername, storename, sep = " "),
              dest_lat = latitude,
              dest_long = longitude) %>% 
    mutate(latitude = as.numeric(latitude),
           longitude = as.numeric(longitude),
           match = 1)
  
  compstores <- comp %>% 
    select(banner, comp_lat=latitude, comp_long=longitude) %>% 
    mutate(match = 1)
  
  df_join <- inner_join(dest, compstores, by="match")
  
  df_join2 <- df_join %>% 
    select(ourbanner, banner, dest_lat, 
           dest_long, comp_lat, comp_long) %>% 
    mutate(ind = seq(1, nrow(df_join), 1),
           dest_lat = as.numeric(dest_lat),
           dest_long = as.numeric(dest_long)) %>% 
    select(competitor=banner, banner=ourbanner, everything()) %>% 
    select(banner, competitor, everything())
  
  compscoords <- select(df_join2, comp_long, comp_lat)
  compscoords <- as.matrix(compscoords)
  
  destcoords <- select(df_join2, dest_long, dest_lat)
  destcoords <- destcoords[1, ]
  destcoords <- as.matrix(destcoords)
  
  km <- spDistsN1(pts=compscoords, pt=destcoords, longlat=TRUE)
  mi <- km*0.621371
  df_join2$straight_miles <- mi
  df2 <- select(df_join2, -ind)
  comp_alldist <- df2 %>% 
    filter(straight_miles <= as.numeric(range_miles)) %>% 
    select(store = banner, competitor, dest_lat, dest_long,
           comp_lat, comp_long, straight_miles) %>% 
    arrange(straight_miles, competitor)
  comp_alldist
}

complist <- create_complist(df = comp_geocoded, 
                            bannername <- "Save Mart",
                            storename ="104", 
                            latitude = "37.778543",
                            longitude = "-121.216972",
                            range_miles = "5")
# View(complist)

# Map ---------------------------------------------------------------------

target_store <- complist %>% 
  select(banner = store, lon = dest_long, lat = dest_lat) %>% 
  distinct() %>% 
  mutate(straight_miles = 0,
         color = "darkblue")
# View(target_store)

complist2 <- complist %>% 
  select(banner = competitor, lon = comp_long, lat = comp_lat, straight_miles) %>% 
  distinct() %>% 
  mutate(color = "red") %>%
  rbind(target_store) %>%
  mutate(popup_info = paste0("<b>", banner, "</b>"))
# View(complist2)

# sbbox <- complist2 %>% 
#   make_bbox(lon = long, lat = lat, f = .1)

# Zoom levels go from 3 (world scale to 20 (house scale)).
ll_means <- sapply(complist2[2:3], mean)
sq_map <- get_map(location = ll_means, maptype = "satellite", source = "google", zoom = 12)

ggmap(sq_map) +
  geom_point(data = complist2, mapping = aes(x = lon, y = lat), 
             color = "tomato",
             size = 1) +
  geom_text(data = complist2, aes(label = paste("  ", as.character(complist2$banner), sep="")), 
            angle = 60, hjust = 0, color = "yellow")

# Leaflet -----------------------------------------------------------------

leaflet(complist2) %>%
  addTiles() %>% 
  setView(lng = as.numeric(ll_means[1]), lat = as.numeric(ll_means[2]), zoom = 12) %>%
  addProviderTiles("OpenStreetMap") %>% 
  addCircleMarkers(lng = ~lon,
                   lat = ~lat,
                   radius = 5,
                   fillColor = complist2$color,
                   # color = "red",
                   stroke = FALSE,
                   fillOpacity = 0.5,
                   popup = ~popup_info)
