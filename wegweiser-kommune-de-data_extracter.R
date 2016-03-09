##############################
#
# Data fetcher for german municipal data on wegweiser-kommune.de
#
# Todos
#
##############################

require(gdata)
require(dplyr)
require(magrittr)
require(tidyr)

bertelsmann_city_data = function(dest_path,
                                 cities,
                                 years_range,
                                 features,
                                 projection,
                                 save2disk=FALSE){
  base_url = "http://www.wegweiser-kommune.de/statistik/kommunale-daten"
  url_tail = "tabelle.xls"

  dest_path = "/media/neumaand/Exchange_local/AXA/Daten/Unsorted/wegweiser-daten/"
  if (!dir.exists(dest_path)) dir.create(dest_path)

  # Transform cities to proper URL format
  cities = cities %>% tolower %>%
    gsub(pattern="ü", replacement="ue") %>%
    gsub(pattern="ö", replacement="oe") %>%
    gsub(pattern="ä", replacement="ae") %>%
    gsub(pattern=" ", replacement="-")

  data_collection = data.frame()

  # Create URLs and output filename
  for (i in 1:length(features)){
    url = paste(base_url,
                cities[1],
                features[i],
                years_range,
                projection,
                paste(cities[2:length(cities)],collapse="+"),
                url_tail,
                sep="+")

    filename = paste(dest_path,
                     paste(features[i],paste(cities,collapse="+"),sep="_"),
                     paste(".xls",sep=""),
                     sep="")

    # Download table as .xls
    download.file(url, filename, quiet = TRUE)

    # Do filetype-dependant cleanup
    data = read.xlsx(filename, sheetIndex = 1, startRow = 13) %>% tbl_df
    tryCatch({data = data %>% gather(city_year, value, -Indikatoren) %>%
      mutate(city_year = gsub(x = city_year, pattern = "\\.(?=[^0-9])", replacement = " ", perl = T)) %>%
      separate(city_year, into = c("Stadt", "Jahr"), sep="\\.") %>%
      mutate(value = gsub(x = value, pattern="k.A.", replacement = NA),
             Indikatoren = gsub(x = Indikatoren, pattern = " ", replacement=".")) %>%
      filter(Indikatoren!="", !grepl("Quelle:", Indikatoren), !grepl("k.A.", Indikatoren))},
      error = function(e) {print("No data for city")},
      finally = {})

    # rbind to data_collection
    data_collection = rbind(data_collection, data)

    # Remove xls and save to csv instead
    file.remove(filename)
    if (save2disk==TRUE){
      write.csv(x = data, file = gsub(filename,pattern = ".xls",replacement = ".csv"))
    }
  }

  if (save2disk==TRUE){
    write.csv(x = data_collection, file = paste(dest_path, "collection_", paste(cities, collapse = "+"), ".csv", sep=""))}

  return(data_collection %>% arrange(Jahr))
}
