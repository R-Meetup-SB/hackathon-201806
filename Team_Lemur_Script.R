library("magrittr")
library("ggplot2")
library("dplyr")
library("stringr")
library("leaflet")

## read in data
data_path <- "hackathon_20180621.xlsx"
sheet_names <- readxl::excel_sheets(data_path) # get sheet names
sheets <- purrr::map(sheet_names, ~ readxl::read_excel(path = data_path, sheet = .x)) # read in all sheets
sheets <- setNames(sheets, sheet_names) # set the names of the sheets

## clean data
mega_data <- dplyr::bind_rows(sheets$Chemistry, sheets$Bacteria, sheets$Nutrients) %>%
  mutate(Parameter = tolower(ParameterCode)) %>% # clean parameter code names
  mutate(Parameter = gsub("e. coil", "e. coli", Parameter, fixed = TRUE)) %>% # clean parameter code typo
  filter(!(Parameter %in% c("duplicate", "do % sat", "temp") | is.na(Parameter))) # remove NA and duplicate parameter codes

## create regulation data
reg <- data.frame(Parameter = unique(mega_data$Parameter),
                  threshold = c(" < 3000", " > 5", " < 8.5", " < 25", " < 235", " < 10000", " < 61"))

## find exceedence instances
mega_data <- left_join(mega_data, reg) %>%
  rowwise() %>%
  mutate(Exceed = eval(parse(text = paste0(Result, threshold)))) 

## get the percent exceedence
mega_data_sum <- mega_data %>%
  group_by(ParameterCode, StationID) %>%
  summarise(P_Exceed = sum(Exceed, na.rm = TRUE),
            Count = length(Exceed),
            Percent_Exceed = P_Exceed/Count * 100)

## prepare data for mapping
mega_data_map <- mega_data_sum %>%
  left_join(sheets$SiteList %>% 
              rename(StationID = `Site Code`,
                     Coord = `Geographic Coordinates (Decimal Degrees)`) %>%
              select(StationID, Coord)) %>%
  rowwise() %>%
  mutate(long = unlist(str_split(Coord, pattern = ","))[1],
         lat = unlist(str_split(Coord, pattern = ","))[2])

## Create a continuous palette function
pal <- colorNumeric(
  palette = "Reds",
  domain = mega_data_map$Percent_Exceed)

## make a map
m <- leaflet(mega_data_map %>%
               filter(ParameterCode == "Conductivity")) %>% # select parameter to map
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addCircleMarkers(lng= ~as.numeric(long), 
             lat= ~as.numeric(lat),
             fillColor = ~pal(Percent_Exceed),
             radius = 10,
             stroke = FALSE)
m  # Print the map


