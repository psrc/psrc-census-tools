# Download ACS Table ----------------------------------------------------
psrc_acs_table<-function(tbl_code, geog, year, acs){
  
  # the api only wants the first part of the table code: for example B02001 from B02001_005
  tbl_prefix <- str_extract(tbl_code, "\\w+(?=_)")
  
  # to get regional values, you have to first get the county level values
  # in the api call for the region, get all counties
  ifelse(geog == 'region', geog_temp <- 'county', geog_temp <- geog)

  data.tbl <- get_acs(geography = geog_temp, state=st, year=year, survey = acs, table = tbl_prefix) %>%
              mutate(NAME = str_extract(NAME, ".*(?=,)")) %>%
              filter(variable == tbl_code) %>%
              mutate(ACS_Year=year, ACS_Type=acs, ACS_Geography=geog)
  
  variable.labels <- load_variables(year, acs, cache = TRUE) %>% rename(variable = name)
  
  data.tbl  <- left_join(data.tbl, variable.labels, by = c("variable")) 

  # for the region and the county, you need to get county level data. for the region you aggregate the counties.
  # make this into a case statement
  if(geog == 'county'| geog == 'region'){
    
    data.tbl <- data.tbl %>%
      filter(NAME %in% psrc.county) 

     if(geog == 'region') {
       
       data.tbl <- data.tbl %>%
         group_by(variable, ACS_Year, ACS_Type, label) %>% 
         summarise(total_region = sum(estimate), 
                   moe_region = moe_sum(moe, estimate)) %>% 
         relocate(ends_with('region'), .after = variable)
       
     }
       
  } else if(geog == 'tract'){
    
    county_or <- paste(psrc.county, collapse='|')
    data.tbl <- data.tbl %>%
      filter(str_detect(NAME, county_or))
      
  } else{
    
    print('Only county, region, or tract geog are supported currently')
  }
  
  data.tbl <- data.tbl %>% filter(variable == tbl_code)
  print(data.tbl)
  return(data.tbl)
  
}

create_tract_map<-function(tract_tbl){
  # Create Map --------------------------------------------------------------
  tract.lyr <- st_read(gdb.nm, tract_layer, crs = spn)
  
  tbl <- tract_tbl %>%
    select(GEOID,estimate) %>%
    mutate(across(everything(), .fns = ~replace_na(.,0))) %>%
    mutate(across(c('GEOID'), as.character))%>%
    group_by(GEOID) %>%
    summarise(Total=sum(estimate))

  c.layer <- left_join(tract.lyr,tbl, by = c("geoid10"="GEOID")) %>%
    st_transform(wgs84)
  
  rng <- range(c.layer$Total)
  max_bin <- max(abs(rng))
  round_to <- 10^floor(log10(max_bin))
  max_bin <- ceiling(max_bin/round_to)*round_to
  breaks <- (sqrt(max_bin)*c(0.1, 0.2,0.4, 0.6, 0.8, 1))^2
  bins <- c(0, breaks)
  
  pal <- colorBin("YlOrRd", domain = c.layer$TotalP, bins = bins)
  
  labels <- paste0("Census Tract ", c.layer$geoidstr, '<p></p>', 
                   'Total: ', prettyNum(round(c.layer$Total, -1), big.mark = ",")) %>% lapply(htmltools::HTML)
  
  m <- leaflet() %>%
    addMapPane(name = "polygons", zIndex = 410) %>% 
    addMapPane(name = "maplabels", zIndex = 500) %>% # higher zIndex rendered on top
    addProviderTiles("CartoDB.VoyagerNoLabels") %>%
    addProviderTiles("CartoDB.VoyagerOnlyLabels", 
                     options = leafletOptions(pane = "maplabels"),
                     group = "map labels") %>%
    addEasyButton(easyButton(
      icon="fa-globe", title="Region",
      onClick=JS("function(btn, map){map.setView([47.615,-122.257],8.5); }"))) %>%
    addPolygons(data=c.layer,
                fillOpacity = 0.7,
                fillColor = pal(c.layer$Total),
                opacity = 0.7,
                weight = 0.7,
                color = "#BCBEC0",
                group="population",
                options = leafletOptions(pane = "polygons"),
                dashArray = "",
                highlight = highlightOptions(
                  weight =5,
                  color = "76787A",
                  dashArray ="",
                  fillOpacity = 0.7,
                  bringToFront = TRUE),
                label = labels,
                labelOptions = labelOptions(
                  style = list("font-weight" = "normal", padding = "3px 8px"),
                  textsize = "15px",
                  direction = "auto")) %>%
    addLegend(pal = pal,
              values = c.layer$estimate,
              position = "bottomright") %>%
    addLayersControl(baseGroups = "CartoDB.VoyagerNoLabels",
                     overlayGroups = c("map labels",
                                       "population")) %>%
    setView(lng=-122.257, lat=47.615, zoom=8.5)
  
  return(m)
  
}
  
  