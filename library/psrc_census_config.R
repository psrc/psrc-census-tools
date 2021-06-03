
psrc.county <- c("King County","Kitsap County","Pierce County","Snohomish County")

# Spatial Layers ----------------------------------------------------------
geodatabase.server <- "AWS-PROD-SQL\\Sockeye"
geodatabase.name <- "ElmerGeo"
gdb.nm <- paste0("MSSQL:server=",geodatabase.server,";database=",geodatabase.name,";trusted_connection=yes")
spn <- 2285
wgs84 <- 4326
tract_layer<-"dbo.tract2010_nowater"

st<-"53"
state.name="Washington"
