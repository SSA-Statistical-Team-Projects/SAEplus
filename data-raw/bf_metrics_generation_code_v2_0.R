

rm(list=ls())

library(sf)         # version 0.8-1
library(rgdal)      # version 1.4-8
library(rgeos)      # version 0.5-2
library(data.table) # version 1.12.2
library(curl)       # version 3.3
library(raster)     # version 3.0-7


##############################
##    USER SPECIFICATIONS   ##
##############################

# edit file paths

# input building footprints data directory
input_filepath <- 'ZZ/DigitizeAfrica_building_footprints/A_Year2'  

# output directory for intermediary datatable files 
dt_filepath <-  'ZZZ/datatables'     

# output directory for final metric layers 
layers_filepath <- 'ZZZZ/output'       

# directory for temporary master grid file (note, all temp files will be automatically deleted)
tempfile_filepath <- 'ZZZZZ'          

# for South Africa (ZAF) use a clipped raster because there is a huge area of sea between mainland and Prince Edward Island which makes processing incredibly slow 
# specify file location of a suitable ZAF raster
ZAF_raster <- 'ZZZZZZ/ZAF.tif'

# version number (as character string) for metric layers
version_number <- 'v2_0'

# do you want to specify countries to process (if no then the code will process all the countries that do not have existing metric layers in the 'layers_filepath' directory)
specify_countries <- 'n'  # 'y'
# if specify_countries = y then edit this list to include the three character country iso codes for the countries you'd like to process
selected_countries <- 'NA' # e.g. c('DJI', 'ZMB')


##############################
##    CHECK COUNTRY LIST    ##
##############################

# run this section to create list of country gdb files to process
if(specify_countries == 'y'){
  
  full <- list.dirs(path=paste(input_filepath),full.names=FALSE,recursive=TRUE) 
  gdbfolders <- full[grep(".gdb",full)]
  gdbfolders <- gdbfolders[substring(gdbfolders,1,3) %in% selected_countries]
  
}else{
  
  existingfiles <- list.files(paste(layers_filepath))
  existingcountries <- unique(substring(existingfiles,1,3))
  full <- list.dirs(path=paste(input_filepath),full.names=FALSE,recursive=TRUE) 
  gdbfolders <- full[grep(".gdb",full)]
  gdbfolders <- gdbfolders[!substring(gdbfolders,1,3) %in% existingcountries]
  
}
country_list <- substring(gdbfolders,1,3)
print(country_list)

# Does printed list include the correct countries? 
# If not:
# 1. Check you have entered the correct details for specify_countries and selected_countries 
# 2. Go to bf directory and check zip files are unzipped, 
# 3. Go to bf directory and check gdb files exist for the countries you'd like to process 

# If you're happy with the list of countries, run the following code

# If you know that the datatable for your focal country/countries already exist, skip 'RUN CODE TO CREATE DATATABLES FOR BFs'


#################################################
##    RUN CODE TO CREATE DATATABLES FOR BFs    ##
#################################################

for(countryit in 1:length(gdbfolders)){
  
  # save country iso code
  ISO_code <- substring(gdbfolders[countryit],1,3)
  
  # list all feature classes in the file geodatabase
  fgdb <- gdbfolders[countryit]
  fgdb <- paste(input_filepath, '/', fgdb, sep='')
  fc_list <- ogrListLayers(fgdb)
  if(length(grep("deleted",fc_list)) > 0){
    fc_list <- fc_list[-grep("deleted",fc_list)]
  }
  
  # create centroids for each of the feature classes
  
  # master grid
  if(!ISO_code == 'ZAF'){
    urlx <- paste("ftp://ftp.worldpop.org/GIS/Mastergrid/Global_2000_2020/",ISO_code,"/L0/",tolower(ISO_code),"_level0_100m_2000_2020.tif",sep='')
    utils::download.file(url = urlx,
                         destfile = paste(tempfile_filepath,"/temp.tif",sep=''),
                         mode="wb",
                         quiet=FALSE,
                         method="libcurl")
  }
  
  for(i in 1:length(fc_list)){
    
    print(fc_list[i])
    centroids_wgs_master <- list()
    bf <- read_sf(dsn=fgdb,layer=paste(fc_list[i]))
    centroids_utm <- st_centroid(bf)
    rm(bf)
    centroids_wgs_master <- st_transform(centroids_utm, crs ="+proj=longlat +datum=WGS84") # reproject
    rm(centroids_utm)
    
    if(ISO_code == 'ZAF'){
      mgrid <- raster(paste(ZAF_raster))
    }else{
      mgrid <- raster(paste(tempfile_filepath,"/temp.tif",sep=''))
    }
    
    mgrid[] <- NA
    
    # get cellID for each building
    points_IDs <- cellFromXY(mgrid, as(centroids_wgs_master, "Spatial"))
    
    rm(mgrid)
    
    for(bf_char in c('Shape_Area','Shape_Length','ImgDate')){   # these character strings match the col headings in Maxar/Ecopia data
      
      dt <- data.table(cellID = points_IDs, centroids_wgs_master[,bf_char])
      dt <- na.omit(dt)
      
      dt <- dt[,1:2]
      names(dt)[2] <- 'bchar'
      dt <- na.omit(dt)
      
      if(bf_char == 'ImgDate'){
        dt$bchar <- substring(dt$bchar,1,4)
      }
      
      saveRDS(dt,paste(dt_filepath,"/",ISO_code,"_dt_",bf_char,"_",fc_list[i],".rds",sep=''))
      
      rm(dt)
    }
    rm(points_IDs,centroids_wgs_master)
  }
  
  unlink(paste(tempfile_filepath,"/temp.tif",sep=''))
}


###############################################
##    RUN CODE TO CREATE BF METRIC LAYERS    ##
###############################################

for(countryit in 1:length(country_list)){
  
  # save country iso code
  ISO_code <- country_list[countryit]
  
  full <- list.files(path=paste(dt_filepath),full.names=FALSE) 
  full <- full[substring(full,1,3) == ISO_code]
  
  if(!ISO_code == 'ZAF'){
    urlx <- paste("ftp://ftp.worldpop.org/GIS/Mastergrid/Global_2000_2020/",ISO_code,"/L0/",tolower(ISO_code),"_level0_100m_2000_2020.tif",sep='')
    utils::download.file(url = urlx,
                         destfile = paste(tempfile_filepath,"/temp.tif",sep=''),
                         mode="wb",
                         quiet=FALSE,
                         method="libcurl")
  }
  
  if(ISO_code == 'ZAF'){
    mgrid <- raster(paste(ZAF_raster))
  }else{
    mgrid <- raster(paste(tempfile_filepath,"/temp.tif",sep=''))
  }
  mgrid[] <- NA
  
  
  for(bf_char in c('Shape_Area','Shape_Length')){
    
    subrds <- full[grep(paste(bf_char),full)]
    subrds <- paste(dt_filepath,'/',subrds,sep='')
    
    dt <- data.table()
    for(dtfile in subrds){
      print(dtfile)
      dt <- rbind(dt, readRDS(dtfile))
    }
    
    # sum building areas for each cellID
    tba <- dt[,.(totarea = sum(bchar)),by=cellID]
    
    tbar <- mgrid
    tbar[tba$cellID] <- tba$totarea
    if(bf_char == 'Shape_Area'){
      writeRaster(tbar,filename = paste(layers_filepath,'/',ISO_code,'_buildings_',version_number,'_total_area.tif',sep=''))
    }
    if(bf_char == 'Shape_Length'){
      writeRaster(tbar,filename = paste(layers_filepath,'/',ISO_code,'_buildings_',version_number,'_total_length.tif',sep=''))
    }
    rm(tbar,tba)
    
    # mean building areas for each cellID
    mba <- dt[,.(meanarea = mean(bchar)),by=cellID]
    
    mbar <- mgrid
    mbar[mba$cellID] <- mba$meanarea
    if(bf_char == 'Shape_Area'){
      writeRaster(mbar,filename = paste(layers_filepath,'/',ISO_code,'_buildings_',version_number,'_mean_area.tif',sep=''))
    }
    if(bf_char == 'Shape_Length'){
      writeRaster(mbar,filename = paste(layers_filepath,'/',ISO_code,'_buildings_',version_number,'_mean_length.tif',sep=''))
    }
    rm(mbar,mba)
    
    
    # cv building areas for each cellID
    cvba <- dt[,.(cvarea = sd(bchar)/mean(bchar)),by=cellID]
    cvba$cvarea[is.na(cvba$cvarea)] <- 0
    
    cvbar <- mgrid
    cvbar[cvba$cellID] <- cvba$cvarea
    if(bf_char == 'Shape_Area'){
      writeRaster(cvbar,filename = paste(layers_filepath,'/',ISO_code,'_buildings_',version_number,'_cv_area.tif',sep=''))
    }
    if(bf_char == 'Shape_Length'){
      writeRaster(cvbar,filename = paste(layers_filepath,'/',ISO_code,'_buildings_',version_number,'_cv_length.tif',sep=''))
    }
    rm(cvbar,cvba)
    
    
    if(bf_char == 'Shape_Area'){  # just want to do this section once - the fact that it's area over length is irrelevant
      
      # sum buildings for each cellID
      bc <- dt[,.N,by=cellID]
      
      bcr <- mgrid
      bcr[bc$cellID] <- bc$N
      
      # create px area raster
      pxarea <- area(mgrid)
      
      # create density raster: bc/px area
      bdensr <- bcr/pxarea
      
      rm(pxarea, bc)
      
      writeRaster(bcr,filename = paste(layers_filepath,'/',ISO_code,'_buildings_',version_number,'_count.tif',sep=''))
      writeRaster(bdensr,filename = paste(layers_filepath,'/',ISO_code,'_buildings_',version_number,'_density.tif',sep=''))
      
      rm(bdensr)
      
      # identify 'clumps' 
      clumps <- clump(bcr, directions=8)
      
      # data table for cells
      clumpdt <- data.table(clump_ID = clumps[],
                            bfcount = bcr[])
      clumpdt <- clumpdt[!is.na(clump_ID),]
      
      # cell count per clump
      cell_count_dt <- clumpdt[,.N,by=clump_ID]
      
      # bf count per clump
      bfcount_dt <- clumpdt[,.(sumcount=sum(bfcount)),by=clump_ID]
      
      # clumps table
      clump_sum_dt <- merge(cell_count_dt, bfcount_dt, by='clump_ID')
      
      # urban threshold
      big_clumps <- clump_sum_dt[N >= 1500 & sumcount >= 5000,]
      
      # ids for urban and rural
      sl <- clumps
      sl[!is.na(clumps)] <- 0
      sl[clumps %in% big_clumps$clump_ID] <- 1
      
      # urban raster (urban=1, rural=0, unsettled=NA)
      raster::writeRaster(sl, filename=paste(layers_filepath,'/',ISO_code,'_buildings_',version_number,'_urban.tif',sep=''), datatype='LOG1S')
      
      rm(clumps, clumpdt, clump_sum_dt, bfcount_dt, big_clumps, sl, cell_count_dt, bcr)
      
    }
    
    rm(dt)
    
  }
  
  # imagery year raster
  
  bf_char <- 'ImgDate'
  
  subrds <- full[grep(paste(bf_char),full)]
  subrds <- paste(dt_filepath,'/',subrds,sep='')
  
  dt <- data.table()
  for(dtfile in subrds){
    print(dtfile)
    dt <- rbind(dt, readRDS(dtfile))
  } 
  
  iy <- dt[,.(imyr = modal(bchar,ties='highest')),by=cellID]
  iy$imyr <- as.numeric(iy$imyr)
  
  iyr <- mgrid
  iyr[iy$cellID] <- iy$imyr
  writeRaster(iyr,filename = paste(layers_filepath,'/',ISO_code,'_buildings_',version_number,'_imagery_year.tif',sep=''))
  
  rm(dt,iy,iyr)
  
  if(!ISO_code == 'ZAF'){
    unlink(paste(tempfile_filepath,"/temp.tif",sep=''))
  }
  
  rm(mgrid)
}







