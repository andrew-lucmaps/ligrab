# ligrab_utilities





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# folders and files
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


coderoot = "D:/LUCA Team/Land Use Capability Assessments Limited/LUCA Directors - Documents/Code/RCode/ligrab/"

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# settings
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# A LIGrabber RGRASS Function: "ligrg.readraster" - reads a raster input file and creates a region from it
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

ligrg.readraster = function(ff.in, obj.out, flags =c('e', 'overwrite')){
  
  execGRASS('r.in.gdal',
            input = ff.in,
            output = obj.out,
            flags = flags)
  
  # create a region from the imported file  
  execGRASS('g.region', raster = obj.out)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# A LIGrabber RGRASS Function: "ligrg.resample" - resamples a raster 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

ligrg.resample = function(obj.in, obj.out, RES, flags_region ='a', flags_resamp = 'overwrite', method = 'median'){
  
  if (!is.character(RES)){RES=as.character(RES)}  
  
  
  # make the resolution of the imported refion be 5 m
  execGRASS('g.region', res = RES, flags = flags_region)
  
  # r.resamp.stats: Resamples raster map layers to a coarser grid using aggregation
  
  #resample 1m DEM to 5m  
  execGRASS('r.resamp.stats',
            input = obj.in,
            output =obj.out,
            method = method,
            flags = flags_resamp)
  
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# A LIGrabber RGRASS Function: "ligrg.saveraster" - takes a raster object and creates a file
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~