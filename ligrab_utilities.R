# ligrab_utilities





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# folders and files
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


coderoot = "D:/LUCA Team/Land Use Capability Assessments Limited/LUCA Directors - Documents/Code/RCode/ligrab/"
ruleset_folder = paste0(coderoot, "rulesets/")
slopeclass_ruleset = paste0(ruleset_folder, 'GRASS_reclass_slope-sharnrules_x10.txt')




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


ligrg.saveraster = function(obj.in, ff.out,  format = 'GTiff', flags = c('c', 'overwrite'),
                            createopt = c('COMPRESS=DEFLATE','TILED=YES')){
  
  
  execGRASS('r.out.gdal', 
            input = obj.in,
            output = ff.out,
            format = format, 
            flags = flags,
            createopt =createopt)
  
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# A LIGrabber RGRASS Function: "ligrg.sagadenoise" - takes a raster object and creates a file
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

ligrg.sagadenoise = function(ff.in, ff.out, SIGMA = 0.8, ITER = 10 , ZONLY = 1){
  
  
  root = basename(ff.in)
  saga_rast_to_denoise_ffn = paste0(root, 'rast_to_denoise.sdat')
  saga_rast_denoised_ffn = paste0(root, 'rast_denoised.sdat')
  
  
  
  #open it and save it in Grass
  execGRASS('r.in.gdal',
            input = ff.in,
            output = 'rast_interim',
            flags = c('e', 'overwrite'))
  
  # list the file
  src_rast <- execGRASS('g.list', type = 'raster', pattern = '*_interim*', intern = TRUE)
  
  # create region boundaries
  execGRASS('g.region', raster = src_rast)
  
  # save as SAGA file
  execGRASS('r.out.gdal',
            input = src_rast,
            output = saga_rast_to_denoise_ffn,
            format = 'SAGA',
            flags = c('c', 'overwrite'))
  
  
  # denoise using the SAGA grid_filter command
  
  # http://www.saga-gis.org/saga_tool_doc/2.2.4/grid_filter_10.html
  # this seems to be way faster than in the GUI...
  system2('saga_cmd', args = c(
    'grid_filter', '10',
    '-INPUT', saga_rast_to_denoise_ffn,
    '-OUTPUT',  saga_rast_denoised_ffn,
    '-SIGMA', SIGMA,
    '-ITER', ITER,
    '-ZONLY', ZONLY)
  )
  
  
  
  #import it back into grass  
  execGRASS('r.in.gdal',
            input = saga_rast_denoised_ffn,
            output = 'rast_denoised_obj')
  
  
  #import it back into grass 
  execGRASS('r.out.gdal', 
            input = "rast_denoised_obj",
            output = ff.out,
            format = 'GTiff', flags = c('c', 'overwrite'),
            createopt = c('COMPRESS=DEFLATE','TILED=YES'))
  
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# A LIGrabber RGRASS Function: "ligrg.slopeclasses" - takes a raster object and creates a file
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

ligrg.slopeclasses = function(ff.in, dir.out, focal_med_size = 11, ITER = 10 , ZONLY = 1){
  
  ff.in = "D:/LUCA Team/Land Use Capability Assessments Limited/LUCA team site - Documents/LUCA-A/Projects/Development Projects/DP-0046-ligrab-R-Development/gis/layers/elevation/DEM_15m.tif"
  dir.out = "D:/LUCA Team/Land Use Capability Assessments Limited/LUCA team site - Documents/LUCA-A/Projects/Development Projects/DP-0046-ligrab-R-Development/gis/layers/elevation/"
  
  
  
  focal_med_size = 11
  
  slopes_ff.out = paste0(dir.out, "slopes_d", focal_med_size,".tif")
  slope_class_ff.out = paste0(dir.out, "slope_classes_d", focal_med_size,".tif")
  
  
  
  #define all the object names to be used in this script
  focal_stats_output_obj_name = paste0('slope_medc_d', focal_med_size, '_')
  focal_stats_output_obj_int_name = paste0(focal_stats_output_obj_name,'int_')
  slope_class_median_obj_name = paste0('slope_class_median_d',focal_med_size,'_')
  
  
  
  #read the file
  ligrg.readraster(ff.in, obj.out = "_dem_to_be_classed_")
  
  #list the raster to idenitfy the correct raster for slope classing
  dem <- execGRASS('g.list', type = 'raster', pattern = '*dem_to_be_classed*', intern =TRUE)
  
  # run the slope aspect algorithm in GRASS
  execGRASS('r.slope.aspect', 
            elevation = dem, 
            slope = 'slope_',
            format = 'degrees', 
            precision = 'FCELL',
            flags = 'overwrite'
  )
  
  # look for the object with "slope" in its name
  slopes <- execGRASS('g.list', type = 'raster', pattern = 'slope_', intern =TRUE)
  
  
  
  
  
  # do a focal stats
  execGRASS('r.neighbors', 
            input = slopes, 
            output = focal_stats_output_obj_name,
            method = 'median', 
            size = focal_med_size, 
            flags = c('c', 'overwrite'))
  
  
  # multiply the focal statistics by 10 and round to an integer
  execGRASS('r.mapcalc', 
            expression = paste0(focal_stats_output_obj_int_name, ' = round(', focal_stats_output_obj_name, ' * 10)'),
            flags = 'overwrite')
  
  # obtain the object name of the integer form of the focal statistcs
  slopeint <- 
    execGRASS('g.list', type = 'raster', 
              pattern = focal_stats_output_obj_int_name, intern =TRUE)
  
  
  
  #reclass the focal statistics according to the slope reclassing rules
  execGRASS('r.reclass',
            input = slopeint, 
            output = slope_class_median_obj_name,
            rules = slopeclass_ruleset,
            flags = 'overwrite')
  
  
  # output the slopes raster to a output raster geotiff file
  execGRASS('r.out.gdal', 
            input = slopes, 
            output = slopes_ff.out, 
            format = 'GTiff', flags = c('c', 'overwrite'),
            createopt = c('COMPRESS=DEFLATE','TILED=YES')
  )
  
  
  slopeclass <- 
    execGRASS('g.list', type = 'raster', 
              pattern = 'slope_class_median_obj_name*', intern =TRUE)
  
  execGRASS('g.region', raster = slopeclass)
  
  execGRASS('r.out.gdal', 
            input = slopeclass, 
            output = slope_class_ff.out,
            # output = file.path(getwd(), 'data_spatial', 'slope',
            #                    paste0('EHILLS_Slope_Classes_', i, '.tif')), 
            format = 'GTiff', flags = c('c', 'overwrite'),
            createopt = c('COMPRESS=DEFLATE','TILED=YES'))
  
}
