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
  
  
  root = dirname(ff.in)
  saga_rast_to_denoise_ffn = paste0(root, '/rast_to_denoise.sdat')
  saga_rast_denoised_ffn = paste0(root, '/rast_denoised.sdat')
  
  
  
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
  
  
  # ff.in = file.path(dirname(lig_dir), "elevation", paste0("DEM_resamp_",target_res,"m.tif"))
  # ff.out = file.path(dirname(lig_dir), "elevation",paste0(basename(tools::file_path_sans_ext(ff.in)),"_DN.tif"))
  # SIGMA = 0.8
  # ITER = 10
  # ZONLY = 1
  # 
  options(useFancyQuotes = FALSE)
  
  args_str = c(
    'grid_filter', '10',
    '-INPUT', paste0("\"", saga_rast_to_denoise_ffn,"\""),
    '-OUTPUT',  paste0("\"", saga_rast_denoised_ffn,"\""),
    '-SIGMA', SIGMA,
    '-ITER', ITER,
    '-ZONLY', ZONLY)
  
  str = 'saga_cmd'
  for (i in args_str){str = paste0(str,' ',i)}
  
  system(str)
  
  # system2('saga_cmd', args = args_str)
  
  # 
  # system2('saga_cmd', args = c(
  #   'grid_filter', '10',
  #   '-INPUT', saga_rast_to_denoise_ffn,
  #   '-OUTPUT',  saga_rast_denoised_ffn,
  #   '-SIGMA', SIGMA,
  #   '-ITER', ITER,
  #   '-ZONLY', ZONLY)
  # )
  
  
  
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
# A LIGrabber RGRASS Function: "ligrg.slopeclasses" - 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

ligrg.slopeclasses = function(ff.in, dir.out, focal_med_size = 11,slopeclass_ruleset = NULL){
  
  coderoot = "D:/LUCA Team/Land Use Capability Assessments Limited/LUCA Directors - Documents/Code/RCode/ligrab/"
  ruleset_folder = paste0(coderoot, "rulesets/")
  default_slopeclass_ruleset_ffname = paste0(ruleset_folder, 'GRASS_reclass_slope-sharnrules_x10.txt')
  # reliefclass_ruleset_ffname = paste0(ruleset_folder, 'GRASS_r-reclass_rules_Relief_55m_diameter.txt')
  
  
  #if the rule set has not been specified then use 
  if (is.null(slopeclass_ruleset)){
    slopeclass_ruleset_ffname = default_slopeclass_ruleset_ffname
  }
  
  
  
  
  # browser()
  
  # ff.in = "D:/LUCA Team/Land Use Capability Assessments Limited/LUCA team site - Documents/LUCA-A/Projects/Development Projects/DP-0046-ligrab-R-Development/gis/layers/elevation/DEM_resamp_5m_DN.tif"
  # dir.out = "D:/LUCA Team/Land Use Capability Assessments Limited/LUCA team site - Documents/LUCA-A/Projects/Development Projects/DP-0046-ligrab-R-Development/gis/layers/elevation/"
  # focal_med_size = 11
  slopes_ff.out = paste0(dir.out, "slopes_d", focal_med_size,".tif")
  slope_class_ff.out = paste0(dir.out, "slope_classes_d", focal_med_size,".tif")
  
  
  
  #define all the object names to be used in this script
  focal_stats_output_obj_name = paste0('slope_median_d', focal_med_size, '_')
  focal_stats_output_obj_int_name = paste0(focal_stats_output_obj_name,'int_')
  slope_class_median_obj_name = paste0('slope_class_d',focal_med_size,'_')
  
  
  #read the file
  ligrg.readraster(ff.in, obj.out = "_dem_to_be_classed_")
  
  
  # #list the raster to idenitfy the correct raster for slope classing
  # dem <- execGRASS('g.list', type = 'raster', pattern = '*dem_to_be_classed*', intern =TRUE)
  
  # run the slope aspect algorithm in GRASS
  execGRASS('r.slope.aspect', 
            elevation = "_dem_to_be_classed_", 
            slope = 'slope_',
            format = 'degrees', 
            precision = 'FCELL',
            flags = 'overwrite'
  )
  
  # # look for the object with "slope" in its name
  # slopes <- execGRASS('g.list', type = 'raster', pattern = 'slope_', intern =TRUE)
  
  # do a focal stats
  execGRASS('r.neighbors', 
            input = 'slope_', 
            output = focal_stats_output_obj_name,
            method = 'median', 
            size = focal_med_size, 
            flags = c('c', 'overwrite'))
  
  
  # multiply the focal statistics by 10 and round to an integer
  execGRASS('r.mapcalc', 
            expression = paste0(focal_stats_output_obj_int_name, ' = round(', focal_stats_output_obj_name, ' * 10)'),
            flags = 'overwrite')
  
  # # obtain the object name of the integer form of the focal statistcs
  # slopeint <- execGRASS('g.list', type = 'raster', pattern = focal_stats_output_obj_int_name, intern =TRUE)
  
  #reclass the focal statistics according to the slope reclassing rules
  execGRASS('r.reclass',
            input = focal_stats_output_obj_int_name, 
            output = slope_class_median_obj_name,
            rules = slopeclass_ruleset_ffname,
            flags = 'overwrite')
  
  
  # output the slopes raster to a output raster geotiff file
  execGRASS('r.out.gdal', 
            input = slopes, 
            output = slopes_ff.out, 
            format = 'GTiff', flags = c('c', 'overwrite'),
            createopt = c('COMPRESS=DEFLATE','TILED=YES')
  )
  
  # # obtain the object name of the 
  # slopeclass <- execGRASS('g.list', 
  #                         type = 'raster', 
  #                         pattern = slope_class_median_obj_name, 
  #                         intern =TRUE)
  
  execGRASS('g.region', raster = slopeclass)
  
  execGRASS('r.out.gdal', 
            input = slope_class_median_obj_name, 
            output = slope_class_ff.out,
            format = 'GTiff', flags = c('c', 'overwrite'),
            createopt = c('COMPRESS=DEFLATE','TILED=YES'))
  
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# A LIGrabber RGRASS Function: "ligrg.reliefclasses" - 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

ligrg.reliefclasses = function(ff.in, ff.out = NULL, focal_med_size = 11, reliefclass_ruleset = NULL){
  
  # ff.in = "D:/LUCA Team/Land Use Capability Assessments Limited/LUCA team site - Documents/LUCA-A/Projects/Development Projects/DP-0046-ligrab-R-Development/gis/layers/elevation/DEM_15m.tif"
  # ff.out = NULL
  # focal_med_size = 11
  
  coderoot = "D:/LUCA Team/Land Use Capability Assessments Limited/LUCA Directors - Documents/Code/RCode/ligrab/"
  ruleset_folder = paste0(coderoot, "rulesets/")
  
  default_reliefclass_ruleset_ffname = paste0(ruleset_folder, 'GRASS_r-reclass_rules_Relief_55m_diameter.txt')
  # file.exists(default_reliefclass_ruleset_ffname)
  
  #if the rule set has not been specified then use 
  if (is.null(reliefclass_ruleset)){
    reliefclass_ruleset_ffname = default_reliefclass_ruleset_ffname
  }
  
  
  
  if (is.null(ff.out)){ff.out = paste0(dirname(ff.in), "/relief_classes_d",focal_med_size,".tif")}
  
  
  slopes_ff.out = paste0(dir.out, "slopes_d", focal_med_size,".tif")
  slope_class_ff.out = paste0(dir.out, "slope_classes_d", focal_med_size,".tif")
  
  #define all the object names to be used in this script
  focal_stats_slope_median_output_obj_name = paste0('slope_median_d', focal_med_size)
  focal_stats_slope_range_output_obj_name = paste0('slope_range_d', focal_med_size)
  focal_stats_slope_range_cm_output_obj_name = paste0(focal_stats_slope_range_output_obj_name,"_cm")
  relief_class_obj_name = paste0('relief_class_d', focal_med_size)
  
  
  
  #read the file
  ligrg.readraster(ff.in, obj.out = "_dem_to_be_classed_for_relief")
  
  
  #list the raster to idenitfy the correct raster for slope classing
  dem <- execGRASS('g.list', type = 'raster', pattern = '_dem_to_be_classed_for_relief', intern =TRUE)
  
  #create the region
  execGRASS('g.region', raster = dem)
  
  #create focal stats using the median
  execGRASS('r.neighbors',
            input = dem,
            output = focal_stats_slope_median_output_obj_name,
            method = 'median', 
            size = focal_med_size, 
            flags = c('c', 'overwrite'))
  
  #take the focal median and calculate the range within the focal area
  execGRASS('r.neighbors', 
            input = focal_stats_slope_median_output_obj_name,
            output = focal_stats_slope_range_output_obj_name,
            method = 'range', 
            size = focal_med_size, 
            flags = c('c', 'overwrite'))
  
  # need an integer dataset for r.reclass so convert to cm
  execGRASS('r.mapcalc', 
            expression = paste0(focal_stats_slope_range_cm_output_obj_name, ' = round(', focal_stats_slope_median_output_obj_name, ' * 100)'),
            flags = 'overwrite'
  )
  
  # now reclassify
  # the ruleset is in cm e.g. 0 thru 82 = 1 Extremely Low
  execGRASS('r.reclass', 
            input = focal_stats_slope_range_output_obj_name, 
            output = relief_class_obj_name,
            rules = reliefclass_ruleset_ffname,
            flags = 'overwrite')
  
  
  # obtain the object name
  relcl <- execGRASS('g.list', 
                     type = 'raster', 
                     pattern = relief_class_obj_name,
                     intern =TRUE)
  
  
  execGRASS('g.region', raster = relcl)
  
  execGRASS('r.out.gdal', 
            input =  relcl,
            output = ff.out,
            format = 'GTiff', flags = c('c', 'overwrite'),
            createopt = c('COMPRESS=DEFLATE','TILED=YES'))
  
  
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# A LIGrabber RGRASS Function: "ligrg.RMS" - 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


ligrg.RMS = function(relief.class.ff.in, slope.class.ff.in, focal_size = 11, 
                     area_thresh = 5000,
                     rms.class.rast.ff.out = NULL, 
                     rms.class.vect.ff.out = NULL,
                     rmsclass_ruleset = NULL){
  
  # 
  # relief.class.ff.in = paste0("D:/LUCA Team/Land Use Capability Assessments Limited/LUCA team site - Documents/LUCA-A/Projects/Development Projects/DP-0046-ligrab-R-Development/gis/layers/elevation/relief_classes_d11.tif")
  # slope.class.ff.in = paste0(("D:/LUCA Team/Land Use Capability Assessments Limited/LUCA team site - Documents/LUCA-A/Projects/Development Projects/DP-0046-ligrab-R-Development/gis/layers/elevation/slope_classes_d11.tif"))
  # focal_size = 11
  # rms.class.rast.ff.out = NULL 
  # rms.class.vect.ff.out = NULL
  # rmsclass_ruleset = NULL
  
  # ff.out = NULL
  
  
  
  
  #if the rule set has not been specified then use the default ruleset
  if (is.null(rmsclass_ruleset)){
    # provide the default ruleset
    coderoot = "D:/LUCA Team/Land Use Capability Assessments Limited/LUCA Directors - Documents/Code/RCode/ligrab/"
    ruleset_folder = paste0(coderoot, "rulesets/")
    default_rmsclass_ruleset_ffname = paste0(ruleset_folder, 'GRASS_reclass_RMS-sharnrules.txt')
    rmsclass_ruleset = default_rmsclass_ruleset_ffname
  }
  
  #create default output file names
  if (is.null(rms.class.rast.ff.out)){rms.class.rast.ff.out = paste0(dirname(relief.class.ff.in), "/RMS_d",focal_med_size,".tif")}
  if (is.null(rms.class.vect.ff.out)){rms.class.vect.ff.out = paste0(dirname(relief.class.ff.in), "/RMS_vect_d",focal_med_size,".shp")}
  
  #create the clean RMS output file name
  rms.clean.class.rast.ff.out = paste0(dirname(relief.class.ff.in), "/RMS_clean_d",focal_med_size,".tif")
  
  # string labels for the input data set object names
  relcl = "relief_classes_for_RMS"
  slpcl = "slope_classes_for_RMS"
  
  #read the input files
  ligrg.readraster(relief.class.ff.in, obj.out = relcl)
  ligrg.readraster(slope.class.ff.in, obj.out = slpcl)
  
  # calculate RMS
  execGRASS('r.mapcalc',
            expression = 
              paste0('rms_classes = int((', slpcl, ' * 10) + ', relcl, ')'),
            flags = c('overwrite'))
  
  # write the RMS to an output file
  execGRASS('r.out.gdal', 
            input = 'rms_classes', 
            output = rms.class.rast.ff.out,
            format = 'GTiff', flags = c('c', 'overwrite'),
            createopt = c('COMPRESS=DEFLATE','TILED=YES'))
  
  
  execGRASS('g.region', raster = 'rms_classes')
  
  # create derived RMS classes based on rule set
  execGRASS('r.reclass', 
            input = 'rms_classes', 
            output = 'rms_derived_classes',
            rules = default_rmsclass_ruleset_ffname,
            flags = 'overwrite')
  
  
  execGRASS('g.region', raster = 'rms_derived_classes')
  
  # strict vectorisation
  execGRASS('r.to.vect',
            input     = 'rms_derived_classes',
            output    = 'rms_derived_vector_raw',
            type      = 'area',
            flags     = c('v', 'overwrite'))
  
  # absorb polygons smaller than 5000 m2
  execGRASS('v.clean',
            input     = 'rms_derived_vector_raw',
            type      = 'area',
            output    = 'rms_derived_vector_cln',
            tool      = 'rmarea',
            threshold = area_thresh, #square meters
            flags     = c('c', 'overwrite'))
  
  # write the vector data to a shape file
  execGRASS('v.out.ogr',
            input     = 'rms_derived_vector_cln',
            type      = 'area',
            format    = 'ESRI_Shapefile',
            output      = rms.class.vect.ff.out,
            flags = 'overwrite')
  
  
  
  # re-rasterise result
  execGRASS('v.to.rast',
            input  = 'rms_derived_vector_cln',
            type   = 'area',
            output = 'rms_derived_classes_clean',
            use    = 'cat', flags = 'overwrite')
  
  # export
  execGRASS('r.out.gdal', 
            input = 'rms_derived_classes_clean', 
            output = 
              rms.clean.class.rast.ff.out, 
            format = 'GTiff', flags = c('c', 'overwrite'),
            createopt = c('COMPRESS=DEFLATE','TILED=YES'))
  
  
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# A LIGrabber SAGA Function: "ligrsaga.execandsave" - 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


ligrsaga.execandsave = function(saga.ff.in, tif.ff.out){
  
  # browser()
  
  #import it back into grass  
  execGRASS('r.in.gdal',
            input = saga.ff.in,
            output = 'rast_obj',
            flags =c('e', 'overwrite'))
  
  execGRASS('g.region', raster = 'rast_obj')

  #import it back into grass 
  execGRASS('r.out.gdal', 
            input = "rast_obj",
            output = tif.ff.out,
            format = 'GTiff', flags = c('c', 'overwrite'),
            createopt = c('COMPRESS=DEFLATE','TILED=YES'))
  
  
  
}

  


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# A LIGrabber SAGA Function: "ligrsaga.fillsinks" - 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

ligrsaga.fillsinks = function(ff.dem.in, minslope = 0.1){

  
  # ff.dem.in = "D:/LUCA Team/Land Use Capability Assessments Limited/LUCA team site - Documents/LUCA-A/Projects/Development Projects/DP-0046-ligrab-R-Development/gis/layers/elevation/DEM_resamp_5m.tif"
  ff.filled.out = paste0(tools::file_path_sans_ext(ff.in),"_filled.sdat")
  ff.fdir.out = paste0(tools::file_path_sans_ext(ff.in),"_flowdir.sdat")
  ff.wshed.out = paste0(tools::file_path_sans_ext(ff.in),"_wshed.sdat")
  ff.minslope.out = paste0(tools::file_path_sans_ext(ff.in),"_minslope.sdat")
  
  ff.filled.out.tif = paste0(tools::file_path_sans_ext(ff.in),"_filled.tif")
  
# create a list of the parameters for the SAGA command
args_str = c(
  'ta_preprocessor', '4',
  '-ELEV', paste0("\"", ff.dem.in,"\""),
  '-FILLED',  paste0("\"", ff.filled.out,"\""),
  '-FDIR',  paste0("\"", ff.fdir.out,"\""),
  '-WSHED',  paste0("\"", ff.wshed.out,"\""),
  '-MINSLOPE',  minslope)

# build the command string from the elements of the list above
str = 'saga_cmd'
for (i in args_str){str = paste0(str,' ',i)}

# send the SAGA command string to the system
system(str)


ligrsaga.execandsave(ff.filled.out, ff.filled.out.tif)


}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# A LIGrabber SAGA Function: "ligrsaga.slopelength" - 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

function(ff.in, ff.out){
  
  
  # ff.in = "D:/LUCA Team/Land Use Capability Assessments Limited/LUCA team site - Documents/LUCA-A/Projects/Development Projects/DP-0046-ligrab-R-Development/gis/layers/elevation/DEM_resamp_5m.tif"
  ff.out.sdat = paste0(dirname(ff.in),"/","SlopeLength.sdat")
  ff.out.tif = paste0(dirname(ff.in),"/","SlopeLength.tif")
  
  args_str = c(
    'ta_hydrology', '7',
    '-DEM', paste0("\"", ff.in,"\""),
    '-LENGTH',  paste0("\"", ff.out,"\""))
  
  str = 'saga_cmd'
  for (i in args_str){str = paste0(str,' ',i)}
  
  system(str)
  
  ligrsaga.execandsave(ff.out.sdat, ff.out.tif)
  
  
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# A LIGrabber SAGA Function: "ligrsaga.BTA" - Basic Terrain Analysis - Includes TWI
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

ligrsaga.BTA = function(ff.in, thresh = 5){
  
  # thresh = 5
  # ff.in = "D:/LUCA Team/Land Use Capability Assessments Limited/LUCA team site - Documents/LUCA-A/Projects/Development Projects/DP-0046-ligrab-R-Development/gis/layers/elevation/DEM_resamp_5m.tif"
  # 
  
  
  BTA_din_saga = paste0(dirname(ff.in),"/BasicTerrainLayers_saga/")
  BTA_din_tif = paste0(dirname(ff.in),"/BasicTerrainLayers_tif/")
  
  if (!dir.exists(BTA_din_saga)){dir.create(BTA_din_saga)}
  if (!dir.exists(BTA_din_tif)){dir.create(BTA_din_tif)}
  
  
  
  # -ELEVATION:<str>      Elevation
  # -SHADE:<str>          Analytical Hillshading
  # -SLOPE:<str>          Slope
  # -ASPECT:<str>         Aspect
  # -HCURV:<str>          Plan Curvature
  # -VCURV:<str>          Profile Curvature
  # -CONVERGENCE:<str>    Convergence Index
  # -SINKS:<str>          Closed Depressions
  # -FLOW:<str>           Total Catchment Area
  # -WETNESS:<str>        Topographic Wetness Index
  # -LSFACTOR:<str>       LS-Factor
  # -CHANNELS:<str>       Channel Network
  # -BASINS:<str>         Drainage Basins
  # -CHNL_BASE:<str>      Channel Network Base Level
  # -CHNL_DIST:<str>      Channel Network Distance
  # -VALL_DEPTH:<str>     Valley Depth
  # -RSP:<str>            Relative Slope Position
  
  params_keys = c(
    "-ELEVATION",
    "-SHADE",
    "-SLOPE",
    "-ASPECT",
    "-HCURV",
    "-VCURV",
    "-CONVERGENCE",
    "-SINKS",
    "-FLOW",
    "-WETNESS",
    "-LSFACTOR",
    "-CHANNELS",
    "-BASINS",
    "-CHNL_BASE",
    "-CHNL_DIST",
    "-VALL_DEPTH",
    "-RSP",
    "-THRESHOLD"
    )
  
  output_layers = c(
  'Analytical_Hillshading',
  'Slope',
  'Aspect',
  'Plan_Curvature',
  'Profile_Curvature',
  'Convergence_Index',
  'Closed_Depressions',
  'Total_Catchment_Area',
  'Topographic_Wetness_Index',
  'LS_Factor',
  'Channel_Network',
  'Drainage_Basins',
  'Channel_Network_Base_Level',
  'Channel_Network_Distance',
  'Valley_Depth',
  'Relative_Slope_Position')
  
  
  params_values = c(paste0("\"",ff.in, "\""),
                    paste0("\"",BTA_din_saga,output_layers,".sdat", "\""), 
                    as.character(thresh))
  
  cmd_str = "saga_cmd ta_compound 0 "
  
  for (i in 1:length(params_keys)){
    cmd_str =  paste0(cmd_str,  params_keys[i]," ", params_values[i]," ")
  }
  
  system(cmd_str)  
    
  for (i in 1:length(output_layers)){
    
  
    clayer = output_layers[i]
    saga_ff.in = paste0(BTA_din_saga, clayer, ".sdat")
    
    geotiff_ff.out = paste0(BTA_din_tif, basename(tools::file_path_sans_ext(saga_ff.in)),".tif")
    
    ligrsaga.execandsave(saga_ff.in, geotiff_ff.out)
  }
  
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# A LIGrabber RGRASS Function: "ligrg.allsteps" - 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

ligrg.allsteps = function(dem_ffn, lig_dir, dem_rsmpl_res = 5, focal_dist = 11, area_thresh = 5000){
  
  
  dem_ffn = "D:/LUCA Team/Land Use Capability Assessments Limited/LUCA team site - Documents/LUCA-A/Projects/2021-07-10-LP0038-R_Hart/GIS/Grid rasters - covariates/2021_DEM_LiDAR_Omakere_HBRC/merged.tif"
  lig_dir = "D:/LUCA Team/Land Use Capability Assessments Limited/LUCA team site - Documents/LUCA-A/Projects/Development Projects/DP-0046-ligrab-R-Development/gis/layers/"
  dem_rsmpl_res = 5
  focal_dist = 11
  area_thresh = 5000
  
  #+++++++++++++++++++++++++++++++++++++++++++++++++
  # Step 1. Copy the DEM to the elevation directory
  #+++++++++++++++++++++++++++++++++++++++++++++++++
  
  dem.orig.ff.out = paste0(lig_dir, 'elevation/DEM_orig.tif')
  
  #read the raster file
  ligrg.readraster(ff.in = dem_ffn, obj.out = 'DEM_in')
  
  #save it to the elevation directory
  ligrg.saveraster(obj.in = 'DEM_in', ff.out = dem.orig.ff.out)
  
  #+++++++++++++++++++++++++++++++++++++++++++++++++
  # Step 2. Resample the Raster
  #+++++++++++++++++++++++++++++++++++++++++++++++++
  
  
  ff.in = dem.orig.ff.out
  rsmpld.dem.ff.out = paste0(lig_dir, "elevation/DEM_resamp_",dem_rsmpl_res,"m.tif")
  
  src_obj_name = 'dem'
  dst_obj_name = 'dem_res'
  
  #read the raster file
  ligrg.readraster(ff.in = dem.in, obj.out = src_obj_name)
  
  #resample to  target resolution
  ligrg.resample(obj.in = src_obj_name, obj.out = dst_obj_name, dem_rsmpl_res)
  
  #save the resampled raster
  ligrg.saveraster(obj.in = dst_obj_name, ff.out = rsmpld.dem.ff.out)
  
  #+++++++++++++++++++++++++++++++++++++++++++++++++
  # Step 3. denoise the resampled DEM
  #+++++++++++++++++++++++++++++++++++++++++++++++++
  
  denoised.dem.ff.out = paste0(tools::file_path_sans_ext(rsmpld.dem.ff.out),"_DN.tif")
  
  ligrg.sagadenoise(ff.in = rsmpld.dem.ff.out, ff.out = denoised.dem.ff.out )
  
  #+++++++++++++++++++++++++++++++++++++++++++++++++
  # Step 4. Get the slope classes
  #+++++++++++++++++++++++++++++++++++++++++++++++++
  
  dir.out = paste0(dirname(denoised.dem.ff.out),"/")
  ligrg.slopeclasses(denoised.dem.ff.out, dir.out, focal_med_size = focal_dist)
  
  #+++++++++++++++++++++++++++++++++++++++++++++++++
  # Step 5. Get the relief classes
  #+++++++++++++++++++++++++++++++++++++++++++++++++
  
  ligrg.reliefclasses(denoised.dem.ff.out,  focal_med_size = focal_dist)
  
  
  #+++++++++++++++++++++++++++++++++++++++++++++++++
  # Step 6. Do the RMS Calculation
  #+++++++++++++++++++++++++++++++++++++++++++++++++
  
  relief.class.ff.in = paste0(lig_dir, "elevation/relief_classes_d",focal_size,".tif")
  slope.class.ff.in = paste0(lig_dir, "elevation/slope_classes_d",focal_size,".tif")
  ligrg.RMS(relief.class.ff.in, slope.class.ff.in, focal_size = focal_dist, area_thresh = area_thresh)
  
  
  #+++++++++++++++++++++++++++++++++++++++++++++++++
  # Step 7. Fill the Sinks
  #+++++++++++++++++++++++++++++++++++++++++++++++++
  
  ligrsaga.fillsinks(denoised.dem.ff.out, minslope = 0.1)
  
  
  #+++++++++++++++++++++++++++++++++++++++++++++++++
  # Step 8. Do the Basic Terrain Analysis on the sink filled data
  #+++++++++++++++++++++++++++++++++++++++++++++++++
  
  
  ff.filled.in = paste0(tools::file_path_sans_ext(denoised.dem.ff.out),"_filled.tif")
  ligrsaga.BTA(ff.filled.in, thresh = 5)
  
  
  }

