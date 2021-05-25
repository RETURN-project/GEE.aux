import ee

# Trigger the authentication flow.
ee.Authenticate()

# Initialize the library.
ee.Initialize()

# Inputs
start_date = ee.Date('1995-01-01')# start date of period of interest
end_date = ee.Date('2020-01-01')# end date of period of interest
init_point = 0# start processing this point onwards
end_point = 2000#None# last point to be processed, if None: this equals the latest point
# name of GEE collection of points - from each of these points we want to extract a time series
points_collection='users/wandadekeersmaecker/RETURN/samples_forest_fire_nat_n10000_VAL'
output_name= '/home/wanda/Documents/data/upscaleRecovery/test/testdata/samples_forest_fire_nat_n10000_VAL_' + str(init_point) + '_' + str(end_point) + '.csv'#  path of output file

# number of points in the collection
npontos=ee.FeatureCollection(points_collection).size().getInfo()

if end_point == None:
 end_point=npontos
print(end_point)
# GEE list of points
lista_pontos = ee.FeatureCollection(points_collection).toList(npontos)

# iterate over the points and write surface reflectance data to a file
with open(output_name,"a") as f:
 for j in range(init_point,end_point):
  print(j)
  # get one point 
  point = ee.Feature(lista_pontos.get(j))
  # CEC
  cec = ee.Image("projects/soilgrids-isric/cec_mean")
  cec_obj=cec.reduceRegion('mean',point.geometry(),30)
  cec_List=cec_obj.getInfo()
  cec_Vals=list(cec_List.values())# CEC values
  # N
  nitrogen = ee.Image("projects/soilgrids-isric/nitrogen_mean")
  nitrogen_obj=nitrogen.reduceRegion('mean',point.geometry(),30)
  nitrogen_List=nitrogen_obj.getInfo()
  nitrogen_Vals=list(nitrogen_List.values())# nitrogen values
  # SOC
  soc = ee.Image("projects/soilgrids-isric/soc_mean")
  soc_obj=soc.reduceRegion('mean',point.geometry(),30)
  soc_List=soc_obj.getInfo()
  soc_Vals=list(soc_List.values())# soc values
  # silt
  silt = ee.Image("projects/soilgrids-isric/silt_mean")
  silt_obj=silt.reduceRegion('mean',point.geometry(),30)
  silt_List=silt_obj.getInfo()
  silt_Vals=list(silt_List.values())# silt values
  # clay
  clay = ee.Image("projects/soilgrids-isric/clay_mean")
  clay_obj=clay.reduceRegion('mean',point.geometry(),30)
  clay_List=clay_obj.getInfo()
  clay_Vals=list(clay_List.values())# clay values
  # CCWD
  cwd = ee.ImageCollection("IDAHO_EPSCOR/TERRACLIMATE").select('def').filterDate(start_date,end_date).filterBounds(point.geometry()).sum()
  cwd_obj=ee.Image(cwd).reduceRegion('mean',point.geometry(),30)
  cwd_List=cwd_obj.getInfo()
  cwd_Vals=list(cwd_List.values())# cwd values
  # elevation
  elev = ee.Image("USGS/SRTMGL1_003")
  elev_obj=elev.reduceRegion('mean',point.geometry(),30)
  elev_List=elev_obj.getInfo()
  elev_Vals=list(elev_List.values())# elev values
  # slope 
  slope = ee.Terrain.slope(ee.Image("USGS/SRTMGL1_003"))
  slope_obj=slope.reduceRegion('mean',point.geometry(),30)
  slope_List=slope_obj.getInfo()
  slope_Vals=list(slope_List.values())# slope values
  # aspect
  aspect = ee.Terrain.aspect(ee.Image("USGS/SRTMGL1_003"))
  aspect_obj=aspect.reduceRegion('mean',point.geometry(),30)
  aspect_List=aspect_obj.getInfo()
  aspect_Vals=list(aspect_List.values())# aspect values
  # treecover
  han = ee.Image("UMD/hansen/global_forest_change_2019_v1_7")
  tcmean500 = han.select('treecover2000').reduceNeighborhood(ee.Reducer.mean(), ee.Kernel.square(500,'meters'))
  tcmean500_obj=tcmean500.reduceRegion('mean',point.geometry(),30)
  tcmean500_List=tcmean500_obj.getInfo()
  tcmean500_Vals=list(tcmean500_List.values())# mean tree cover in 500m square kernel
  tcmean1000 = han.select('treecover2000').reduceNeighborhood(ee.Reducer.mean(), ee.Kernel.square(1000,'meters'))
  tcmean1000_obj=tcmean1000.reduceRegion('mean',point.geometry(),30)
  tcmean1000_List=tcmean1000_obj.getInfo()
  tcmean1000_Vals=list(tcmean1000_List.values())# mean tree cover in 1000m square kernel
  tcmean5000 = han.select('treecover2000').reduceNeighborhood(ee.Reducer.mean(), ee.Kernel.square(5000,'meters'))
  tcmean5000_obj=tcmean5000.reduceRegion('mean',point.geometry(),30)
  tcmean5000_List=tcmean5000_obj.getInfo()
  tcmean5000_Vals=list(tcmean5000_List.values())# # mean tree cover in 5000m square kernel
  tcsd500 = han.select('treecover2000').reduceNeighborhood(ee.Reducer.stdDev(), ee.Kernel.square(500,'meters'))
  tcsd500_obj=tcsd500.reduceRegion('mean',point.geometry(),30)
  tcsd500_List=tcsd500_obj.getInfo()
  tcsd500_Vals=list(tcsd500_List.values())# sd tree cover in 500m square kernel
  tcsd1000 = han.select('treecover2000').reduceNeighborhood(ee.Reducer.stdDev(), ee.Kernel.square(1000,'meters'))
  tcsd1000_obj=tcsd1000.reduceRegion('mean',point.geometry(),30)
  tcsd1000_List=tcsd1000_obj.getInfo()
  tcsd1000_Vals=list(tcsd1000_List.values())# sd tree cover in 1000m square kernel
  tcsd5000 = han.select('treecover2000').reduceNeighborhood(ee.Reducer.stdDev(), ee.Kernel.square(5000,'meters')) 
  tcsd5000_obj=tcsd5000.reduceRegion('mean',point.geometry(),30)
  tcsd5000_List=tcsd5000_obj.getInfo()
  tcsd5000_Vals=list(tcsd5000_List.values())# sd tree cover in 5000m square kernel
  # population density
  pop = ee.ImageCollection("JRC/GHSL/P2016/POP_GPW_GLOBE_V1").filterDate('1995-01-01','2020-01-01').mean()
  pop500 = pop.reduceNeighborhood(ee.Reducer.mean(), ee.Kernel.square(500,'meters'))
  pop500_obj=pop500.reduceRegion('mean',point.geometry(),250)
  pop500_List=pop500_obj.getInfo()
  pop500_Vals=list(pop500_List.values())# mean population count in 500m square kernel
  pop1000 = pop.reduceNeighborhood(ee.Reducer.mean(), ee.Kernel.square(1000,'meters'))
  pop1000_obj=pop1000.reduceRegion('mean',point.geometry(),250)
  pop1000_List=pop1000_obj.getInfo()
  pop1000_Vals=list(pop1000_List.values())# mean population count in 1000m square kernel
  pop5000 = pop.reduceNeighborhood(ee.Reducer.mean(), ee.Kernel.square(5000,'meters'))
  pop5000_obj=pop5000.reduceRegion('mean',point.geometry(),250)
  pop5000_List=pop5000_obj.getInfo()
  pop5000_Vals=list(pop5000_List.values())# mean population count in 5000m square kernel
  pop10000 = pop.reduceNeighborhood(ee.Reducer.mean(), ee.Kernel.square(10000,'meters'))
  pop10000_obj=pop10000.reduceRegion('mean',point.geometry(),250)
  pop10000_List=pop10000_obj.getInfo()
  pop10000_Vals=list(pop10000_List.values())# mean population count in 10000m square kernel
  # Distance to roads
  roads = ee.FeatureCollection("users/wandadekeersmaecker/RETURN/Brazil_roads")
  roads_obj=roads.distance(100000).reduceRegion('mean',point.geometry(),30)
  roads_List=roads_obj.getInfo()
  roads_Vals=list(roads_List.values())# distance to nearest road
  # Distance to primary, secondary and tertiary roads
  selroads = ee.FeatureCollection("users/wandadekeersmaecker/RETURN/Brazil_roads_sub")
  selroads_obj=selroads.distance(500000).reduceRegion('mean',point.geometry(),30)
  selroads_List=selroads_obj.getInfo()
  selroads_Vals=list(selroads_List.values())# distance to nearest primary, secondary, and tertiary road
  # ecoregion of the point
  ecoreg = point.get('ecoReg').getInfo()
  id = point.get('id').getInfo()
  # vector to be exported
  write_vector=[id,ecoreg,point.geometry().coordinates().getInfo(),cec_Vals,nitrogen_Vals,soc_Vals,silt_Vals,clay_Vals,cwd_Vals, elev_Vals,slope_Vals,aspect_Vals,tcmean500_Vals,tcmean1000_Vals,tcmean5000_Vals,tcsd500_Vals,tcsd1000_Vals,tcsd5000_Vals,pop500_Vals,pop1000_Vals,pop5000_Vals,pop10000_Vals,roads_Vals,selroads_Vals]#,distBU_Vals,dist_Vals
  f.write(str(write_vector)+'\n')
  f.flush()
