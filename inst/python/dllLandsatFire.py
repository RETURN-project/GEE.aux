import ee

# Trigger the authentication flow.
ee.Authenticate()

# Initialize the library.
ee.Initialize()

# Inputs
start_date = ee.Date('1995-01-01')# start date of period of interest
end_date = ee.Date('2020-01-01')# end date of period of interest
init_point = 300000# start processing this point onwards
end_point = 302000#None# last point to be processed, if None: this equals the latest point
# name of GEE collection of points - from each of these points we want to extract a time series
points_collection='users/wandadekeersmaecker/RETURN/samples_forest_loss_fire_n10000'
output_name= '/home/wanda/Documents/data/upscaleRecovery/test/testdata/samples_forest_fire_nat_n10000_' + str(init_point) + '_' + str(end_point) + '.csv'#  path of output file

# number of points in the collection
npontos=ee.FeatureCollection(points_collection).size().getInfo()

if end_point == None:
 end_point=npontos
print(end_point)
# GEE list of points
lista_pontos = ee.FeatureCollection(points_collection).toList(npontos)

# function to mask Landsat 8 surface reflectance data
def maskL8sr(image):
 # Bits 3 and 5 are cloud shadow and cloud, respectively.
 cloudShadowBitMask = 1 << 3
 cloudsBitMask = 1 << 5  
 snowBitMask = 1 << 4
 cloudconf1 = 1 << 6
 cloudconf2 = 1 << 7
 cirrusconf1 = 1 << 8
 cirrusconf2 = 1 << 9
 aeroconf1 = 1 << 6
 aeroconf2 = 1 << 7
 # Get the pixel QA band.
 qa = image.select('pixel_qa')
 aero = image.select('sr_aerosol')
 # Both flags should be set to zero, indicating clear conditions.
 mask = qa.bitwiseAnd(cloudShadowBitMask).eq(0).And(qa.bitwiseAnd(cloudsBitMask).eq(0)).And(qa.bitwiseAnd(snowBitMask).eq(0))
 #Return the masked image, scaled to reflectance, without the QA bands.
 return (image.updateMask(mask).select("B[0-9]*").copyProperties(image, ["system:time_start"]))

# function to mask Landsat 4, 5, 7 surface reflectance data
def maskL457sr(image):
 qa = image.select('pixel_qa')
 op = image.select('sr_atmos_opacity')
 # If the cloud bit (5) is set and the cloud confidence (7) is high
 # or the cloud shadow bit is set (3), then it's a bad pixel.
 # Bits 3 and 5 are cloud shadow and cloud, respectively.
 cloudShadowBitMask = 1 << 3
 cloudsBitMask = 1 << 5
 snowBitMask = 1 << 4
 cloudconf1 = 1 << 6
 cloudconf2 = 1 << 7
 
 mask = (qa.bitwiseAnd(cloudShadowBitMask).eq(0).And(qa.bitwiseAnd(cloudsBitMask).eq(0)).And(qa.bitwiseAnd(snowBitMask).eq(0)))
 return (image.updateMask(mask).select("B[0-9]*").copyProperties(image, ["system:time_start"]))

# function to calculate NBR for landsat 8
def calcNBRl8(im):
 return (im.toFloat().normalizedDifference(['B5', 'B7']).rename('NBR').copyProperties(im, ["system:time_start"]))

# function to calculate NBR for landsat 4,5,7
def calcNBRl457(im):
 return (im.toFloat().normalizedDifference(['B4', 'B7']).rename('NBR').copyProperties(im, ["system:time_start"]))

# iterate over the points and write surface reflectance data to a file
with open(output_name,"a") as f:
 for j in range(init_point,end_point):
  print(j)
  # get one point
  point = ee.Feature(lista_pontos.get(j))
  # Landsat 8,7,5,4 surface reflectance image collection,
  #filtered using date range and spatial boundary, cloud masked, NBR derived
  colL8 = (ee.ImageCollection("LANDSAT/LC08/C01/T1_SR").filterDate(start_date,end_date).filterBounds(point.geometry()).map(maskL8sr).map(calcNBRl8))
  colL7 = (ee.ImageCollection("LANDSAT/LE07/C01/T1_SR").filterDate(start_date,end_date).filterBounds(point.geometry()).map(maskL457sr).map(calcNBRl457))
  colL5 = (ee.ImageCollection("LANDSAT/LT05/C01/T1_SR").filterDate(start_date,end_date).filterBounds(point.geometry()).map(maskL457sr).map(calcNBRl457))
  colL4 = (ee.ImageCollection("LANDSAT/LT04/C01/T1_SR").filterDate(start_date,end_date).filterBounds(point.geometry()).map(maskL457sr).map(calcNBRl457))
  # combine all collections
  col = colL8.merge(colL7).merge(colL5).merge(colL4)
  # convert the collection to an image where each band represents a different date
  original=col.select('NBR').toBands().reduceRegion('mean',point.geometry(),30)
  # Import the fire collection
  # collection containing the day of year of fire
  colFire = (ee.ImageCollection("ESA/CCI/FireCCI/5_1").filterDate(start_date,end_date).filterBounds(point.geometry())).select('BurnDate')
  # collection containing the confidence of fire occurrence
  colFireConf = (ee.ImageCollection("ESA/CCI/FireCCI/5_1").filterDate(start_date,end_date).filterBounds(point.geometry())).select('ConfidenceLevel')
  # extract a fire time series for the point of interest
  fre = colFire.toBands().reduceRegion('mean',point.geometry(),30)
  freConf=colFireConf.toBands().reduceRegion('mean',point.geometry(),30)
  # get the fire information
  freList=fre.getInfo()
  freDts = list(freList.keys())# dates
  freVals=list(freList.values())# fire doy
  freConfList=freConf.getInfo()#
  freConfDts = list(freConfList.keys())# dates
  freConfVals=list(freConfList.values())# fire confidence
  # ecoregion of the point
  ecoreg = point.get('first').getInfo()
  a=original.getInfo()
  # extract dates of the observations
  values=list(a.keys())
  # extract observations
  anp=list(a.values())
  # vector to be exported
  write_vector=[j,ecoreg,point.geometry().coordinates().getInfo(),values,anp, freDts,freVals, freConfDts, freConfVals]
  f.write(str(write_vector)+'\n')
  f.flush()
