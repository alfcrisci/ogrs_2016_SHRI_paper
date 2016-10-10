# Calculate LST



setwd("./LC81930292015196LGN00")

RADIANCE_MULT_BAND_10 = 3.3420E-04
RADIANCE_MULT_BAND_11 = 3.3420E-04
RADIANCE_ADD_BAND_10 = 0.10000
RADIANCE_ADD_BAND_11 = 0.10000
K1_CONSTANT_BAND_10 = 774.8853
K1_CONSTANT_BAND_11 = 480.8883
K2_CONSTANT_BAND_10 = 1321.0789
K2_CONSTANT_BAND_11 = 1201.1442

band_10 <- raster("LC81930292015196LGN00_B10.TIF") #change image name accordingly
band_11 <- raster("LC81930292015196LGN00_B11.TIF") #change image name accordingly

#Calculate TOA from DN:

toa_band10 <- calc(band_10, fun=function(x){RADIANCE_MULT_BAND_10 * x + RADIANCE_ADD_BAND_10})
toa_band11 <- calc(band_11, fun=function(x){RADIANCE_MULT_BAND_11 * x + RADIANCE_ADD_BAND_11})
#Calculate LST in Kelvin for Band 10 and Band 11
temp10_kelvin <- calc(toa_band10, fun=function(x){K2_CONSTANT_BAND_10/log(K1_CONSTANT_BAND_10/x + 1)})
temp11_kelvin <- calc(toa_band11, fun=function(x){K2_CONSTANT_BAND_11/log(K1_CONSTANT_BAND_11/x + 1)})

#Convert Kelvin to Celsius for Band 10 and 11
temp10_celsius <- calc(temp10_kelvin, fun=function(x){x - 273.15})
temp11_celsius <- calc(temp11_kelvin, fun=function(x){x - 273.15})

#Export raster images
writeRaster(temp10_celsius, "parma10_c.tif")
writeRaster(temp11_celsius, "parma11_c.tif")
