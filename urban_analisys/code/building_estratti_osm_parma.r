
library("osmar")
library("osmar2")
library("overpass")
library("rgeos")
library("raster")

surce("aux_parma.r")

parma_osm=osmar2::get_osm(osmar2::complete_file(), osmar2::osmsource_file("Parma.osm"))

saveRDS(parma_osm,"parma_osm.rds")

parma_osm=readRDS("parma_osm.rds")

summary(parma_osm$ways)

buildingPaths <- find(parma_osm, way(tags(k == "building" & v == "yes"))) #id
buildingPaths_ids <- find_down(parma_osm, way(buildingPaths))
buildingPaths_sub <- subset(parma_osm, ids = buildingPaths_ids)

way_ids <- unique(buildingPaths_sub$ways$refs$id)
way_lns <- vector("list", length(way_ids))
for (i in 1:length(way_lns)) {
  way_lns[[i]] <- Lines(osmar:::ways_nodes2Line(way_ids[i], buildingPaths_sub$ways, buildingPaths_sub$nodes), way_ids[i])
}
way_lns <- osmar:::remove_emptyLines(way_lns)
splns <- SpatialLines(way_lns, proj4string = osm_crs())
dat <- cbind(buildingPaths_sub$ways$attrs, type = as.factor("way"))
daf <- data.frame(id = unique(dat$id))
rownames(daf) <- daf$id
ret <- SpatialLinesDataFrame(splns, daf)
saveRDS(ret,"parma_building.rds")


building_lines <- as_sp(buildingPaths_sub, "lines")

res=sapply(buildingPaths,function(x) osmway2sp(x))
parma_osm_building=do.call("rbind",res)
saveRDS(parma_osm_building,"parma_osm_building.rds")

#references

# http://rstudio-pubs-static.s3.amazonaws.com/14475_c444fba107bd4d5db64427bfeb9cb8d6.html
# http://www.omicron.dk/openstreetmap.html
# http://rgeomatic.hypotheses.org/category/osrm




