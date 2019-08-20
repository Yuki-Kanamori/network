##please change here
dirname = "/Users/Yuki/Dropbox/Network/revised_data"

#####packages#####
##=====if needed=====##
#install.packages("dplyr")
#install.packages("devtools")

##=====necessary=====##
#install.packages("TMB")
#install.packages("INLA", repos=c(getOption("repos"), INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)

#require(devtools)
#devtools::install_github("nwfsc-assess/geostatistical_delta-GLMM") 
#devtools::install_github("james-thorson/VAST")

##if you get some errors and cannot download the VAST, please try the below code.
##i confirmed that the VAST can be installed in the latest version of R (version 3.6.0) and RStudio (Version 1.2.1335).
#devtools::install_github("james-thorson/VAST", INSTALL_opts=c("--no-multiarch --no-test-load"))

require(dplyr)
require(TMB)
require(SpatialDeltaGLMM)
require(VAST)

#####Data#####
setwd(dir = dirname)
tok = read.csv("new_chiba3.csv", fileEncoding = "CP932")
tok = rbind(filter(tok, between(M, 4, 9)) %>% mutate(Season = "SS"), filter(tok, between(M, 1, 3)) %>% mutate(Season = "AW"), filter(tok, between(M, 10, 12)) %>% mutate(Season = "AW"))
sakana = c("ishigarei", "konosiro", "kouika", "kurumaebi", "maanago", "makogarei", "suzuki")[5]
df = filter(tok, FISH == sakana)
summary(df)

#####vast#####
Version = "VAST_v4_2_0"
#####3 Settings#####
n_x = 100
DateFile = paste0(getwd(), "/vast", Sys.Date(), "_lognorm_log", n_x, sakana)
dir.create(DateFile)
###3.1 Spatial settings###
Method = c("Grid", "Mesh", "Spherical_mesh")[2]
Kmeans_Config = list("randomseed" = 1, "nstart" = 100, "iter.max" = 1000)
###3.2 Model settings###
FieldConfig = c(Omega1 = 1, Epsilon1 = 1, Omega2 = 1, Epsilon2 = 1)
RhoConfig = c(Beta1 = 0, Beta2 = 0, Epsilon1 = 0, Epsilon2 = 0)
VesselConfig = c(Vessel = 0, VesselYear = 0)
ObsModel = c(PosDist = 1, Link = 0) #[1] = 1(lognormal) or 2(gamma), [2] = 0
###3.3 Stratification for results###
strata.limits <- data.frame(STRATA = "All_areas")

#####4. Prepare the data#####
###4.1 Data-frame for catch-rate data###
head(df)
df <- df[, c("Y", "Season", "Lat", "Lon", "CATCH", "NUM", "GEAR")]
colnames(df) <- c("Year", "Season", "Lat", "Lon", "Catch_KG", "Effort", "Gear")
summary(df)
Data_Geostat <- df

#####4.2 Extrapolation grid#####
Extrapolation_List = SpatialDeltaGLMM::Prepare_Extrapolation_Data_Fn(
  Regio = "other", 
  strata.limits = strata.limits, 
  observations_LL = Data_Geostat[, c("Lat", "Lon")], 
)
###4.3 derived objects for spatio-temporal estimation###
Spatial_List = SpatialDeltaGLMM::Spatial_Information_Fn(
  n_x = n_x,
  Lon = Data_Geostat[, "Lon"], 
  Lat = Data_Geostat[, "Lat"], 
  Extrapolation_List = Extrapolation_List, 
  Method = "Mesh",
  grid_size_km = 0.5, #methodがsperical_meshの時はgrid_size_LL, それ以外の時はgrid_size_km
  randomseed = Kmeans_Config[["randomseed"]], 
  nstart = Kmeans_Config[["nstart"]], 
  iter.max = Kmeans_Config[["iter.max"]], 
  DirPath = DateFile,
  Save_Results = FALSE) 
Data_Geostat <- cbind(Data_Geostat, knot_i = Spatial_List[["knot_i"]])
setwd(dir = DateFile)
write.csv(Data_Geostat, "Data_Geostat.csv", fileEncoding = "CP932")

#####5 Build and run model#####
###5.1 Build model###
TmbData = Data_Fn(
  Version = Version,
  FieldConfig = FieldConfig,
  RhoConfig = RhoConfig,
  ObsModel = ObsModel,
  b_i = Data_Geostat[, "Catch_KG"], 
  a_i = Data_Geostat[, "Effort"],
  c_iz = as.numeric(as.factor(Data_Geostat[, "Season"])) -1,
  s_i = Data_Geostat[, "knot_i"] - 1,
  t_iz = as.matrix(Data_Geostat[, c("Year")]),
  v_i = as.numeric(Data_Geostat[, "Gear"]) - 1,
  a_xl = Spatial_List$a_xl,
  MeshList = Spatial_List$MeshList,
  GridList = Spatial_List$GridList,
  Method = Spatial_List$Method,
  Aniso = 1, #Using isotropic 2D AR1 hyperdistribution, so switching to Aniso=0
  Options = c(SD_site_density = 1, 
              SD_site_longensity = 1, 
              Calculate_Range = 1, 
              Calculate_evenness = 1, 
              Calculate_effective_area = 1)
)
TmbList = Build_TMB_Fn(TmbData = TmbData,
                       RunDir = DateFile,
                       Version = Version,
                       RhoConfig = RhoConfig,
                       loc_x = Spatial_List$loc_x,
                       Method = Spatial_List$Method)
Obj = TmbList[["Obj"]]
Opt = TMBhelper::Optimize(obj = Obj, 
                          lower = TmbList[["Lower"]], 
                          upper = TmbList[["Upper"]],
                          getsd = TRUE, 
                          savedir = DateFile, 
                          bias.correct = TRUE)
Report = Obj$report()
Save = list("Opt" = Opt, 
            "Report" = Report, 
            "ParHat" = Obj$env$parList(Opt$par),
            "TmbData" = TmbData)
save(Save, file=paste0(DateFile,"Save.RData"))

##----------------------
## 6 Diagnostic plots
# 6.1 Plot data
SpatialDeltaGLMM::Plot_data_and_knots(Extrapolation_List = Extrapolation_List,
                                      Spatial_List = Spatial_List, 
                                      Data_Geostat = Data_Geostat,
                                      PlotDir = DateFile)
# 6.2 Convergence
pander::pandoc.table(Opt$diagnostics[,c('Param','Lower','MLE',
                                        'Upper','final_gradient')] )
# 6.3 Diagnostics for encounter-probability component
Enc_prob = SpatialDeltaGLMM::Check_encounter_prob(Report = Report,
                                                  Data_Geostat = Data_Geostat, 
                                                  DirName = DateFile)
Q = SpatialDeltaGLMM::QQ_Fn(TmbData = TmbData, 
                            Report = Report)
# 6.5 Diagnostics for plotting residuals on a map
# Get region-specific settings for plots
MapDetails_List = SpatialDeltaGLMM::MapDetails_Fn(
  "Region"="other",
  "NN_Extrap"=Spatial_List$PolygonList$NN_Extrap, 
  "Extrapolation_List"=Extrapolation_List)
# Decide which years to plot
Year_Set = seq(min(Data_Geostat[,'Year']),max(Data_Geostat[,'Year']))
Years2Include = which( Year_Set %in% sort(unique(Data_Geostat[,'Year'])))
SpatialDeltaGLMM:::plot_residuals(Lat_i = Data_Geostat[,"Lat"], 
                                  Lon_i = Data_Geostat[, "Lon"], 
                                  TmbData = TmbData,
                                  Report = Report, 
                                  Q = Q, 
                                  savedir = DateFile,
                                  MappingDetails = MapDetails_List[["MappingDetails"]],
                                  PlotDF = MapDetails_List[["PlotDF"]],
                                  MapSizeRatio = MapDetails_List[["MapSizeRatio"]],
                                  Xlim = MapDetails_List[["Xlim"]], 
                                  Ylim = MapDetails_List[["Ylim"]],
                                  FileName = DateFile, 
                                  Year_Set = Year_Set, 
                                  Years2Include = Years2Include,
                                  Rotate = MapDetails_List[["Rotate"]], 
                                  Cex = MapDetails_List[["Cex"]],
                                  Legend = MapDetails_List[["Legend"]], 
                                  zone = MapDetails_List[["Zone"]],
                                  mar = c(0, 0, 2, 0), 
                                  oma = c(3.5, 3.5, 0, 0), 
                                  cex = 1)
# 6.6 Model selection
Opt$AIC

#----------------------
# 7 Model output
# 7.1 Direction of “geometric anisotropy
SpatialDeltaGLMM::PlotAniso_Fn(FileName = paste0(DateFile,"Aniso.png"), 
                               Report = Report, 
                               TmbData = TmbData)
head(MapDetails_List[["PlotDF"]])
# 7.2 Density surface for each year
Map = SpatialDeltaGLMM::PlotResultsOnMap_Fn(plot_set = c(3),
                                            MappingDetails = MapDetails_List[["MappingDetails"]],
                                            Report = Report, 
                                            Sdreport = Opt$SD, 
                                            PlotDF = MapDetails_List[["PlotDF"]],
                                            MapSizeRatio = MapDetails_List[["MapSizeRatio"]],
                                            Xlim = MapDetails_List[["Xlim"]], 
                                            Ylim = MapDetails_List[["Ylim"]],
                                            FileName = DateFile, 
                                            Year_Set = Year_Set, 
                                            Years2Include = Years2Include,
                                            Rotate = MapDetails_List[["Rotate"]], 
                                            Cex = MapDetails_List[["Cex"]],
                                            Legend = MapDetails_List[["Legend"]], 
                                            zone = MapDetails_List[["Zone"]],
                                            mar = c(0, 0, 2, 0), 
                                            oma = c(3.5, 3.5, 0, 0), 
                                            cex = 1, 
                                            plot_legend_fig = TRUE)
# 7.3 Index of abundance
Index = SpatialDeltaGLMM::PlotIndex_Fn(DirName = DateFile,
                                       TmbData = Save$TmbData, 
                                       Sdreport = Save$Opt[["SD"]],
                                       Year_Set = Year_Set,
                                       Years2Include = Years2Include, #年数？
                                       use_biascorr = TRUE
)
pander::pandoc.table(Index$Table[, c("Year","Fleet","Estimate_metric_tons", 
                                     "SD_log", "SD_mt")])
# 7.4 Center of gravity and range expansion/contraction
COG = SpatialDeltaGLMM::Plot_range_shifts(Report = Report, 
                                          TmbData = TmbData, 
                                          Sdreport = Opt[["SD"]],
                                          Znames = colnames(TmbData$Z_xm),
                                          PlotDir = DateFile, 
                                          Year_Set = Year_Set)$COG_Table

Area = SpatialDeltaGLMM::Plot_range_shifts(Report = Report, 
                                           TmbData = TmbData, 
                                           Sdreport = Opt[["SD"]],
                                           Znames = colnames(TmbData$Z_xm),
                                           PlotDir = DateFile, 
                                           Year_Set = Year_Set)$EffectiveArea_Table
write.csv(COG, "COG_anc.csv")
write.csv(Area, "Area_anc.csv")