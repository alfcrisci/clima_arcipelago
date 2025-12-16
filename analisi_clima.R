library(openxlsx)
library(hydroTSM)
library(climatol)
library(climate)
library(dplyr)
library(lubridate)
library(ggh4x)
library(xts)
library(doBy)
cat("\14")

###################################################################

setwd("C:\\aaa_lavori\\lav_arcipelago")

###################################################################
sheet_names <- getSheetNames("termo_pluvio_elba.xlsx")

# [1] "capraia"            "giglio"             "portoferraio"       "monte_perone"      
# [5] "capraia_termo"      "giglio_termo"       "portoferraio_termo" "monte_perone_termo"


#######################################################################################################################

capraia_prec=read.xlsx("termo_pluvio_elba.xlsx","capraia")
capraia_termo=read.xlsx("termo_pluvio_elba.xlsx","capraia_termo")

capraia_prec <- capraia_prec |>
  mutate(
    Date = as.Date(date, format = "%d/%m/%Y"),
    Year = lubridate::year(Date),
    Month = lubridate::month(Date))

capraia_prec<-capraia_prec[,c("Date","Year","Month","prec")]

capraia_termo <- capraia_termo |>
  mutate(
    Date = as.Date(date, format = "%d/%m/%Y"),
    Year = lubridate::year(Date),
    Month = lubridate::month(Date))

capraia_termo<-capraia_termo[,c("Date","Year","Month","tmax","tmin")]
capraia_termo$Year=NULL
capraia_termo$Month=NULL


capraia_full=left_join(capraia_prec,capraia_termo,by="Date")

capraia_full$tmax<-as.numeric(capraia_full$tmax)
capraia_full$tmin<-as.numeric(capraia_full$tmin)
capraia_full$prec<-as.numeric(capraia_full$prec)
capraia_full<-na.omit(capraia_full)
capraia_xts<-xts(capraia_full[,c("prec","tmax","tmin")],order.by = as.Date(capraia_full$Date))
capraia_xts <- capraia_xts[!duplicated(index(capraia_xts) ),  ]

png("climograph_2020_capraia.png",900,700)
m_capraia <- climograph(pcp=capraia_xts$prec["202001/202012"], tmx=capraia_xts$tmax["202001/202012"], tmn=capraia_xts$tmin["202001/202012"], na.rm=TRUE, plot.pcp.probs=FALSE, plot.temp.probs=FALSE, 
                main="Capraia Isola")
dev.off()


capraia_full_m <- capraia_full |>
  group_by(Year,Month) |>
  summarise(
    Rain = sum(prec),
    Temp_max = mean(tmax, rm.na = TRUE),
    Temp_min = mean(tmin, rm.na = TRUE), .groups = 'drop') |>
    mutate(Mth = factor(month.abb[Month], 
                      levels = c("Jan", "Feb", "Mar", "Apr",
                                 "May", "Jun", "Jul", "Aug",
                                 "Sep", "Oct", "Nov", "Dec")))


capraia_full_m <- capraia_full_m |> mutate(Tavg = (Temp_max + 2 * Temp_min)/3)


#############################################################################


capraia_summary = capraia_full_m %>% 
  group_by(Mth) %>% 
  summarise(tmax = mean(Temp_max, na.rm = TRUE), 
            tmin = mean(Temp_min, na.rm = TRUE),
            tavg = mean(Tavg, na.rm = TRUE), 
            prec = sum(Rain) / n_distinct(Year))            

capraia_summary = as.data.frame(t(capraia_summary[, c(5,2,3,4)])) 
capraia_summary = round(capraia_summary, 1)
colnames(capraia_summary) = month.abb
print(capraia_summary)

png("diagramma_WL_capraia.png")
climatol::diagwl(capraia_summary,cols=NULL, mlab = "en", stname= "Capraia Isola", alt = 274, per = "2018-24", p3line = FALSE)
dev.off()



#######################################################################################################################

giglio_prec=read.xlsx("termo_pluvio_elba.xlsx","giglio")
giglio_termo=read.xlsx("termo_pluvio_elba.xlsx","giglio_termo")

giglio_prec <- giglio_prec |>
  mutate(
    Date = as.Date(date, format = "%d/%m/%Y"),
    Year = lubridate::year(Date),
    Month = lubridate::month(Date))

giglio_prec<-giglio_prec[,c("Date","Year","Month","prec")]

giglio_termo <- giglio_termo |>
  mutate(
    Date = as.Date(date, format = "%d/%m/%Y"),
    Year = lubridate::year(Date),
    Month = lubridate::month(Date))

giglio_termo<-giglio_termo[,c("Date","Year","Month","tmax","tmin")]
giglio_termo$Year=NULL
giglio_termo$Month=NULL


giglio_full=left_join(giglio_prec,giglio_termo,by="Date")

giglio_full$tmax<-as.numeric(giglio_full$tmax)
giglio_full$tmin<-as.numeric(giglio_full$tmin)
giglio_full$prec<-as.numeric(giglio_full$prec)
giglio_full<-na.omit(giglio_full)
giglio_xts<-xts(giglio_full[,c("prec","tmax","tmin")],order.by = as.Date(giglio_full$Date))
giglio_xts <- giglio_xts[!duplicated(index(giglio_xts) ),  ]
plot(giglio_xts)
png("climograph_2020_giglio.png",900,700)
m_giglio <- climograph(pcp=giglio_xts$prec["202001/202012"], tmx=giglio_xts$tmax["202001/202012"], tmn=giglio_xts$tmin["202001/202012"], na.rm=TRUE, plot.pcp.probs=FALSE, plot.temp.probs=FALSE, 
                       main="giglio Isola")
dev.off()


giglio_full_m <- giglio_full |>
  group_by(Year,Month) |>
  summarise(
    Rain = sum(prec),
    Temp_max = mean(tmax, rm.na = TRUE),
    Temp_min = mean(tmin, rm.na = TRUE), .groups = 'drop') |>
  mutate(Mth = factor(month.abb[Month], 
                      levels = c("Jan", "Feb", "Mar", "Apr",
                                 "May", "Jun", "Jul", "Aug",
                                 "Sep", "Oct", "Nov", "Dec")))


giglio_full_m <- giglio_full_m |> mutate(Tavg = (Temp_max + 2 * Temp_min)/3)


#############################################################################


giglio_summary = giglio_full_m %>% 
  group_by(Mth) %>% 
  summarise(tmax = mean(Temp_max, na.rm = TRUE), 
            tmin = mean(Temp_min, na.rm = TRUE),
            tavg = mean(Tavg, na.rm = TRUE), 
            prec = sum(Rain) / n_distinct(Year))            

giglio_summary = as.data.frame(t(giglio_summary[, c(5,2,3,4)])) 
giglio_summary = round(giglio_summary, 1)
colnames(giglio_summary) = month.abb
print(giglio_summary)

png("diagramma_WL_giglio.png")
climatol::diagwl(giglio_summary,cols=NULL, mlab = "en", stname= "Giglio Castello", alt = 470, per = "2015-25", p3line = FALSE)
dev.off()


#######################################################################################################################

portoferraio_prec=read.xlsx("termo_pluvio_elba.xlsx","portoferraio")
portoferraio_termo=read.xlsx("termo_pluvio_elba.xlsx","portoferraio_termo")

portoferraio_prec <- portoferraio_prec |>
  mutate(
    Date = as.Date(date, format = "%d/%m/%Y"),
    Year = lubridate::year(Date),
    Month = lubridate::month(Date))

portoferraio_prec<-portoferraio_prec[,c("Date","Year","Month","prec")]

portoferraio_termo <- portoferraio_termo |>
  mutate(
    Date = as.Date(date, format = "%d/%m/%Y"),
    Year = lubridate::year(Date),
    Month = lubridate::month(Date))

portoferraio_termo<-portoferraio_termo[,c("Date","Year","Month","tmax","tmin")]
portoferraio_termo$Year=NULL
portoferraio_termo$Month=NULL


portoferraio_full=left_join(portoferraio_prec,portoferraio_termo,by="Date")

portoferraio_full$tmax<-as.numeric(portoferraio_full$tmax)
portoferraio_full$tmin<-as.numeric(portoferraio_full$tmin)
portoferraio_full$prec<-as.numeric(portoferraio_full$prec)
portoferraio_full<-na.omit(portoferraio_full)
portoferraio_xts<-xts(portoferraio_full[,c("prec","tmax","tmin")],order.by = as.Date(portoferraio_full$Date))
portoferraio_xts <- portoferraio_xts[!duplicated(index(portoferraio_xts) ),  ]

plot(portoferraio_xts)

png("climograph_2020_portoferraio.png",900,700)
m_portoferraio <- climograph(pcp=portoferraio_xts$prec["202001/202012"], tmx=portoferraio_xts$tmax["202001/202012"], tmn=portoferraio_xts$tmin["202001/202012"], na.rm=TRUE, plot.pcp.probs=FALSE, plot.temp.probs=FALSE, 
                             main="portoferraio Isola")
dev.off()


portoferraio_full_m <- portoferraio_full |>
  group_by(Year,Month) |>
  summarise(
    Rain = sum(prec),
    Temp_max = mean(tmax, rm.na = TRUE),
    Temp_min = mean(tmin, rm.na = TRUE), .groups = 'drop') |>
  mutate(Mth = factor(month.abb[Month], 
                      levels = c("Jan", "Feb", "Mar", "Apr",
                                 "May", "Jun", "Jul", "Aug",
                                 "Sep", "Oct", "Nov", "Dec")))


portoferraio_full_m <- portoferraio_full_m |> mutate(Tavg = (Temp_max + 2 * Temp_min)/3)


#############################################################################


portoferraio_summary = portoferraio_full_m %>% 
  group_by(Mth) %>% 
  summarise(tmax = mean(Temp_max, na.rm = TRUE), 
            tmin = mean(Temp_min, na.rm = TRUE),
            tavg = mean(Tavg, na.rm = TRUE), 
            prec = sum(Rain) / n_distinct(Year))            

portoferraio_summary = as.data.frame(t(portoferraio_summary[, c(5,2,3,4)])) 
portoferraio_summary = round(portoferraio_summary, 1)
colnames(portoferraio_summary) = month.abb
print(portoferraio_summary)

png("diagramma_WL_portoferraio.png")
climatol::diagwl(portoferraio_summary,cols=NULL, mlab = "en", stname= "Portoferraio", alt = 10, per = "2012-25", p3line = FALSE)
dev.off()



#######################################################################################################################

monte_perone_prec=read.xlsx("termo_pluvio_elba.xlsx","monte_perone")
monte_perone_termo=read.xlsx("termo_pluvio_elba.xlsx","monte_perone_termo")

monte_perone_prec <- monte_perone_prec |>
  mutate(
    Date = as.Date(date, format = "%d/%m/%Y"),
    Year = lubridate::year(Date),
    Month = lubridate::month(Date))

monte_perone_prec<-monte_perone_prec[,c("Date","Year","Month","prec")]

monte_perone_termo <- monte_perone_termo |>
  mutate(
    Date = as.Date(date, format = "%d/%m/%Y"),
    Year = lubridate::year(Date),
    Month = lubridate::month(Date))

monte_perone_termo<-monte_perone_termo[,c("Date","Year","Month","tmax","tmin")]
monte_perone_termo$Year=NULL
monte_perone_termo$Month=NULL


monte_perone_full=left_join(monte_perone_prec,monte_perone_termo,by="Date")

monte_perone_full$tmax<-as.numeric(monte_perone_full$tmax)
monte_perone_full$tmin<-as.numeric(monte_perone_full$tmin)
monte_perone_full$prec<-as.numeric(monte_perone_full$prec)
monte_perone_full$tmax[which(monte_perone_full$tmax < -30)]=14
monte_perone_full$tmin[which(monte_perone_full$tmin < -20)]=5
monte_perone_full<-na.omit(monte_perone_full)
monte_perone_xts<-xts(monte_perone_full[,c("prec","tmax","tmin")],order.by = as.Date(monte_perone_full$Date))
monte_perone_xts <- monte_perone_xts[!duplicated(index(monte_perone_xts) ),  ]
plot(monte_perone_xts)
png("climograph_2020_monte_perone.png",900,700)
m_monte_perone <- climograph(pcp=monte_perone_xts$prec["202001/202012"], tmx=monte_perone_xts$tmax["202001/202012"], tmn=monte_perone_xts$tmin["202001/202012"], na.rm=TRUE, plot.pcp.probs=FALSE, plot.temp.probs=FALSE, 
                             main="monte_perone Isola")
dev.off()


monte_perone_full_m <- monte_perone_full |>
  group_by(Year,Month) |>
  summarise(
    Rain = sum(prec),
    Temp_max = mean(tmax, rm.na = TRUE),
    Temp_min = mean(tmin, rm.na = TRUE), .groups = 'drop') |>
  mutate(Mth = factor(month.abb[Month], 
                      levels = c("Jan", "Feb", "Mar", "Apr",
                                 "May", "Jun", "Jul", "Aug",
                                 "Sep", "Oct", "Nov", "Dec")))


monte_perone_full_m <- monte_perone_full_m |> mutate(Tavg = (Temp_max + 2 * Temp_min)/3)


#############################################################################


monte_perone_summary = monte_perone_full_m %>% 
  group_by(Mth) %>% 
  summarise(tmax = mean(Temp_max, na.rm = TRUE), 
            tmin = mean(Temp_min, na.rm = TRUE),
            tavg = mean(Tavg, na.rm = TRUE), 
            prec = sum(Rain) / n_distinct(Year))            

monte_perone_summary = as.data.frame(t(monte_perone_summary[, c(5,2,3,4)])) 
monte_perone_summary = round(monte_perone_summary, 1)
colnames(monte_perone_summary) = month.abb
print(monte_perone_summary)

png("diagramma_WL_monte_perone.png")
climatol::diagwl(monte_perone_summary,cols=NULL, mlab = "en", stname= "Monte Perone ", alt = 713, per = "2012-25", p3line = FALSE)
dev.off()


###################################################################################################
# Portoferraio	TOS11000012	42.796	10.36	10
# Monte Perone	TOS11000511	42.775	10.191	713
# Giglio Castello	TOS03003269	42.358	10,907	470
# Capraia Isola	TOS03003145	43.063	9.821	274




# ggplot(dataMeteo3, mapping = aes(x = interaction(Mth, Year),
#                                  group = 1)) +
#   geom_col(aes(y = Rain), fill = "grey") +
#   geom_point(aes(y = newTemp), size = 2, col = "blue") +
#   geom_smooth(
#     method = "gam", formula = y ~ s(x, bs = "cc"),
#     aes(x = as.numeric(interaction(Mth, Year)), y = newTemp),
#     col = "blue") +
#   geom_point(aes(y = newTemp), size = 3, col = "blue", 
#              fill = "white", shape = 21, stroke = 1) +
#   scale_y_continuous(
#     minor_breaks = scales::breaks_width(20),
#     name = "Mean Monthly Total Rainfall (mm)",
#     sec.axis = sec_axis(~ (. - 162.69) / 3.02,
#                         name = "Mean Monthly Daily Temperature (°C)",
#                         breaks = c(-10, 0, 10, 20, 30))) +
#   guides(x = "axis_nested",
#          y = guide_axis(minor.ticks=TRUE),
#          y.sec = guide_axis(minor.ticks=TRUE)) +
#   theme_bw(base_size = 18) +
#   theme(
#     panel.grid.minor = element_blank(),
#     axis.title.x = element_blank(),
#     axis.text.x = element_text(face = "bold", size = rel(0.5)),
#     axis.ticks = element_line(colour = "red"),
#     ggh4x.axis.nestline.x = element_line(linewidth = 0.6),
#     ggh4x.axis.nesttext.x = element_text(colour = "blue", 
#                                          face = "bold", 
#                                          size = rel(1.2))
#   )



##########################################################################################








########################################################################
# references
# https://www.statforbiology.com/2024/r_ggplot_dualscaledaxes/
  
