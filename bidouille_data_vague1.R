library(leaflet)
library(purrr)
library(stringr)
library(dplyr)
library(ggplot2)
library(readr)
library(skimr)
library(sf)
library(cartogram)
library(scales)

dfsongs <-  read_csv("./ANR_RECORDS/Data_Vague1/wave1_logs_songs_streamed_2014-2020.csv.gz")  
dfstreams <-  read_csv("./ANR_RECORDS/Data_Vague1/wave1_logs_streams_2014-2020.csv.gz")
dfartists <-  read_csv("./ANR_RECORDS/Data_Vague1/wave1_logs_tags_artists_streamed_2014-2020.csv.gz")

# set the dominant genre of artsits (genre of the highest genre tag score )
determine_dominant_genre <- function(dfartists){
  genredom_artist <- dfartists %>%  filter(tag_type=="genre") %>%  group_by(artist_id) %>% slice_max(., tag_score, with_ties=FALSE) %>% select(artist_id,tag_name)   
  names(genredom_artist)[names(genredom_artist)=="tag_name"] <-  "genredom"
  genredom_artist <- (genredom_artist %>% as.data.frame())
  #check if all artist are covered by genredom affectation 
  if(!(genredom_artist[,"artist_id"] %>% as_vector() ) %in% dfartists$artist_id %>%  all){
    warning("###Something went wrong with dominant genre determination\n")
  }
  dfartists <- dfartists %>%  left_join(genredom_artist) 
  return(dfartists)
}

#dominant genre affectation 
dfartists <-  determine_dominant_genre(dfartists)







Artist_Timeline_with_Genre <-  function(userID, dfstreams, dfartists, minimalListeningTime= 60 ){
  TL <- dfstreams %>% filter(hashed_id==userID & media_type=="song") %>%  arrange(datetime) %>% select(-media_type, -loc_country, -loc_long, -loc_lat)
  # join with songs
  TL <- TL %>% 
    filter(!is.na(media_id)) %>% 
    inner_join(select(dfsongs, -c(physical_release_date, digital_release_date, count)) , by="media_id")  
  # select vars
  TL <-  TL %>%  select( datetime, listening_time, media_duration, media_id, artist_id.x, song_title, artist_name, album_title)
  # minimal listening time
  TL <- TL %>% filter(listening_time> minimalListeningTime ) 

  #adding genre to songs
   # artists_and_genre <- dfartists %>%  filter(tag_type=="genre")  
  ATLG <-  left_join(TL, dfartists, by=c("artist_id.x" = "artist_id"))
  # keep only known genre
  # ATLG <-  ATLG %>%  filter(!(is.na(tag_name)))
  names(ATLG)[names(ATLG) == "artist_id.x"] <- "artist_id"
  return(ATLG)  
}



  
## genre map 
voronoi_of_genre <- function(ATLG, type="circle", remove_rare_prct= 0) {
  switch (type,
    "circle" = {envel <-  st_point(x=c(0,0))
                envel <- st_buffer(envel, dist= 1)
    },
    "square" = {envel <- st_as_sfc(st_bbox(c(xmin=0,xmax=1,ymin=0,ymax=1)))
    },
    "longlat" ={envel <- st_as_sfc(st_bbox(c(xmin=-180,xmax=180,ymin=-90,ymax=90)))
    }
   )
  
  genres <- ATLG$genredom %>% unique() %>% na.omit()
  weighted_genre_of_user <- ATLG  %>%  group_by(genredom) %>%  summarise(nbplays = n()) %>%  na.omit()
  if(remove_rare_prct > 0 ){
    threshold_plays <-  weighted_genre_of_user$nbplays %>%  quantile( . , probs= remove_rare_prct , names = F)
    weighted_genre_of_user <-  filter(weighted_genre_of_user, nbplays > threshold_plays)
    genres <- weighted_genre_of_user$genredom %>%  unique() %>%  na.omit()
  }
  
genre_pts <-  st_sample(envel, genres %>% length(),type = "random", exact = T)

#voronoi of points
vor <-  st_voronoi(st_union(genre_pts), envel)   
#transform into polygons sf 
vor <- st_cast(vor) %>% st_as_sf()
vor <-  st_intersection(vor, envel, tolerance=.1)
vor$genre <-  genres 
vor$weight <- weighted_genre_of_user$nbplays
return(vor)
}


dorling_cartogram_genres <- function(ATLG, voronoiGenre) {
  
  if (anyNA(voronoiGenre)) {
    warning("NA values in voronoi object")
  }
  voronoiGenre$log10weight <- log(voronoiGenre$weight, base = 10)
  
  
  # CRS has to be defined (anything projected)
  st_crs(voronoiGenre) <-  3857
  st_transform(voronoiGenre, 3857)
  # proportion occupée' par le genre le plus lu
  prop_du_plus_lu <-  max(voronoiGenre$weight) / sum(voronoiGenre$weight) * 100
  dorling_genre <-   cartogram_dorling(voronoiGenre, "weight", k = prop_du_plus_lu)
  #add centroids coords for later use
  centro_coords <-  st_centroid(dorling_genre) %>% st_coordinates() %>%  as.data.frame()
  dorling_genre$x_centroid <-  centro_coords$X
  dorling_genre$y_centroid <-  centro_coords$Y
  
  return(dorling_genre)
}


user <-  sample(dfstreams$hashed_id,1)
ATLG <-  Artist_Timeline_with_Genre(user, dfstreams, dfartists)
vg <-  voronoi_of_genre(ATLG,type = "circle",remove_rare_prct = 0.25)
dg <- dorling_cartogram_genres(ATLG, vg)




make_weight_breaks <- function(dg){
maxw <- max(dg$weight)
breaks <- c(1, 10, min(100,maxw), min(1000,maxw), min(2000,maxw) ,min(5000,maxw), min(10000,maxw),min(20000,maxw), min(40000,maxw) ) %>%  unique()
return(breaks)
}


my_breaks <-  make_weight_breaks(dg)

ggplot(dg)+
  geom_sf(aes(fill=weight))+
  geom_sf_text(aes(label=genre, size=weight), color="white")+
  scale_fill_gradient( trans="log10", breaks = my_breaks, labels = my_breaks)+
  guides(size=FALSE)+
  theme(
    plot.background = element_rect(fill = "gray10"),    # Background de l'ensemble du graphique
    panel.background = element_rect(fill = "gray10"),   # Arrière-plan de la zone de traçage
    panel.grid = element_blank()     ,    # Toutes les lignes de la grille
   
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text=element_blank(),
    
    legend.background= element_blank(),
    #
    legend.title = element_text(colour="gray90"),
    legend.text = element_text(colour="gray90")
  )



## variation plot on sphere 

ggplot(dg)+
  # stat_sf_coordinates()+
  # coord_map("ortho", orientation = c(39,-98,0))+
  geom_sf(aes(fill=weight))+
  coord_sf(crs=st_crs(3575))






generate_cartogramme_leaflet <- function(dorling_genre){
  maxw <- max(dorling_genre$weight)
  my_breaks <- c(1, 10, 100, 1000, min(10000,maxw),min(50000,maxw),min(100000,maxw) ) %>%  unique()
  
#add popup html text 
dorling_genre <- dorling_genre %>% mutate(popup_txt = str_c("<strong>", genre, "</strong>",
                       "<br/>",
                       weight," plays") %>% map(htmltools::HTML))

pal <- colorBin("viridis", domain = dorling_genre$weight, bins = my_breaks)

dorling_genre <- st_transform(dorling_genre, 4326)
st_crs(dorling_genre ) <- 4326
mumu <- dorling_genre %>% 
leaflet(elementId = "dorlingGenres") %>%
  addPolygons(fillColor = ~pal(weight),
              color="gray",
              label= ~popup_txt,
              weight = .1,
              smoothFactor = 0.5,
              opacity = 1.0,
              fillOpacity = 0.7,
              group="# of plays",
              highlightOptions = highlightOptions(color = "white",
                                                  weight = 2,
                                                  bringToFront  = TRUE),
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = ~log10weight,
                direction = "auto")
            
              ) %>%
    addLegend(pal = pal,
          values = ~n,
          opacity = 0.7,
          title = "Number of Plays",
          position = "bottomright")


return(mumu)
}




lg <- generate_cartogramme_leaflet(dg) 
dorling_genre <- dg

dg$HTMLgenre <-  dg$genre  %>% str_replace(., pattern = "_", replacement = "<br>") %>%  str_c("<center>",.,"</center>" )  %>%  map(htmltools::HTML)


dg$HTMLgenre
lg
addLabelOnlyMarkers(lg, data=dg,
                        lng=~x_centroid, lat=~y_centroid,
                        group = "Genres",
                        layerId = "GenresLabels",
                        label = ~HTMLgenre,
                        labelOptions = labelOptions(noHide = TRUE, direction = 'center',
                                                    textsize = "15px",
                                                    textOnly = T, permanent = T 
                        ))


add_genre_labels <- function(leafletObject, dorling_genre){

   #font size computation
  #dirty ad hoc scaling 
  bins <- 2* (dorling_genre$weight %>%  log10 %>% quantile() %>%  round + 1)^1.5 %>%  round   
  #scale to [2-26]
  bins <- rescale(bins, to=c(2,26)) %>% round
  labelssizes <- paste0(bins,"px")
  
  probasQuantiles <-  seq(0,1, length.out = length(bins) + 1)
dorling_genre$quartile_weight <-   cut(dorling_genre$log10weight,breaks=quantile(dorling_genre$log10weight,probasQuantiles,names = FALSE),labels =labelssizes  ,include.lowest = T)

  # formatting labels 
  dorling_genre$HTMLgenre <-  dorling_genre$genre  %>% str_replace(., pattern = "_", replacement = "<br>") %>%  str_c("<center>",.,"</center>" ) %>%  map(htmltools::HTML)



#split into many layers , one size of textlabel for each quantile
quantiles_layers <- dorling_genre %>%  split(.,.$quartile_weight)
names(quantiles_layers) %>%
  purrr::walk(function(current_split) {
    leafletObject<<-leafletObject %>% #seems like there's supposed to be two carrots here, i had problems without one (cf stack overflow)
      addLabelOnlyMarkers(data=quantiles_layers[[current_split]],
                       lng=~x_centroid, lat=~y_centroid,
                       group = "Genres",
                       label = ~HTMLgenre,
                       labelOptions = labelOptions(noHide = TRUE, direction = 'center',
                                                 textsize = current_split,
                                                 textOnly = T
                                                 ))
    
  })
leafletObject <- leafletObject %>% addLayersControl(overlayGroups = c("# of plays", "Genres"),
                     options = layersControlOptions(collapsed = FALSE)) 

return(leafletObject)
}


make_one_cartogram <- function(){
user <-  sample(dfstreams$hashed_id,1)
ATLG <-  Artist_Timeline_with_Genre(user, dfstreams, dfartists)
voronoiGenre <-  voronoi_of_genre(ATLG,type = "circle",remove_rare_prct = 0.25)
dorling_genre <- dorling_cartogram_genres(ATLG, voronoiGenre)
cartogram_leaflet <-  generate_cartogramme_leaflet(dorling_genre)
cartogram_leaflet <-  add_genre_labels(cartogram_leaflet, dorling_genre)
return(cartogram_leaflet)
}



make_one_cartogram()


user <-  sample(dfstreams$hashed_id,1)
ATLG <-  Artist_Timeline_with_Genre(user, dfstreams, dfartists,minimalListeningTime = 60)






# artists circles in genre circles by number of plays 
voronoi_of_artist <- function(ATLG,  circle_of_genre,   remove_rare_prct= 0) {
  g <-  circle_of_genre$tag_name
  artists_of_genre  <- ATLG %>%filter(tag_type=="genre" & tag_name==g) %>%select(artist_id)  %>% unique() %>% na.omit()
  weighted_artist_of_user <- ATLG %>% filter(artist_id %in% artists_of_genre$artist_id) %>%  group_by(artist_id) %>%  summarise(nbplays = n())
    if(remove_rare_prct > 0 ){
      threshold_plays <-  weighted_artist_of_user$nbplays %>%  quantile( . , probs= remove_rare_prct , names = F)
    weighted_artist_of_user <-  filter(weighted_artist_of_user, nbplays > threshold_plays)
  }
  
  artists_pts <-  st_sample(circle_of_genre,  weighted_artist_of_user%>% nrow(),type = "random", exact = T)
  
  #voronoi of points
  vor <-  st_voronoi(st_union(artists_pts), st_geometry(circle_of_genre))   
  #transform into polygons sf 
  vor <- st_cast(vor) %>% st_as_sf()
  vor <-  st_intersection(vor, circle_of_genre, tolerance=.1)
  vor$genredom <-  g 
  vor$nbplays <-  weighted_artist_of_user$nbplays
  vor$artist_id  <-  weighted_artist_of_user$artist_id
  return(vor)
}



dorling_cartogram_artist <- function(ATLG, voronoiArtist) {
  # weighting genre by number of plays 
  g <-  voronoiArtist$genredom[1]
  artists_of_genre  <- ATLG %>%filter(tag_type=="genre" , tag_name==g) %>%select(artist_id, artist_name)  %>% unique() %>% na.omit()
  weighted_artist_of_user <- ATLG %>% filter(artist_id %in% voronoiArtist$artist_id) %>%  group_by(artist_id) %>%  summarise(nbplays = n())
  if (anyNA(weighted_artist_of_user$nbplays)) {
    warning("NA values in number of plays by genre")
  }
  #affect voronoi polys with weight of artisit in this genre
  voronoiArtist$artist_weight <-  weighted_artist_of_user$nbplays
  voronoiArtist$artist_log10weight <- log(voronoiArtist$weight, base = 10)
  #  Dorling Cartogramm
  # CRS has to be defined (anything projected)
  st_crs(voronoiArtist) <-  3857
  st_transform(voronoiArtist, 3857)
  # proportion occupée' par le genre le plus lu
  prop_du_plus_lu <-  max(voronoiArtist$artist_weight) / sum(voronoiArtist$artist_weight) * 100
  dorling_artist <-   cartogram_dorling(voronoiArtist, "artist_weight", k = prop_du_plus_lu,)
  #add centroids coords for later use
  centro_coords <-  st_centroid(dorling_artist) %>% st_coordinates() %>%  as.data.frame()
  dorling_artist$x_centroid <-  centro_coords$X
  dorling_artist$y_centroid <-  centro_coords$Y
  dorling_artist$artist_name <- artists_of_genre[match(dorling_artist$artist_id, artists_of_genre$artist_id), "artist_name"] 
  return(dorling_artist)
}




add_artist_dorling <- function(leaflet_genre , drlng_artist){
  maxw <- max(drlng_artist$artist_weight)
  my_breaks <- c(1, 10, min(100,maxw), min(1000,maxw), min(2000,maxw) ,min(5000,maxw), min(10000,maxw),min(20000,maxw), min(30000,maxw),min(40000,maxw) ) %>%  unique()
  
  #add popup html text 
  drlng_artist <- drlng_artist %>% mutate(popup_txt = str_c("<strong>", artist_name, "</strong>",
                                                              "<br/>",
                                                              artist_weight," plays") %>% map(htmltools::HTML))
  
  pal <- colorBin("magma", domain = drlng_artist$artist_weight, bins = my_breaks)
  
  mumu <- leaflet_genre %>%
    addPolygons(data= drlng_artist , 
                fillColor = ~pal(artist_weight),
                color="gray",
                label= ~popup_txt,
                weight = .1,
                smoothFactor = 0.5,
                opacity = 1.0,
                fillOpacity = 0.7,
                group="# of plays",
                highlightOptions = highlightOptions(color = "white",
                                                    weight = 2,
                                                    bringToFront  = TRUE),
                labelOptions = labelOptions(
                  style = list("font-weight" = "normal", padding = "3px 8px"),
                  textsize = ~artist_log10weight,
                  direction = "auto")
                
    ) %>%
      addLegend(pal = pal,
              values = ~n,
              opacity = 0.7,
              title = "Number of Plays",
              position = "bottomright")
  return(mumu)
}

 


leaflet_genre_cartogram <- function(){
  user <-  sample(dfstreams$hashed_id,1)
  ATLG <-  Artist_Timeline_with_Genre(user, dfstreams, dfartists)
  voronoiGenre <-  voronoi_of_genre(ATLG,type = "circle",remove_rare_prct = 0.25)
  dorling_genre <- dorling_cartogram_genres(ATLG, voronoiGenre)
  cartogram_leaflet <-  generate_cartogramme_leaflet(dorling_genre)
  cartogram_leaflet <-  add_genre_labels(cartogram_leaflet, dorling_genre)
  return(cartogram_leaflet)
}



leaflet_artist_cartogram <- function(){
  user <-  sample(dfstreams$hashed_id,1)
  ATLG <-  Artist_Timeline_with_Genre(user, dfstreams, dfartists)
  voronoiGenre <-  voronoi_of_genre(ATLG,type = "circle",remove_rare_prct = 0.25)
  circlegenre <-  sample_n(voronoiGenre,1)
  va <- voronoi_of_artist(ATLG,circlegenre,remove_rare_prct = 0.25)
  da <- dorling_cartogram_artist(ATLG, va)
  cartogram_leaflet <-  generate_cartogramme_leaflet()
  cartogram_leaflet <-  add_genre_labels(cartogram_leaflet, dorling_genre)
  return(cartogram_leaflet)
}

user <-  sample(dfstreams$hashed_id,1)
ATLG <-  Artist_Timeline_with_Genre(user, dfstreams, dfartists)
voronoiGenre <-  voronoi_of_genre(ATLG,type = "circle",remove_rare_prct = 0.25)
circlegenre <-  sample_n(voronoiGenre,1)
va <- voronoi_of_artist(ATLG,circlegenre,remove_rare_prct = 0.25)
da <- dorling_cartogram_artist(ATLG, va)

va$




dodo <- dorling_cartogram_artist(ATLG, voronoiArtist)
dodo %>% mutate(popup_txt = str_c("<strong>", artist_name, "</strong>",
                                  "<br/>",
                                  artist_weight," plays") %>% map(htmltools::HTML))




#marche bien 
tutu <-  make_one_cartogram()

mumu <- add_artist_dorling(tutu, dorling_artists)

mumu


for (dt in dts[1:20]){
  current_artist <- ATLG %>% filter(datetime==dt) %>% select(artist_id.x,artist_name) %>% unique() 
  names(current_artist) <- c("artist_id","artist_name")
  genredom <- dfartists %>% filter(artist_id == current_artist$artist_id) %>% select(genredom) %>% unique() %>% as.character()
  
  cat(dt,"   ",current_artist$artist_name, "   genre dominant",genredom,"\n")
  
  cercleGenre  <-  match(genredom ,dorling_genre$tag_name)
  cat("   cercle du cartogramme numéro ", cercleGenre)
  
  pt <-  st_sample(dorling_genre[cercleGenre, ],1) 
  pt <-  st_sf(pt)
  pt$artist_id <- current_artist$artist_id
  pt$artist_name <-  current_artist$artist_name  
  cat("\n")  
}





# my_breaks  <- c(1, 10, 100, 1000, 5000)
# ggplot(voronoiGenre)+
#   geom_sf(aes(fill=weight))+
#   geom_sf_label(aes(label=tag_name),label.padding =  unit(0.1, "lines"), label.size = 0,size=2)+
#   scale_fill_gradient( trans="log10", breaks = my_breaks, labels = my_breaks)+
#   theme_void()



###################################################"
#tangent circles 

xc1 <- 0
yc1 <- 0
center1  <-  st_point(x=c(xc1,yc1))
radius1 <- dorling_genre %>% as.data.frame() %>% arrange(weight %>% desc) %>% pull(weight) %>% nth(1)
circle1 <- st_buffer(center1, dist= radius1)

radius2 <-  dorling_genre %>% as.data.frame() %>% arrange(weight %>% desc) %>% pull(weight) %>% nth(2)

angle <- runif(1,0,2*pi)
xc2 <-   cos(angle)*(radius1+radius2) 
yc2 <-   sin(angle)*(radius1 +radius2)
center2 <-  st_point(x=c(xc2,yc2))
circle2 <-  st_buffer(center2, dist=radius2)



tangent_circle_center <- function(center1, center2, radius1, radius2, radius3 ){
radius3 <-  dorling_genre %>% as.data.frame() %>% arrange(weight %>% desc) %>% pull(weight) %>% nth(3)
circle11 <-  st_buffer(center1, dist = radius1 +radius3)
circle22 <-  st_buffer(center2, dist = radius2 +radius3)
centres_possibles <-  st_intersection(st_boundary(circle11), st_boundary(circle22))  %>% st_sfc() %>% st_cast("POINT")
center3 <- centres_possibles %>%  sample(1)
return(center3)
}


radius3 <-  dorling_genre %>% as.data.frame() %>% arrange(weight %>% desc) %>% pull(weight) %>% nth(3)
center3 <-  tangent_circle_center(center1 , center2 ,radius1 , radius2 , radius3)
circle3 <-  st_buffer(center3, dist=radius3)



dfcenters <-  center1 %>% st_sfc() %>% st_sf()
dfcenters <-  rbind(dfcenters,center2 %>% st_sfc() %>% st_sf())
dfcenters <-  rbind(dfcenters,center3 %>% st_sfc() %>% st_sf())


dfcircles <- circle1 %>% st_sfc() %>% st_sf()
dfcircles <-  rbind(dfcircles, circle2 %>%  st_sfc() %>% st_sf)
dfcircles <-  rbind(dfcircles, circle3 %>%  st_sfc() %>% st_sf)


plot(dfcircles)


rank=4

next_circle <-  function(rank, dorling_genre, dfcircles, radiuses){
radius <-  radiuses %>% nth(rank)
radius1 <-  radiuses %>% nth(rank-2)
radius2<-  radiuses %>% nth(rank-1)

circle11 <-  st_buffer(dfcircles[rank-1,], dist = radius3)
circle22 <-  st_buffer(dfcircles[rank-2,], dist = radius3)
centres_possibles <-  st_intersection(st_boundary(circle11), st_boundary(circle22))  %>%  st_cast("POINT")
centroid <- st_centroid(st_union(dfcircles))
idxnearest <- st_nearest_feature(centroid, centres_possibles)
center3 <- centres_possibles[-idxnearest,]
circle <-  st_buffer(center3, dist=radius)
return(circle)
}

radiuses <-dorling_genre %>% as.data.frame() %>% arrange(weight %>% desc) %>% pull(weight)   
idxcicrcles <-  seq(from=4, to=length(radiuses))

for( i in idxcicrcles){
  cat(i )
  c <-  next_circle(i, dorling_genre, dfcircles, radiuses)
  dfcircles <- rbind(dfcircles,c)
  }


c <-  next_circle(6, dorling_genre, dfcircles, radiuses)
dfcircles <- rbind(dfcircles,c)
plot(dfcircles)


plot(dfcircles)
plot(C4, add=T)


# idée mattia : diagramme de venn -> anamorphose sur le diagramme 

