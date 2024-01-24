
library(tidyverse)
theme_minimal()
## Themes
{
  theme_minimal_btt<-function(){
    theme_minimal()+
      theme(
        panel.grid = element_blank(),
        axis.text = element_text(color="black",size=12),
        text = element_text(family = font_tw),
        title = element_text(size=16,face="bold"),
        axis.title = element_text(size=14,face="bold"),
        legend.text = element_text(color="black",size=12),
        strip.text = element_text(family = font_tw,size = 14,color = "black",face = "bold")
      )
  }
  
  
theme_minimal_tt<-function(){
    theme_minimal()+
      theme(
        axis.text = element_text(color="black",size=12),
        text = element_text(family = font_tw),
        title = element_text(size=16,face="bold"),
        axis.title = element_text(size=14,face="bold"),
        legend.text = element_text(color="black",size=12),
        strip.text = element_text(family = font_tw,size = 14,color = "black",face = "bold")
      )
}


theme_tt<-function(){
  theme(
    axis.text = element_text(color="black",size=12),
    text = element_text(family = font_tw),
    title = element_text(size=16,face="bold"),
    axis.title = element_text(size=14,face="bold"),
    legend.text = element_text(color="black",size=12),
    strip.text = element_text(family = font_tw,size = 14,color = "black",face = "bold")
  )
}

theme_minimal_bttx<-function(){
  theme_minimal()+
    theme(
      panel.grid = element_blank(),
      axis.text = element_text(color="black",size=12),
      axis.text.x = element_text(angle = 45),
      text = element_text(family = font_tw),
      title = element_text(size=16,face="bold"),
      axis.title = element_text(size=14,face="bold"),
      legend.text = element_text(color="black",size=12),
      strip.text = element_text(family = font_tw,size = 14,color = "black",face = "bold")
    )
}


theme_minimal_ttx<-function(){
  theme_minimal()+
    theme(
      axis.text = element_text(color="black",size=12),
      axis.text.x = element_text(angle = 45),
      text = element_text(family = font_tw),
      title = element_text(size=16,face="bold"),
      axis.title = element_text(size=14,face="bold"),
      legend.text = element_text(color="black",size=12),
      strip.text = element_text(family = font_tw,size = 14,color = "black",face = "bold")
    )
}

laranja_sarto<-function() "#F88A27"
  
}

##functions
{
  loc.label<-function(labels,groups=1){
    if(!is.numeric(labels)){stop("labels must be numeric")}
    dt<-data.frame(
      a=labels,
      b=groups
    )
    dt %>% 
      group_by(b) %>% 
      mutate(a=1-(cumsum(a)-a/2)) %>% 
      ungroup() %>% 
      select(a) %>% 
      as.matrix() %>% 
      as.vector() %>% 
      return()
  }
  
  get_middle<-function(cutted){
    lapply(cutted, function(x){
      as.numeric(str_split(str_remove_all(x,"\\(|\\]"),pattern = ",",simplify = T)) %>% 
        mean()
    }) %>% unlist()
  }
  
  wd<-function(){
      if(.Platform$GUI=="RStudio"){
        library(rstudioapi)
        if(getActiveDocumentContext()$path==""){
          setwd(enc2utf8(selectDirectory(caption = "Select WD")))
        }else{
          getActiveDocumentContext()$path %>%
            dirname() %>%
            setwd()
        }
      }else{
        setwd(tcltk::tk_choose.dir(caption = "Select WD"))
      }
  }
  
  
  flextheme<-function(x){
    x %>% 
      font(fontname = "Montserrat",part="all") %>% 
      fontsize(size=16,part = "header") %>% 
      fontsize(size=14,part="body") %>% 
      bold(part="header") %>% 
      align(align = "center",part="all") %>% 
      bg(bg="#00BFC4",part="header") %>% 
      return()
  }
  
  
  make_gif<-function(plots,path,delay=2){
    images<-lapply(plots, function(x){
      file<-tempfile(fileext=".jpg")
      ggsave(file,plot=x,width = 3200,height = 2300,units = "px",dpi=300)
      return(magick::image_read(file))
    })
    images<-magick::image_join(images)
    
    invisible(magick::image_write_gif(images,path=path,delay=delay))
    return()
  }
  
  get_TM<-function(gtfs_obj){
    shapes_as_sf(gtfs_obj$shapes,crs=4326) %>% 
      mutate(dist=sf::st_length(geometry)) %>% 
      left_join(
        gtfs_obj$trips %>% 
          group_by(shape_id) %>% 
          reframe(n=n()),by="shape_id"
      ) %>% 
      mutate(dist_tot=dist*n) %>% 
      .$dist_tot %>% 
      units::set_units("km") %>% 
      sum(na.rm = T)
  }
  
  set_frequency<-function(
    gtfs_obj,
    line_id,
    initial_time,
    frequency
  ){
    
    headway<-1/frequency*60
    trips<-gtfs_obj$trips %>% 
      dplyr::filter(route_id==line_id) %>% 
      dplyr::select(trip_id) %>% 
      as.matrix() %>% as.vector() 
    
    trips_depart<-gtfs_obj$stop_times %>% 
      dplyr::filter(trip_id %in% trips) %>% 
      dplyr::filter(!is.na(departure_time)) %>% 
      dplyr::mutate(departure_time = as.POSIXct( as.character(departure_time), format = "%H:%M:%S" ) ) %>% 
      dplyr::group_by(trip_id) %>% 
      dplyr::reframe(departure_time = min( departure_time , na.rm = T))
    
    trips<-trips_depart %>% 
      dplyr::left_join(gtfs_obj$trips,by="trip_id") %>% 
      dplyr::filter(departure_time >= (initial_time+headway*60) ) %>% 
      dplyr::arrange(departure_time) %>% 
      dplyr::group_by(direction_id) %>% 
      dplyr::mutate(new_depart=seq(min(departure_time),min(departure_time)+headway*60*length(departure_time)-1,headway*60)) %>% 
      dplyr::mutate(diff = as.numeric(new_depart - departure_time)) %>% 
      dplyr::ungroup()
    
    gtfs_obj$stop_times<-dplyr::bind_rows(
      gtfs_obj$stop_times %>% 
        dplyr::filter( (trip_id %in% trips$trip_id ) == FALSE),
      gtfs_obj$stop_times %>% 
        dplyr::filter( trip_id %in% trips$trip_id ) %>% 
        dplyr::left_join(trips %>% dplyr::select(trip_id,diff),by="trip_id") %>% 
        dplyr::mutate(arrival_time = arrival_time+diff,
                      departure_time = departure_time+diff) %>% 
        dplyr::select(-diff)
    )
    
    return(gtfs_obj)
    
  }
  
  
  exec_fun<-function(x,.fun){
    .fun(x)
  }
  
}

  
