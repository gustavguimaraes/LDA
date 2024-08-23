
library(tidyverse)
theme_minimal()
## Themes
{
  theme_minimal_btt<-function(sz=16){
    theme_minimal()+
      theme(
        panel.grid = element_blank(),
        axis.text = element_text(color="black",size=round(0.75*sz,0)),
        text = element_text(family = font_tw),
        title = element_text(size=sz,face="bold"),
        axis.title = element_text(size=round(0.875*sz,0),face="bold"),
        legend.text = element_text(color="black",size=round(0.75*sz,0)),
        strip.text = element_text(family = font_tw,size = round(0.875*sz,0),color = "black",face = "bold")
      )
  }
  
  
theme_minimal_tt<-function(sz=16){
    theme_minimal()+
      theme(
        axis.text = element_text(color="black",size=round(0.75*sz,0)),
        text = element_text(family = font_tw),
        title = element_text(size=sz,face="bold"),
        axis.title = element_text(size=round(0.875*sz,0),face="bold"),
        legend.text = element_text(color="black",size=round(0.75*sz,0)),
        strip.text = element_text(family = font_tw,size = round(0.875*sz,0),color = "black",face = "bold")
      )
}

font_tw <-  'Titillium Web'

theme_tt<-function(sz=16){
  theme(
    axis.text = element_text(color="black",size=round(0.75*sz,0)),
    text = element_text(family = font_tw),
    title = element_text(size=sz,face="bold"),
    axis.title = element_text(size=round(0.875*sz,0),face="bold"),
    legend.text = element_text(color="black",size=round(0.75*sz,0)),
    strip.text = element_text(family = font_tw,size = round(0.875*sz,0),color = "black",face = "bold")
  )
}

theme_minimal_bttx<-function(sz=16){
  theme_minimal()+
    theme(
      panel.grid = element_blank(),
      axis.text = element_text(color="black",size=round(0.75*sz,0)),
      axis.text.x = element_text(angle = 45),
      text = element_text(family = font_tw),
      title = element_text(size=sz,face="bold"),
      axis.title = element_text(size=round(0.875*sz,0),face="bold"),
      legend.text = element_text(color="black",size=round(0.75*sz,0)),
      strip.text = element_text(family = font_tw,size = round(0.875*sz,0),color = "black",face = "bold")
    )
}


theme_minimal_ttx<-function(sz=16){
  theme_minimal()+
    theme(
      axis.text = element_text(color="black",size=round(0.75*sz,0)),
      axis.text.x = element_text(angle = 45),
      text = element_text(family = font_tw),
      title = element_text(size=sz,face="bold"),
      axis.title = element_text(size=round(0.875*sz,0),face="bold"),
      legend.text = element_text(color="black",size=round(0.75*sz,0)),
      strip.text = element_text(family = font_tw,size = round(0.875*sz,0),color = "black",face = "bold")
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
        getActiveDocumentContext()$path %>%
            dirname() %>%
            setwd()
      }else{
        #setwd(tcltk::tk_choose.dir(caption = "Select WD"))
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
  
  
  make_plot_gif<-function(plots,path,delay=2){
    images<-lapply(plots, function(x){
      file<-tempfile(fileext=".jpg")
      ggsave(file,plot=x,width = 3200,height = 2300,units = "px",dpi=300)
      return(magick::image_read(file))
    })
    images<-magick::image_join(images)
    
    invisible(magick::image_write_gif(images,path=path,delay=delay))
    return()
  }
  
  make_gif<-function(image_paths,out_path=paste0(getwd(),"/gif.gif"),delay=1,density=NULL){
    lapply(image_paths, magick::image_read,density=density) %>% 
      magick::image_join() %>% 
      magick::image_write_gif(path = out_path,delay=delay)
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


file2clip <- function(fpath){
  if(!file.exists(fpath)){
    stop(paste("The file",fpath,"doesn't exist."))
  }else{
    fpath<-suppressWarnings(try(normalizePath(fpath)))
    system(
      paste0(
        'powershell "Set-Clipboard -Path \'',
        fpath,
        '\'"'
      ),
      intern = T,minimized = F
    )
  }
  
  
}


zap_message <- function( text = '', phone_number = NULL,
                         file = NULL){
  
  if(length(text)>1){
    warning("Text string with a length greater than 1, using the first item.")
    text<-text[1]
  }
  if(is.null(phone_number)){
    stop("Invalid number")
  }else{
    script<-paste0(
      'start whatsapp://"send?phone=',
      phone_number,
      "&text=",
      str_replace_all(text," ","%20"),
      '"'
    )
    if(is.null(file)){
      shell(script)
      Sys.sleep(2)
      KeyboardSimulator::keybd.press("enter")
    }else{
      shell(script)
      Sys.sleep(1.5)
      file2clip(file)
      KeyboardSimulator::keybd.press("ctrl",hold = T)
      KeyboardSimulator::keybd.type_string("v")
      KeyboardSimulator::keybd.release("ctrl")
      Sys.sleep(2)
      KeyboardSimulator::keybd.press("enter")
    }
    
  }
  
  
}


Filter2 <- function(x,f,...){
  x[f(x,...)]
}

  
