library(amt)
library(ggplot2)
library(move)


rFunction <-function(data){
  
  #make sure that there is a location.long and location.lat in data set
  coo <- data.frame(coordinates(data))
  names(coo) <- c("location.long","location.lat")
  data_df <- as.data.frame(data)
  names(data_df) <- make.names(names(data_df),allow_=FALSE)
  data_df <- data.frame(data_df,coo)

  
### Create track for each individual using function from *amt* package
  tr2 <-data_df %>% make_track(., .x=location.long, .y=location.lat, .t=timestamp, 
                               crs=sp::CRS("+init=epsg:4326"), 
                               Individual_id=trackId) #careful, this usually coincides with the animal ID, but not always... if you want the latter search in the data set for either "individual.local.identifier" or "local.identifier" (any of the two should be in about every data set), dont use tag.local.identifier as several animals can wear the same tag in succession or one animal can wear different tags after each other, and we do want info about animals..

    
  summary<-tr2 %>% nest(data= -"Individual_id") %>%  
    mutate(sr = map(data, summarize_sampling_rate)) %>% 
    dplyr::select(Individual_id , sr) %>% 
    unnest(cols=sr) 
  
  names(summary) <-c("Individual_id", "Min_fix_interval","Q1_fix_interval" ,"Median_fix_interval",
                     "Mean_fix_interval","Q3_fix_interval","Max_fix_interval","SD_fix_interval",
                     "Number_of_relocations", "Fix_interval_unit")
  
##I am dropping the first and third quartile of the fix interval here from the output, 
###they can also be included if required
  
#write.csv(summary[,-c(3,6)], file= "Summary_output.csv")
write.csv(summary[,-c(3,6)], file= paste0(Sys.getenv(x = "APP_ARTIFACTS_DIR", "/tmp/"),"Fix_interval_summary_output.csv"))
  
#### plotting the time individuals were radio collared
  plot.new()
  #pdf( "Time_summary.pdf")
  pdf(paste0(Sys.getenv(x = "APP_ARTIFACTS_DIR", "/tmp/"), "Time_summary.pdf"))
  summary_plot <-ggplot(data_df) +
    geom_point(aes(x = timestamp, y = as.factor(trackId))) +
    labs(x= "Time", y= "Individual_id")+
    theme(legend.position = "none")+
    theme_bw()
  
  print(summary_plot)
  dev.off()
}
