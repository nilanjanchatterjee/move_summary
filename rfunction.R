library(amt)
library(ggplot2)
library(move)


rFunction <-function(data){
  data_df <- as.data.frame(data)
  
### Create track for each individual using function from *amt* package
  tr2 <-data_df %>% make_track(., .x=location_long, .y=location_lat, .t=timestamp, 
                               crs=sp::CRS("+init=epsg:4326"), 
                               Individual_id=tag_local_identifier)
  
  summary<-tr2 %>% nest(data= -"Individual_id") %>%  
    mutate(sr = map(data, summarize_sampling_rate)) %>% 
    dplyr::select(Individual_id , sr) %>% 
    unnest(cols=sr) 
  
##I am dropping the first and third quartile of the fix interval here from the output, 
###they can also be included if required
  
#write.csv(summary[,-c(3,6)], file= "Summary_output.csv")
write.csv(summary[,-c(3,6)], file= paste0(Sys.getenv(x = "APP_ARTIFACTS_DIR", "/tmp/"),"Summary_output.csv"))
  
#### plotting the time individuals were radio collared
  plot.new()
  #pdf( "Time_summary.pdf")
  pdf(paste0(Sys.getenv(x = "APP_ARTIFACTS_DIR", "/tmp/"), "Time_summary.pdf"))
  summary_plot <-ggplot(data_df) +
    geom_point(aes(x = timestamp, y = as.factor(tag_local_identifier))) +
    labs(x= "Time", y= "Individual_id")+
    theme(legend.position = "none")+
    theme_bw()
  
  print(summary_plot)
  dev.off()
}
