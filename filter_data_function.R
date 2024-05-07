# Function to filter our data
#
# PARAMS: path = where your folder with the database of faces is 
#         number_pics = minimun number of pics that you want in your new dataset
#         new_path = where you want your new dataset
filter_data <- function(path, number_pics, new_path=NULL, test=TRUE) 
{
  if(test){
    # Grab all folder paths under destination
    all_paths<-list.dirs(path = path, full.names = TRUE, recursive = FALSE)
    
    # Create empty list, contaning number of pics inside that path
    sizes<-c()
    # Loop through all the paths
    for (i in 1:length(all_paths)) {
      # Find the number of images in that path, and store in our list
      sizes<-c(sizes,length(list.files(all_paths[i])))
    }
    
    # Create a new list with only the number that are bigger than the specified 
    list_new_paths <- c()
    for (i in 1:length(all_paths)){
      if(sizes[i]>=number_pics)
        list_new_paths<-c(list_new_paths, all_paths[i])
    }
    
    # Create the new folders in the new location
    dir.create(new_path)
    file.copy(from=list_new_paths, to=new_path, 
              overwrite = TRUE, recursive = TRUE, 
              copy.mode = TRUE)
    return(list_new_paths)
  }
  else
  {
    # Grab all folder paths under destination
    all_paths<-list.dirs(path = path, full.names = TRUE, recursive = FALSE)
    
    # Create empty list, contaning number of pics inside that path
    sizes<-c()
    # Loop through all the paths
    for (i in length(all_paths)*0.8:length(all_paths)) {
      # Find the number of images in that path, and store in our list
      sizes<-c(sizes,length(list.files(all_paths[i])))
    }
    
    # Create a new list with only the number that are bigger than the specified 
    list_new_paths <- c()
    for (i in 1:length(all_paths)*0.8){
      if(sizes[i]>=number_pics)
        list_new_paths<-c(list_new_paths, all_paths[i])
    }
    
    # Create the new folders in the new location
    dir.create(new_path)
    file.copy(from=list_new_paths, to=new_path, 
              overwrite = TRUE, recursive = TRUE, 
              copy.mode = TRUE)
    return(list_new_paths)
    
  }
}