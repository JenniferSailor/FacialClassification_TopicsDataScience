# Since in the process for our PCA we vectorize the number of the pictures, it was
# hard to see which person was associated with that picture of number "number".
# So this function has the goal to help with that.
#
# PARAMS:
#        path -> the path where all the folders of the pictures is.
#        number -> the number associated with that picture.
# RETURN: 
#        This function returns the full name of the person in that picture number.

name_associated_w_pic_num<-function(path, number)
{ 
  # Grab all the paths and all the files in that path
  all_paths<-list.dirs(path = path, # i.e. "C:/Users/hhenr/Documents/test".
                       full.names = TRUE, recursive = FALSE)
  all_files<-list.files(all_paths)
  
  # Grab the first and last name associated with that picture.
  f_name <- unlist((strsplit(all_files[number], split = "_")))[1]
  l_name <- unlist((strsplit(all_files[number], split = "_")))[2]
  
  # Return a concatenated version of the names without the separation that comes
  # the file name.
  return(paste(f_name, l_name))
}

#i.e. use 
#full_name <- name_associated_w_pic_num("C:/Users/hhenr/Documents/test",1)