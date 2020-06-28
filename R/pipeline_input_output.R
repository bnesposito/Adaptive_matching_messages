library(dplyr)
library(readr)
library(gert) # for git
# https://github.com/r-lib/gert

# The following is necessary for making parallel MCMC work in RStudio
parallel:::setDefaultClusterOptions(setup_strategy = "sequential")

# sourcing code for simulating from posterior and calculating optimal matching
source("matching_functions.R")

# read in qualtrics output files and merge them into consolidated and cleaned file
prior_data_merge = function() {
    output_filenames = list.files("../Pipeline/Qualtrics_output/")
    
    output_filepaths = paste("../Pipeline/Qualtrics_output/", 
                             output_filenames , sep="")
    
    # Reading in all files in output folder 
    qualtrics_output = output_filepaths %>% 
        map(read_csv) %>% 
        map(~ slice(.x, 3:n())) # dropping the first 2 rows
        
    # create variable with source filename
    for (i in 1:length(output_filenames)) {
        qualtrics_output[[i]]$sourcefile = output_filenames[1]
    }
    
    qualtrics_output=
        qualtrics_output %>%         
        bind_rows() 
    
    # calculate outcome variable as sum of scores
    Y=qualtrics_output %>% 
        select(paste("Q", 101:113, sep=""))%>% 
        sapply(as.numeric) %>% 
        rowSums()
        
    qualtrics_output= qualtrics_output %>% 
        mutate(Y=Y) %>% 
        select(-one_of(paste("Q", 101:113, sep="")))
        
    qualtrics_output %>% 
    write_csv(paste("../Pipeline/Match_files/",
                    Sys.Date(), 
                    "_merged_processed_output", ".csv", sep = "" ))    
}




# read in prior data, run thompson sampling, and store result in daily match file
prior_data_to_match = function(prior_data_file="thompson_test.csv"){
    # number of types
    k=4 
    
    # read in prior outcome data
    prior_data = read_csv(paste("../Pipeline/Match_files/", 
                                 prior_data_file, sep = "" )) %>% 
        mutate(U=factor(U, levels=1:k), 
               V=factor(V, levels=1:k))
    
    # generate types for new wave
    U= tibble(U=factor(rep(1:k, length.out = k^2, each=k), levels=1:k))
    V= tibble(V=factor(rep(1:k, length.out = k^2), levels=1:k))
    
    # calculate thompson matching for the next wave
    best_matching = thompson_matching(prior_data, U, V)
    
    # write to dated file with new matching
    write_csv2(best_matching$matching,
                paste("../Pipeline/Match_files/", 
                      Sys.Date(), "_matching.csv", sep = "" ))
}



# read in daily match file, export recipient types to sender folders
match_to_sender_surveys = function(){
    
}


# read in message file, export sender types and messages to recipient folders
messages_to_recipient_surveys = function(){
    
}




# commit and push all files in Qualtrics_input folder to Github
update_github = function(repo = "../") {
    git_add(files = "*", repo = repo)
    
    git_commit(repo = repo,
               message = paste0("Update as at: ", Sys.time()))
    
    # this uses my stored ssh key
    git_push(repo = repo)
}
