library(dplyr)
library(readr)
library(gert) # for git
# https://github.com/r-lib/gert

# sourcing code for simulating from posterior and calculating optimal matching
source("matching_functions.R")


# read in prior data, run thompson sampling, and store result in daily match file
prior_data_to_match = function(prior_data_file="thompson_test.csv"){
    # number of types
    k=4 
    
    # read in prior outcome data
    prior_data = read_csv2(paste("../Pipeline/Qualtrics_output/", 
                                 prior_data_file, sep = "" )) %>% 
        mutate(U=factor(U, levels=1:k), 
               V=factor(V, levels=1:k))
    
    # generate types for new wave
    U= tibble(factor(rep(1:k, length.out = k, each=k), levels=1:k))
    V= tibble(factor(rep(1:k, length.out = k), levels=1:k))
    
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
update_github = function(repo = "../../") {
    git_add(files = "*", repo = repo)
    
    git_commit(repo = repo,
               message = paste0("Update as at: ", Sys.time()))
    
    # this uses my stored ssh key
    git_push(repo = repo)
}
