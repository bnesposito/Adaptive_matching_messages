library(dplyr)
library(readr)
library(gert) # for git
# https://github.com/r-lib/gert

# The following is necessary for making parallel MCMC work in RStudio
parallel:::setDefaultClusterOptions(setup_strategy = "sequential")

# sourcing code for simulating from posterior and calculating optimal matching
source("matching_functions.R")

# type codes to construct types U and V
type_codes = tibble(
    gender = c("F","F","M","M"),
    country = c("IN", "US","IN", "US"),
    string = c("Indian woman", "American woman", "Indian man", "American man"),
    U= 1:4,
    V=1:4
)

# read in qualtrics output files and merge them into consolidated and cleaned file
prior_data_merge = function() {
    output_filenames = list.files("../Pipeline/Qualtrics_output/")
    
    output_filepaths = paste("../Pipeline/Qualtrics_output/", 
                             output_filenames , sep="")
    
    # Reading in all files in output folder 
    qualtrics_output = output_filepaths %>% 
        map(read_csv) %>% 
        map(~ slice(.x, 3:n())) # dropping the first 2 rows
        
    # create variables with source filename, gender, country, side of match
    for (i in 1:length(output_filenames)) {
        qualtrics_output[[i]]$sourcefile = output_filenames[i]
        qualtrics_output[[i]]$gender = substr(output_filenames[i],1,1)
        qualtrics_output[[i]]$country = substr(output_filenames[i],3,4)
        qualtrics_output[[i]]$match_side = substr(output_filenames[i],6,6)
    }
    
    # which files correspond to senders?
    sender = (map(output_filenames, substr, 6, 6)==1)
    
    # export merged senders file  
    qualtrics_output[sender] %>%         
        bind_rows() %>% 
        left_join(type_codes,by = c("gender", "country")) %>% # merge in type to construct U and V 
        write_csv(paste("../Pipeline/Match_files/",
                        Sys.Date(), 
                        "_merged_processed_output_senders", ".csv", sep = "" ))  
    
    qualtrics_output_receiver = 
        qualtrics_output[!sender] %>% 
        bind_rows() %>% 
        left_join(type_codes,by = c("gender", "country")) # merge in type to construct U and V 
        
        
    # calculate outcome variable as sum of scores for recipients
    Y=qualtrics_output_receiver %>% 
        select(paste("Q", 101:113, sep=""))%>% 
        sapply(as.numeric) %>% 
        rowSums()
    
    # export merged receivers file    
    qualtrics_output_receiver %>% 
        mutate(Y=Y) %>% 
        write_csv(paste("../Pipeline/Match_files/",
                        Sys.Date(), 
                        "_merged_processed_output_receivers", ".csv", sep = "" ))  
    
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
    write_csv(best_matching$matching,
                paste("../Pipeline/Match_files/", 
                      Sys.Date(), "_matching.csv", sep = "" ))
}



# read in daily match file, export recipient types to sender folders
# index is current running index for Qualtrics for each of the 4 types 
match_to_sender_surveys = function(index = c(0,0,0,0)){
    matching = paste("../Pipeline/Match_files/", 
          Sys.Date(), "_matching.csv", sep = "" ) %>% 
        read_csv() %>% 
        left_join(type_codes[-5],by = "U")
    
    for (i in 1:nrow(matching)) {
        # update running index for each of the sender types
        index[matching[[i, "U"]]] = index[matching[[i, "U"]]] +1
        
        recipient_path = paste("../Pipeline/Qualtrics_input/", 
                               matching[i, "gender"], "-",
                               matching[i, "country"],
                               "-1/", index[matching[[i, "U"]]],
                               "_recipient.txt", sep="")

        write(matching[[i,"string"]], recipient_path)
    }
}


# read in message file, export sender types and messages to recipient folders
messages_to_recipient_surveys = function(index = c(0,0,0,0)){
    # read in compiled data from senders
    merged_processed_output_senders =
        read_csv(paste("../Pipeline/Match_files/",
                        Sys.Date(), 
                        "_merged_processed_output_senders", ".csv", sep = "" ))
    
    for (i in 1:nrow(merged_processed_output_senders)){
    # NEED TO CHANGE THIS TO GET THE RIGHT V    
        V= 1 # merged_processed_output_senders[[i, "V"]]
        message = merged_processed_output_senders[[i,"Q1"]]
        sender = merged_processed_output_senders[[i,"string"]]
        
        index[V] = index[V] +1
        
        # # path for storing file with message for recipient
        # message_path = paste("../Pipeline/Qualtrics_input/", row["recipient"], "-2/", index,
        #                      "_msg.txt", sep="")
        # # path for storing file with sender type (in recipient folder)
        # sender_path = paste("../Pipeline/Qualtrics_input/", row["recipient"], "-2/", index,
        #                     "_sender.txt", sep="")
    }
}




# commit and push all files in Qualtrics_input folder to Github
update_github = function(repo = "../") {
    git_add(files = "*", repo = repo)
    
    git_commit(repo = repo,
               message = paste0("Update: ", Sys.time()))
    
    # this uses my stored ssh key
    git_push(repo = repo)
}
