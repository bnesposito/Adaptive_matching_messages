library(dplyr)
library(gert) # for git

export_row_recipient = function(row, index) {
    # path for storing file with message for recipient
    message_path = paste("../Pipeline/Qualtrics_input/", row["recipient"], "-2/", index,
                         "_msg.txt", sep="")
    # path for storing file with sender type (in recipient folder)
    sender_path = paste("../Pipeline/Qualtrics_input/", row["recipient"], "-2/", index,
                        "_sender.txt", sep="")
    
    write(row[["Message"]], message_path)
    write(row[["sender"]], sender_path)
}


export_row_sender = function(row, index) {
    # path for storing file with recipient type (in sender folder)
    recipient_path = paste("../Pipeline/Qualtrics_input/", row["recipient"], "-1/", index,
                           "_recipient.txt", sep="")
    
    write(row[["recipient"]], recipient_path)
}


export_messages_to_folders = function(message_filename = "test.xlsx"){
    
    # Importing messages
    messages = readxl::read_xlsx(paste("../Pipeline/Qualtrics_output/", 
                               message_filename, sep = "" )) 
    # Fixing spaces in col names
    names(messages) = sub(" ", ".", names(messages)) 
    # re-defining type names for compatibility with paths
    recipient_lookup = tibble(Recipient.Type = c("f/US", "m/US", "f/India", "m/India"),
                         recipient = c("F-US", "M-US", "F-IN", "M-IN"))
    sender_lookup = tibble(Sender.Type = c("f/US", "m/US", "f/India", "m/India"),
                              sender = c("F-US", "M-US", "F-IN", "M-IN"))
    messages = left_join(messages, recipient_lookup, by = "Recipient.Type") %>% 
        left_join(sender_lookup, by = "Sender.Type") %>% 
        group_by(recipient) %>% 
        mutate(recipient_index = row_number()) %>%  # running index within recipient types
        ungroup() %>% 
        group_by(sender) %>% 
        mutate(sender_index = row_number()) %>%  # running index within sender types
        ungroup()
        
    # write message from all rows to the respective recipient / sender subfolders
    for (i in 1:nrow(messages)) {
        export_row_recipient(messages[i,], messages[i,"recipient_index"])
        export_row_sender(messages[i,], messages[i,"sender_index"])
    }
}


# https://github.com/r-lib/gert
update_github = function(repo = "../Pipeline/Qualtrics_input/") {
    git_add(files = "*", repo = repo)
    
    git_commit(repo = repo,
               message = paste0("Update as at: ", Sys.time()))
    
    # this uses my stored ssh key
    git_push(repo = repo)
}

export_messages_to_folders()

update_github()
