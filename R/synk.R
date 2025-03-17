## my own function from https://stackoverflow.com/questions/71393752/get-a-refresh-token-for-dropbox-api-using-rdrop2-and-drop-auth
# drop_auth_RT <- function (new_user = FALSE, key = "mmhfsybffdom42w", secret = "l8zeqqqgm1ne5z0", cache = TRUE, rdstoken = NA) 
# {
#   if (new_user == FALSE & !is.na(rdstoken)) {
#     if (file.exists(rdstoken)) {
#       .dstate$token <- readRDS(rdstoken)
#     }
#     else {
#       stop("token file not found")
#     }
#   }
#   else {
#     if (new_user && file.exists(".httr-oauth")) {
#       message("Removing old credentials...")
#       file.remove(".httr-oauth")
#     }
#     dropbox <- httr::oauth_endpoint(authorize = "https://www.dropbox.com/oauth2/authorize?token_access_type=offline",
#                                     access = "https://api.dropbox.com/oauth2/token")
#     # added "?token_access_type=offline" to the "authorize" parameter so that it can return an access token as well as a refresh token
#     dropbox_app <- httr::oauth_app("dropbox", key, secret)
#     dropbox_token <- httr::oauth2.0_token(dropbox, dropbox_app, 
#                                           cache = cache)
#     if (!inherits(dropbox_token, "Token2.0")) {
#       stop("something went wrong, try again")
#     }
#     .dstate$token <- dropbox_token
#   }
# }


drop_synk_up <- function(){
  # saveRDS(kundlista, "data/aterfyndet.db")
  drop_upload("data/aterfyndet.db",
              path = dropboxpath, 
              mode = "overwrite") #dtoken = token
}
