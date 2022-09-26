# Adjust the encoding of the actual script to UTF-8 so that it can handle Arabic characters
eval(parse("J:\\My Drive\\Consulting\\Analytics_Infrastructure\\ETL_pipeline_analytics_dashboard_in_R\\Analytics_Dashboard_in_R_Full_Script.R", encoding = "UTF-8"))

# Step 2: Send an email to the relevant users telling them that the update was finished
## Load the gmailr package
if (system.file(package = "pacman") == "") {
  install.packages("pacman")
}

pacman::p_load(gmailr)

# Set the authentication parameters
user_email <- "omar.elmaria@kemitt.com"
options(encoding = "UTF-8", gargle_oauth_email = user_email, httr_oob_default = TRUE)
path_to_email_oauth_token <- "J:\\My Drive\\Consulting\\Analytics_Infrastructure\\ETL_pipeline_analytics_dashboard_in_R\\R Email.json"

## Option 1: Via the "sendmailR" package (NOT receommended because it gets blocked after some time)
# sendmail(from = "<omar.elmaria@kemitt.com>", 
#          to = c("<omar.elmaria@kemitt.com>", "<mahmoud@kemitt.com>", "<hedayat@kemitt.com>", "<rashwan@kemitt.com>", "<business@kemitt.com>", "<a.bediwy@kemitt.com>"), 
#          subject = paste0("Daily Automatic Data Refresh - ", Sys.time()), 
#          msg = "The analytics dashboard is now fed with the most up-to-date data",
#          control = list(smtpServer = "ASPMX.L.GOOGLE.COM"))

## Option 2: Via the "gmailr" package (Preferred method because it uses the Google API) 
gm_auth_configure(path = path_to_email_oauth_token)
gm_auth(email = "omar.elmaria@kemitt.com") # cache = ".secrets" is needed the first time you run this through an interactive session
email <-
  gm_mime() %>%
  gm_to(c("omar.elmaria@kemitt.com", "mahmoud@kemitt.com", "hedayat@kemitt.com", "rashwan@kemitt.com", "business@kemitt.com", "a.bediwy@kemitt.com")) %>%
  gm_from("omar.elmaria@kemitt.com") %>%
  gm_subject(paste0("Daily Automatic Data Refresh - ", Sys.time())) %>%
  gm_text_body("The analytics dashboard is now fed with the most up-to-date data")

# If all is good with your draft, then you can send it
gm_send_message(email)