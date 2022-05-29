# First time authenticating the "gmailr" service
options(httr_oob_default = TRUE) # This MUST exist
gm_auth_configure(path = "I:\\My Drive\\Consulting\\Analytics_Infrastructure\\ETL_Pipeline_Analytics_Dashboard\\R Email.json")
gm_auth(email = "omar.elmaria@kemitt.com", cache = ".secrets")

test_email <-
  gm_mime() %>%
  gm_to(c("omar.elmaria@kemitt.com", "omar.elmaria@deliveryhero.com")) %>%
  gm_from("omar.elmaria@kemitt.com") %>%
  gm_subject("this is just a gmailr test") %>%
  gm_text_body("Can you hear me now?")

# Verify it looks correct
gm_create_draft(test_email)

# If all is good with your draft, then you can send it
gm_send_message(test_email)
