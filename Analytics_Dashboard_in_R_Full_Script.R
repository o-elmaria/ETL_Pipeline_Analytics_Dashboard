# Notes to users of this script:
# 1. Always make sure that you follow these steps when opening the file for the first time. "File" --> "Reopen with Encoding" --> "UTF-8"

# 2. Vendor Commission Rates Data Frame
  # Please ensure that the vendor commission rates are always updated in the G-sheet titled "Vendor_Commission_Rates" --> https://docs.google.com/spreadsheets/d/1djlEQ424Zq_8-sfnfGr4gTMPfz_mQO0KlM0bp-uJssI/edit?usp=sharing
  # Please do NOT change the link or the tab name of the vendor commissions sheet

# 3. How to amend the script so that it takes new bases into account? The example given below assumes that you want to add a new base called "op 5"
  # Add a variable called "op5_base_id" to the code under step 2 and assign the ID of the base to it. You can find the base's ID in Airtable's API documentation --> https://airtable.com/api
  # You also need to call the function "load_data_from_airtable_func" and assign its output to a new variable called "df_op5" like so --> load_data_from_airtable_func(private_api_key = api_id, base_id = op5_base_id, table_name = main_table_name)
    # Please add this piece of code under step 3
  # Finally, you need to add "df_op5" to the "rbind" command under step 3

# 4. Kemitt Email account
  # If you are using a different account than omar.elmaria@kemitt.com, please modify the user_email variable under step 2 to reflect the new email

# 5. Structure and format of Airtable bases 
  # Please make sure that all Airtable bases (op2, op3, etc.) have similar columns with identical names and formats

################################################################################################################################################################
###################################################################### SCRIPT STARTS HERE ######################################################################
################################################################################################################################################################

# Step 1: Load the libraries
if (system.file(package = "pacman") == "") {
  install.packages("pacman")  
} 

pacman::p_load(dplyr, # A package for data manipulation using data frames 
               ggplot2, # A package for plotting graphs
               rtable, # A package for connecting R to Airtable
               rlang,
               lubridate, # A package for date and time manipulation
               ggpubr, # A package for displaying descriptive statistics
               formattable, # A package that formats numbers and text
               googlesheets4, # A package that allows R to interact with Google-sheets
               tidyr, # Another package for data wrangling
               rmarkdown,
               gargle, # A package for google email authentication tokens
               plyr, # A package that allows rbind and cbind operations with NA filling among other things
               stringr, # A package that performs regex operations
               sendmailR, # A package for sending emails
               keyringr, # A package for de-crypting passwords. Used for the API key of Airtable
               FedData) # A package to do some string manipulation (e.g., get the rightmost 'n' characters of a character string) among other things

##-----------------------------------------------------------------------END OF STEP 1-----------------------------------------------------------------------##

# Step 2: Input section

## Step 2.1: Specify the input variables for the charts
start_date <- as.Date("2021-09-06")
end_date <- Sys.Date() # Today's date

## Step 2.2.1: Specify the base IDs for pulling data from Airtable through the API. Refer to the API documentation for more details
op2_base_id <- "appgdUO4BR84dv5Rv" # The ID of the "op 2" base
op3_base_id <- "appwDydCb32yYQze0" # The ID of the "op 3" base
op4_base_id <- "appuHAre2011Hqdmh" # The ID of the "op 4" base
main_table_name <- "Kemitt Dashboard" # The main table where the data lives

### Step 2.2.2: Extract the encrypted API key of Airtable and de-crypt it. # Every account has an API key which has access to all the data in the Airtable bases, so exercise caution when sharing it with 3rd party services
credential_label <- "Airtable_API_Key"
credential_path <- paste('C:\\DPAPI\\passwords\\', Sys.info()["nodename"], '\\', credential_label, '.txt', sep="")
api_id <- decrypt_dpapi_pw(credential_path)

## Step 2.3: Specify the user's email and set the encoding + Google's authentication token
user_email <- "omar.elmaria@kemitt.com"
options(encoding = "UTF-8", gargle_oauth_email = user_email)

## Step 2.4: Vendor commission rates G-sheet ID. Full link --> https://docs.google.com/spreadsheets/d/1djlEQ424Zq_8-sfnfGr4gTMPfz_mQO0KlM0bp-uJssI/edit?pli=1#gid=0
vendor_commission_sheet_id <- "1djlEQ424Zq_8-sfnfGr4gTMPfz_mQO0KlM0bp-uJssI"
vendor_commission_tab_name <- "Commission Rates"

## Step 2.5: The ID of the G-sheet that contains the exported data after cleaning
exported_data_sheet_id <- "1eCNlsGZlPQnHHppzZNf-1w7iXVGoXYkUfzU5N3Ok32o"

## Step 2.6: Relevant columns in the downloaded dataframe from Airtable. These column names are NOT the original names, rather their English versions
relevant_cols <- c("record_id", "Order_Number", "Order_Date", "Delivery_Date", "Vendor", "Product_Name", "Payment_Method", 
                   "Product_RetailPrice", "Ship_Rev", "Tot_GMV_Incl_Ship", "Order_Status", "Complaint", "Cancellation_Reason", "Shipping_Party", "Category", 
                   "Sub-category", "Route", "Promo Code", "Discount_Value", "Last Modified", "Base_Num")

## Step 2.7: Specify the English names of the Egyptian governorates 
route_ar <- c("سماعيل", "غردق", "القاهرة الجديدة", "سوان", "سكندرية", "قصر", "البحر الأحمر", "البحير", "الجيز", "الدقهل", "الزقاز", "السويس", "الشرقية", "الغربية",
              "الفيوم", "القاهرة", "القاهره الجديده", "قليوب", "المنصور", "المنوفي", "المنيا", "الوادي الجديد", "سيوط", "سويف", "بورسعيد", "تحت التجهيز", "جنوب سينا",
              "حلوان", "دمياط", "سوهاج", "طنطا", "قنا", "الشيخ", "مطروح", "القاهره", "بنها") 

route_en <- c("Ismailia", "Hurghada", "New_Cairo", "Aswan", "Alex", "Luxor", "Red_Sea", "Behira", "Giza", "Dakahlia", "Zagazig", "Suez", "Sharkia", "Gharbia", "Fayoum", 
              "Cairo", "New_Cairo", "Qalyoubia", "Mansoura", "Menoufia", "Menia", "ElWady_ElGedeed", "Asyut", "Beni_Suef", "Portsaid", "Under_Prep", "South Sinai", 
              "Helwan", "Domyat", "Suhaj", "Tanta", "Qena", "Kafr_ElSheikh", "Matrouh", "Cairo", "Banha")

## Step 2.8: Specify the English names of the order statuses
order_status_ar <- c("طلب ملغي", "تم التسليم", "رفض التسليم", "طلب استبدال", "تم الاسترجاع", "مع شركة الشحن", "تحت التجهيز", "طلب استرجاع", "جاهز للتحميل" )

order_status_en <- c("Cancelled", "Delivered", "Delivery_Rejected", "Replacement_Request", "Returned", "With_Shipping_Comp", "Under_Prep", "Return_Request", "Ready_to_Load")

## Step 2.9: 
final_col_order <- c("record_id", "Base_Num", "Order_Number", "Order_Items", "Order_Date", "Delivery_Date", "Vendor", "Product_Name", "Category", "Sub-category", "Payment_Method",
                     "Order_Status", "Complaint", "Cancellation_Reason", "Shipping_Party", "Route",
                     "Product_RetailPrice", "Price_point_interval", "Ship_Rev_Non_Dist", "Ship_Rev_Dist", "Tot_GMV_Incl_Ship_Old", "Tot_GMV_Incl_Ship_Calc", 
                     "Commission_Rate", "Commission_Rev", "Tot_Rev_Incl_Ship", "Promo Code", "Discount_Value", "Last Modified")

##--------------------------------------------------END OF STEP 2 AND INPUT SECTION (NO MORE INPUTS NEEDED)--------------------------------------------------##

# Step 3: Load data from Airtable to R
load_data_from_airtable_func <- function(private_api_key, base_id, table_name) {
  setup(api_key = private_api_key,
        base = base_id,
        table = table_name)
  records <- list_records()
  df <- records_to_tibble(records) # Changes the Airtable records to a tibble that we can do analyses on
  return(df)
}

# ***FUNCTION CALLING***
df_op2 <- load_data_from_airtable_func(private_api_key = api_id, base_id = op2_base_id, table_name = main_table_name)
df_op3 <- load_data_from_airtable_func(private_api_key = api_id, base_id = op3_base_id, table_name = main_table_name)
df_op4 <- load_data_from_airtable_func(private_api_key = api_id, base_id = op4_base_id, table_name = main_table_name)

# Add a column to each df_op(x) showing which base it comes from, then combine all data frames together
vars_in_workspace <- str_extract(ls(), pattern = "df_op[0-9]")
vars_in_workspace <- vars_in_workspace[!is.na(vars_in_workspace)]
df_op_combined <- do.call(rbind.fill, 
                          lapply(vars_in_workspace,
                                 function(x) {
                                   df <- get(x)
                                   df$Base_Num <- paste0("op", substr_right(x, 1))
                                   return(df)
                                   }
                                 )
                          )

##-----------------------------------------------------------------------END OF STEP 3-----------------------------------------------------------------------##

# Step 4: Load supplementary data from other sources (e.g., Vendor commission rates and marketing spend). Note: We chose to ignore the marketing spend until we can clarify the source(s)
df_vendor_commission <- unique(read_sheet(ss = vendor_commission_sheet_id, sheet = vendor_commission_tab_name))

##-----------------------------------------------------------------------END OF STEP 4-----------------------------------------------------------------------##

# Step 3: Clean the data and reformat in a way that makes it usable

## Step 3.1: Rename the columns
col_rename_func <- function(df) {
  # Rename all columns to English names
  df_colnames <- colnames(df)
  
  colnames(df)[df_colnames == "المنتج"] <- "Product_Name"
  colnames(df)[df_colnames == "المورد/المصمم"] <- "Vendor"
  colnames(df)[df_colnames == "تاريخ التسليم"] <- "Delivery_Date"
  colnames(df)[df_colnames == "تاريخ الطلب"] <- "Order_Date"
  colnames(df)[df_colnames == "رقم الطلب "] <- "Order_Number"
  colnames(df)[df_colnames == "حالة الطلب"] <- "Order_Status"
  colnames(df)[df_colnames == "قيمة المنتج"] <- "Product_RetailPrice"
  colnames(df)[df_colnames == "وسيلة الدفع"] <- "Payment_Method"
  colnames(df)[df_colnames == "المحافظة"] <- "Route"
  colnames(df)[df_colnames == "مسؤول المتابعة"] <- "CS_Person"
  colnames(df)[df_colnames == "وسيلة التواصل"] <- "Comms_Method"
  colnames(df)[df_colnames == "شكوى من طلب"] <- "Complaint"
  colnames(df)[df_colnames == "سبب الغاء الطلب"] <- "Cancellation_Reason"
  colnames(df)[df_colnames == "مبلغ التحصيل"] <- "Amount_Collected"
  colnames(df)[df_colnames == "ملاحظات"] <- "Comments"
  colnames(df)[df_colnames == "تاريخ عمل الشكوى"] <- "Complaint_Date"
  colnames(df)[df_colnames == "اجمالي الطلب"] <- "Tot_GMV_Incl_Ship"
  colnames(df)[df_colnames == "الشحن عن طريق"] <- "Shipping_Party"
  colnames(df)[df_colnames == "قيمة الشحن(الفعلية)"] <- "Actual_Shipping_Cost"
  colnames(df)[df_colnames == "ملخص المشكلة"] <- "Problem_Summary"
  colnames(df)[df_colnames == "عدد القطع"] <- "Num_of_Pieces"
  colnames(df)[df_colnames == "عنوان التسليم"] <- "Delivery_Address"
  colnames(df)[df_colnames == "رقم التليفون"] <- "Phone_Number"
  colnames(df)[df_colnames == "فاليو - المقدم المدفوع"] <- "Valu_Down_Payment"
  colnames(df)[df_colnames == "فاليو - رسم الشراء"] <- "Valu_Purchase_Fee"
  colnames(df)[df_colnames == "قيمة الخصم"] <- "Discount_Value"
  colnames(df)[grep("داشبورد", df_colnames)] <- "Ship_Rev"
  
  return(df)
}

# ***FUNCTION CALLING*** --> "col_rename_func" to change the Arabic column names to English ones
df_op_combined <- col_rename_func(df_op_combined)

##-----------------------------------------------------------------------END OF STEP 3.1-----------------------------------------------------------------------##

## Step 3.2: Form a new data frame containing only the relevant columns and re-order them. Then, calculate the shipping revenue per order item by distributing the revenue of the order evenly on each order item
ship_rev_dist_calc_func <- function(df, columns) {
  df_relevant <- df[, columns]
  
  # Count the number of items per order and divide the total shipping cost by that number
  orders_ship_cost <- df_relevant %>% 
    group_by(Order_Number) %>% 
    dplyr::summarise(Order_Items = n(), # Counts the number of items (rows) per order. N.B. Must explicitly define the package to be called 
                     Ship_Rev_Dist = round(sum(Ship_Rev, na.rm = TRUE) / Order_Items, 2)) %>% # Divides the total shipping revenue by the number of items within an order 
    arrange(Order_Number) %>% # Sorts rows by "Order_Number"
    na.omit() # Removes rows with NA
  
  # Join the table above to df_relevant and calculate a new "Tot_GMV" column
  df_clean <- df_relevant %>% 
    left_join(orders_ship_cost, by = "Order_Number") %>% # LEFT JOINs "orders_ship_cost" to "df_relevant" using "Order_Number" as a key
    mutate(Tot_GMV_Incl_Ship_Calc = Product_RetailPrice + Ship_Rev_Dist) %>% # Calculate a new total GMV column by adding the product's retail price and distributed shipping rev
    dplyr::rename(Tot_GMV_Incl_Ship_Old = Tot_GMV_Incl_Ship, Ship_Rev_Non_Dist = Ship_Rev) %>% # Rename these columns so that they can be easily identified and excluded from any analysis
    arrange(Order_Number, desc(Product_RetailPrice)) # Sort the rows by "Order_Number" and descending order of "Product_RetailPrice"
  
  return(df_clean)
}

# ***FUNCTION CALLING*** --> "ship_rev_dist_calc_func" to select the relevant columns and calculate correct values from GMV and shipping revenue. Assign the result to df_clean
df_clean <- ship_rev_dist_calc_func(df_op_combined, relevant_cols)

##-----------------------------------------------------------------------END OF STEP 3.2-----------------------------------------------------------------------##

## Step 3.3: Change the Arabic text in some columns to English
arab_to_eng_func <- function(data_frame, column, pattern, replacement) {
  for (i in 1:length(replacement)) {
    search_indices <- grepl(pattern[i], data_frame[[column]]) # To select a single column within a data frame, use [[ ]] 
    data_frame[search_indices, column] <- replacement[i]
  }
  
  return(data_frame)
}

# *** FUNCTION CALLING*** --> arab_to_eng_func on three columns "Route", "Order_Status", and "Vendor" to change the Arabic "values" under these columns to English ones
### Route
df_clean <- arab_to_eng_func(df_clean, "Route", route_ar, route_en) # You need to do the assignment in the global environment for the function to update the data frame

### Order status
df_clean <- arab_to_eng_func(df_clean, "Order_Status", order_status_ar, order_status_en)

### Shipping party
df_clean[df_clean$Shipping_Party == "المورد" & !is.na(df_clean$Shipping_Party), "Shipping_Party"] <- "Vendor" # The second condition is necessary so that R doesn't give an error

##-----------------------------------------------------------------------END OF STEP 3.3-----------------------------------------------------------------------##

## Step 3.4: Change the data types of some string columns to categorical and some date/time columns to date
fields_date <- c("Order_Date", "Delivery_Date", "Last Modified")
fields_cat <- c("Payment_Method", "Order_Status", "Shipping_Party", "Category", "Sub-category", "Route")

df_clean <- df_clean %>%
  mutate_at(fields_cat, as.factor) %>%
  mutate_at(fields_date, as.Date)

## Step 3.5: Combine similar payment method types together
df_clean[grepl("cod", df_clean$Payment_Method, ignore.case = TRUE), "Payment_Method"] <- "COD" # Eliminate inconsistencies in the COD name

## Step 3.6: Add a price point interval column to df_clean
df_clean <- df_clean %>% 
  mutate(Price_point_interval = cut(Product_RetailPrice, breaks = seq(0, 50000, 1000), right = TRUE, ordered_result = TRUE, dig.lab = 10))

##-----------------------------------------------------------------------END OF STEP 3.4-----------------------------------------------------------------------##

# Step 4: Join the supplementary data to df_clean
df_clean <- df_clean %>% 
  left_join(df_vendor_commission %>% select(Vendor, "Commission_Rate" = Commission), by = "Vendor") %>% 
  mutate(Commission_Rev = Commission_Rate * Product_RetailPrice,
         Tot_Rev_Incl_Ship = Commission_Rev + Ship_Rev_Dist)

# Re-arrange the columns after adding the new fields
df_clean <- df_clean[, final_col_order]

##-----------------------------------------------------------------------END OF STEP 4-----------------------------------------------------------------------##

# Step 5: Create summaries from the clean data set for plotting

# Daily time series
df_daily_time_series_final <- df_clean %>%
  group_by(Order_Date) %>%
  dplyr::summarise(order_items = n(), # Total number of items within ALL orders. Must call the package "dplyr" explicitly here
                   orders = n_distinct(Order_Number), # Total number of orders (several items within one order are counted as one order)
                   items_per_order = order_items / orders,
                   gmv = sum(Product_RetailPrice, na.rm = TRUE),
                   ship_rev = sum(Ship_Rev_Dist, na.rm = TRUE),
                   comm_rev = sum(Commission_Rev, na.rm = TRUE),
                   vendor_cut = gmv - comm_rev,
                   ship_rev_pct_gmv = round(ship_rev / gmv, 3),
                   comm_pct_gmv = round(comm_rev / gmv, 3),
                   vendor_cut_pct_gmv = round(vendor_cut / gmv, 3),
                   rev_incl_ship = sum(Tot_Rev_Incl_Ship, na.rm = TRUE),
                   gmv_incl_ship = sum(Tot_GMV_Incl_Ship_Calc, na.rm = TRUE),
                   AOV = gmv / orders) %>%
  filter(Order_Date >= start_date & Order_Date <= end_date) %>% 
  mutate(rev_l7D = lag(rev_incl_ship, 7),
         rev_wow_gr = round(rev_incl_ship / lag(rev_incl_ship, 7) - 1, 2),
         orders_l7D = lag(orders, 7),
         orders_wow_gr = round(orders / lag(orders, 7) - 1, 2),
         order_items_l7D = lag(order_items, 7),
         order_items_wow_gr = round(order_items / lag(order_items, 7) - 1, 2),
         
         rev_l30D = lag(rev_incl_ship, 30),
         rev_mom_gr = round(rev_incl_ship / lag(rev_incl_ship, 30) - 1, 2),
         orders_l30D = lag(orders, 30),
         orders_mom_gr = round(orders / lag(orders, 30) - 1, 2),
         order_items_l30D = lag(order_items, 30), 
         order_items_mom_gr = round(order_items / lag(order_items, 30) - 1, 2),
         timestamp = Sys.time()) %>% 
  arrange(Order_Date)

# Remove infinite values from the data frame as they cause problems when they are uploaded to Google Sheets
df_daily_time_series_final <- do.call(data.frame,
                                      lapply(df_daily_time_series_final,
                                             function(x) {replace(x, is.infinite(x), NA)}))

#------------------------------------------------------#------------------------------------------------------#

# Weekly time series 
df_weekly_time_series_final <- df_clean %>%
  mutate(Calendar_Week = round_date(Order_Date, unit = "week")) %>% # Create a new variable that rounds the date to the nearest week
  group_by(Calendar_Week) %>%
  dplyr::summarise(order_items = n(), # Total number of items within ALL orders
            orders = n_distinct(Order_Number), # Total number of orders (several items within one order are counted as one order)
            items_per_order = order_items / orders,
            gmv = sum(Product_RetailPrice, na.rm = TRUE),
            ship_rev = sum(Ship_Rev_Dist, na.rm = TRUE),
            comm_rev = sum(Commission_Rev, na.rm = TRUE),
            vendor_cut = gmv - comm_rev,
            ship_rev_pct_gmv = round(ship_rev / gmv, 3),
            comm_pct_gmv = round(comm_rev / gmv, 3),
            vendor_cut_pct_gmv = round(vendor_cut / gmv, 3),
            rev_incl_ship = sum(Tot_Rev_Incl_Ship, na.rm = TRUE),
            gmv_incl_ship = sum(Tot_GMV_Incl_Ship_Calc, na.rm = TRUE),
            AOV = gmv / orders) %>%
  filter(Calendar_Week >= start_date & Calendar_Week <= end_date) %>% 
  mutate(rev_l1W = lag(rev_incl_ship, 1),
         rev_wow_gr = round(rev_incl_ship / lag(rev_incl_ship, 1) - 1, 2),
         orders_l1W = lag(orders, 1),
         orders_wow_gr = round(orders / lag(orders, 1) - 1, 2),
         order_items_l1W = lag(order_items, 1),
         order_items_wow_gr = round(order_items / lag(order_items, 1) - 1, 2),
         
         rev_l4W = lag(rev_incl_ship, 4),
         rev_mom_gr = round(rev_incl_ship / lag(rev_incl_ship, 4) - 1, 2),
         orders_l4W = lag(orders, 4),
         orders_mom_gr = round(orders / lag(orders, 4) - 1, 2),
         order_items_l4W = lag(order_items, 4), 
         order_items_mom_gr = round(order_items / lag(order_items, 4) - 1, 2),
         timestamp = Sys.time()) %>% 
  arrange(Calendar_Week)

# Remove infinite values from the data frame as they cause problems when they are uploaded to Google Sheets
df_weekly_time_series_final <- do.call(data.frame,
                                       lapply(df_weekly_time_series_final,
                                              function(x) {replace(x, is.infinite(x), NA)}))

#------------------------------------------------------#------------------------------------------------------#

# Category summary
df_mkc_final <- df_clean %>%
  filter(!is.na(Category)) %>% 
  group_by(Category) %>% 
  dplyr::summarise(order_items = n(),
            orders = n_distinct(Order_Number),
            items_per_order = order_items / orders,
            gmv = sum(Product_RetailPrice, na.rm = TRUE),
            ship_rev = sum(Ship_Rev_Dist, na.rm = TRUE),
            comm_rev = sum(Commission_Rev, na.rm = TRUE),
            vendor_cut = gmv - comm_rev,
            ship_rev_pct_gmv = round(ship_rev / gmv, 3),
            comm_pct_gmv = round(comm_rev / gmv, 3),
            vendor_cut_pct_gmv = round(vendor_cut / gmv, 3),
            rev_incl_ship = sum(Tot_Rev_Incl_Ship, na.rm = TRUE),
            gmv_incl_ship = sum(Tot_GMV_Incl_Ship_Calc, na.rm = TRUE),
            AOV = round(gmv / orders, 0)) %>% 
  mutate(timestamp = Sys.time())

# Cumulative KPIs
cum_func <- function(dataframe, grp_col, cum_col1, cum_col2) {
  grp_col <- enquo(grp_col)
  cum_col1 <- enquo(cum_col1)
  cum_col2 <- enquo(cum_col2)
  
  new_data_frame <- dataframe %>% 
    dplyr::select(!!grp_col, !!cum_col1, !!cum_col2) %>% 
    dplyr::arrange(desc(!!cum_col1)) %>% 
    dplyr::mutate(cum_order_items = cumsum(!!cum_col1),
           cum_order_items_pct = round(cum_order_items / sum(!!cum_col1), 3),
           
           cum_rev_incl_ship = cumsum(!!cum_col2),
           cum_rev_incl_ship_pct = round(cum_rev_incl_ship / sum(!!cum_col2), 3))
}
df_cum_freq_mkc_final <- cum_func(df_mkc_final, Category, order_items, rev_incl_ship) # Global assignment
df_cum_freq_mkc_final <- df_cum_freq_mkc_final %>% 
  mutate(timestamp = Sys.time())

#------------------------------------------------------#------------------------------------------------------#

# Vendor summary
df_vendor_final <- df_clean %>%
  filter(!is.na(Vendor)) %>% 
  group_by(Vendor) %>% 
  dplyr::summarise(order_items = n(),
            orders = n_distinct(Order_Number),
            items_per_order = order_items / orders,
            gmv = sum(Product_RetailPrice, na.rm = TRUE),
            ship_rev = sum(Ship_Rev_Dist, na.rm = TRUE),
            comm_rev = sum(Commission_Rev, na.rm = TRUE),
            vendor_cut = gmv - comm_rev,
            ship_rev_pct_gmv = round(ship_rev / gmv, 3),
            comm_pct_gmv = round(comm_rev / gmv, 3),
            vendor_cut_pct_gmv = round(vendor_cut / gmv, 3),
            rev_incl_ship = sum(Tot_Rev_Incl_Ship, na.rm = TRUE),
            gmv_incl_ship = sum(Tot_GMV_Incl_Ship_Calc, na.rm = TRUE),
            AOV = round(gmv / orders, 0)) %>% 
  arrange(desc(rev_incl_ship))

# Cumulative KPIs
df_cum_freq_vendor_final <- cum_func(df_vendor_final, Vendor, order_items, rev_incl_ship) # Global assignment
df_cum_freq_vendor_final <- df_cum_freq_vendor_final %>% 
  mutate(timestamp = Sys.time())

#------------------------------------------------------#------------------------------------------------------#

# Payment methods summary
df_pay_method_final <- df_clean %>% 
  group_by(Payment_Method) %>% 
  dplyr::summarise(order_items = n(),
            rev_incl_ship = sum(Tot_Rev_Incl_Ship, na.rm = TRUE)) %>% 
  mutate(pct_of_tot_order_items = round(order_items / sum(order_items), 3),
         pct_of_tot_rev_incl_ship = round(rev_incl_ship / sum(rev_incl_ship), 3),
         timestamp = Sys.time())

##-----------------------------------------------------------------------END OF STEP 5-----------------------------------------------------------------------##

# Step 6: Export the cleaned and aggregated data frames to a G-sheet
exported_data_sheet_names <- sort(sheet_names(exported_data_sheet_id))

# Define a function that exports the data to a G-sheet
export_func <- function(dataframe_var, sheetid_var, sheetname_var) {
  sheet_write(data = dataframe_var, ss = sheetid_var, sheetname_var)
}

# Export all the data to the G-sheet in one go
all_df_names <- names(.GlobalEnv) # Gets all the names of the data frames in the R environment
df_for_export <- sort(all_df_names[grep("_final|_clean", all_df_names)]) # Extracts the names of the data frames that will be exported

for (i in 1:length(exported_data_sheet_names)) {
  export_func(eval(parse(text = df_for_export[i])), exported_data_sheet_id, exported_data_sheet_names[i])
}

##-----------------------------------------------------------------------END OF STEP 6-----------------------------------------------------------------------##

# Step 7: Send an email to the relevant users telling them that the update was finished
sendmail(from = "<omar.elmaria@kemitt.com>", 
         to = c("<omar.elmaria@kemitt.com>", "<mahmoud@kemitt.com>", "<hedayat@kemitt.com>", "<rashwan@kemitt.com>"), 
         subject = paste0("Daily Automatic Data Refresh - ", Sys.time()), 
         msg = "The analytics dashboard is now fed with the most up-to-date data",
         control = list(smtpServer = "ASPMX.L.GOOGLE.COM"))