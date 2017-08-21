AUC_AUR_Summarise <- function(df){
 df %>%  summarise("Forecast TY AUR of Sales" = sum(subset(`Retail$`,    Source == "Forecast"), na.rm = TRUE)/sum(subset(`Unit Sales`, Source == "Forecast"), na.rm = TRUE),
            "TY AUC of Receipts" = sum(subset(`Cost Rcpts`, Source == "Forecast"), na.rm = TRUE)/sum(subset(`Unit Rcpts`, Source == "Forecast"), na.rm = TRUE),
            "Budget AUR of Sales"= sum(subset(`Retail$`,    Source == "Budget"),na.rm = TRUE)/sum(subset(`Unit Sales`, Source == "Budget"), na.rm = TRUE),
            "Budget AUC of Receipts" = sum(subset(`Cost Rcpts`, Source == "Budget"), na.rm = TRUE)/sum(subset(`Unit Rcpts`, Source == "Budget"), na.rm = TRUE),
            "LY AUR of Sales" = sum(subset(`Retail$`,    Source == "LY"), na.rm = TRUE)/sum(subset(`Unit Sales`, Source == "LY"), na.rm = TRUE),
            "LY AUC of Receipts" = sum(subset(`Cost Rcpts`, Source == "LY"), na.rm = TRUE)/sum(subset(`Unit Rcpts`, Source == "LY"), na.rm = TRUE),
            "AUR % Change (TY vs Budget)" = as.numeric((`Forecast TY AUR of Sales`-`Budget AUR of Sales`)/`Budget AUR of Sales`)*100,
            "AUC % Change (TY vs Budget)" = as.numeric((`TY AUC of Receipts`-`Budget AUC of Receipts`)/`Budget AUC of Receipts`)*100,
            "AUR % Change (TY vs LY)" = as.numeric((`Forecast TY AUR of Sales`-`LY AUR of Sales`)/`LY AUR of Sales`)*100,
            "AUC % Change (TY vs LY)" = as.numeric((`TY AUC of Receipts`-`LY AUC of Receipts`)/`LY AUC of Receipts`)*100,
            "GM Budget" = sum((subset(`Retail$`, Source == "Budget") - subset(`Cost$`, Source == "Budget")), na.rm = TRUE)/ sum(subset(`Retail$`, Source == "Budget"), na.rm = TRUE)*100,
            "GM Forecast/Actual" = sum((subset(`Retail$`, Source == "Forecast") - subset(`Cost$`, Source == "Forecast")), na.rm = TRUE)/ sum(subset(`Retail$`, Source == "Forecast"), na.rm = TRUE)*100,
            "GM LY" = sum((subset(`Retail$`, Source == "LY") - subset(`Cost$`, Source == "LY")), na.rm = TRUE)/ sum(subset(`Retail$`, Source == "LY"), na.rm = TRUE)*100,
            "GM Budget (Dollars)" = sum((subset(`Retail$`, Source == "Budget") - subset(`Cost$`, Source == "Budget")), na.rm = TRUE),
            "GM Forecast/Actual (Dollars)" = sum((subset(`Retail$`, Source == "Forecast") - subset(`Cost$`, Source == "Forecast")), na.rm = TRUE),
            "GM LY (Dollars)" = sum((subset(`Retail$`, Source == "LY") - subset(`Cost$`, Source == "LY")), na.rm = TRUE))
}


# Output_PCF_Brand_Channel_fun_test <- PCF_post_proc %>% 
#   group_by(`Brand`, `Channel`, `Fiscal Quarter`) %>% 
#   AUC_AUR_Summarise()