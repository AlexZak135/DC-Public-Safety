# Title: DC Public Safety Analysis
# Author: Alexander Zakrzeski
# Date: August 10, 2024

# Part 1: Setup and Configuration

# Load to import, clean, and wrangle data
library(dplyr)
library(forcats)
library(purrr)
library(readr)
library(stringr)
library(tibble)
library(tidyr)

# Load to visualize data
library(ggplot2)
library(gt)
library(scales)

# Load to get marginal effects
library(margins)

# Load for natural language processing
library(ldatuning)
library(textstem)
library(tidytext)
library(topicmodels)
library(vader)

# Define a function to tokenize text
tokenize_text <- function(dataframe, column) {   
  ngrams <- map_df(1:3, function(number) {  
    # Tokenize text into n-grams with specified size "number" 
    processed <- dataframe |>  
      drop_na(all_of(column)) |> 
      unnest_tokens(ngram, column, token = "ngrams", n = number) |> 
      drop_na(ngram) 
    
    # Additional processing for unigrams 
    if (number == 1) {  
      processed <- processed |>  
        filter(!str_detect(ngram, "^[0-9]+$")) |>
        anti_join(stop_words, by = c("ngram" = "word")) |> 
        mutate(ngram = lemmatize_words(ngram))  
    } 
    
    # Add a column to indicate whether it is a unigram, bigram, or trigram
    processed |>
      mutate(type = case_when( 
        number == 1 ~ "unigram", 
        number == 2 ~ "bigram",  
        number == 3 ~ "trigram"   
        )) |> 
      relocate(ngram, .after = type) 
  }) 
  
  # Return the dataframe 
  return(ngrams) 
}

# Define a function to plot proportions in a stacked bar chart
plot_props <- function(dataframe, column1, column2, column3, margin_size) {   
  # Create a stacked bar chart to display proportions  
  plot <- ggplot(dataframe, aes(x = !!sym(column1), y = !!sym(column2), 
                                fill = !!sym(column3))) +   
    geom_col(width = 0.8, position = "stack") +
    geom_text(aes(label = pct), position = position_stack(vjust = 0.5), 
              size = 5, color = "#ffffff") + 
    geom_hline(yintercept = 0, linewidth = 1.35, color = "#000000") + 
    scale_y_continuous(limits = c(0, 1), labels = percent) +
    labs(x = "", y = "") +
    guides(fill = guide_legend(title = "", reverse = TRUE)) + 
    coord_flip() + 
    theme_void() + 
    theme(plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
          text = element_text(family = "Roboto"), 
          plot.title = element_text(margin = margin(0, 0, 15, 0), hjust = 0.5, 
                                    size = 17, face = "bold"), 
          panel.grid.major.x = element_line(linetype = 3, linewidth = 0.3, 
                                            color = "#808080"),  
          axis.text.x = element_text(size = 15, color = "#000000"), 
          axis.text.y = element_text(margin = margin(0, margin_size, 0, 0), 
                                     size = 15, color = "#000000", hjust = 1), 
          legend.position = "top", 
          legend.key.size = unit(0.55, "cm"), 
          legend.text = element_text(size = 15), 
          legend.spacing.x = unit(0.25, "cm"),
          legend.margin = margin(0, 0, 12.5, 0))  
  
  # Dynamically set the color palette and title of the plot 
  if (identical(dataframe, involvement)) {     
    plot <- plot + 
      scale_fill_manual(values = c("#c41230", "#005288")) +  
      ggtitle(label = str_squish("Figure 3: Monthly Involvement in 
                                  Community-Police Programs"))
  } else if (identical(dataframe, interaction_quant)) { 
    plot <- plot +  
      scale_fill_manual(values = c("#005288", "#0078ae", "#828284", "#ffac1c", 
                                   "#c41230")) +  
      ggtitle(label = "Figure 4: Sentiment Regarding Interactions with the MPD")  
  } else if (identical(dataframe, effect_actions)) { 
    plot <- plot +
      scale_fill_manual(values = c("#005288", "#0078ae", "#5e9732", 
                                   "#828284")) +
      ggtitle(label = str_squish("Figure 8: Effectiveness of Police Actions on
                                  Public Safety")) 
  }
  
  # Return the plot 
  return(plot) 
}

# Define a function to customize the theme of a horizontal bar chart
theme_custom <- function() {   
  # Create an empty theme  
  empty <- theme_void() 
  
  # Add the various styling elements to the theme 
  custom <- empty + theme(     
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),  
    text = element_text(family = "Roboto"), 
    plot.title = element_text(margin = margin(0, 0, 15, 0), hjust = 0.5, 
                              size = 17, face = "bold"),
    panel.grid.major.x = element_line(linetype = 3, linewidth = 0.3, 
                                      color = "#808080"), 
    axis.title.x = element_text(margin = margin(10, 0, 0, 0), size = 15), 
    axis.text.x = element_text(size = 14, color = "#000000"),
    axis.text.y = element_text(margin = margin(0, -20, 0, 0), size = 14, 
                               color = "#000000", hjust = 1), 
    legend.position = "top", 
    legend.key.size = unit(0.55, "cm"), 
    legend.text = element_text(size = 15), 
    legend.spacing.x = unit(0.25, "cm"),
    legend.margin = margin(0, 0, 10, 0)  
    ) 
  
  # Return the custom theme 
  return(custom)  
}

# Part 2: Data Preprocessing

# Load the data from the CSV file
safety <- read_csv("DC-Public-Safety-Data.csv") |>  
  # Rename columns, create a new column, and change the position of a column
  rename_with(tolower) |>
  rename(id = objectid, 
         ethnicity = eth, 
         residency = livedindistrict,
         int_freq = interactfreq, 
         int_feel = interactfeel, 
         int_comment = mpd_comments, 
         imp_qual = mostimportantquality, 
         involv = involvement) |>
  unite(lead_qual, moralstandards:communicator, sep = ", ", na.rm = TRUE) |> 
  relocate(involv, .after = residency) |> 
  # Drop specific columns
  select(-c(source, agebin, enforcenuisance_effect, solveviolentcrimes_effect,
            targetguns_effect:enforcetraffic_effect, 
            nuisance_priority:recordofinnovation, ward_string:nmiss)) |> 
  # Modify values in columns and create a new column
  mutate(id = as.character(id),  
         gender = na_if(gender, "Other") |> 
                  factor(levels = c("Male", "Female")), 
         ethnicity = case_when(   
           ethnicity == "African American" ~ "Black", 
           ethnicity == "Caucasian" ~ "White", 
           ethnicity == "Unknown" ~ NA_character_, 
           TRUE ~ "Other"     
           ) |>      
                     factor(levels = c("Other", "Black", "White")), 
         age = if_else( 
           age > 100, NA_integer_, age        
           ),  
         residency = case_when( 
           residency %in% c("<1 year", "1-5 years", "6-10 years") ~ "<=10",  
           residency %in% c("11-15 years", "16-20 years", ">20 years") ~ ">10", 
           TRUE ~ NA_character_   
           ),  
         involv = case_when(  
           involv %in% c("Slightly Involved (1 - 6 Hour(s))",  
                         "Involved (7 - 12 Hours)", 
                         "Very Involved (12+ Hours)") ~ "Yes",  
           involv == "Not Involved" ~ "No",  
           TRUE ~ NA_character_   
           ), 
         int_freq = case_when( 
           int_freq %in% c("Monthly", "Weekly", "Daily") ~ "At Least Monthly",  
           is.na(int_freq) ~ NA_character_, 
           TRUE ~ "Less than Monthly"  
           ),
         int_feel = str_to_title(int_feel) |>
                    factor(levels = c("Very Positive", "Positive", "Neutral", 
                                      "Negative", "Very Negative")),
         int_comment = if_else(   
           int_comment %in% c(".", "n/a", "N/a", "N/A", "Na", "no", "No", "NO",
                              "No at this time", "No.", "none", "None", "None.", 
                              "Not at the moment", "Not at this time"), 
           NA_character_, int_comment   
           ),
         int_score = vader_df(int_comment) |> pull(compound), 
         across(c(police_effect, mpd_effect), ~ case_when( 
           .x == "Very Effective" ~ "Effective", 
           .x == "Very Ineffective" ~ "Ineffective", 
           TRUE ~ .x   
           )),
         imp_qual = if_else( 
           imp_qual %in% c(".", "#NAME?", "Do not know", "Dont know", 
                           "I dont know", "Idk", "N/a", "N/A", "No", "None", 
                           "Not sure"),   
           NA_character_, imp_qual   
           ),  
         lead_qual = str_to_title(lead_qual) |> 
                     str_replace_all(c("About" = "about", 
                                       "And" = "and",
                                       "The" = "the", 
                                       "To" = "to")) |>
                     na_if("")) |>
  # Change the position of the newly created column 
  relocate(int_score, .after = int_comment)

# Remove objects from global environment
rm(incl_nt, neu_set)

# Part 3: Demographics

# Drop rows and modify values in a column
demographic1 <- safety |> 
  drop_na(gender, ethnicity, residency) |> 
  mutate(residency = if_else(  
    residency == ">10",  
    "Resided for Over 10 Years in D.C.", "Resided for up to 10 Years in D.C."      
    )) |>  
  # Get proportions 
  count(gender, ethnicity, residency) |>
  group_by(residency) |>
  mutate(prop = round(n / sum(n), 2)) |> 
  ungroup() |>
  select(-n)

# Create a faceted bar chart to display proportions
ggplot(demographic1, aes(x = ethnicity, y = prop, fill = gender)) +   
  geom_col(width = 0.85, position = "dodge") + 
  geom_text(aes(label = percent(prop, accuracy = 1)), 
            position = position_dodge(0.85), vjust = -1, hjust = 0.5, 
            size = 5.5) +
  geom_hline(yintercept = 0, linewidth = 1.15, color = "#000000") +
  scale_y_continuous(limits = c(0, 0.48), breaks = seq(0, 0.48, by = 0.15), 
                     labels = percent) + 
  scale_fill_manual(values = c("#5e9732", "#0078ae")) + 
  labs(title = "Figure 1: Percentage of Respondents by Ethnicity and Gender", 
       x = "", y = "") +
  guides(fill = guide_legend(title = "")) +
  facet_grid(~ residency, scales = "free") + 
  theme_void() + 
  theme(plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"), 
        panel.spacing.x = unit(2.25, "lines"), 
        text = element_text(family = "Roboto"),
        plot.title = element_text(margin = margin(0, 0, 15, 0), hjust = 0.5, 
                                  size = 17, face = "bold"), 
        strip.text = element_text(margin = margin(0, 0, 15, 0), size = 15),
        panel.grid.major.y = element_line(linetype = 3, linewidth = 0.3, 
                                          color = "#808080"),  
        axis.text.x = element_text(margin = margin(-2.5, 0, 0, 0), size = 15,
                                   color = "#000000"), 
        axis.text.y = element_text(margin = margin(0, 5, 0, 0), size = 15, 
                                   color = "#000000"), 
        legend.position = "top", 
        legend.key.size = unit(0.55, "cm"), 
        legend.text = element_text(size = 15), 
        legend.spacing.x = unit(0.25, "cm"),
        legend.margin = margin(0, 0, 12.5, 0))

# Select the necessary columns and drop rows
demographic2 <- safety |> 
  select(ward, age) |>
  drop_na(ward, age)

# Create a faceted density plot to display distributions
ggplot(demographic2, aes(x = age)) +
  geom_density(fill = "#0078ae") +
  geom_hline(yintercept = 0, linewidth = 0.8, color = "#000000") +
  scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 25)) + 
  scale_y_continuous(limits = c(0, 0.06), breaks = seq(0, 0.06, by = 0.02),  
                     labels = label_number(drop0trailing = TRUE)) + 
  labs(title = "Figure 2: Distribution of Respondents' Ages by Ward", 
       x = "Age (years)", y = "Density") +
  facet_wrap(~ ward, scales = "free", ncol = 4) +
  theme_void() + 
  theme(plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"), 
        panel.spacing.x = unit(2.25, "lines"), 
        panel.spacing.y = unit(1.25, "lines"),
        text = element_text(family = "Roboto"),
        plot.title = element_text(margin = margin(0, 0, 15, 0), hjust = 0.5, 
                                  size = 17, face = "bold"), 
        strip.text = element_text(margin = margin(0, 0, 10, 0), size = 13),
        panel.grid.major = element_line(linetype = 3, linewidth = 0.3, 
                                        color = "#808080"), 
        axis.title.x = element_text(margin = margin(10, 0, 0, 0), size = 15),
        axis.title.y = element_text(margin = margin(0, 10, 0, 0), angle = 90, 
                                    size = 15),
        axis.text.x = element_text(margin = margin(-2.5, 0, 0, 0), size = 12,
                                   color = "#000000"), 
        axis.text.y = element_text(margin = margin(0, 5, 0, 0), size = 12, 
                                   color = "#000000")) 

# Part 4: Experiences with Police

# Define a function to perform aggregation and get proportions
produce_props <- function(column) {   
  # Drop rows and concatenate values from multiple columns
  processed <- safety |> 
    drop_na(gender, ethnicity, all_of(column)) |> 
    unite(eth_gender, ethnicity, gender, sep = " ")  
  
  # Additional processing to modify values and set factor levels   
  if (column == "involv") { 
    processed <- processed |> 
      mutate(involv = if_else(  
        involv == "Yes", "Involvement", "No Involvement"       
        ) |>             
                      factor(levels = c("No Involvement", "Involvement")))
  }
  
  # Get proportions and modify values in a column
  processed <- processed |>   
    count(eth_gender, !!sym(column)) |>
    group_by(eth_gender) |>
    mutate(prop = n / sum(n), 
           pct = if_else(      
             prop >= 0.05, percent(prop, accuracy = 1), "" 
             ), 
           eth_gender = case_when(
             eth_gender == "Other Female" ~ "Females of Other Ethnicities",
             eth_gender == "Other Male" ~ "Males of Other Ethnicities",
             !str_detect(eth_gender, "Other") ~ paste0(eth_gender, "s") 
             ) |>       
                        factor(levels = c("White Females", "White Males", 
                                          "Females of Other Ethnicities",  
                                          "Males of Other Ethnicities", 
                                          "Black Females", "Black Males"))) |>
    ungroup() |> 
    select(-n)
  
  # Return the dataframe 
  return(processed) 
}

# Output the dataframes with the proportions
involvement <- produce_props("involv")
interaction_quant <- produce_props("int_feel")

# Output the stacked bar charts to display the proportions
figure3 <- plot_props(involvement, "eth_gender", "prop", "involv", -17.5)
figure4 <- plot_props(interaction_quant, "eth_gender", "prop", "int_feel", 
                      -17.5) 

# Tokenize to generate n-grams, drop rows, and get term frequencies
interaction_qual <- safety |>  
  tokenize_text("int_comment") |> 
  drop_na(gender) |>
  count(gender, ngram) |> 
  # Filter based on the set conditions
  filter((gender == "Female" & ngram %in% c("crime", "friendly", "helpful", 
                                            "polite", "professional", "safe", 
                                            "stop", "traffic")) |
         (gender == "Male" & ngram %in% c("arrest", "crime", "drug", "friendly", 
                                          "helpful", "issue", "professional",
                                          "stop"))) |>
  # Generate a new column and modify values in an existing column
  mutate(sentiment = if_else(        
    ngram %in% c("friendly", "helpful", "law", "polite", "professional", 
                 "safe"),  
    "Positive", "Negative"    
    ) |>              
                     factor(levels = c("Positive", "Negative")),
    ngram = reorder_within(str_to_title(ngram), n, gender, sep = "_")) 

# Create a faceted bar chart to display term frequencies
ggplot(interaction_qual, aes(x = ngram, y = n, fill = sentiment)) + 
  geom_col(width = 0.825) +
  geom_hline(yintercept = 0, linewidth = 1.15, color = "#000000") + 
  scale_x_discrete(labels = function(x) str_replace_all(x, "_.*", "")) + 
  scale_y_continuous(breaks = pretty_breaks(3), 
                     labels = label_number(drop0trailing = TRUE)) +
  scale_fill_manual(values = c("#5e9732", "#c41230")) +
  labs(title = str_squish("Figure 5: Term Frequencies from Descriptions of 
                           Interactions with the MPD"), 
       x = "", y = "Frequency") +
  guides(fill = guide_legend(title = "")) +
  facet_wrap(~ gender, scales = "free") +
  coord_flip() + 
  theme_void() + 
  theme(plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),  
        panel.spacing.x = unit(2.25, "lines"), 
        text = element_text(family = "Roboto"),  
        plot.title = element_text(margin = margin(0, 0, 15, 0), hjust = 0.5, 
                                  size = 17, face = "bold"), 
        strip.text = element_text(margin = margin(0, 0, 10, 0), size = 15), 
        panel.grid.major.x = element_line(linetype = 3, linewidth = 0.3, 
                                          color = "#808080"), 
        axis.title.x = element_text(margin = margin(10, 0, 0, 0), size = 15), 
        axis.text.x = element_text(size = 14, color = "#000000"), 
        axis.text.y = element_text(margin = margin(0, -5, 0, 0), size = 14, 
                                   color = "#000000", hjust = 1),
        legend.position = "top", 
        legend.key.size = unit(0.55, "cm"), 
        legend.text = element_text(size = 15), 
        legend.spacing.x = unit(0.25, "cm"),
        legend.margin = margin(0, 0, 5, 0)) 

# Process the data and create a document-term matrix
interaction_dtm <- safety |> 
  tokenize_text("int_comment") |>
  filter(type == "unigram") |>
  drop_na(ethnicity) |>
  add_count(ngram) |>
  filter(between(n, 2, 65) & !ngram %in% c("black", "comment", "cop", 
                                           "department", "department's", 
                                           "interact", "metro", "metropolitan", 
                                           "policeman", "public", "safety", 
                                           "ward", "white")) |>
  select(-n) |>
  count(ethnicity, ngram) |>
  cast_dtm(document = ethnicity, term = ngram, value = n) |>
  as.matrix()

# Perform hyperparameter tuning to get the optimal number of topics
interaction_hp <- FindTopicsNumber(interaction_dtm, 
                                   topics = seq(from = 2, to = 20, by = 1), 
                                   metrics = "CaoJuan2009", 
                                   method = "Gibbs", 
                                   control = list(seed = 123)) |>   
  as_tibble() |>
  filter(CaoJuan2009 == min(CaoJuan2009))

# Use latent dirichlet allocation for topic modeling
interaction_lda <- LDA(interaction_dtm, k = interaction_hp$topics, 
                       control = list(seed = 123)) 

# Perform processing steps to obtain the top five words for each topic
interaction_lda_op1 <- interaction_lda |>
  tidy() |>
  group_by(topic) |>
  slice_max(beta, n = 5, with_ties = FALSE) |> 
  ungroup() |>
  arrange(topic, desc(beta)) |>
  rename(ngram = term,
         probability = beta) |>
  mutate(topic = str_c("Topic ", topic), 
         ngram = reorder_within(str_to_title(ngram), probability, topic, 
                                sep = "_")) 

# Create a faceted bar chart to display the term probabilities of topics
ggplot(interaction_lda_op1, aes(x = ngram, y = probability)) +   
  geom_col(width = 0.8, fill = "#0078ae") +
  geom_hline(yintercept = 0, linewidth = 1.15, color = "#000000") + 
  scale_x_discrete(labels = function(x) str_replace_all(x, "_.*", "")) +
  scale_y_continuous(breaks = pretty_breaks(3), 
                     labels = label_number(drop0trailing = TRUE)) + 
  labs(title = str_squish("Figure 6: Top Terms in Topics from Descriptions of 
                           Interactions with the MPD"),  
       x = "", y = "Term Probability") +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  theme_void() +
  theme(plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),  
        panel.spacing.x = unit(2.25, "lines"), 
        panel.spacing.y = unit(1.25, "lines"), 
        text = element_text(family = "Roboto"),   
        plot.title = element_text(margin = margin(0, 0, 15, 0), hjust = 0.5, 
                                  size = 17, face = "bold"), 
        strip.text = element_text(margin = margin(0, 0, 10, 0), size = 14), 
        panel.grid.major.x = element_line(linetype = 3, linewidth = 0.3, 
                                          color = "#808080"), 
        axis.title.x = element_text(margin = margin(10, 0, 0, 0), size = 14), 
        axis.text.x = element_text(size = 13, color = "#000000"), 
        axis.text.y = element_text(margin = margin(0, -7.5, 0, 0), size = 13, 
                                   color = "#000000", hjust = 1))

# Perform processing steps to find the most likely topic for each document
interaction_lda_op2 <- interaction_lda |>
  tidy(matrix = "gamma") |>
  mutate(gamma = round(gamma, 3)) |>
  group_by(document) |> 
  slice_max(gamma, n = 1) |> 
  ungroup() |>
  group_by(document) |>
  arrange(topic) |>
  ungroup()

# Part 5: Police Effectiveness

# Select the columns and drop rows
effect_df <- safety |>  
  select(gender, ethnicity, age, int_freq, ward, mpd_effect, police_effect) |>
  drop_na() |>
  # Set factor levels and modify values in columns
  mutate(gender = fct_relevel(gender, "Female"), 
         ethnicity = fct_relevel(ethnicity, "White"),
         int_freq = fct_relevel(int_freq, "Less than Monthly"),
         across(c(mpd_effect, police_effect), ~ if_else(  
           .x == "Effective", 1, 0  
           ))) 

# Generate point-biserial correlation coefficients 
cor(effect_df$age, effect_df$mpd_effect)
cor(effect_df$age, effect_df$police_effect)

# Perform chi-square tests
chisq.test(effect_df$gender, effect_df$mpd_effect)
chisq.test(effect_df$ethnicity, effect_df$mpd_effect)
chisq.test(effect_df$int_freq, effect_df$mpd_effect)
chisq.test(effect_df$gender, effect_df$police_effect)
chisq.test(effect_df$ethnicity, effect_df$police_effect)
chisq.test(effect_df$int_freq, effect_df$police_effect)

# Run the multivariate logistic regression models with fixed effects
effect_lr1 <- glm(mpd_effect ~ gender + ethnicity + age + int_freq + ward, 
                  family = binomial, data = effect_df)
effect_lr2 <- glm(police_effect ~ gender + ethnicity + age + int_freq + ward,   
                  family = binomial, data = effect_df) 

# Confirm no multicollinearity among predictors and linearity in the log odds
car::vif(effect_lr1)
car::vif(effect_lr2)
plot(effect_df$age, predict(effect_lr1, type = "link"))
plot(effect_df$age, predict(effect_lr2, type = "link"))

# Display the summaries of both logistic regression models in a table 
effect_table <- map_dfc(list(effect_lr1, effect_lr2), function(lr) {  
  # Get the marginal effects and filter appropriately   
  processed <- as_tibble(margins_summary(lr)) |>    
    filter(!str_detect(factor, "ward")) |> 
    # Modify values in the columns and create new columns
    mutate(across(c(AME, lower, upper), ~ if_else(   
      abs(.x) >= 0.005, as.character(round(.x, 2)), as.character(round(.x, 3))  
      )),   
      `p-value` = case_when( 
        p < 0.001 ~ "<0.001", 
        abs(p) >= 0.005 ~ as.character(round(p, 2)), 
        TRUE ~ as.character(round(p, 3))   
        ),
      factor = case_when(   
        factor == "age" ~ "Age",
        factor == "ethnicityBlack" ~ "Black",
        factor == "ethnicityOther" ~ "Other",
        factor == "genderMale" ~ "Male", 
        factor == "int_freqAt Least Monthly" ~ "At Least Monthly"  
        )) |> 
    unite(`95% CI`, c(lower, upper), sep = ", ") |> 
    # Drop columns, rename a column, and add rows 
    select(-c(SE, z, p)) |>
    rename(Variable = factor) |>
    bind_rows(tibble(Variable = c("Ethnicity", "White", "Gender", "Female", 
                                  "Interaction", "Less than Monthly"),  
                     AME = c("", "—", "", "—", "", "—"), 
                     `95% CI` = c("", "—", "", "—", "", "—"),
                     `p-value` = c("", "", "", "", "", ""))) |>
    # Change the order of the columns and modify values of a column
    slice(1, 6, 7, 2, 3, 8, 9, 4, 10, 11, 5) |>
    mutate(Variable = if_else(  
      !Variable %in% c("Age", "Ethnicity", "Gender", "Interaction"), 
      paste0("\u00A0\u00A0\u00A0\u00A0", Variable), Variable 
      ))    
  
  # Dynamically rename columns based on the model
  if (identical(lr, effect_lr1)) {   
    processed <- processed |>  
      rename(AME_one = AME,
             `95% CI_one` = `95% CI`, 
             `p-value_one` = `p-value`) 
  } else if (identical(lr, effect_lr2)) { 
    processed <- processed |> 
      select(-Variable) |>
      rename(AME_two = AME,
             `95% CI_two` = `95% CI`,
             `p-value_two` = `p-value`)
  }
})

# Create a table to display the outputs from the regression models
gt(effect_table) |>
  tab_header(title = md("**Table 1: Comparative Results of Multivariate Logistic
                           Regression Models**")) |>
  cols_align(align = "center", columns = -Variable) |>
  cols_label(AME_one = "AME",
             `95% CI_one` = "95% CI", 
             `p-value_one` = "p-value", 
             AME_two = "AME",
             `95% CI_two` = "95% CI",
             `p-value_two` = "p-value") |>
  tab_spanner(label = "Model 1", columns = c(AME_one, `95% CI_one`, 
                                             `p-value_one`)) |>
  tab_spanner(label = "Model 2", columns = c(AME_two, `95% CI_two`, 
                                             `p-value_two`)) |>
  tab_footnote(footnote = "Includes fixed effects for D.C. wards") |>
  tab_footnote(footnote = "AME = Average Marginal Effect, CI = Confidence 
                           Interval") |>
  tab_options(table.width = "95%", table.font.names = "Roboto", 
              table.font.size = px(18)) |>
  opt_stylize(style = 6) 

# Tokenize to generate n-grams and calculate tf-idf
effect_tfidf <- safety |>
  tokenize_text("imp_qual") |>  
  filter(type == "unigram") |> 
  drop_na(ethnicity) |>
  count(ethnicity, ngram) |> 
  filter(n > 3) |> 
  bind_tf_idf(ngram, ethnicity, n) |>
  select(-c(tf, idf)) |>
  # Filter based on the set conditions
  filter((ethnicity == "Black" & ngram %in% c("alert", "consistency", "genuine", 
                                              "service")) |
         (ethnicity == "Other" & ngram %in% c("competent", "culture", 
                                              "diverse")) | 
         (ethnicity == "White" & ngram %in% c("awareness", "connection", 
                                              "lead"))) |>  
  # Modify values in a column and create a new column
  mutate(ngram = str_to_title(ngram), 
         scaled_tf_idf = tf_idf * 1000) 

# Create a bar chart to display scaled tf-idf scores 
ggplot(effect_tfidf, aes(x = reorder(ngram, scaled_tf_idf), y = scaled_tf_idf, 
                         fill = ethnicity)) +
  geom_col(width = 0.825) +
  geom_hline(yintercept = 0, linewidth = 1.35, color = "#000000") +
  scale_fill_manual(values = c("#0078ae", "#5e9732", "#ffac1c")) +
  labs(title = str_squish("Figure 7: TF-IDF Scores from Descriptions of 
                           Qualities of Effective Policing"),  
       x = "", y = "Scaled TF-IDF Score (x1,000)") +
  guides(fill = guide_legend(title = "")) +
  coord_flip() + 
  theme_custom()

# Select columns, rename columns, and reshape the data to be longer
effect_actions <- safety |> 
  select(moretraining_effect:increaseforce_effect) |>
  rename_with(~ str_remove(.x, "_effect$")) |>
  pivot_longer(cols = everything(),
               names_to = "action",
               values_to = "response") |>
  # Drop rows and modify values in columns
  drop_na(response) |>
  mutate(action = case_when( 
    action == "communityrelations" ~ "Improve Community Relationships",
    action == "increaseforce" ~ "Increase in Force Size",
    action == "increasepresance" ~ "Increase in Presence", 
    action == "infrastructure" ~ "Improve Infrastructure",
    action == "moretraining" ~ "Increase/Improve Training",
    action == "respond911" ~ "911 Call Responsiveness"      
    ) |>          
                  factor(levels = c("Improve Infrastructure", 
                                    "Increase in Force Size",
                                    "Increase in Presence",
                                    "Increase/Improve Training",
                                    "911 Call Responsiveness",
                                    "Improve Community Relationships")),
         response = factor(response, levels = c("Very Effective", "Effective",
                                                "Somewhat Effective", 
                                                "Not Effective"))) |>
  # Get proportions
  count(action, response) |>
  group_by(action) |>
  mutate(prop = n / sum(n),
         pct = if_else(  
           prop >= 0.05, percent(prop, accuracy = 1), "" 
           )) |> 
  ungroup() |> 
  select(-n)

# Output the stacked bar chart to display the proportions
figure8 <- plot_props(effect_actions, "action", "prop", "response", -12.5) 

# Part 6: Police Chief

# Reshape the data to be longer and tokenize to generate n-grams
chief_issue <- safety |>      
  pivot_longer(cols = c(issue1, issue2, issue3), 
               names_to = "issue_rank", 
               values_to = "response") |>  
  tokenize_text("response") |> 
  # Modify values in certain columns and change a data type
  mutate(issue_rank = case_when(  
    issue_rank == "issue1" ~ "Top Issue", 
    issue_rank == "issue2" ~ "Second Most Important Issue", 
    issue_rank == "issue3" ~ "Third Most Important Issue"       
    ) |>              
                      factor(levels = c("Third Most Important Issue", 
                                        "Second Most Important Issue", 
                                        "Top Issue")),   
    ngram = case_when(     
      ngram == "drug" ~ "drugs",
      ngram == "gang" ~ "gangs",
      ngram == "train" ~ "training",
      ngram == "violent crimes" ~ "violent crime", 
      TRUE ~ ngram  
      )) |>  
  # Generate term frequencies, filter, and modify values in a column
  count(issue_rank, ngram) |>
  filter(ngram %in% c("community policing", "community relations", 
                      "crime reduction", "drugs", "gangs", "illegal guns", 
                      "traffic", "training", "violent crime", "youth")) |> 
  mutate(ngram = str_to_title(ngram)) 

# Create a stacked bar chart to display term frequencies 
ggplot(chief_issue, aes(x = reorder(ngram, n), y = n, fill = issue_rank)) +  
  geom_col(width = 0.825) + 
  geom_hline(yintercept = 0, linewidth = 1.35, color = "#000000") +
  scale_fill_manual(values = c("#ffac1c", "#5e9732", "#0078ae")) +
  labs(title = str_squish("Figure 9: Term Frequencies for Top Issues the Police 
                           Chief Should Focus On"),  
       x = "", y = "Frequency") +
  guides(fill = guide_legend(title = "", reverse = TRUE)) +
  coord_flip() + 
  theme_custom()  

# Drop rows, breaks values into multiple rows, and modify values in a column
chief_lead <- safety |> 
  drop_na(lead_qual) |> 
  separate_longer_delim(lead_qual, delim = ", ") |> 
  mutate(lead_qual = case_when( 
    lead_qual == "Builds High Morale Among Police Officers" ~ "Builds Morale", 
    lead_qual == "Has High Ethical and Moral Standards" ~ "Ethical Standards", 
    lead_qual == "Is Approachable and Can Relate to Others" ~ "Approachable", 
    lead_qual == "Is Committed to Ongoing Training" ~ "Committed to Training", 
    lead_qual == "Is Creative and Open to New Ideas" ~ "Creative and Open", 
    lead_qual == "Optimistic about the Future" ~ "Optimistic", 
    lead_qual == "Provides Clear Vision and Goals" ~ "Clear Vision", 
    lead_qual == "Strong Work Ethic" ~ "Work Ethic", 
    lead_qual == "Willingness to Listen" ~ "Willing to Listen", 
    TRUE ~ lead_qual     
    )) |>
  # Generate proportions and the appropriate labels 
  count(lead_qual) |> 
  mutate(prop = round(n / sum(n), 2), 
         label = if_else(    
           prop < 0.11, "10% or Less", "Greater than 10%"  
           )) 

# Create a stacked bar chart to display proportions
ggplot(chief_lead, aes(x = reorder(lead_qual, prop), y = prop, fill = label)) +  
  geom_col(width = 0.825) + 
  geom_text(aes(label = percent(prop)), vjust = 0.25, hjust = -0.25, 
            size = 4.85) +
  geom_hline(yintercept = 0, linewidth = 1.35, color = "#000000") + 
  scale_y_continuous(limits = c(0, 0.175), breaks = seq(0, 0.175, by = 0.05), 
                     labels = percent) + 
  scale_fill_manual(values = c("#828284", "#005288")) +
  labs(title = str_squish("Figure 10: Percentage of Key Leadership Qualities for 
                           the Police Chief"),  
       x = "", y = "") + 
  guides(fill = guide_legend(title = "")) + 
  coord_flip() + 
  theme_custom()