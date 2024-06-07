
library(shiny) 
library(rsconnect) 
library(ggplot2) 
library(dplyr) 
library(ggtree) 
library(rotl)  
library(slider) 
library(tidyquant)  
library(gt)  
library(plotbiomes) 
library(rgbif) 
library(sp) 
library(rinat)
library(RColorBrewer)
library(curl) 
library(maps)


# Définir le serveur
server <- function(input, output, session) {


world <- map_data("world")

cover_genus_garden_full <- read.csv(curl::curl("https://raw.githubusercontent.com/MazzarineL/SBG_app/main/data/cover_genus_garden_500.csv") )
cover_species_garden_full <- read.csv(curl::curl("https://raw.githubusercontent.com/MazzarineL/SBG_app/main/data/cover_species_garden_500.csv") )
whit_part1.1 <- read.csv(curl::curl("https://raw.githubusercontent.com/MazzarineL/SBG_app/main/data/data_env_gift_part1.csv"), sep = ";")
whit_part1.2 <-  read.csv(curl::curl("https://raw.githubusercontent.com/MazzarineL/SBG_app/main/data/data_env_gift_part2.csv"), sep = ";")
whit_part1.3 <-  read.csv(curl::curl("https://raw.githubusercontent.com/MazzarineL/SBG_app/main/data/data_env_gift_part3.csv"), sep = ";")
whit_part1.4 <-  read.csv(curl::curl("https://raw.githubusercontent.com/MazzarineL/SBG_app/main/data/data_env_gift_part1.4.csv"), sep = ",")
whit_part2 <-  read.csv(curl::curl("https://raw.githubusercontent.com/MazzarineL/SBG_app/main/data/data_env_gift_part4.csv"), sep = ";")

whit_part1.4 <- whit_part1.4 %>% dplyr::select(-biome)

whit_part1 <- rbind(whit_part1.1, whit_part1.2,whit_part1.3,whit_part1.4)  


  observeEvent(input$action, {
    withProgress(message = 'Chargement des données...', value = 0, {
      output$treePlot <- NULL
      req(input$Garden != "")
      
      # Initialisation des variables
      taxonomy_merge <- cover_genus_garden_full
      input_code <- input$Garden
      
      # Mise à jour de la progression
      incProgress(1/6, detail = "Traitement des codes jardins...")
      
      # Si une seule option est sélectionnée
      if (length(input_code) == 1) {
        taxonomy_merge$code_garden[!is.na(taxonomy_merge$code_garden)] <- input_code
        taxonomy_merge$code_garden[taxonomy_merge$pres == 0] <- NA
      } else {
        selected_values <- paste(input_code, collapse = "|")
        taxonomy_merge$code_garden[!grepl(selected_values, taxonomy_merge$code_garden)] <- NA
        entier <- c("fr", "ne", "la")
        diff <- setdiff(entier, input_code)
        taxonomy_merge$code_garden <- gsub(paste(diff, collapse = "|"), "", taxonomy_merge$code_garden)
        taxonomy_merge$code_garden <- gsub("_+", "_", taxonomy_merge$code_garden)
        taxonomy_merge$code_garden <- gsub("^_|_$", "", taxonomy_merge$code_garden)
      }
      
      incProgress(2/6, detail = "Filtrage des données...")

      taxonomy_merge$pres[is.na(taxonomy_merge$pres)] <- 0
      taxonomy_merge <- taxonomy_merge %>%
        mutate(code_garden = na_if(code_garden, ""))

      taxonomy_merge <- taxonomy_merge[!is.na(taxonomy_merge$ott_id.family), ]
      
      incProgress(3/6, detail = "Génération de l'arbre phylogénétique...")

      my_tree <- rotl::tol_induced_subtree(ott_ids = taxonomy_merge$ott_id.family)
      sp_name <- gsub("_.*", "", my_tree$tip.label)
      my_tree$tip.label <- sp_name
      family <- taxonomy_merge$family
      g <- split(family, taxonomy_merge$code_garden)

      incProgress(4/6, detail = "Création du graphique...")

      output$treePlot <- renderPlot({
        isolate({
          tree_plot <- ggtree::ggtree(my_tree, layout = "circular") +
            geom_tiplab(size = 2, offset = 0.5)

          g2 <- ggtree::groupOTU(tree_plot, g, "family") +
            aes(color = family) +
            theme(legend.position = "right") +
            scale_color_manual(
              name = "Family",
              values = c(
                "fr_ne"    = "orange", 
                "fr_la"    = "darkgreen", 
                "fr_la_ne" = "darkblue", 
                "fr"       = "purple", 
                "ne"       = "red", 
                "la"       = "brown",
                "la_ne"    = "pink",
                "NA"       = "grey"
              ),
              labels = c(
                "fr_ne"    = "Available in Fribourg and Neuchâtel", 
                "fr_la"    = "Available in Fribourg and Lausanne", 
                "fr_la_ne" = "Available in Fribourg, Lausanne, and Neuchâtel", 
                "fr"       = "Available in Fribourg",
                "ne"       = "Available in Neuchâtel", 
                "la"       = "Available in Lausanne",
                "la_ne"    = "Available in Lausanne and Neuchâtel",
                "NA"       = "Not available"
              ),
              breaks = c(
                "fr_ne", 
                "fr_la", 
                "fr_la_ne", 
                "fr", 
                "ne", 
                "la", 
                "la_ne", 
                "NA"
              )
            ) +
            theme(legend.title = element_text(size = 20), 
                  legend.text = element_text(size = 15)) 

          print(g2)

          output$downloadFullPlot <- downloadHandler(
            filename = function() {
              paste0("Tree_garden_plot_", Sys.Date(), ".pdf")
            },
            content = function(file) {
               ggsave(filename = file, plot = g2, device = "pdf", width = 50, height = 50, units = "cm")

            }
          )
        })
      })
    })
  })


############################################

observeEvent(c(input$action, input$step, input$window), {
  withProgress(message = 'Chargement des données...', value = 0, {
    req(input$Garden != "")
    
    family_test <- input$family
    output$onlygenus <- NULL
    output$mytable <- NULL
    output$FamilyPlot <- NULL
    output$textgenus <- NULL
    genus_cover <- NULL
    step <- input$step
    window <- input$window
    input_code <- input$Garden
    cover_genus_garden <- cover_genus_garden_full

    # Étape de mise à jour de la progression
    incProgress(1/6, detail = "Préparation des données...")
    

      if (length(input_code) == 1) {
        cover_genus_garden$code_garden <- ifelse(!grepl(paste(input_code, collapse = "|"), cover_genus_garden$code_garden), NA, cover_genus_garden$code_garden)
        cover_genus_garden$code_garden[!is.na(cover_genus_garden$code_garden)] <- paste(input_code, collapse = "_") 
      } else {
        selected_values <- paste(input_code, collapse = "|")
        cover_genus_garden$code_garden[!grepl(selected_values, cover_genus_garden$code_garden)] <- NA
        entier <- c("fr", "ne", "la")
        diff <- setdiff(entier, input_code)
        cover_genus_garden$code_garden <- gsub(paste(diff, collapse = "|"), "", cover_genus_garden$code_garden)
        cover_genus_garden$code_garden <- gsub("_+", "_", cover_genus_garden$code_garden)
        cover_genus_garden$code_garden <- gsub("^_|_$", "", cover_genus_garden$code_garden)
      }

      incProgress(2/6, detail = "Filtrage des données...")

      unique_genera_count <- cover_genus_garden_full %>%
        filter(family == family_test) %>%
        distinct(genus) %>%
        nrow()

      if (unique_genera_count == 1) {
        output$onlygenus <- renderTable({
          genus_line <- subset(cover_species_garden_full, family == family_test)
          genus_line
        })
        output$textgenus <- renderText({
          "Tree not available, there is only one genus in this family"
        })
        output$FamilyPlot <- renderPlot({})
      } else {
        cover_genus_garden <- cover_genus_garden %>%
          filter(family == family_test)
        cover_genus_garden$pres[is.na(cover_genus_garden$pres)] <- 0
        cover_genus_garden <- cover_genus_garden %>%
          mutate(code_garden = na_if(code_garden, ""))

        cover_genus_garden <- cover_genus_garden[!is.na(cover_genus_garden$ott_id.family), ]
        cover_genus_garden$genus <- gsub("^x ", "", cover_genus_garden$genus)
        cover_genus_garden$pres[is.na(cover_genus_garden$code_garden)] <- 0

        incProgress(3/10, detail = "Génération de l'arbre phylogénétique...")

        tree <- rotl::tol_induced_subtree(ott_ids = cover_genus_garden$uid)
        tree$tip.label <- gsub("^x_|\\(genus_in_kingdom_Archaeplastida\\)_|_.*", "", tree$tip.label)

        p <- ggtree(tree) + geom_tiplab()

        df_rangement <- data.frame(genus = get_taxa_name(p))
        df_rangement <- merge(df_rangement, cover_genus_garden, by = "genus", all.x = TRUE, sort = FALSE)
        df_rangement <- df_rangement %>%
          mutate(date = as.Date('2023-01-01') + row_number() - 1)
        df_rangement <- df_rangement %>%
          dplyr::mutate(reg_7day = slider::slide_dbl(pres, .f = ~sum(.x, na.rm = TRUE), .before = window, .after = window, .step = step))
        df_rangement <- df_rangement %>%
          dplyr::mutate(pres = if_else(reg_7day == 0 & pres == 0, 3, pres))

        incProgress(4/6, detail = "Préparation de la table...")

        output$mytable <- gt::render_gt({
          df_rangement_priority <- df_rangement %>% filter(pres == 3) %>% select(genus)

          length_to_pad <- (3 - length(df_rangement_priority$genus) %% 3) %% 3
          padded_genus <- c(df_rangement_priority$genus, rep(NA, length_to_pad))

          matrix_genus <- matrix(padded_genus, ncol = 3, byrow = TRUE)
          df_table <- as.data.frame(matrix_genus)

          gt(df_table) %>%
            gt::tab_header(
              title = md("Genus to select")
            )
        })

        output$downloadTable <- downloadHandler(
          filename = function() {
            paste("Priority_", family_test, ".csv", sep = "")
          },
          content = function(file) {
            df_rangement_priority <- df_rangement %>% filter(pres == 3) %>% select(genus)
            write.csv(df_rangement_priority, file, row.names = FALSE)
          }
        )

        incProgress(5/6, detail = "Préparation de l'intrigue de la famille...")

        output$FamilyPlot <- renderPlot({
          isolate({
            tree_family <- ggtree::ggtree(tree, layout = "circular") +
              theme(legend.position = "right", legend.key.size = unit(3, "lines")) +
              geom_tiplab(size = 3, offset = 0.5)

            genus_cover <- split(df_rangement$genus, df_rangement$pres)

            tree_family <- ggtree::groupOTU(tree_family, genus_cover, "species") + aes(color = species) +
              theme(
                legend.position = "right",
                legend.text = element_text(size = 14),
                legend.title = element_text(size = 14)
              ) +
              scale_color_manual(
                name = "Genus",
                values = c("0" = "orange", "1" = "darkgreen", "3" = "blue"),
                labels = c("Not available", "Available", "Priority"),
                breaks = c("0", "1", "3")
              ) +
              labs(title = "Tree of ",family_test) +
              theme(legend.title = element_text(size = 20),  
                   legend.text = element_text(size = 15)) 

            print(tree_family)

            output$downloadFamilyPlot <- downloadHandler(
              filename = function() {
                paste0("Tree_plot_", family_test, ".pdf")
              },
              content = function(file) {
              ggsave(filename = file, plot = tree_family, device = "pdf", width = 40, height = 40, units = "cm")
              }
            )
          })
        })

        incProgress(6/6, detail = "Finalisation...")
      }
    })
  })

##################################################################
  observeEvent(input$action, {
    
    output$whitplot  <- NULL
    req(input$Garden != "")
    cover_whit <- cover_species_garden_full
    input_code <- input$Garden
 
 whit_part1$mean_wc2.0_bio_30s_12 <- as.numeric(whit_part1$mean_wc2.0_bio_30s_12)
 whit_part1$mean_wc2.0_bio_30s_01 <- as.numeric(whit_part1$mean_wc2.0_bio_30s_01)


  whit_part1 <- whit_part1 %>%
  dplyr::select(species, where(is.numeric))
  
  whit_part1 <- whit_part1 %>%
  dplyr::group_by(species) %>%
  dplyr::summarise_all(mean, na.rm = TRUE)

  whit_part2 <- whit_part2[!duplicated(whit_part2$species), ]

  data_env_select <- whit_part1[, c(1,4,5)]
  mean_df_select <- whit_part2[, c(1,4, 5)]

  colnames(data_env_select) <- c("species","temperature", "precipitation")
  colnames(mean_df_select) <- c("species", "temperature", "precipitation")
  mean_df_select$temperature <- mean_df_select$temperature / 10
  data_clim <- rbind(mean_df_select, data_env_select)  
  data_clim$precipitation <- as.numeric(data_clim$precipitation)
  data_clim$temperature <- as.numeric(data_clim$temperature)
  data_clim$species <- as.factor(data_clim$species)
  data_clim$precipitation <- data_clim$precipitation / 10
# Parcourir chaque valeur unique dans genus de cover_whit

unique_species <- unique(cover_whit$species)
for (species in unique_species) {
  # Sélectionner les lignes correspondantes dans select_taxo
  select_taxo <- cover_whit[cover_whit$species == species, ]
  
  # Extraire les valeurs uniques dans la colonne garden
  unique_gardens <- unique(select_taxo$garden)
  
  # Trier les valeurs uniques par ordre alphabétique et les combiner en une chaîne
  sorted_gardens <- sort(unique_gardens)
  code_garden <- paste(sorted_gardens, collapse = "_")
  
  # Mettre cette chaîne dans cover_whit$code_garden pour le species en cours
  cover_whit$code_garden[cover_whit$species == species] <- code_garden
}
    if(length(input_code) == 1) {
    cover_whit$code_garden <- ifelse(!grepl(paste(input_code, collapse = "|"), cover_whit$code_garden), NA, cover_whit$code_garden)
    cover_whit$code_garden[!is.na(cover_whit$code_garden)] <- paste(input_code, collapse = "_")
    cover_whit <- cover_whit %>% filter(!is.na(code_garden))
    } else {
    selected_values <- paste(input_code, collapse = "|")
    cover_whit$code_garden[!grepl(selected_values, cover_whit$code_garden)] <- NA
    entier <- c("fr", "ne", "la")
    diff <- setdiff(entier, input_code)
    diff <- setdiff(entier, input_code)
    cover_whit$code_garden <- gsub(paste(diff, collapse = "|"), "", cover_whit$code_garden)
    cover_whit$code_garden <- gsub("_+", "_", cover_whit$code_garden)
    cover_whit$code_garden <- gsub("^_|_$", "", cover_whit$code_garden)
    cover_whit <- cover_whit %>% filter(!is.na(code_garden))
      }
  cover_whit <- cover_whit %>%  dplyr::distinct(species, .keep_all = TRUE)
  data_clim <- merge(data_clim, cover_whit, by = "species")
  data_clim$jardin <- as.factor(data_clim$code_garden)
  data_clim <- data_clim %>%  dplyr::mutate(temperature = ifelse(species %in% c("Drosera spathulata", "Duvalia modesta"), 25, temperature))

  data_clim <- data_clim %>% filter(!is.na(temperature))
  data_clim <- data_clim[!duplicated(data_clim$species), ]


# Votre code ggplot avec l'ajout de scale_color_manual()
 output$whitplot <- renderPlot({

  isolate({

whit <- plotbiomes::whittaker_base_plot() +
  geom_point(data = data_clim, 
             aes(x = temperature, 
                 y = precipitation,
                 color = code_garden),  
             size   = 0.5,             
             shape  = 16,
             alpha  = 0.8) +

  scale_color_manual(
    name = "Family",
    values = c(
      "fr_ne"    = "orange", 
      "fr_la"    = "darkgreen", 
      "fr_la_ne" = "darkblue", 
      "fr"       = "purple", 
      "ne"       = "red", 
      "la"       = "brown",
      "la_ne"    = "pink",
      "NA" = "grey"
    ),
    labels = c(
      "fr_ne"    = "Available in Fribourg and Neuchâtel", 
      "fr_la"    = "Available in Fribourg and Lausanne", 
      "fr_la_ne" = "Available in Fribourg, Lausanne, and Neuchâtel", 
      "fr"       = "Available in Fribourg",
      "ne"       = "Available in Neuchâtel", 
      "la"       = "Available in Lausanne",
      "la_ne"    = "Available in Lausanne and Neuchâtel",
      "NA" = "Not available"
    ),
    breaks = c(
      "fr_ne", 
      "fr_la", 
      "fr_la_ne", 
      "fr", 
      "ne", 
      "la", 
      "la_ne", 
      "NA"
    )
  ) +
  labs(title = paste("Whittaker plot")) +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white", color = NA)) +
  guides(color = guide_legend(override.aes = list(size = 3))) 
print(whit)


output$dlwhitplot <- downloadHandler(
      filename = function() {
        paste0("whit_full_plot_", Sys.Date(), ".pdf")
      },
      content = function(file) {
        ggsave(filename = file, plot = whit, device = "pdf", width = 14, height = 10)
      }
        )
      })
  })


output$whitplotFamily <- renderPlot({
isolate({

family_test <- input$family
data_clim_sub <- subset(data_clim, family == family_test)

data_clim_sub <- data_clim_sub %>%
  filter(!is.na(temperature) & !is.na(precipitation) & 
         is.finite(temperature) & is.finite(precipitation))


 whitfamily <- plotbiomes::whittaker_base_plot() +
  geom_point(data = data_clim_sub, 
             aes(x = temperature, 
                 y = precipitation,
                 color = code_garden),  
             size = 1,             
             shape = 16,
             alpha = 0.8) +
  scale_color_manual(
    name = "Garden",
    values = c(
      "fr_ne"    = "orange", 
      "fr_la"    = "darkgreen", 
      "fr_la_ne" = "darkblue", 
      "fr"       = "purple", 
      "ne"       = "red", 
      "la"       = "brown",
      "la_ne"    = "pink",
      "NA" = "grey"
    ),
    labels = c(
      "fr_ne"    = "Available in Fribourg and Neuchâtel", 
      "fr_la"    = "Available in Fribourg and Lausanne", 
      "fr_la_ne" = "Available in Fribourg, Lausanne, and Neuchâtel", 
      "fr"       = "Available in Fribourg",
      "ne"       = "Available in Neuchâtel", 
      "la"       = "Available in Lausanne",
      "la_ne"    = "Available in Lausanne and Neuchâtel",
      "NA" = "Not available"
    ),
    breaks = c(
      "fr_ne", 
      "fr_la", 
      "fr_la_ne", 
      "fr", 
      "ne", 
      "la", 
      "la_ne", 
      "NA"
    )
  ) +
  labs(title = "Whittaker plot") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white", color = NA)) +
  guides(color = guide_legend(override.aes = list(size = 1))) 

print(whitfamily)


 output$dlwhitplotFamily <- downloadHandler(
          filename = function() {
            paste0("whit_plot_", family_test, ".pdf")
          },
          content = function(file) {
            ggsave(filename = file, plot = whitfamily, device = "pdf", width = 14, height = 10)
          }
        )
      })
      })


output$whitplotFamilyKernel <- renderPlot({
isolate({

family_test <- input$family
data_clim_sub <- subset(data_clim, family == family_test)

data_clim_sub <- data_clim_sub %>%
  filter(!is.na(temperature) & !is.na(precipitation) & 
         is.finite(temperature) & is.finite(precipitation))


 whitfamilyKernel <- plotbiomes::whittaker_base_plot() +
  geom_point(data = data_clim_sub, 
             aes(x = temperature, 
                 y = precipitation,
                 color = code_garden),  
             size = 1,             
             shape = 16,
             alpha = 0.8) +

             
  stat_density_2d(data = data_clim, 
                  aes(x = temperature, 
                      y = precipitation, 
                      color = code_garden),
                  size = 0.5,  
                  alpha = 0.3, 
                  h = 10) +  
  
  
  scale_color_manual(
    name = "Garden",
    values = c(
      "fr_ne"    = "orange", 
      "fr_la"    = "darkgreen", 
      "fr_la_ne" = "darkblue", 
      "fr"       = "purple", 
      "ne"       = "red", 
      "la"       = "brown",
      "la_ne"    = "pink",
      "NA" = "grey"
    ),
    labels = c(
      "fr_ne"    = "Available in Fribourg and Neuchâtel", 
      "fr_la"    = "Available in Fribourg and Lausanne", 
      "fr_la_ne" = "Available in Fribourg, Lausanne, and Neuchâtel", 
      "fr"       = "Available in Fribourg",
      "ne"       = "Available in Neuchâtel", 
      "la"       = "Available in Lausanne",
      "la_ne"    = "Available in Lausanne and Neuchâtel",
      "NA" = "Not available"
    ),
    breaks = c(
      "fr_ne", 
      "fr_la", 
      "fr_la_ne", 
      "fr", 
      "ne", 
      "la", 
      "la_ne", 
      "NA"
    )
  ) +
  labs(title = paste("Whittaker plot for", family_test)) +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white", color = NA)) +
   guides(color = guide_legend(override.aes = list(size = 5))) 

print(whitfamilyKernel)


 output$dlwhitplotFamilyKernel <- downloadHandler(
          filename = function() {
            paste0("whit_plot_Kernel_", family_test, ".pdf")
          },
          content = function(file) {
            ggsave(filename = file, plot = whitfamilyKernel, device = "pdf", width = 14, height = 10)
          }
        )
      })
      })






output$whitplotSelect <- renderPlotly({
  isolate({
    family_test <- input$family
    data_clim_sub <- subset(data_clim, family == family_test)

   data_clim_sub <- data_clim_sub %>%
         filter(!is.na(temperature) & !is.na(precipitation) & 
         is.finite(temperature) & is.finite(precipitation))



  data_clim_sub$garden <- as.factor(data_clim_sub$garden)
  data_clim_sub$garden <- gsub("fr", "Fribourg", gsub("ne", "Neuchâtel", gsub("la", "Lausanne", data_clim_sub$garden)))
  data_clim_sub$garden <- ifelse(data_clim_sub$garden == "Fribourg_LausanNeuchâtel_Neuchâtel", 
                                   "Fribourg_Lausanne_Neuchâtel", 
                                   data_clim_sub$garden)
  data_clim_sub$garden <- ifelse(data_clim_sub$garden == "LausanNeuchâtel_Neuchâtel", 
                                   "Lausanne_Neuchâtel", 
                                   data_clim_sub$garden)
  data_clim_sub$garden <- ifelse(data_clim_sub$garden == "LausanNeuchâtel", 
                                   "Lausanne", 
                                   data_clim_sub$garden)




    data_clim_sub <- subset(data_clim, family == family_test)
    data_clim_sub <- data_clim_sub %>%
      filter(!is.na(temperature) & !is.na(precipitation) & 
             is.finite(temperature) & is.finite(precipitation))

    plot <- ggplot(data_clim_sub, aes(x = temperature, y = precipitation, color = code_garden, text = paste("Species:", species))) + 
      geom_point(size = 1, shape = 16, alpha = 0.8) +
      labs(title = "Whittaker plot") +
      theme_minimal() +
      theme(panel.background = element_rect(fill = "white", color = NA)) +
      scale_color_manual(
        name = "Garden",
        values = c(
          "fr_ne"    = "orange", 
          "fr_la"    = "darkgreen", 
          "fr_la_ne" = "darkblue", 
          "fr"       = "purple", 
          "ne"       = "red", 
          "la"       = "brown",
          "la_ne"    = "pink",
          "NA" = "grey"
        ),
        labels = c(
          "fr_ne"    = "Available in Fribourg and Neuchâtel", 
          "fr_la"    = "Available in Fribourg and Lausanne", 
          "fr_la_ne" = "Available in Fribourg, Lausanne, and Neuchâtel", 
          "fr"       = "Available in Fribourg",
          "ne"       = "Available in Neuchâtel", 
          "la"       = "Available in Lausanne",
          "la_ne"    = "Available in Lausanne and Neuchâtel",
          "NA" = "Not available"
        ),
        breaks = c(
          "fr_ne", 
          "fr_la", 
          "fr_la_ne", 
          "fr", 
          "ne", 
          "la", 
          "la_ne", 
          "NA"
        )
      ) +
      coord_cartesian(xlim = c(-15, 30), ylim = c(-5, 450))
    ggplotly(plot, tooltip = "text") 
  })
})


})


  ##############################################

# Filtrer les données en fonction du jardin sélectionné
filtered_data <- reactive({
  req(input$Garden)
  cover_species_garden_full %>%
    filter(garden %in% input$Garden)
})

# Mettre à jour les choix de genre en fonction de la famille sélectionnée
observeEvent(input$family, {
  req(filtered_data())
  updateSelectInput(session, "GPS_genus", choices = unique(filtered_data() %>% filter(family == input$family) %>% pull(genus)))
})

# Mettre à jour les choix d'espèce en fonction du genre sélectionné
observeEvent(input$GPS_genus, {
  req(filtered_data())
  updateSelectInput(session, "GPS_species", choices = unique(filtered_data() %>% filter(family == input$family & genus == input$GPS_genus) %>% pull(species)))
})

# Fonction réactive pour le tracé de la carte
observeEvent(input$goButton, {
  req(filtered_data())

  family_map <- filtered_data()

  if (!is.null(input$GPS_species) && length(input$GPS_species) > 0) {
    family_map <- family_map %>% filter(species %in% input$GPS_species)
  }

  # Initialiser un dataframe vide pour stocker les données GPS
  all_gps_data <- data.frame()

  # Afficher la barre de chargement
  withProgress(message = 'Chargement des données...', value = 0, {
    n <- nrow(family_map)
    # Boucle pour chaque espèce
    for (i in 1:n) {
      tryCatch({
        species_name <- family_map$species[i]
        # Recherche des données sur iNaturalist
        especetest <- rinat::get_inat_obs(query = species_name, maxresults = 100)
        selected_columns <- c("longitude", "latitude", "quality_grade", "captive_cultivated")
        data_inat <- especetest[selected_columns]
        data_inat <- data_inat %>%
          dplyr::filter(quality_grade == "research" & captive_cultivated == "false") %>%
          select(longitude, latitude)

        # Recherche des données sur GBIF
        gbif_data <- rgbif::occ_data(scientificName = species_name, hasCoordinate = TRUE, limit = 100)

        # Vérifier si les données de GBIF existent
        if (!is.null(gbif_data$data)) {
          data_gbif <- gbif_data$data
          coordinates(data_gbif) <- c("decimalLongitude", "decimalLatitude")
          proj4string(data_gbif) <- sp::CRS("+proj=longlat +datum=WGS84")
          data_gbif_wgs84 <- sp::spTransform(data_gbif, CRS("+init=epsg:4326"))
          longitude <- sp::coordinates(data_gbif_wgs84)[, 1]
          latitude <- sp::coordinates(data_gbif_wgs84)[, 2]
          df_with_long_lat <- data.frame(longitude = longitude, latitude = latitude)
          data_gbif_selected <- df_with_long_lat 
          data_gbif_selected <- data_gbif_selected[!duplicated(data_gbif_selected[c("longitude", "latitude")]), ]
        } else {
          # Si les données de GBIF sont vides, créer un dataframe vide
          data_gbif_selected <- data.frame(longitude = numeric(0), latitude = numeric(0))
        }

        # Étape 3: Si les deux jeux de données sont non vides, fusionner les données et effectuer les étapes restantes
        if (is.data.frame(data_inat) && nrow(data_inat) > 0 && 
            is.data.frame(data_gbif_selected) && nrow(data_gbif_selected) > 0) {
          
          # Fusionner les données de localisation GBIF et iNaturalist
          data_inat$Source <- "iNaturalist"
          data_gbif_selected$Source <- "GBIF"
          
          # Ajout de la colonne species_gps dans data_gbif_selected
          data_gbif_selected$species_gps <- species_name 
          # Ajout de la colonne species_gps dans data_inat
          data_inat$species_gps <- species_name
          
          # Fusionner les données
          data_gps <- rbind(data_inat, data_gbif_selected)
          
          # Ajouter au dataframe global
          all_gps_data <- rbind(all_gps_data, data_gps)
        }
        
        # Étape 1: Si data_inat est vide, ajouter uniquement les données de data_gbif_selected à all_gps_data
        if (is.data.frame(data_inat) && nrow(data_inat) == 0) {
          data_gbif_selected$Source <- "GBIF"
          data_gbif_selected$species_gps <- species_name
          
          all_gps_data <- rbind(all_gps_data, data_gbif_selected)
        }
        
        # Étape 2: Si data_gbif_selected est vide, ajouter uniquement les données de data_inat à all_gps_data
        if (is.data.frame(data_gbif_selected) && nrow(data_gbif_selected) == 0) {
          data_inat$Source <- "iNaturalist"
          data_inat$species_gps <- species_name
          
          all_gps_data <- rbind(all_gps_data, data_inat)
        }
        
        # Mettre à jour la barre de progression
        incProgress(1/n, detail = paste("Traitement des données", i, "sur", n))
      }, error = function(e) {
        if (grepl("replacement has 1 row, data has 0", e$message)) {
          output$errortext <- renderText({
            paste("Species", species_name, "don't have any data")
          })
        } else {
          # Si une autre erreur se produit, imprimez l'erreur complète
          cat("Error:", e$message, "\n")
        }
      })
    }
  })

  cover_map <- cover_species_garden_full

  unique_species <- unique(cover_map$species)
  for (species in unique_species) {
    # Sélectionner les lignes correspondantes dans select_taxo
    select_taxo <- cover_map[cover_map$species == species, ]
    
    # Extraire les valeurs uniques dans la colonne garden
    unique_gardens <- unique(select_taxo$garden)
    
    # Trier les valeurs uniques par ordre alphabétique et les combiner en une chaîne
    sorted_gardens <- sort(unique_gardens)
    code_garden <- paste(sorted_gardens, collapse = "_")
    
    # Mettre cette chaîne dans cover_map$code_garden pour le species en cours
    cover_map$code_garden[cover_map$species == species] <- code_garden
  }

  cover_map <- cover_map %>% dplyr::distinct(species, .keep_all = TRUE)
  all_gps_data <- merge(all_gps_data, cover_map, by.x = "species_gps", by.y = "species")
  all_gps_data$code_garden <- as.factor(all_gps_data$code_garden)

  all_gps_data$code_garden <- gsub("fr", "Fribourg", gsub("ne", "Neuchâtel", gsub("la", "Lausanne", all_gps_data$code_garden)))

  all_gps_data$code_garden <- ifelse(all_gps_data$code_garden == "LausanNeuchâtel_Neuchâtel", 
                                   "Lausanne_Neuchâtel", 
                                   all_gps_data$code_garden)

  all_gps_data$code_garden <- ifelse(all_gps_data$code_garden == "LausanNeuchâtel", 
                                   "Lausanne", 
                                   all_gps_data$code_garden)

  all_gps_data$code_garden <- ifelse(all_gps_data$code_garden == "Fribourg_LausanNeuchâtel_Neuchâtel", 
                                   "Fribourg_Lausanne_Neuchâtel", 
                                   all_gps_data$code_garden)

 all_gps_data$code_garden <- ifelse(all_gps_data$code_garden == "Fribourg_LausanNeuchâtel", 
                                   "Fribourg_Lausanne", 
                                   all_gps_data$code_garden)


 
output$map <- renderLeaflet({
# Créer une palette de couleurs pour les espèces
    species_palette <- colorFactor(palette = "viridis", domain = all_gps_data$species_gps)
  
    leaflet::leaflet(all_gps_data) %>%
      leaflet::addTiles() %>%
      leaflet::addCircleMarkers(
        ~longitude, ~latitude,
        color = ~species_palette(species_gps),
        fillOpacity = 0.7,
        radius = 5,
        stroke = FALSE
      ) %>%
      leaflet::addLegend(
        "bottomright",
        pal = species_palette,
        values = ~species_gps,
        title = "Species",
        opacity = 1
      )
  })


output$mapsSimple <- renderPlot({
  # Create a color palette for species using viridis
  species_palette <- scale_color_viridis_d()

  ggplot() +
    geom_polygon(data = world, aes(x = long, y = lat, group = group), fill = "lightgray", color = "white") +
    geom_point(data = all_gps_data, aes(x = longitude, y = latitude, color = species_gps, shape = code_garden), size = 1.5) +
    species_palette +
    labs(x = "Longitude", y = "Latitude", color = "Species", shape = "Garden") +
    theme_minimal() +
    theme(legend.position = "right") +
    coord_fixed(ratio = 1.2, xlim = c(min(world$long) - 20, max(world$long) + 20), ylim = c(min(world$lat) - 10, max(world$lat) + 10))
})

  # Fonction pour télécharger la carte
output$downloaddistrib <- downloadHandler(
  filename = function() {
    paste("distribution_map", Sys.Date(), ".pdf", sep = "")
  },
  content = function(file) {

  species_palette <- scale_color_viridis_d()

  carte <- ggplot() +
    geom_polygon(data = world, aes(x = long, y = lat, group = group), fill = "lightgray", color = "white") +
    geom_point(data = all_gps_data, aes(x = longitude, y = latitude, color = species_gps, shape = code_garden), size = 3) +
    species_palette +
    labs(x = "Longitude", y = "Latitude", color = "Species", shape = "Garden") +
    theme_minimal() +
    theme(legend.position = "right") + 
    theme(legend.title = element_text(size = 30), 
    legend.text = element_text(size = 25),
    legend.key.size = unit(2, "cm")) +
    coord_fixed(ratio = 1.2, xlim = c(min(world$long) - 20, max(world$long) + 20), ylim = c(min(world$lat) - 10, max(world$lat) + 10))
    
    ggsave(file, plot = carte, device = "pdf", width = 40, height = 30, units = "in",limitsize = FALSE)
  }
)

})




  
  ##############################################################

   cover_species <- cover_species_garden_full

  cover_species$garden <- gsub("fr", "Fribourg", gsub("ne", "Neuchâtel", gsub("la", "Lausanne", cover_species$garden)))
  cover_species$garden <- ifelse(cover_species$garden == "Fribourg_LausanNeuchâtel_Neuchâtel", 
                                   "Fribourg_Lausanne_Neuchâtel", 
                                   cover_species$garden)
  cover_species$garden <- ifelse(cover_species$garden == "LausanNeuchâtel_Neuchâtel", 
                                   "Lausanne_Neuchâtel", 
                                   cover_species$garden)
    cover_species$garden <- ifelse(cover_species$garden == "LausanNeuchâtel", 
                                   "Lausanne", 
                                   cover_species$garden)


  observe({
    updateSelectInput(session, "selected_family", choices = unique(cover_species$family))
  })
  
  observe({
    if (!is.null(input$selected_family) && input$selected_family != "") {
      updateSelectInput(session, "selected_genus", choices = c("", unique(cover_species$genus[cover_species$family == input$selected_family])))
    } else {
      updateSelectInput(session, "selected_genus", choices = c("", NULL))
    }
  })
  
  observe({
    if (!is.null(input$selected_family) && input$selected_family != "" && !is.null(input$selected_genus) && input$selected_genus != "") {
      updateSelectInput(session, "selected_species", choices = c("", unique(cover_species$species[cover_species_garden_full$family == input$selected_family & cover_species_garden_full$genus == input$selected_genus])))
    } else {
      updateSelectInput(session, "selected_species", choices = c("", NULL))
    }
  })
  
  select_species <- reactive({
    filtered_species <- cover_species
    if (!is.null(input$selected_family) && input$selected_family != "") {
      filtered_species <- filtered_species %>%
        filter(family == input$selected_family)
    }
    if (!is.null(input$selected_genus) && input$selected_genus != "") {
      filtered_species <- filtered_species %>%
        filter(genus == input$selected_genus)
    }
    if (!is.null(input$selected_species) && input$selected_species != "") {
      filtered_species <- filtered_species %>%
        filter(species == input$selected_species)
    }
    return(filtered_species)
  })
  
  output$selectedData <- renderTable({
    select_species()
  })


output$downloadTablespecies <- downloadHandler(
    filename = function() {
        paste("species_garden", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
        write.csv(select_species(), file, row.names = FALSE)
    }
  )

}

shinyApp(ui = ui, server = server)



