library(tidyverse)
library(sf)
library(shiny)
library(bslib)
library(shinyWidgets)
library(waiter)
library(munsell)
source("enum.R")
options(dplyr.summarise.inform = FALSE)

eurobaro <- readRDS("data/eurobaro.rds")
eurobaro_spatial <- readRDS("data/eurobaro_spatial.rds")
global_polys <- readRDS("data/global_polys.rds")

# Define UI for application that draws a histogram
ui <- page_sidebar(
  title = tagList(
    "Eurobarometer 101.1",
    actionButton("source", label = "Data source", icon = icon("diaspora"))
  ),
  sidebar = sidebar(
    virtualSelectInput(
      "x",
      label = "Wähle eine X-Variable aus",
      choices = setNames(names(titles), titles),
      selected = "eu_attitude"
    ),
    
    uiOutput("x_text"),
    
    virtualSelectInput(
      "y",
      label = "Wähle eine Y-Variable aus",
      choices = setNames(names(titles), titles),
      selected = "eu_covid"
    ),
    
    uiOutput("y_text"),
    
    virtualSelectInput(
      "group",
      label = "Wähle eine Gruppierung aus",
      choices = c("Keine" = "", setNames(names(titles_groups), titles_groups))
    ),
    
    conditionalPanel(
      "input.group != ''",
      uiOutput("group_text")
    )
  ),
  
  tags$head(
    tags$style(HTML("
    .card { position: relative; }
    .waiter-overlay {
      border-radius: calc(0.375rem - 1px);
    }
  "))
  ),
  
  useWaiter(),
  waiterShowOnLoad(spin_2()),
  autoWaiter(
    c("xy_dist", "xy_lm", "x_map", "y_map"),
    html = tagList(
      bs5_spinner(), br(),
      span("Wird visualisiert...", style = "color: black; font-family: sans-serif; margin-top: 10px;")
    ),
    color = "white"
  ),
  
  layout_columns(
    col_widths = c(
      4, 4, 4,
      6, 6,
      6, 6
    ),
    
    uiOutput("x_card"),
    uiOutput("y_card"),
    uiOutput("xy_card"),
    
    card(
      card_header("Statistische Verteilung"),
      card_body(plotOutput("xy_dist"), class = "p-0"),
      fill = TRUE
    ),
    card(
      card_header("Statistischer Zusammenhang"),
      card_body(plotOutput("xy_lm"), class = "p-0"),
      fill = TRUE
    ),
    card(
      card_header("Räumliche Verteilung"),
      card_body(plotOutput("x_map"), class = "p-0"),
      fill = TRUE
    ),
    card(
      card_header("Räumliche Verteilung"),
      card_body(plotOutput("y_map"), class = "p-0"),
      fill = TRUE
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  waiter_hide()
  #w1 <- Waiter$new(id = c("xy_dist", "xy_lm"), color = transparent(0.5))
  #w2 <- Waiter$new(id = c("x_map", "y_map"), color = transparent(0.5))
  
  observe({
    showModal(
      modalDialog(
        tags$table(
          style = "border-collapse: separate; border-spacing: 10px;",
          tags$tr(
            tags$th(tags$b("Data source:"), style = "vertical-align: top;"),
            tags$th("This application uses data from the Eurobarometer 101.1, 2024 / ZA8841", style = "vertical-align: top;"),
            style = "padding-bottom: 10px;"
          ),
          tags$tr(
            tags$th(tags$b("Source attribution:"), style = "vertical-align: top;"),
            tags$th("European Commission / GESIS Data Archive", style = "vertical-align: top;"),
            style = "padding-bottom: 10px;"
          ),
          tags$tr(
            tags$th(tags$b("Disclaimer:"), style = "vertical-align: top;"),
            tags$th("The author of this app has modified the original data for visualization purposes. The European Commission and GESIS bear no responsibility for the results or interpretations presented in this tool.", style = "vertical-align: top;")
          )
        )
      )
    )
  }) |>
    bindEvent(input$source)
  
  .data <- reactive({
    group <- input$group
    if (!nzchar(group)) group <- NULL

    #w1$show()
    
    eurobaro |>
      select(country, group = !!group, x = !!input$x, y = !!input$y)
  })
  
  .data_spatial <- reactive({
    group <- input$group
    if (!nzchar(group)) group <- NULL

    #w2$show()
    
    eurobaro_spatial <- eurobaro_spatial |>
      select(country, group = !!group, x = !!input$x, y = !!input$y)

    if (!is.null(group)) {
      eurobaro_spatial <- eurobaro_spatial |>
        group_by(country, group)
    } else {
      eurobaro_spatial <- eurobaro_spatial |>
        group_by(country)
    }

    eurobaro_spatial |>
      summarise(across(c(x, y), ~{
        new <- levels(.x)[median(as.numeric(.x), na.rm = TRUE)]
        factor(new, levels = levels(eurobaro[[input[[cur_column()]]]]))
      })) |>
      ungroup()
  })
  
  output$x_text <- renderUI({
    tags$blockquote(
      tags$p(
        tags$b("Frage:"), tags$br(),
        questions[[input$x]],
        style = "display:block;font-size:14px;"
      )
    )
  })
  
  output$y_text <- renderUI({
    tags$blockquote(
      tags$p(
        tags$b("Frage:"), tags$br(),
        questions[[input$y]],
        style = "display:block;font-size:14px;"
      )
    )
  })
  
  output$group_text <- renderUI({
    tags$blockquote(
      tags$p(
        tags$b("Frage:"), tags$br(),
        questions[[input$group]],
        style = "display:block;font-size:14px;"
      )
    )
  })
  
  output$x_card <- renderUI({
    vals <- eurobaro[[input$x]]
    value_box(
      title = HTML(paste("Median:<br>", titles[[input$x]])),
      value = levels(vals)[median(as.numeric(vals), na.rm = TRUE)],
      showcase = icon("x"),
      min_height = 135
    )
  })
  
  output$y_card <- renderUI({
    vals <- eurobaro[[input$y]]
    value_box(
      title = HTML(paste("Median:<br>", titles[[input$y]])),
      value = levels(vals)[median(as.numeric(vals), na.rm = TRUE)],
      showcase = icon("y"),
      height = 135
    )
  })
  
  output$xy_card <- renderUI({
    x <- as.numeric(eurobaro[[input$x]])
    y <- as.numeric(eurobaro[[input$y]])
    value_box(
      title = "Correlation between X and Y",
      value = round(cor(x, y, method = "spearman", use = "complete.obs"), 2),
      showcase = icon("link"),
      height = 135
    )
  })
  
  output$xy_dist <- renderPlot({
    .data <- pivot_longer(.data(), c(x, y)) |>
      mutate(name = case_match(name, "x" ~ input$x, "y" ~ input$y))
    has_group <- nzchar(input$group)

    ggplot(na.omit(.data)) +
      geom_bar(
        aes(
          x = value,
          y = if (has_group) {
            after_stat(count / ave(count, PANEL, fill, FUN = sum))
          } else {
            after_stat(count / ave(count, PANEL, FUN = sum))
          },
          fill = if (has_group) group
        ),
        stat = "count",
        position = position_dodge(width = 0.7),
        width = 0.5
      ) +
      facet_wrap(
        ~name,
        scales = "free_x",
        labeller = as_labeller(unlist(titles))
      ) +
      scale_y_continuous(labels = \(x) x * 100, expand = c(0, 0)) +
      scale_x_discrete(labels = label_wrap_gen(12)) +
      labs(
        fill = if (has_group) titles_groups[[input$group]],
        x = NULL,
        y = "Anteil (in %)"
      ) +
      theme_bw() +
      theme(
        panel.grid = element_blank(),
        strip.background = element_rect(color = NA, fill = NA),
        strip.text = element_text(hjust = 0, face = "bold")
      )
  }, res = 90)
  
  
  output$xy_lm <- renderPlot({
    .data <- .data() |>
      group_by(x, y) |>
      summarise(count = n()) |>
      mutate(cut_count = sum(count), prop = count / sum(count)) |>
      ungroup()
    has_group <- nzchar(input$group)

    ggplot(na.omit(.data)) +
      geom_bar(
        aes(x, y = prop, width = cut_count, fill = y),
        stat = "identity",
        position = "fill",,
        color = "black",
        show.legend = TRUE
      ) +
      facet_grid(~x, scales = "free_x", space = "free_x") +
      scale_fill_brewer(palette = "RdYlBu", drop = FALSE, direction = -1) +
      scale_x_discrete(labels = label_wrap_gen(12), position = "top") +
      scale_y_continuous(labels = \(x) x * 100) +
      labs(
        x = titles[[input$x]],
        y = "Anteil (in %)",
        fill = label_wrap_gen(15)(titles[[input$y]])[[1]]
      ) +
      theme_minimal() +
      theme(
        panel.grid = element_blank(),
        strip.text = element_blank()
      )
  }, res = 90)
  
  
  output$x_map <- renderPlot({
    .data <- .data_spatial()
    has_group <- nzchar(input$group)
    lims <- list(x = c(2500000, 6000000), y = c(1500000, 5200000))

    p <- ggplot() +
      geom_sf(data = global_polys, fill = "grey80", color = NA) +
      geom_sf(data = na.omit(.data), aes(fill = x), show.legend = TRUE) +
      coord_sf(xlim = lims$x, ylim = lims$y) +
      scale_fill_brewer(palette = "RdYlBu", drop = FALSE, direction = -1) +
      labs(
        x = NULL,
        y = NULL,
        fill =paste0(label_wrap_gen(20)(titles[[input$x]])[[1]], "\n", "Median")
      ) +
      theme_bw() +
      theme(
        panel.grid = element_blank(),
        strip.background = element_rect(color = NA, fill = NA),
        strip.text = element_text(hjust = 0, face = "bold"),
        axis.text = element_blank(),
        axis.ticks = element_blank()
      )
    
    if (has_group) {
      p <- p + facet_wrap(~group, ncol = 3)
    }
    
    p
  }, res = 90)
  
  output$y_map <- renderPlot({
    .data <- .data_spatial()
    has_group <- nzchar(input$group)
    lims <- list(x = c(2500000, 6000000), y = c(1500000, 5200000))

    p <- ggplot() +
      geom_sf(data = global_polys, fill = "grey80", color = NA) +
      geom_sf(data = drop_na(.data), aes(fill = y), show.legend = TRUE) +
      coord_sf(xlim = lims$x, ylim = lims$y) +
      scale_fill_brewer(palette = "RdYlBu", drop = FALSE, direction = -1) +
      labs(
        x = NULL,
        y = NULL,
        fill = paste0(label_wrap_gen(20)(titles[[input$y]])[[1]], "\n", "Median")
      ) +
      theme_bw() +
      theme(
        panel.grid = element_blank(),
        strip.background = element_rect(color = NA, fill = NA),
        strip.text = element_text(hjust = 0, face = "bold"),
        axis.text = element_blank(),
        axis.ticks = element_blank()
      )
    
    if (has_group) {
      p <- p + facet_wrap(~group, ncol = 3)
    }
    
    p
  }, res = 90)
}

# Run the application 
shinyApp(ui = ui, server = server)
