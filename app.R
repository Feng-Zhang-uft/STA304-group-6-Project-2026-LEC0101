library(shiny)
library(ggplot2)

task2_mode_labels <- c("Public transit", "Driving", "Passenger/Carpooling",
                       "Walking", "Taxi/Ride-hailing")
survey <- readxl::read_excel("STA304_Group6_Data_Cleaned.xlsx", sheet = "clean_data")

commute_labels <- c(
  commute_time_min = "One-way travel time (minutes)",
  commute_distance_km = "One-way travel distance (km)",
  commute_cost_cad = "One-way travel cost (CAD)"
)

rating_label <- "Public transit preference score (1–7)"

group_names <- c("Travel Modes", "One-Way Commute Details", "Travel Preferences and Priorities")
views <- c("Preferred mode" = "preferred_mode", "Current mode" = "current_mode",
           "Preferred vs. Current" = "comparison")
stopifnot(all(c("preferred_mode", "current_mode", "vehicle_access") %in% names(survey)))
colour_keys <- list(task2_mode_labels, c("Preferred", "Current"), "Histogram colour", paste("Boxplot colour"))
schemes <- list(
  Pastel = c("#9EC9E2", "#F6B599", "#ADD9B4", "#C9B9DF", "#EAD486", "#F2ACC8", "#8ECFC6"),
  Bright = c("#0072B2", "#E69F00", "#009E73", "#CC79A7", "#56B4E9", "#D55E00", "#AE9A00"),
  "Blue tones" = c("#08306B", "#15539A", "#2171B5", "#358BC3", "#58A3D2", "#84BCDF", "#B1D5EA")
)
pages <- c("Background Information", "Explore", "Conclusion")
questions <- c(
  "Which transportation modes do students prefer when commuting to campus?",
  "Does the mean public-transit preference score differ between students with and without access to a privately owned vehicle?",
  "How are one-way commute cost, distance, and time associated with public-transit preference scores?"
)
methods <- c(
  "Report the percentage who prefer each mode. Use a one-sample proportion test.",
  "Use Welch\u2019s two-sample t-test to compare the groups\u2019 mean preference scores on the 1\u20137 scale.",
  "Use multiple linear regression to estimate how each factor relates to the preference score while holding the other two constant."
)

background <- tagList(
  h2("Target Population"),
  p("Students enrolled in STA304H5 at the University of Toronto Mississauga in Fall 2026."),
  h2("Research Questions and Statistical Methods"),
  tags$table(class = "research",
    tags$thead(tags$tr(tags$th(scope = "col", "Research question"),
                       tags$th(scope = "col", "Statistical method"))),
    tags$tbody(lapply(seq_along(questions), function(i) {
      tags$tr(tags$td(questions[i]), tags$td(methods[i]))
    }))
  ),
  h2("Data Collection Methodology"),
  p(paste(
    "Our survey asked STA304H5 students at the University of Toronto Mississauga",
    "about their commuting habits and transportation preferences.",
    "The results are based on 67 responses."
  ))
)

custom_picker <- function(group, index) {
  id <- paste("paint", group, index, sep = "_")
  div(class = "swatch",
    tags$input(id = id, type = "color", value = schemes$Pastel[index],
      oninput = "Shiny.setInputValue(this.id, this.value)"),
    tags$label(`for` = id, colour_keys[[group]][index]))
}

travel_results <- tagList(
  h2(textOutput("question", inline = TRUE)),
  div(class = "results travel-results",
    div(h4(textOutput("chart_type", inline = TRUE)), uiOutput("bars")),
    div(h4("Survey responses by travel mode"), tableOutput("numbers"), textOutput("valid_n"))
  ),
  h3("What do the results show?"),
  textOutput("interpretation")
)

explore <- sidebarLayout(
  sidebarPanel(width = 3,
    selectInput("group", "Variable group", group_names),
    conditionalPanel("input.group == 'Travel Modes'", selectInput("field1", "Variable", views)),
    conditionalPanel("input.group == 'One-Way Commute Details'",
      selectInput("trip_variable", "Variable", c("Commute time (minutes)" = "commute_time_min",
        "Commute distance (km)" = "commute_distance_km", "Commute cost (CAD)" = "commute_cost_cad"))),
    conditionalPanel("input.group == 'Travel Preferences and Priorities'",
   conditionalPanel("input.group == 'Travel Preferences and Priorities' || input.group == 'One-Way Commute Details'",
   sliderInput("rating_range", "Zoom in on a range of scores",
                         min = 1,
                         max = 7,
                         value = c(1, 7),
                         step = 1))),
    selectInput("scheme", "Colour palette", c(names(schemes), "Custom")),
    lapply(seq_along(colour_keys), function(i) {
      view <- switch(i,
        "input.group == 'Travel Modes' && input.field1 != 'comparison'",
        "input.group == 'Travel Modes' && input.field1 == 'comparison'",
        "input.group == 'One-Way Commute Details'",
        "input.group == 'Travel Preferences and Priorities'")
      conditionalPanel(paste("input.scheme == 'Custom' &&", view),
        lapply(seq_along(colour_keys[[i]]), function(j) {
            custom_picker(i, j)
          })
        )
    }),
   conditionalPanel(
     "input.group == 'Travel Modes'",
     radioButtons(
       "measure", "Show values as",
       c("Percentage (%)" = "percent", "Number of students" = "count")
     )
   ),
   conditionalPanel(
     "input.group == 'One-Way Commute Details'",
     div(
       class = "form-group",
       tags$strong("Survey rating scale"),
       div("1–7 points")
     )
   ),
   conditionalPanel(
     "input.group == 'Travel Preferences and Priorities'",
     div(
       class = "form-group",
       tags$strong("Survey rating scale"),
       div("Preference score (1–7)")
     )
   ),
    conditionalPanel("input.group == 'Travel Modes'",
      sliderInput("travel_axis_max", "Horizontal axis maximum",
                  min = 0, max = 100, value = 100, step = 1),
      helpText("Move left to zoom in. Bars beyond the selected maximum are clipped; labels show the full values.")
    ),
    selectInput("access", "Private vehicle access",
      c("All respondents" = "all", "No access" = "0", "Vehicle access" = "1"))
  ),
  mainPanel(
    width = 9,
    
    conditionalPanel(
      "input.group == 'Travel Modes'",
      travel_results
    ),
    
    conditionalPanel(
      "input.group == 'Travel Preferences and Priorities'",
      h2("Public Transit Preference by Vehicle Access"),
      plotOutput("access_plot"),
      h3("What this means"),
      textOutput("access_takeaway")
    ),
    
    conditionalPanel(
      "input.group == 'One-Way Commute Details'",
      h2("How commute factors relate to transit preference"),
      plotOutput("commute_plot"),
      h3("What this means"),
      textOutput("commute_takeaway")
    )
  )
)
heading <- "Exploring Factors Influencing UTM STA304H5 Students\u2019 Campus Transportation"
ui <- fluidPage(
  tags$head(tags$title(paste(heading, "Preferences")), tags$style(HTML("
    body {color:#171717;font-family:Arial,sans-serif}
    .mast {display:grid;grid-template-columns:1fr auto;align-items:center;
      gap:14px 20px;margin:0 -15px;padding:16px 4%;background:#fff1f5}
    .mast h1 {display:contents}
    .mast h1 span {font-size:clamp(22px,2.25vw,46px);line-height:1.15;font-weight:650}
    .mast .first {grid-column:1/-1}
    #page {width:auto;margin:0}
    #page .shiny-options-group {display:flex;flex-wrap:wrap;border-radius:24px;
      background:#f7dce5;padding:4px}
    #page label.radio-inline {margin:0;padding:0}
    #page input {position:absolute;opacity:0}
    #page label span {display:block;padding:6px 15px;color:#111;border-radius:22px;
      font-size:clamp(15px,1.15vw,19px)}
    #page input:checked + span {background:white;font-weight:600}
    #page input:focus-visible + span {outline:2px solid #444}
    .content {max-width:1450px;width:94%;margin:30px auto}
    .background {font-size:clamp(16px,1.55vw,22px);line-height:1.55}
    .background h2 {font-size:1.23em;font-weight:650;margin:32px 0 14px}
    .research {width:100%;table-layout:fixed;border-collapse:collapse}
    .research th,.research td {background:white;border:1px solid #ccc;text-align:left;
      vertical-align:top;padding:14px 18px}
    .results {display:grid;grid-template-columns:minmax(340px,1.3fr) minmax(245px,1fr);
      min-width:605px;gap:20px}
    .travel-results {grid-template-columns:minmax(0,1.1fr) minmax(0,1fr);min-width:0;
      padding-bottom:14px;border-bottom:1px solid #ddd}
    .travel-results h4 {font-weight:600;margin:15px 0}
    .travel-results table {width:100%;font-size:13px;overflow-wrap:anywhere}
    @media(max-width:900px){.travel-results {grid-template-columns:minmax(0,1fr)}}
    .mode-row {display:grid;grid-template-columns:minmax(85px,30%) 1fr;align-items:center}
    .mode-name {text-align:right;padding-right:8px;overflow-wrap:anywhere;font-size:13px}
    .tracks {padding:6px 0;border-left:1px solid #ccc}
    .lane {position:relative;height:27px}
    .bar {height:23px}
    .bar-value {position:absolute;top:2px;white-space:nowrap;color:#191919;font-size:12px}
    .axis-line {margin-left:30%;border-top:1px solid #aaa;display:flex;justify-content:space-between}
    .axis-name {margin-left:30%;text-align:center}
    .key {display:inline-block;margin-right:1em}
    .chip {display:inline-block;width:12px;height:12px;margin-right:5px}
    .swatch {display:flex;gap:9px;align-items:center;margin:5px 0}
    @media(max-width:850px){.mast {grid-template-columns:1fr}
      #page {justify-self:end}.content {width:100%}
      .research th,.research td {padding:9px}}
  "))),
  tags$header(class = "mast",
    h1(span(class = "first", heading), span("Preferences")),
    radioButtons("page", NULL, choices = pages, selected = pages[1], inline = TRUE)),
  tags$main(class = "content",
    conditionalPanel("input.page == 'Background Information'", div(class = "background", background)),
    conditionalPanel("input.page == 'Explore'", explore),
    conditionalPanel(
      "input.page == 'Conclusion'",
      div(
        class = "background conclusion",
        div(
          style = "margin-bottom: 22px;",
          h2("Main Takeaway"),
          p(paste(
            "Driving was the most popular choice among respondents, but most of",
            "them actually used public transit to get to campus."
          ))
        ),
        
        div(
          style = "margin-bottom: 22px;",
          h2("Commute factors show different patterns"),
          p(paste(
            "Students without access to a private vehicle generally preferred",
            "public transit more. We also looked at travel time, distance and",
            "cost together. Longer travel times went with a stronger preference",
            "for transit. For distance, we saw the opposite pattern: students",
            "travelling farther tended to prefer transit less. There was no",
            "clear link with travel cost."
          ))
        ),
        
        div(
          h2("Keep in Mind"),
          p(paste(
            "These findings describe the students who responded. They may not",
            "represent all STA304H5 students or all UTM students."
          ))
        )
      )
    )
  )
)

summarise_mode <- function(x) {
  valid <- x[!is.na(x)]
  stopifnot(all(valid %in% 1:5))
  counts <- tabulate(as.integer(valid), nbins = 5)
  pct <- rep(NA_real_, 5)
  if (length(valid) > 0) {
    pct <- 100 * counts / length(valid)
  }
  list(count = counts, pct = pct, n = length(valid))
}

format_pct <- function(x) {
  ifelse(is.na(x), "N/A", sprintf("%.1f%%", x))
}

server <- function(input, output, session) { added_plot_colour <- reactive({
  req(input$group, input$scheme)
  
  if (input$scheme != "Custom") {
    return(schemes[[input$scheme]][1])
  }
  
  key <- if (input$group == "One-Way Commute Details") 3 else 4
  chosen <- input[[paste("paint", key, 1, sep = "_")]]
  
  if (is.null(chosen)) schemes$Pastel[1] else chosen
})
  selection <- reactive({
    req(input$field1)
    switch(input$field1, preferred_mode = c(Preferred = "preferred_mode"),
           current_mode = c(Current = "current_mode"),
           comparison = c(Preferred = "preferred_mode", Current = "current_mode"))
  })
  summaries <- reactive({
    fields <- selection()
    rows <- survey
    if (req(input$access) != "all") {
      rows <- survey[survey$vehicle_access %in% as.integer(input$access), , drop = FALSE]
    }
    lapply(fields, function(field) {
      summarise_mode(rows[[field]])
    })
  })
  output$question <- renderText({
    switch(req(input$field1),
      preferred_mode = "How do students prefer to get to campus?",
      current_mode = "How do students currently get to campus?",
      comparison = "How do preferred and current travel modes compare?")
  })
  output$chart_type <- renderText({
    if (length(selection()) == 2) return("Grouped Horizontal Bar Chart")
    "Horizontal Bar Chart"
  })
  palette_values <- reactive({
    key <- 1L
    if (length(selection()) == 2) key <- 2L
    indices <- seq_along(colour_keys[[key]])
    if (req(input$scheme) != "Custom") return(schemes[[input$scheme]][indices])
    vapply(indices, function(i) {
      colour <- input[[paste("paint", key, i, sep = "_")]]
      if (is.null(colour)) return(schemes$Pastel[i])
      colour
    }, character(1))
  })
  output$numbers <- renderTable({
    parts <- summaries()
    table <- data.frame("Travel mode" = task2_mode_labels, check.names = FALSE)
    if (length(parts) == 1) {
      table$Respondents <- parts[[1]]$count
      table$Percentage <- format_pct(parts[[1]]$pct)
    } else {
      for (series in names(parts)) {
        x <- parts[[series]]
        table[[paste0(series, ", n (%)")]] <- paste0(x$count, " (", format_pct(x$pct), ")")
      }
    }
    table
  }, striped = TRUE, digits = 0, rownames = FALSE)
  output$valid_n <- renderText({
    parts <- summaries()
    if (length(parts) == 1L) {
      return(sprintf("Valid responses for this selection: %d.", parts[[1]]$n))
    }
    sprintf("Valid responses: preferred = %d; current = %d.",
            parts$Preferred$n, parts$Current$n)
  })
  output$interpretation <- renderText({
    parts <- summaries()
    describe <- function(series) {
      x <- parts[[series]]
      if (x$n == 0L) {
        return(sprintf("No valid responses are available for %s mode in this selection.",
                       tolower(series)))
      }
      wording <- switch(series, Preferred = "preferred", Current = "used")
      peak <- max(x$count)
      leaders <- task2_mode_labels[x$count == peak]
      lead <- paste(leaders, collapse = " and ")
      opening <- sprintf("Among %d valid responses in the current selection,", x$n)
      pct <- 100 * peak / x$n
      if (length(leaders) > 1L) {
        return(sprintf("%s %s were tied as the most commonly %s modes, each selected by %d respondents (%.2f%%).",
                       opening, lead, wording, peak, pct))
      }
      sprintf("%s %s was the most commonly %s mode, selected by %d respondents (%.2f%%).",
              opening, lead, wording, peak, pct)
    }
    findings <- vapply(names(parts), describe, character(1))
    paste(paste(findings, collapse = " "),
          "These results only describe the respondents shown.",
          "They do not explain why students choose a particular mode of transportation or represent all UTM students.")
  })
  output$bars <- renderUI({
    upper <- req(input$travel_axis_max)
    if (upper == 0) return(p("Move the slider above 0 to display the chart."))
    parts <- summaries()
    if (any(vapply(parts, function(x) x$n == 0, logical(1)))) {
      return(p("No valid responses for this selection."))
    }
    fills <- palette_values()
    percent <- input$measure == "percent"
    axis_name <- "Number of respondents"
    if (percent) axis_name <- "Respondents (%)"
    legend <- NULL
    if (length(parts) == 2) {
      legend <- lapply(seq_along(parts), function(j) {
        span(class = "key", span(class = "chip", style = paste0("background:", fills[j])),
             names(parts)[j])
      })
    }
    make_row <- function(i) {
      lanes <- lapply(seq_along(parts), function(j) {
        x <- parts[[j]]
        value <- x$count[i]
        label <- as.character(value)
        if (percent) {
          value <- x$pct[i]
          label <- format_pct(value)
        }
        fill <- fills[i]
        if (length(parts) == 2) fill <- fills[j]
        width <- min(100, 100 * value / upper)
        label_style <- sprintf("left:min(calc(%.4f%% + 3px),calc(100%% - 4em))", width)
        div(class = "lane",
            div(class = "bar", style = sprintf("background:%s;width:%.4f%%", fill, width)),
            span(class = "bar-value", style = label_style, label))
      })
      grid_style <- sprintf(paste0("background-image:linear-gradient(to right,#edf0f2 1px,transparent 1px);",
                                   "background-size:%.4f%% 100%%"), 1000 / upper)
      div(class = "mode-row", span(class = "mode-name", task2_mode_labels[i]),
          div(class = "tracks", style = grid_style, lanes))
    }
    ticks <- unique(round(seq(0, upper, length.out = min(5, upper + 1))))
    axis_ticks <- lapply(ticks, function(tick) {
      style <- sprintf("position:absolute;left:%.3f%%;transform:translateX(-50%%)", 100 * tick / upper)
      span(style = style, tick)
    })
    tagList(div(legend), lapply(seq_along(task2_mode_labels), make_row),
            div(class = "axis-line", style = "position:relative;height:22px;margin-right:1px", axis_ticks),
            div(class = "axis-name", axis_name))
  })
  # Shared vehicle-access filter for the two added plots
  filtered_data <- reactive({
    selected_access <- req(input$access)
    
    if (selected_access == "all") {
      return(survey)
    }
    
    survey[
      survey$vehicle_access %in% as.integer(selected_access),
      ,
      drop = FALSE
    ]
  })
  
  # RQ2: preference score by vehicle access
  access_data <- reactive({
    data <- filtered_data()[
      ,
      c("vehicle_access", "transit_preference_1to7"),
      drop = FALSE
    ]
    
    data <- data[complete.cases(data), , drop = FALSE]
    
    data$vehicle_access <- factor(
      data$vehicle_access,
      levels = c(0, 1),
      labels = c(
        "Without private vehicle access",
        "With private vehicle access"
      )
    )
    
    validate(need(
      nrow(data) > 0,
      "There are no preference scores in this selection."
    ))
    
    data
  })
  
  output$access_plot <- renderPlot({
    data <- access_data()
    
    ggplot(
      data,
      aes(x = vehicle_access, y = transit_preference_1to7)
    ) +
      geom_boxplot(fill = added_plot_colour(), width = 0.55) +
      stat_summary(
        fun = mean,
        geom = "point",
        shape = 23,
        size = 3.5,
        fill = "white",
        colour = "black"
      ) +
      scale_x_discrete(labels = c(
        "Without private vehicle access" = "Without access",
        "With private vehicle access" = "With access"
      )) +
      scale_y_continuous(breaks = 1:7) +
      coord_cartesian(ylim = input$rating_range) +
      labs(
        x = "Access to a privately owned vehicle",
        y = rating_label,
        caption = "Diamonds show averages; lines inside boxes show middle scores."
      ) +
      theme_minimal(base_size = 12)
  })
  
  output$access_takeaway <- renderText({
    data <- access_data()
    groups <- split(data$transit_preference_1to7, data$vehicle_access)
    
    without_access <- groups[["Without private vehicle access"]]
    with_access <- groups[["With private vehicle access"]]
    
    if (length(without_access) == 0 || length(with_access) == 0) {
      return(sprintf(
        paste(
          "The average preference score in this selection is %.2f out of 7.",
          "Choose 'All respondents' to compare the two access groups."
        ),
        mean(data$transit_preference_1to7)
      ))
    }
    
    difference <- mean(without_access) - mean(with_access)
    
    description <- sprintf(
      paste(
        "Students without private vehicle access averaged %.2f out of 7;",
        "students with access averaged %.2f. The averages were %.2f points apart."
      ),
      mean(without_access),
      mean(with_access),
      abs(difference)
    )
    
    if (length(without_access) < 2 || length(with_access) < 2 ||
        var(without_access) + var(with_access) == 0) {
      return(description)
    }
    
    test <- t.test(without_access, with_access, var.equal = FALSE)
    
    if (test$p.value < 0.05) {
      higher_group <- if (difference > 0) {
        "without access"
      } else {
        "with access"
      }
      
      paste(
        description,
        sprintf("The survey suggests that students %s gave higher scores on average.",
                higher_group)
      )
    } else {
      paste(
        description,
        "The survey results are too uncertain to say whether the group averages differ."
      )
    }
  })
  
  # RQ3: one commute factor at a time, while holding the other two at their averages
  commute_data <- reactive({
    columns <- c("transit_preference_1to7", names(commute_labels))
    data <- filtered_data()[, columns, drop = FALSE]
    data <- data[complete.cases(data), , drop = FALSE]
    
    validate(
      need(
        nrow(data) > 4,
        "There are too few complete responses for this comparison."
      ),
      need(
        length(unique(data$transit_preference_1to7)) > 1,
        "All students in this selection gave the same preference score."
      )
    )
    
    data
  })
  
  commute_model <- reactive({
    model <- lm(
      transit_preference_1to7 ~ commute_time_min +
        commute_distance_km + commute_cost_cad,
      data = commute_data()
    )
    
    validate(need(
      model$rank == 4,
      "The commute factors cannot be compared separately in this selection."
    ))
    
    model
  })
  
  output$commute_plot <- renderPlot({
    factor_name <- req(input$trip_variable)
    data <- commute_data()
    
    prediction_data <- data.frame(
      commute_time_min = rep(mean(data$commute_time_min), 100),
      commute_distance_km = rep(mean(data$commute_distance_km), 100),
      commute_cost_cad = rep(mean(data$commute_cost_cad), 100)
    )
    
    prediction_data[[factor_name]] <- seq(
      min(data[[factor_name]]),
      max(data[[factor_name]]),
      length.out = 100
    )
    
    fitted_scores <- predict(
      commute_model(),
      newdata = prediction_data,
      interval = "confidence"
    )
    
    prediction_data$score <- fitted_scores[, "fit"]
    prediction_data$lower <- fitted_scores[, "lwr"]
    prediction_data$upper <- fitted_scores[, "upr"]
    
    ggplot(
      prediction_data,
      aes(x = .data[[factor_name]], y = score)
    ) +
      geom_ribbon(
        aes(ymin = lower, ymax = upper),
        fill = added_plot_colour(),
        alpha = 0.2
      ) +
      geom_line(colour = added_plot_colour(), linewidth = 1) +
      labs(
        x = commute_labels[[factor_name]],
        y = "Estimated average preference score",
        caption = paste(
          "The other two commute factors are held at their averages.",
          "A wider band means greater uncertainty.",
          sep = "\n"
        )
      ) +
      theme_minimal(base_size = 12)
  })
  
  output$commute_takeaway <- renderText({
    factor_name <- req(input$trip_variable)
    model <- commute_model()
    
    step_size <- switch(
      factor_name,
      commute_time_min = 10,
      commute_distance_km = 5,
      commute_cost_cad = 1
    )
    
    comparison <- switch(
      factor_name,
      commute_time_min = "an extra 10 minutes",
      commute_distance_km = "an extra 5 km",
      commute_cost_cad = "an extra CAD $1 in travel cost"
    )
    
    held_fixed <- switch(
      factor_name,
      commute_time_min = "distance and cost",
      commute_distance_km = "travel time and cost",
      commute_cost_cad = "travel time and distance"
    )
    
    coefficients <- summary(model)$coefficients
    change <- coefficients[factor_name, "Estimate"] * step_size
    p_value <- coefficients[factor_name, "Pr(>|t|)"]
    
    if (!is.finite(p_value)) {
      return("There is too little information in this selection to judge the relationship.")
    }
    if (p_value >= 0.05) {
      return(sprintf(
        "For trips with the same %s, the survey did not show a clear link between %s and transit preference.",
        held_fixed,
        commute_labels[[factor_name]]))}
    direction <- if (change > 0) "higher" else "lower"
    sprintf(
      "For trips with the same %s, %s was linked to an estimated average preference score about %.2f points %s on the 1–7 scale.",
      held_fixed,
      comparison,
      abs(change),
      direction)})}

shinyApp(ui, server)
