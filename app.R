# Load required libraries
library(shiny) # Create interactive dashboard
library(bslib) # Bootstrap library
library(jsonlite) # Work with JSON
library(dplyr) # Word with data
library(shinyvalidate) # Validate inputs
library(leaflet) # Plot maps
library(bsicons) # Bootstrap icons
library(DT) # Show data table
library(plotly) # Build graphs
library(lubridate) # Working with dates
library(thematic) # Apply different themes to ggplot2 graphs
library(scales) # Customize scale value on axes
library(stringr) # Work with strings
library(forecast) # Time series forecasting
library(httr) # Make API call


# ------------------------------------------------------------------------------
# Load the global data
STATES <- read.csv("states.csv")
CITIES <- read.csv("cities.csv")

PROPERTY_TYPES <- c(
  "", "Single Family", "Condo", "Townhouse", "Manufactured",
  "Multi-Family", "Apartment", "Land"
)
FOR_OPTIONS <- c("Sale", "Rent")
LIVING_AREA_OPTIONS <- c(
  "", "0 - 500", "501 - 800", "801 - 1,200",
  "1,201 - 1,800", "1,801+"
)
LOT_AREA_OPTIONS <- c(
  "", "0 - 5K", "5K - 10K", "10K - 20K",
  "20K - 50K", "50K - 100K",
  "100K - 1M", "1M - 5M",
  "5M - 10M", "10M+"
)
# Map input selections to numeric ranges
LIVING_AREA_MAP <- list(
  "0 - 500" = c(0, 500),
  "501 - 800" = c(501, 800),
  "801 - 1,200" = c(801, 1200),
  "1,201 - 1,800" = c(1201, 1800),
  "1,801+" = c(1801, Inf)
)

LOT_AREA_MAP <- list(
  "0 - 5K" = c(0, 5000),
  "5K - 10K" = c(5001, 10000),
  "10K - 20K" = c(10001, 20000),
  "20K - 50K" = c(20001, 50000),
  "50K - 100K" = c(50001, 100000),
  "100K - 1M" = c(100001, 1000000),
  "1M - 5M" = c(1000001, 5000000),
  "5M - 10M" = c(5000001, 10000000),
  "10M+" = c(10000001, Inf)
)

MAX_BEDROOMS <- 20
MAX_BATHROOMS <- 20

create_label <- function(label) {
  return(paste0("<b>", label, ":</b>"))
}

# Apply thematic to shiny app
thematic_shiny(font = "auto")

# For a single-page dashboards with an optional sidebar.
# https://rstudio.github.io/bslib/reference/index.html#dashboard-layouts
ui <- shinyUI(page_sidebar(
  # Use bslib for theming
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  tags$head(
    # Load CSS file
    tags$link(rel = "stylesheet", type = "text/css", href = "app.css")
  ),

  # Application title
  title = tags$div(
    tags$img(src = "primary-logo.png", height = 35),
    tags$h1("Rental and Sale Listings over the US", class = "fs-3 text-black mb-0"),
    class = "d-flex align-items-center column-gap-3 py-2"
  ),

  # Sidebar
  sidebar = sidebar(
    tags$p(paste(
      "Welcome! This dashboard helps you explore sale or rental",
      "listings across the US (default view is in Atlantic City,",
      "New Jersey)."
    )),
    selectizeInput(
      inputId = "state",
      label = HTML(create_label("State")),
      choices = STATES$state_name,
      selected = "",
      options = list(placeholder = "Please select")
    ),
    selectizeInput(
      inputId = "city",
      label = HTML(create_label("City")),
      choices = NULL,
      selected = "",
      options = list(placeholder = "Please select")
    ),
    tags$div(
      selectizeInput(
        inputId = "propertyType",
        label = HTML(create_label("Property type")),
        choices = PROPERTY_TYPES,
        selected = "",
        options = list(placeholder = "Please select")
      ),
      selectInput(
        inputId = "forType",
        label = HTML(create_label("For")),
        choices = FOR_OPTIONS
      ),
      class = "d-flex column-gap-3"
    ),
    tags$div(
      numericInput(
        inputId = "bedrooms",
        label = HTML(create_label("Bedrooms")),
        value = NULL,
        min = 1,
        max = MAX_BEDROOMS
      ),
      numericInput(
        inputId = "bathrooms",
        label = HTML(create_label("Bathrooms")),
        value = NULL,
        min = 1,
        max = MAX_BATHROOMS
      ),
      class = "d-flex column-gap-3"
    ),
    # Search & reset buttons
    tags$div(
      actionButton("search",
        "Search",
        class = "btn btn-primary flex-grow-1"
      ),
      actionButton("reset",
        bs_icon("arrow-repeat"),
        class = "btn btn-secondary"
      ),
      class = "d-flex column-gap-3"
    ),
    hr(),
    tags$p(paste("Use these conditions below to filter the data")),
    sliderInput(
      inputId = "priceSlider",
      label = HTML(create_label("Price range ($)")),
      min = 0,
      max = 100000000,
      value = c(0, 100000000),
      ticks = FALSE
    ),
    tags$div(
      selectizeInput(
        inputId = "livingArea",
        label = HTML(create_label("Living area (sq ft)")),
        choices = LIVING_AREA_OPTIONS,
        selected = "",
        options = list(placeholder = "Please select")
      ),
      selectizeInput(
        inputId = "lotArea",
        label = HTML(create_label("Lot area (sq ft)")),
        choices = LOT_AREA_OPTIONS,
        selected = "",
        options = list(placeholder = "Please select")
      ),
      class = "d-flex column-gap-3"
    ),
    width = 360,
    class = "scroll-bar-thin"
  ),

  # Main content
  layout_columns(
    col_widths = c(6, 6),
    layout_columns(
      col_widths = c(12, 12),
      row_heights = c(3, 2),
      layout_columns(
        col_widths = c(12, 12),
        row_heights = list("auto", 1),
        layout_columns(
          col_widths = c(4, 4, 4),
          uiOutput("lowPrice"),
          uiOutput("averagePrice"),
          uiOutput("highPrice")
        ),
        card(
          card_header("Changes in listing prices through the years",
            class = "bg-primary"
          ),
          card_body(plotOutput("listedDateAndPriceGraph"))
        )
      ),
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header("Density of living area (sq ft)",
            class = "bg-primary"
          ),
          card_body(plotOutput("livingAreaDensityGraph"))
        ),
        card(
          card_header("Median price by listing type",
            class = "bg-primary"
          ),
          card_body(plotOutput("medianPriceByListingType"))
        )
      )
    ),
    layout_columns(
      col_widths = c(12, 12),
      row_heights = c(3, 2),
      card(
        card_header("Listing locations by state and city",
          class = "bg-primary"
        ),
        card_body(leafletOutput("map"), class = "p-0")
      ),
      layout_columns(
        col_widths = c(4, 8),
        card(
          card_header("Additional info", class = "bg-primary"),
          card_body(
            tags$div(
              tags$label(HTML(create_label("Agent name"))),
              tags$p(textOutput("agentName", inline = TRUE)),
              tags$label(HTML(create_label("Agent phone number"))),
              tags$p(textOutput("agentPhoneNumber", inline = TRUE)),
              tags$label(HTML(create_label("Listing company"))),
              tags$p(textOutput("listingCompany", inline = TRUE))
            )
          )
        ),
        card(
          card_header("Listing history table", class = "bg-primary"),
          card_body(DTOutput("listingHistory"))
        )
      )
    )
  )
))

# ------------------------------------------------------------------------------

# Function to process JSON and return two data frames
create_dataframes_from_json <- function(json) {
  # Convert json into R dataframe
  raw_df <- fromJSON(json)
  
  # Remove nested history dataframe from property dataframe
  property_df <- as.data.frame(raw_df) %>%
    distinct(latitude, longitude, .keep_all = TRUE)
  
  property_df$history <- NULL
  
  property_df$formatedListedDate <- as_date(property_df$listedDate)
  
  property_df$listingType <- str_replace(
    property_df$listingType, "New Construction", "New build"
  )
  
  property_df$listingType <- str_replace(
    property_df$listingType, "Short Sale", "Short sale"
  )
  
  # Extract property history records into a new dataframe
  # lapply: Apply a function over a list or vector
  nested_history_list <- lapply(names(raw_df$history), function(date) {
    # Get sub dataframe by a specific date and convert them into dataframe
    # Double square brackets: get column data
    nested_history_df <- raw_df$history[[date]]
    nested_history_df$date <- date
    nested_history_df$id <- raw_df$id
    
    return(nested_history_df)
  })
  
  # Combine all nested dataframes into a single data frame
  history_df <- bind_rows(nested_history_list)
  
  if ("event" %in% names(history_df)) {
    history_df <- history_df %>%
      filter(!is.na(event)) %>%
      rename(
        "Date" = date,
        "Listing type" = listingType,
        "Price" = price,
        "Event" = event
      )
  } else {
    history_df <- NULL
  }
  
  return(list(property_df = property_df, history_df = history_df))
}

# Check numeric input range
is_not_smaller_than <- function(value, limit) {
  if (value < limit) {
    paste("Value must greater than or equal", limit)
  }
}

is_not_greater_than <- function(value, limit) {
  if (value > limit) {
    paste("Value must smaller than or equal", limit)
  }
}

# Add rule to numeric inputs
add_numeric_rule <- function(input_validator, id, min_limit = 1, max_limit) {
  input_validator$add_rule(id, sv_optional())
  input_validator$add_rule(id, is_not_smaller_than, limit = min_limit)
  input_validator$add_rule(id, is_not_greater_than, limit = max_limit)
}

create_value_box <- function(title, value, icon_name, theme_color) {
  value_box(
    title = HTML(create_label(title)),
    value = value,
    showcase = bs_icon(icon_name, size = "3rem"),
    showcase_layout = showcase_top_right(
      width = "min-content"
    ),
    max_height = "9rem",
    theme = theme_color
  )
}

getColor <- function(prices, minPrice = 0, maxPrice = 0) {
  lapply(prices, function(price) {
    if (price == minPrice) {
      "green"
    } else if (price == maxPrice) {
      "red"
    } else {
      "darkblue"
    }
  })
}

# Function to update the slider input based on dataset
update_slider_range <- function(session, id, min, max) {
  updateSliderInput(
    session,
    inputId = id,
    min = min,
    max = max,
    value = c(min, max)
  )
}

write_json_output <- function(json) {
  # Create the filename with the timestamp
  file_name <- paste0("data_", format(Sys.time(), "%H-%M-%S"), ".json")

  # Write JSON data to the file in the same folder
  write_json(fromJSON(json), path = file_name, pretty = TRUE)
}

# Define server logic
server <- shinyServer(function(input, output, session) {
  # Add theme customize dropdown
  bs_themer()

  primary_color <- reactive({
    unname(bs_get_variables(session$getCurrentTheme(), "primary"))
  })

  secondary_color <- reactive({
    unname(bs_get_variables(session$getCurrentTheme(), "secondary"))
  })

  info_color <- reactive({
    unname(bs_get_variables(session$getCurrentTheme(), "info"))
  })

  warning_color <- reactive({
    unname(bs_get_variables(session$getCurrentTheme(), "warning"))
  })

  light_color <- reactive({
    unname(bs_get_variables(session$getCurrentTheme(), "light"))
  })

  success_color <- reactive({
    unname(bs_get_variables(session$getCurrentTheme(), "success"))
  })

  dark_bg_subtle_color <- reactive({
    unname(bs_get_variables(session$getCurrentTheme(), "dark-bg-subtle"))
  })

  # Validate inputs
  input_validator <- InputValidator$new()
  input_validator$add_rule("state", sv_required())
  add_numeric_rule(input_validator, "bedrooms", max_limit = MAX_BEDROOMS)
  add_numeric_rule(input_validator, "bathrooms", max_limit = MAX_BATHROOMS)

  # Create a data frame with data from local JSON
  dataframes <- reactiveVal(create_dataframes_from_json("nj-data.json"))

  # Observe state when its value change, load corresponding CITIES
  observeEvent(input$state, {
    # Filter cities based on selected state
    filtered_cities <- CITIES$city[CITIES$state_name == input$state]

    updateSelectInput(
      session,
      inputId = "city",
      choices = filtered_cities,
      selected = ""
    )
  })

  # Event for calling API on button click
  observeEvent(input$search, {
    input_validator$enable()

    # Make sure all inputs are valid before clicking the search button
    req(input_validator$is_valid())

    # Determine API URL based on 'forType' input
    base_url <- if (input$forType == FOR_OPTIONS[1]) {
      "https://api.rentcast.io/v1/listings/sale"
    } else {
      "https://api.rentcast.io/v1/listings/rental/long-term"
    }

    search_query <- list()
    search_query$state <- STATES$state_code[STATES$state_name == input$state]
    search_query$status <- "Active"
    search_query$limit <- "500"

    if (nzchar(input$city)) {
      search_query$city <- input$city
    }

    if (nzchar(input$propertyType)) {
      search_query$propertyType <- input$propertyType
    }

    if (!is.na(input$bedrooms)) {
      search_query$bedrooms <- as.character(input$bedrooms)
    }

    if (!is.na(input$bathrooms)) {
      search_query$bathrooms <- as.character(input$bathrooms)
    }

    # Trigger API call
    response <- GET(
      base_url,
      query = search_query,
      add_headers("X-Api-Key" = "?"),
      content_type("application/octet-stream"),
      accept("application/json")
    )

    # Handle and store the API response
    if (status_code(response) == 200) {
      # Output as text
      api_json_data <- content(response, as = "text")
      # write_json_output(api_json_data)
      dataframes(create_dataframes_from_json(api_json_data))

      # Clear outputs when no data
      output$agentName <- renderText(NULL)
      output$agentPhoneNumber <- renderText(NULL)
      output$listingCompany <- renderText(NULL)
      output$listingHistory <- renderDT(NULL)
    } else {
      showModal(modalDialog(
        title = "API call failed",
        paste("The API call failed with status code:", status_code(response)),
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
    }

    input_validator$disable()
  })

  # Event for reseting input fields
  observeEvent(input$reset, {
    updateSelectInput(session, inputId = "city", choices = NULL, selected = "")
    updateSelectInput(session, inputId = "propertyType", selected = "")
    updateSelectInput(session, inputId = "forType", selected = FOR_OPTIONS[1])
    updateNumericInput(session, inputId = "bedrooms", value = "")
    updateNumericInput(session, inputId = "bathrooms", value = "")
  })

  # Update price range based on data
  observe({
    update_slider_range(
      session,
      "priceSlider",
      min(dataframes()$property_df$price, na.rm = TRUE),
      max(dataframes()$property_df$price, na.rm = TRUE)
    )
  })

  # Data filtered by price range
  filtered_property_df <- reactive({
    result <- dataframes()$property_df %>%
      filter(
        price >= input$priceSlider[1],
        price <= input$priceSlider[2]
      )

    if (isTruthy(input$livingArea)) {
      result <- result %>%
        filter(
          squareFootage >= LIVING_AREA_MAP[[input$livingArea]][1],
          squareFootage <= LIVING_AREA_MAP[[input$livingArea]][2]
        )
    }

    if (isTruthy(input$lotArea)) {
      result <- result %>%
        filter(
          lotSize >= LOT_AREA_MAP[[input$lotArea]][1],
          lotSize <= LOT_AREA_MAP[[input$lotArea]][2]
        )
    }

    if (nrow(result) == 0) {
      showModal(modalDialog(
        title = "No data available",
        "The data returned from API call or from selected filter has no results.",
        easyClose = TRUE,
        footer = tagList(
          modalButton("Close")
        )
      ))

      # Clear outputs when no data
      output$agentName <- renderText(NULL)
      output$agentPhoneNumber <- renderText(NULL)
      output$listingCompany <- renderText(NULL)
      output$listingHistory <- renderDT(NULL)
    }

    return(result)
  })

  # Extract insight from filtered  data
  filtered_min_price <- reactive({
    min(filtered_property_df()$price, na.rm = TRUE)
  })
  filtered_median_price <- reactive({
    median(filtered_property_df()$price, na.rm = TRUE)
  })
  filtered_max_price <- reactive({
    max(filtered_property_df()$price, na.rm = TRUE)
  })

  # Value box 1
  output$lowPrice <- renderUI({
    if (is.infinite(filtered_max_price())) {
      create_value_box(
        "Best deal",
        "NA",
        "houses-fill",
        "green"
      )
    } else {
      create_value_box(
        "Best deal",
        paste0("$", prettyNum(filtered_min_price(), big.mark = ",")),
        "houses-fill",
        "green"
      )
    }
  })

  # Value box 2
  output$averagePrice <- renderUI({
    if (is.na(filtered_median_price())) {
      create_value_box(
        "Overall median price",
        "NA",
        "cash-coin",
        "yellow"
      )
    } else {
      create_value_box(
        "Overall median price",
        paste0("$", prettyNum(filtered_median_price(), big.mark = ",")),
        "cash-coin",
        "yellow"
      )
    }
  })

  # Value box 3
  output$highPrice <- renderUI({
    if (is.infinite(filtered_max_price())) {
      create_value_box(
        "Most expensive",
        "NA",
        "buildings-fill",
        "red"
      )
    } else {
      create_value_box(
        "Most expensive",
        paste0("$", prettyNum(filtered_max_price(), big.mark = ",")),
        "buildings-fill",
        "red"
      )
    }
  })

  # Listed date and price graph (price trend)
  output$listedDateAndPriceGraph <- renderPlot({
    ggplot(
      filtered_property_df(),
      aes(
        x = formatedListedDate,
        y = price
      )
    ) +
      geom_line(linewidth = 0.8, color = primary_color()) +
      geom_smooth(fill = light_color(), color = warning_color()) +
      scale_y_continuous(labels = label_dollar(scale_cut = cut_short_scale())) +
      labs(
        x = "Listed date",
        y = "Price"
      ) +
      theme(
        panel.background = element_blank(),
        text = element_text(
          size = 14,
          # unname: get only the value
          # strsplit: get the first font family value
          family = strsplit(
            unname(bs_get_variables(
              session$getCurrentTheme(),
              "font-family-sans-serif"
            )),
            ","
          )[[1]][1]
        ),
        # Hide the legend
        legend.position = "none",
        axis.line = element_line(linewidth = 0.8),
        axis.title = element_text(size = 16),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10))
      )
  })

  # Living area density
  output$livingAreaDensityGraph <- renderPlot({
    ggplot(
      filtered_property_df(),
      aes(x = squareFootage)
    ) +
      geom_density(fill = primary_color()) +
      scale_y_continuous(labels = scales::label_number()) +
      labs(
        x = "Living area",
        y = "Density"
      ) +
      theme(
        panel.background = element_blank(),
        text = element_text(
          size = 14,
          # unname: get only the value
          # strsplit: get the first font family value
          family = strsplit(
            unname(bs_get_variables(
              session$getCurrentTheme(),
              "font-family-sans-serif"
            )),
            ","
          )[[1]][1]
        ),
        # Hide the legend
        legend.position = "none",
        axis.line = element_line(linewidth = 0.8),
        axis.title = element_text(size = 16),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10))
      )
  })

  # Price analysis by listing type
  output$medianPriceByListingType <- renderPlot({
    average_price_by_listing_type <- filtered_property_df() %>%
      group_by(listingType) %>%
      summarise(median_price = median(price))

    ggplot(
      average_price_by_listing_type,
      aes(x = listingType, y = median_price)
    ) +
      geom_bar(
        stat = "identity",
        aes(fill = ifelse(median_price == min(median_price),
          primary_color(),
          dark_bg_subtle_color()
        ))
      ) +
      scale_y_continuous(labels = label_dollar(scale_cut = cut_short_scale())) +
      scale_fill_identity() +
      labs(
        x = "Listing type",
        y = "Median price"
      ) +
      theme(
        panel.background = element_blank(),
        text = element_text(
          size = 14,
          # unname: get only the value
          # strsplit: get the first font family value
          family = strsplit(
            unname(bs_get_variables(
              session$getCurrentTheme(),
              "font-family-sans-serif"
            )),
            ","
          )[[1]][1]
        ),
        # Hide the legend
        legend.position = "none",
        axis.line = element_line(linewidth = 0.8),
        axis.title = element_text(size = 16),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10))
      )
  })

  # Map
  output$map <- renderLeaflet({
    # Use a custom icon from bootstrap's Glyphicons
    custom_icon <- awesomeIcons(
      # https://getbootstrap.com/docs/3.3/components/
      icon = "home",
      iconColor = "white",
      markerColor = getColor(
        filtered_property_df()$price,
        filtered_min_price(),
        filtered_max_price()
      )
    )

    labels <- paste0(
      create_label("Address"), " ",
      filtered_property_df()$formattedAddress, "<br>",
      create_label("Price ($)"), " ",
      # Add ',' to price number
      prettyNum(filtered_property_df()$price, big.mark = ","), "<br>",
      create_label("Bedrooms"), " ",
      filtered_property_df()$bedrooms, "<br>",
      create_label("Bathrooms"), " ",
      filtered_property_df()$bathrooms, "<br>",
      create_label("Living area (sq ft)"), " ",
      filtered_property_df()$squareFootage, "<br>",
      create_label("Lot area (sq ft)"), " ",
      filtered_property_df()$lotSize, "<br>",
      create_label("HOA fee"), " ",
      filtered_property_df()$hoa$fee, "<br>",
      create_label("Days on market"), " ",
      filtered_property_df()$daysOnMarket, "<br>",
      create_label("Listing type"), " ",
      filtered_property_df()$listingType
    )

    leaflet(filtered_property_df()) %>%
      # Add Satellite tiles and assign it to group "Map"
      addTiles(group = "Map") %>%
      addAwesomeMarkers(
        lng = ~longitude,
        lat = ~latitude,
        icon = custom_icon,
        # Use 'id' as the layer ID for each house
        layerId = ~id,
        # Output labels as HTML
        label = lapply(labels, HTML),
        labelOptions = labelOptions(textsize = "15px")
      ) %>%
      # Add Satellite tiles and assign it to group "Satellite"
      addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
      # Choose one of several base layers
      addLayersControl(
        baseGroups = c("Map", "Satellite"),
        # collapsed = TRUE: the layers control will be rendered as an icon that
        # expands when hovered over
        options = layersControlOptions(collapsed = TRUE)
      )
  })

  # Event for clicking on a house on the map
  # map: id, _marker_click: click event
  observeEvent(input$map_marker_click, {
    # Get the property ID of the clicked marker
    property_id <- input$map_marker_click$id

    # Get additional info from filtered data
    property_by_id <- filtered_property_df() %>%
      filter(id == property_id)

    output$agentName <- renderText({
      property_by_id$listingAgent$name
    })
    output$agentPhoneNumber <- renderText({
      property_by_id$listingAgent$phone
    })
    output$listingCompany <- renderText({
      property_by_id$listingOffice$name
    })
    
    if (!is.null(dataframes()$history_df)) {
      # Filter the history data for the selected house
      filtered_history <- dataframes()$history_df %>% filter(id == property_id)
      filtered_history$Price <- paste0("$", 
                                       prettyNum(filtered_history$Price, 
                                                 big.mark = ","))
      
      # Display columns in a new order
      new_order <- c("Date", "Event", "Listing type", "Price")
      selected_history <- filtered_history[, new_order] %>%
        arrange(desc(Date))
      
      # Render the history table
      output$listingHistory <- renderDT({
        datatable(selected_history, 
                  options = list(pageLength = 2), 
                  rownames = FALSE)
      })
    }
  })
})

# Run the app
shinyApp(ui, server)
