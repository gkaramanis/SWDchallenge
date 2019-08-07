library(shiny)
library(tidyverse)

media_franchises <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-02/media_franchises.csv")
media_franchises <- media_franchises %>%
    select(franchise, revenue_category, revenue) %>% 
    complete(revenue_category, franchise) %>% 
    replace_na(list(revenue = 0)) %>% 
    group_by(franchise) %>% 
    mutate(revenue_perc = round(revenue/sum(revenue)*100, 1)) %>% 
    select(franchise, revenue_category, revenue, revenue_perc) %>% 
    arrange(franchise, -revenue_perc) %>%
    mutate(order = row_number())

ui <- fluidPage(
    includeCSS("styles.css"),
    
    verticalLayout(
        mainPanel(
            h3("Media Franchises Powerhouses"),
            h5("Different revenue streams as percentage of the total revenue.\nThe outer ring shows the largest revenue stream (full circle is 100%)"),
            selectInput("franchiseInput", "Select franchise:",
                        choices = media_franchises$franchise,
                        selected = T,
                        width = "100%"),
            plotOutput("percPlot", height = "300px"),
            tableOutput("percTable")
        )
    )
)
server <- function(input, output) {
    output$percPlot <- renderPlot({
        media_franchises %>% filter(franchise == input$franchiseInput) %>% 
        ggplot() +
            geom_col(aes(x = order, y = revenue_perc,
                         fill = revenue_category), width = 3) +
            scale_x_reverse() +
            ylim(0, 100) +
            scale_fill_brewer(palette = "Dark2") +
            coord_polar(theta = "y") +
            # coord_flip() +
            theme_void() +
            theme(
                legend.position = "none",
                plot.background = element_rect(fill = "#fff5ba", color = "#fff5ba")
            )
    }, bg="transparent")
    
    output$percTable <- renderTable(
        media_franchises %>% 
            filter(franchise == input$franchiseInput &
                       revenue_perc > 0) %>% 
            ungroup() %>% 
            select("Revenue Category" = revenue_category,
                   "% of total revenue" = revenue_perc),
        
        colnames = T, digits = 1, width = "100%"
    )
}

shinyApp(ui = ui, server = server)
