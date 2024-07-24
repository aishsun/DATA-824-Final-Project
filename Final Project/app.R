library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(dplyr)
library(DT)
library(readr)

data <- read_csv("DUI.csv")

# Define UI
ui <- dashboardPage(
    dashboardHeader(title = "DUI and Fatalities Dashboard"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
            menuItem("Visualizations Overview", tabName = "visualizations", icon = icon("bar-chart")),
            menuItem("Bar Charts", tabName = "bar_charts", icon = icon("bar-chart")),
            menuItem("Scatter Plots", tabName = "scatter_plots", icon = icon("scatter-chart")),
            menuItem("Box Plots", tabName = "box_plots", icon = icon("box"))
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName = "overview",
                    fluidRow(
                        box(title = "Introduction", status = "primary", solidHeader = TRUE, width = 12,
                            p("This app provides visualizations and analyses of DUI and fatalities data across all 50 U.S. states.")
                        )
                    )
            ),
            tabItem(tabName = "visualizations",
                    fluidRow(
                        box(title = "Total DUI by State", status = "primary", solidHeader = TRUE, width = 6,
                            plotlyOutput("bar_dui", height = "720px")
                        ),
                        box(title = "Total Fatalities by State", status = "primary", solidHeader = TRUE, width = 6,
                            plotlyOutput("bar_fatalities", height = "720px")
                        )
                    ),
                    fluidRow(
                        box(title = "Fatalities vs DUI", status = "primary", solidHeader = TRUE, width = 6,
                            plotlyOutput("scatter_dui_fatalities", height = "400px")
                        ),
                        box(title = "Population vs DUI", status = "primary", solidHeader = TRUE, width = 6,
                            plotlyOutput("scatter_dui_population", height = "400px")
                        )
                    ),
                    fluidRow(
                        box(title = "Box Plot for DUI by Sunday Sales", status = "primary", solidHeader = TRUE, width = 6,
                            plotlyOutput("box_plot", height = "400px")
                        ),
                        box(title = "Pie Chart of DUI and Sunday Sales", status = "primary", solidHeader = TRUE, width = 6,
                            plotlyOutput("pie_chart_dui_sunday_sales", height = "400px")
                        )
                    ),
                    fluidRow(
                        box(title = "Summary Table", status = "primary", solidHeader = TRUE, width = 12,
                            DTOutput("summary_table"),
                            style = "overflow-x: scroll;"
                        )
                    )
            ),
            tabItem(tabName = "bar_charts",
                    fluidRow(
                        box(title = "Bar Chart", status = "primary", solidHeader = TRUE, width = 12,
                            selectInput("x_axis_bar", "Select X Axis", choices = c("State", "Sunday.Sales"), selected = "State"),
                            selectInput("y_axis_bar", "Select Y Axis", choices = c("DUI", "Fatalities", "Population"), selected = "DUI"),
                            plotlyOutput("bar_chart", height = "750px")
                        )
                    )
            ),
            tabItem(tabName = "scatter_plots",
                    fluidRow(
                        box(title = "Scatter Plot", status = "primary", solidHeader = TRUE, width = 12,
                            selectInput("x_axis_scatter", "Select X Axis", choices = c("DUI", "Fatalities", "Population"), selected = "DUI"),
                            selectInput("y_axis_scatter", "Select Y Axis", choices = c("DUI", "Fatalities", "Population"), selected = "Fatalities"),
                            plotlyOutput("scatter_plot", height = "500px")
                        )
                    )
            ),
            tabItem(tabName = "box_plots",
                    fluidRow(
                        box(title = "Box Plot", status = "primary", solidHeader = TRUE, width = 12,
                            selectInput("x_axis_box", "Select X Axis", choices = c("Sunday.Sales"), selected = "Sunday.Sales"),
                            selectInput("y_axis_box", "Select Y Axis", choices = c("DUI", "Fatalities", "Population"), selected = "DUI"),
                            plotlyOutput("dynamic_box_plot", height = "500px")
                        )
                    )
            )
        )
    )
)

# Define server 
server <- function(input, output) {
    output$bar_dui <- renderPlotly({
        p <- ggplot(data, aes(x = fct_reorder(State, DUI), y = DUI)) +
            geom_bar(stat = "identity", fill = "steelblue") +
            coord_flip() +
            labs(title = "Total DUI by State", x = "State", y = "DUI") +
            theme(axis.text.y = element_text(size = 10, angle = 0))
        ggplotly(p)
    })
    
    output$bar_fatalities <- renderPlotly({
        p <- ggplot(data, aes(x = fct_reorder(State, Fatalities), y = Fatalities)) +
            geom_bar(stat = "identity", fill = "red") +
            coord_flip() +
            labs(title = "Total Fatalities by State", x = "State", y = "Fatalities") +
            theme(axis.text.y = element_text(size = 10, angle = 0))
        ggplotly(p)
    })
    
    output$scatter_dui_fatalities <- renderPlotly({
        p <- ggplot(data, aes(x = DUI, y = Fatalities)) +
            geom_point(color = "blue") +
            labs(title = "DUI vs Fatalities", x = "DUI", y = "Fatalities")
        ggplotly(p)
    })
    
    output$scatter_dui_population <- renderPlotly({
        p <- ggplot(data, aes(x = DUI, y = Population)) +
            geom_point(color = "green") +
            labs(title = "DUI vs Population", x = "DUI", y = "Population")
        ggplotly(p)
    })
    
    output$box_plot <- renderPlotly({
        p <- ggplot(data, aes(x = Sunday.Sales, y = DUI)) +
            geom_boxplot(aes(color = Sunday.Sales)) +
            labs(title = "Box Plot of DUI by Sunday Sales", x = "Sunday Sales", y = "DUI")
        ggplotly(p)
    })
    
    output$pie_chart_dui_sunday_sales <- renderPlotly({
        data_sunday_sales <- data %>%
            group_by(Sunday.Sales) %>%
            summarise(Total_DUI = sum(DUI))
        
        p <- plot_ly(data_sunday_sales, labels = ~Sunday.Sales, values = ~Total_DUI, type = 'pie', textinfo = 'label+percent',
                     insidetextorientation = 'radial') %>%
            layout(title = 'DUI Distribution by Sunday Sales')
        p
    })
    
    output$bar_chart <- renderPlotly({
        x_var <- input$x_axis_bar
        y_var <- input$y_axis_bar
        
        p <- ggplot(data, aes_string(x = x_var, y = y_var)) +
            geom_bar(stat = "identity", fill = "steelblue") +
            coord_flip() +
            labs(title = paste("Bar Chart of", y_var, "by", x_var), x = x_var, y = y_var) +
            theme(axis.text.y = element_text(size = 10, angle = 0))
        ggplotly(p)
    })
    
    output$scatter_plot <- renderPlotly({
        x_var <- input$x_axis_scatter
        y_var <- input$y_axis_scatter
        
        p <- ggplot(data, aes_string(x = x_var, y = y_var)) +
            geom_point(color = "blue") +
            labs(title = paste("Scatter Plot of", y_var, "by", x_var), x = x_var, y = y_var)
        ggplotly(p)
    })
    
    output$dynamic_box_plot <- renderPlotly({
        x_var <- input$x_axis_box
        y_var <- input$y_axis_box
        
        p <- ggplot(data, aes_string(x = x_var, y = y_var)) +
            geom_boxplot(aes_string(color = x_var)) +
            labs(title = paste("Box Plot of", y_var, "by", x_var), x = x_var, y = y_var)
        ggplotly(p)
    })
    
    output$summary_table <- renderDT({
        summary_data <- data %>%
            summarise(
                Total_DUI = sum(DUI, na.rm = TRUE),
                Total_Fatalities = sum(Fatalities, na.rm = TRUE),
                Average_DUI = mean(DUI, na.rm = TRUE),
                Average_Fatalities = mean(Fatalities, na.rm = TRUE),
                Max_DUI = max(DUI, na.rm = TRUE),
                Max_Fatalities = max(Fatalities, na.rm = TRUE),
                Min_DUI = min(DUI, na.rm = TRUE),
                Min_Fatalities = min(Fatalities, na.rm = TRUE),
                Median_DUI = median(DUI, na.rm = TRUE),
                Median_Fatalities = median(Fatalities, na.rm = TRUE),
                SD_DUI = sd(DUI, na.rm = TRUE),
                SD_Fatalities = sd(Fatalities, na.rm = TRUE),
                Range_DUI = max(DUI, na.rm = TRUE) - min(DUI, na.rm = TRUE),
                Range_Fatalities = max(Fatalities, na.rm = TRUE) - min(Fatalities, na.rm = TRUE)
            )
        datatable(summary_data, options = list(scrollX = TRUE))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
