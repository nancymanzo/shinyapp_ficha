# Cargar las librerías necesarias
library(tidyverse)
library(DT)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)
library(shinybusy)
library(googledrive)
library(googlesheets4)
library(readxl)
library(readr)
library(ggplot2)
library(scales)
library(dplyr)
library(plotly)
library(lubridate)
library(RColorBrewer)
library(janitor)
library(mxmaps)
library(stringr)
library(wordcloud2)
library(wordcloud)
library(dashboardthemes)
library(shinythemes)
library(extrafont)
library(showtext)
library(jsonlite)
library(data.table)
library(leaflet)
library(shiny.router)
library(latex2exp)
library(tibble)
library(tibbletime)
library(wesanderson)
library(colourpicker)
library(shinycssloaders)
library(shinyFeedback)
library(rmarkdown)

# hf108 hf208
# Cargar los datos y ajustar la codificación
usuarias <- read_excel("00_base_usuarias.xlsx") %>%
  # filter(!all_vars(c(NA, "NA", NULL))) %>%
  mutate(`unidad-desc` = case_when(
    `unidad-desc` %in% c("Clinica de la Mujer de Oxaca") ~ "Clínica de la Mujer de Oaxaca",
    `unidad-desc` %in% c("HIGA Dr. Diego Paroissien") ~ "HIGA Dr. Diego Paroissien",
    `unidad-desc` %in% c("HIGA Dr. Jose Penna") ~ "HIGA Dr. José Penna",
    `unidad-desc` %in% c("HIGA Evita de Lanus") ~ "HIGA Evita de Lanús",
    `unidad-desc` %in% c("HIGA San Martin") ~ "HIGA San Martín",
    `unidad-desc` %in% c("Hospital Eva Peron") ~ "Hospital Eva Perón",
    `unidad-desc` %in% c("Hospital General Dr Aurelio Valdivieso",
                         "Hospital General Dr. Aurelio Valdivieso",
                         "Hospital General Dr. Aurelio Valvidieso",
                         "Hospital General Dr. Aurrlio Valdivieso") ~ "Hospital General Dr. Aurelio Valdivieso",
    `unidad-desc` %in% c("Hospital Jose Bernardo Iturraspe") ~ "Hospital José Bernardo Iturraspe",
    `unidad-desc` %in% c("Hospital Zonal General de Agudos Dr. Carlos Bocalandro") ~ "Hospital Zonal General de Agudos Dr. Carlos Bocalandro",
    `unidad-desc` %in% c("Hospital Zonal General de Agudos General Manuel Belgrano") ~ "Hospital Zonal General de Agudos General Manuel Belgrano",
    TRUE ~ `unidad-desc`)) %>%
  janitor::clean_names()

prestadores <- read_excel("00_base_prestadores_de_servicios.xlsx") %>%
  # filter(!all_vars(c(NA, "NA", NULL))) %>%
  janitor::clean_names()

preguntas <- read_excel("preguntas_clave.xlsx")
preguntas<-preguntas %>% filter(!is.na(variables),
                                !is.na(pregunta))
preguntas_choices <- setNames(preguntas$variables, preguntas$pregunta)
preguntas_choices_1 <- setNames(preguntas$variables_1, preguntas$pregunta_1)

# usuarias <- usuarias %>% select_if(~ !all(is.na(.)))
# prestadores <- prestadores %>% select_if(~ !all(is.na(.)))

# agregar_etiquetas <- function(datos, var_fill, color_texto, color_relleno, mostrar_etiquetas) {
#   if (mostrar_etiquetas) {
#     return(
#       geom_label(
#         # aes_string(label = "sprintf('%d (%.1f%%)', n, percentage)", group = var_fill),  # Agrupar por la variable seleccionada en fill
#         # aes(label = sprintf("%.1f%%", percentage)), na.rm = TRUE,
#         aes(label = sprintf("%.1f%%", percentage)), group = var_fill, na.rm = TRUE,
#         position = position_fill(vjust = 0.5),  # Centrar etiquetas en cada barra apilada
#         size = 5,
#         color = color_texto, # Usar el color seleccionado por el usuario para el texto de las etiquetas
#         fill = color_relleno, # Usar el color seleccionado por el usuario para el relleno de las etiquetas
#         label.padding = unit(0.25, "lines"),  # Ajusta el espacio alrededor del texto
#         label.size = 0.3,  # Tamaño del borde alrededor de las etiquetas
#         show.legend = FALSE
#       )
#     )
#   } else {
#     return(NULL)  # No mostrar etiquetas
#   }
# }

agregar_etiquetas <- function(datos, var_fill, color_texto, color_relleno, mostrar_etiquetas) {
  if (mostrar_etiquetas) {
    return(
      geom_label(
        data = datos,  # Asegúrate de pasar el conjunto de datos correcto
        aes(label = sprintf("%.0f (%.1f%%)", n, percentage), group = !!sym(var_fill)),  # Etiquetas con n (sin decimales) y porcentaje
        na.rm = TRUE,
        position = position_fill(vjust = 0.5),  # Centrar etiquetas en cada barra apilada
        size = 5,
        color = color_texto,  # Color del texto de las etiquetas
        fill = color_relleno,  # Color de relleno de las etiquetas
        label.padding = unit(0.25, "lines"),  # Espacio alrededor del texto
        label.size = 0.3,  # Tamaño del borde alrededor de las etiquetas
        show.legend = FALSE
      )
    )
  } else {
    return(NULL)  # No mostrar etiquetas
  }
}

# Función para agregar geom_label condicionalmente con mejor alineación y agrupamiento
agregar_etiquetas_1 <- function(datos_1, var_fill_1, color_texto_1, color_relleno_1, mostrar_etiquetas_1) {
  if (mostrar_etiquetas) {
    return(
      geom_label(
        aes_string(label = "sprintf('%d (%.1f%%)', n, percentage)", group = var_fill),  # Agrupar por la variable seleccionada en fill
        position = position_fill(vjust = 0.5),  # Centrar etiquetas en cada barra apilada
        size = 5,
        color = color_texto_1, # Usar el color seleccionado por el usuario para el texto de las etiquetas
        fill = color_relleno_1, # Usar el color seleccionado por el usuario para el relleno de las etiquetas
        label.padding = unit(0.25, "lines"),  # Ajusta el espacio alrededor del texto
        label.size = 0.3,  # Tamaño del borde alrededor de las etiquetas
        show.legend = FALSE
      )
    )
  } else {
    return(NULL)  # No mostrar etiquetas
  }
}

### Lógica de la UI y el Server

# UI (Interfaz de Usuario)
ui <- shinyUI(
  tagList(
    includeCSS("./www/style.css"),
    fluidPage(
      class = 'p-2',
      tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
                tags$style(HTML("
                .navbar {
              background-color: #f15a29 !important;
              border: none;
              margin: 0 !important;
              padding: 0 !important;
              width: 100%;
              }
              .navbar .navbar-nav {float: right;,
               background-color: #f15a29 !important;}

              .navbar {
                background-color: #f15a29 !important;
                border: none;
              }
              
              .navbar-nav > li > a, .navbar-nav-right > li > a {
                color: white !important;
                font-size: 14px;
                padding: 15px 20px;
              }
              
              .navbar-nav > li > a:hover, .navbar-nav-right > li > a:hover {
                background-color: #d9534f !important;
                color: white !important;
              }
              
              .navbar-nav > li.active > a, .navbar-nav-right > li.active > a {
                background-color: #f15a29 !important;
                color: white !important;
              }
              
              .navbar-brand {
                color: white !important;
                font-weight: bold;
                font-size: 24px;
              }
                
                ")),
                tags$footer(style = "
                  position:fixed;
                  bottom:0px;
                  width:100%;
                  height:20px;
                  color: black;
                  padding: 0px;
                  background-color: #f15a29;
                  z-index: 100;")),
      add_busy_spinner(onstart = FALSE, spin = "fading-circle", color = "#f15a29"),
      navbarPage(
        tags$img(src = "Logo_AVGM.png", height = "60px", style = "margin-right:10px; background-color:#f15a29"),  # Añadir la imagen
        header = busy_start_up(
          loader = spin_epic("flower", color = "#f15a29"),
          text = "Cargando",
          timeout = 1500,
          color = "#f15a29",
          background = "white"
        ),
        useShinydashboard(),
        # Inicio -----------------------------------------------------------------------
        # tabPanel(
        #   title = p("Inicio", style = "font-weight:bold; color: #ffffff;"), id = "Inicio"
        #   # Otros elementos de la UI...
        # ),

        # Usuarias ------------------------------------------------------------
        tabPanel(
          title = p("Usuarias", style = "font-weight:bold; color: #ffffff;"),
          sidebarPanel(
            width = 3,
            "Seleccione variables a filtrar",
            textInput(
              inputId = "grafico_titulo",
              label = "Título del gráfico",
              value = "Cuestionario de usuarias"
            ),
            # Añadir un selectInput para ajustar el tamaño del texto
            selectInput(
              inputId = "tamanio_texto",
              label = "Tamaño del texto",
              choices = c("Pequeño" = 10, "Mediano" = 15, "Grande" = 20),
              selected = 15
            ),
            selectInput(
              inputId = "paleta_discreta",
              label = "Seleccione la paleta de colores",
              choices = c("Set1", "Set2", "Set3", "Dark2", "Paired", "Accent", "Pastel1",
                          "Pastel2", "Blues", "Reds", "Greens", "Greys", "Purples",
                          "YlGn", "Spectral", "Zissou1", "GrandBudapest", "Moonrise1",
                          "viridis", "magma", "plasma", "cividis", "d3_category20",
                          "d3_category10", "npg", "aaas", "lancet", "ucscgb"),
              selected = "Paired"
            ),
            colourInput(
              inputId = "color_texto",
              label = "Color del texto de etiquetas",
              value = "black"
            ),
            colourInput(
              inputId = "color_relleno",
              label = "Color de relleno de etiquetas",
              value = "white"
            ),
            checkboxInput(
              inputId = "mostrar_etiquetas",
              label = "Mostrar etiquetas",
              value = TRUE
            ),
            colourInput(
              inputId = "color_fondo",
              label = "Color de fondo del gráfico",
              value = "white"
            ),
            colourInput(
              inputId = "color_texto_global",
              label = "Color del texto global",
              value = "black"
            ),
            selectInput(
              inputId = "posicion_leyenda",
              label = "Posición de la leyenda",
              choices = c("Arriba" = "top", "Derecha" = "right", "Abajo" = "bottom", "Izquierda" = "left", "Sin leyenda" = "none"),
              selected = "bottom"  # Ajustar la leyenda en la parte inferior
            ),
            # Añadir botones para descargar datos y gráfico
            downloadButton("download_data", "Descargar Datos"),
            downloadButton("download_plot", "Descargar Gráfico")
          ),
          mainPanel(
            width = 9,
            fluidRow(
              column(
                width = 4,
                selectInput(
                  inputId = "usuarias_pais",
                  label = "Seleccione el país",
                  choices = c("Latinoamérica", unique(sort(usuarias$`pais_desc`))),
                  multiple = F,
                  selected = "Latinoamérica"
                )
              ),
              column(
                width = 4,
                selectInput(
                  inputId = "usuarias_estado",
                  label = "Seleccione el estado",
                  choices = unique(sort(usuarias$`estado_desc`)),
                  multiple = TRUE
                )
              ),
              column(
                width = 4,
                selectInput(
                  inputId = "usuarias_unidad",
                  label = "Selecciona la unidad (hospital)",
                  choices = unique(sort(usuarias$`unidad_desc`)),
                  multiple = TRUE
                )
              )
            ),
            fluidRow(
              column(
                width = 6,
                selectInput(
                  inputId = "variable_y",
                  label = "Seleccione la variable para el eje Y",
                  choices = preguntas_choices,
                  selected = names(preguntas_choices)[4]
                )
              ),
              column(
                width = 6,
                selectInput(
                  inputId = "variable_fill",
                  label = "Seleccione la variable para cruzar",
                  choices = preguntas_choices,
                  selected = names(preguntas_choices)[3]
                )
              )
            ),
            hr(),
            plotOutput("gr_usuarias"))
        ),

        # Nueva pestaña de Fichas de Usuarias en la UI
        tabPanel(
          useShinyFeedback(),  # Habilitar notificaciones
          
          title = p("Fichas de Usuarias", style = "font-weight:bold; color: #ffffff;"),
          sidebarLayout(
            sidebarPanel(
              selectInput(
                "entidad",
                "Selecciona la entidad:",
                choices = NULL  # Se cargan dinámicamente desde la base
              ),
              actionButton("generar", "Previsualizar la ficha aquí"),
              downloadButton("descargar", "Descargar en Word")
            ),
            mainPanel(
              htmlOutput("vista_html")  # Mostrar la ficha en HTML
            )
          )
        ),
        # prestadores ------------------------------------------------------------
        tabPanel(#"prestadores",
          title = p("Prestadores/as de servicios", style = "font-weight:bold; color: #ffffff;"),
          sidebarPanel(
            width = 3,
            "Seleccione variables a filtrar",
            textInput(
              inputId = "grafico_titulo_1",
              label = "Título del gráfico",
              value = "Cuestionario de prestadores/as de servicios" # Título predeterminado
            ),
            colourInput(
              inputId = "color_inicio_1",
              label = "Color de inicio (menor valor)",
              value = "#489CF0" # Color predeterminado
            ),
            colourInput(
              inputId = "color_fin_1",
              label = "Color de fin (mayor valor)",
              value = "#138A45" # Color predeterminado
            ),
            colourInput(
              inputId = "color_texto_1",
              label = "Color del texto de etiquetas",
              value = "black" # Color predeterminado
            ),
            colourInput(
              inputId = "color_relleno_1",
              label = "Color de relleno de etiquetas",
              value = "white" # Color predeterminado
            ),
            selectInput(
              inputId = "posicion_etiquetas_1",
              label = "Posición de las etiquetas",
              choices = c("Centro" = "center", "Fuera" = "outside"),
              selected = "center"
            ),
            checkboxInput(
              inputId = "mostrar_etiquetas_1",
              label = "Mostrar etiquetas",
              value = TRUE # Predeterminado activado
            ),
            colourInput(
              inputId = "color_fondo_1",
              label = "Color de fondo del gráfico",
              value = "white" # Color predeterminado
            ),
            colourInput(
              inputId = "color_texto_global_1",
              label = "Color del texto global",
              value = "black" # Color predeterminado
            ),
            selectInput(
              inputId = "posicion_leyenda_1",
              label = "Posición de la leyenda",
              choices = c("Arriba" = "top", "Derecha" = "right", "Abajo" = "bottom", "Izquierda" = "left", "Sin leyenda" = "none"),
              selected = "none" # Posición predeterminada
            )
          ),
          mainPanel(
            width = 9,
            fluidRow( # Utilizamos fluidRow para distribuir los selectInput en una fila
              column(
                width = 4,
                selectInput(
                  inputId = "prestadores_pais",
                  label = "Seleccione el país",
                  choices = c("Latinoamérica", unique(sort(prestadores$`pais_desc`))),
                  multiple = F,
                  selected = "Latinoamérica"
                )
              ),
              column(
                width = 4,
                selectInput(
                  inputId = "prestadores_estado",
                  label = "Seleccione el estado",
                  choices = unique(sort(prestadores$`estado_desc`)),
                  multiple = TRUE
                )
              ),
              column(
                width = 4,
                selectInput(
                  inputId = "prestadores_unidad",
                  label = "Selecciona la unidad (hospital)",
                  choices = unique(sort(prestadores$`unidad_desc`)),
                  multiple = TRUE
                )
              )
            ),
            fluidRow( # Nueva fila para seleccionar la variable del eje Y
              column(
                width = 12,
                selectInput(
                  inputId = "variable_y_1",
                  label = "Seleccione la variable para el eje Y",
                  choices = preguntas_choices_1,  # Usar las preguntas como etiquetas y variables como valores
                  selected = names(preguntas_choices_1)[1] # Selección predeterminada
                )
              )
            ),
            plotOutput("gr_prestadores_1")
          )
        ),
        # tabPanel(title = p("Prestadores/as de servicios", style = "font-weight:bold; color: #ffffff;")),
        # tabPanel(title = p("Descarga de datos", style = "font-weight:bold; color: #ffffff;"))
      )
    )
  )
)

# Server (Lógica del Servidor)
server <- function(input, output, session) {
  # Reactivo para filtrar datos de usuarias
  usuarias_reactive <- reactive({
    usuarias %>%
      filter(
        if(is.null(input$usuarias_pais) || input$usuarias_pais == "Latinoamérica") TRUE else `pais_desc` %in% input$usuarias_pais,
        if(is.null(input$usuarias_estado)) TRUE else `estado_desc` %in% input$usuarias_estado,
        if(is.null(input$usuarias_unidad)) TRUE else `unidad_desc` %in% input$usuarias_unidad
      )
  })

  # Reactivo para obtener la variable seleccionada para el eje Y
  variable_y_reactive <- reactive({
    selected_question <- input$variable_y
    var_clave <- preguntas_choices[[selected_question]]

    if (is.null(var_clave) || length(var_clave) == 0) {
      return(NULL)
    }
    return(var_clave)
  })

  # Reactivo para obtener la variable seleccionada para el fill
  variable_fill_reactive <- reactive({
    selected_fill <- input$variable_fill
    var_clave_fill <- preguntas_choices[[selected_fill]]

    if (is.null(var_clave_fill) || length(var_clave_fill) == 0) {
      return(NULL)
    }
    return(var_clave_fill)
  })

  # Verificar si la variable seleccionada para el fill es continua o discreta
  output$isFillContinuous <- reactive({
    datos <- usuarias_reactive()
    var_fill <- variable_fill_reactive()

    # Determinamos si es numérica (continua) o no (discreta)
    is.numeric(datos[[var_fill]])
  })

  outputOptions(output, "isFillContinuous", suspendWhenHidden = FALSE)

  # Gráfico de usuarias
  output$gr_usuarias <- renderPlot({
    var_y <- variable_y_reactive()
    var_fill <- variable_fill_reactive()

    if (is.null(var_y) || is.null(var_fill)) {
      return(NULL)
    }


    # datos_filtrados <- usuarias_reactive() %>%
    #   filter(!is.na(var_y)) %>%
    #   group_by(across(all_of(var_y)), across(all_of(var_fill))) %>%
    #   summarise(n = n()) %>%
    #   mutate(percentage = n / sum(n) * 100) %>%
    #   ungroup() %>%
    #   bind_rows(
    #     summarise(.,
    #               across(where(is.numeric), sum),
    #               # ~(n/sum(n)*100)),  # Sumar columnas numéricas
    #               var_y = "Total",  # Asignar "Total por País"
    #               .by = unique(var_fill)  # Mantener grupos de edad
    #     ))


    # var_fill_existentes <- intersect(var_fill, colnames(datos_usuarias))

    datos_filtrados <- usuarias_reactive() %>%
      filter(!is.na(var_y)) %>%
      group_by(across(all_of(var_y)), across(all_of(var_fill))) %>%
      summarise(#n1 = n(),
        n = n()) %>%
      mutate(percentage = n / sum(n) * 100) %>%
      ungroup() %>%
      bind_rows(
        summarise(.,
                  n = sum(n),
                  percentage = (n/ sum(n())),
                  var_y = var_y,  # Asigna "Total" a la columna de país
                  .by = var_fill))


    # Definir una paleta de colores según la elección del usuario
    paleta_discreta <- switch(input$paleta_discreta,
                              "Zissou1" = wes_palette("Zissou1", length(unique(datos_filtrados[[var_fill]])), type = "discrete"),
                              "GrandBudapest" = wes_palette("GrandBudapest1", length(unique(datos_filtrados[[var_fill]])), type = "discrete"),
                              "Moonrise1" = wes_palette("Moonrise1", length(unique(datos_filtrados[[var_fill]])), type = "discrete"),
                              "viridis" = viridis::viridis(length(unique(datos_filtrados[[var_fill]])), option = "viridis"),
                              "magma" = viridis::viridis(length(unique(datos_filtrados[[var_fill]])), option = "magma"),
                              "plasma" = viridis::viridis(length(unique(datos_filtrados[[var_fill]])), option = "plasma"),
                              "cividis" = viridis::viridis(length(unique(datos_filtrados[[var_fill]])), option = "cividis"),
                              "d3_category20" = pal_d3("category20")(length(unique(datos_filtrados[[var_fill]]))),
                              "d3_category10" = pal_d3("category10")(length(unique(datos_filtrados[[var_fill]]))),
                              "npg" = pal_npg()(length(unique(datos_filtrados[[var_fill]]))),
                              "aaas" = pal_aaas()(length(unique(datos_filtrados[[var_fill]]))),
                              "lancet" = pal_lancet()(length(unique(datos_filtrados[[var_fill]]))),
                              "ucscgb" = pal_ucscgb()(length(unique(datos_filtrados[[var_fill]]))),
                              brewer.pal(n = length(unique(datos_filtrados[[var_fill]])), input$paleta_discreta))

    # Aplicar el tamaño del texto seleccionado
    tamanio_texto <- as.numeric(input$tamanio_texto)

    p <- datos_filtrados %>%
      mutate(var_y = case_when(
        var_y %in% c(NA, "NA")~ "Total",
        is.na(var_y) ~ "Total",  # Asignar "Total" cuando var_y es NA
        TRUE ~ as.character(var_y)  # Mantener el valor original si no es NA
      )) %>%
      ggplot(aes_string(y =c(var_y), x = "n", fill = var_fill)) +
      # geom_col(position = "fill", stat = "identity") +  # Barras apiladas normalizadas
      geom_col(position = "fill")+
      scale_y_discrete(labels = function(y) str_wrap(y, width = 10)) +
      scale_x_continuous(labels = scales::percent)+
      agregar_etiquetas(datos_filtrados, var_fill, input$color_texto, input$color_relleno, input$mostrar_etiquetas) +
      coord_flip() +
      theme_minimal() +
      theme(
        legend.position = input$posicion_leyenda,
        legend.text = element_text(size = tamanio_texto, color = input$color_texto_global),
        legend.title = element_text(size = tamanio_texto, color = input$color_texto_global),
        legend.key.width = unit(1, "cm"),  # Ajustar el ancho de la leyenda
        plot.background = element_rect(fill = input$color_fondo),
        panel.background = element_rect(fill = input$color_fondo),
        axis.text = element_text(color = input$color_texto_global),
        axis.text.x = element_text(color = input$color_texto_global, size = tamanio_texto),
        # axis.text.x = element_text(size = tamanio_texto, label = function(x) str_wrap(x, width = 10)),
        axis.title = element_text(color = input$color_texto_global),
        plot.title = element_text(color = input$color_texto_global, hjust = 0, face = "bold", size = tamanio_texto)
      ) +
      labs(
        title = input$grafico_titulo,
        y = "",
        x = "",
        fill = " "
      ) +
      guides(fill = guide_legend(ncol = 2))  # Leyenda con dos columnas

    # Detectar si la variable 'fill' es continua o discreta
    if (is.numeric(datos_filtrados[[var_fill]])) {
      # Si la variable es continua, usar una escala continua
      p <- p + scale_fill_gradient(
        low = input$color_inicio,
        high = input$color_fin
      )
    } else {
      # Si la variable es discreta, usar la paleta discreta
      p <- p + scale_fill_manual(values = paleta_discreta, na.value = "gray70")
    }

    # Renderizar el gráfico
    print(p)
  })

  # Descargar los datos filtrados
  output$download_data <- downloadHandler(
    filename = function() {
      paste("datos_filtrados", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      openxlsx::write.xlsx(usuarias_reactive(), file, row.names = FALSE)
    }
  )

  # Descargar el gráfico
  output$download_plot <- downloadHandler(
    filename = function() {
      paste("grafico_usuarias", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      ggsave(file, plot = last_plot(), device = "png", width = 10, height = 6)
    }
  )

  # Reactivo para filtrar datos de usuarias
  prestadores_reactive <- reactive({
    prestadores %>%
      filter(
        if(input$prestadores_pais == "Latinoamérica") TRUE else `pais_desc` %in% input$prestadores_pais,
        if(!is.null(input$prestadores_estado)) `estado_desc` %in% input$prestadores_estado else TRUE,
        if(!is.null(input$prestadores_unidad)) `unidad_desc` %in% input$prestadores_unidad else TRUE
      )
  })

  # Reactivo para obtener la variable seleccionada para el eje Y
  variable_y_reactive_1 <- reactive({
    selected_question_1 <- input$variable_y_1
    var_clave_1 <- preguntas_choices_1[[selected_question_1]]

    # Verificar si var_clave está vacío
    if (is.null(var_clave_1) || length(var_clave_1) == 0) {
      print("Clave de variable es NULL o vacía.")
      return(NULL)
    }
    return(var_clave_1)
  })

  # Función para determinar la posición del geom_label
  calcular_posicion_1 <- reactive({
    if (input$posicion_etiquetas_1 == "center") {
      return(position_stack(vjust = 0.5))  # Centrado dentro de la barra
    } else {
      return(position_nudge(x = 0.5))  # Fuera de la barra
    }
  })

  # Función para agregar geom_label condicionalmente
  agregar_etiquetas_1 <- reactive({
    if (input$mostrar_etiquetas_1) {
      return(
        geom_label(aes(label = sprintf("%d (%.1f%%)", n, percentage)),
                   position = calcular_posicion_1(),  # Posición determinada por el usuario
                   size = 3,
                   color = input$color_texto_1, # Usar el color seleccionado por el usuario para el texto de las etiquetas
                   fill = input$color_relleno_1, # Usar el color seleccionado por el usuario para el relleno de las etiquetas
                   show.legend = FALSE)
      )
    } else {
      return(NULL)  # No mostrar etiquetas
    }
  })

  # Gráfico de usuarias
  output$gr_prestadores_1 <- renderPlot({
    var_y_1 <- variable_y_reactive_1()

    # Verificar si la variable reactiva es NULL
    if (is.null(var_y_1)) {
      print("Variable reactiva es NULL. No se graficará.")
      return(NULL)
    }

    prestadores_reactive() %>%
      filter(!is.na(var_y_1)) %>%
      group_by(across(all_of(var_y_1))) %>% # Agrupar por la variable seleccionada
      summarise(n = n()) %>%
      mutate(percentage = n / sum(n) * 100) %>%
      ungroup() %>%
      ggplot(aes_string(y =str_wrap(var_y_1,10), x = str_wrap("n",5), fill = "n")) + # Usar la variable reactiva en y
      geom_col(show.legend = TRUE) +  # Mostrar leyenda
      scale_fill_gradient(
        low = input$color_inicio_1,
        high = input$color_fin_1
      ) +
      # scale_y_discrete(labels = function(y) str_wrap(y, width = 10)) +
      # scale_x_continuous(labels = scales::percent)+
      agregar_etiquetas_1() +  # Llamar a la función reactiva para agregar etiquetas si está activado
      coord_flip() +
      theme_minimal() +
      theme(
        legend.position = input$posicion_leyenda_1, # Cambiar la posición de la leyenda según la selección del usuario
        legend.text = element_text(size = 15, color = input$color_texto_global_1), # Color del texto de la leyenda
        legend.title = element_text(size = 25, color = input$color_texto_global_1), # Color del título de la leyenda
        plot.background = element_rect(fill = input$color_fondo_1), # Cambiar el fondo del gráfico
        panel.background = element_rect(fill = input$color_fondo_1), # Cambiar el fondo del panel
        axis.text = element_text(color = input$color_texto_global_1), # Cambiar el color de texto de los ejes
        axis.title = element_text(color = input$color_texto_global_1), # Cambiar el color del título de los ejes
        plot.title = element_text(color = input$color_texto_global_1, hjust = 0, face = "bold") # Cambiar el color, posición y estilo del título
      ) +
      labs(
        title = input$grafico_titulo_1,  # Usar el título proporcionado por el usuario
        y = "",
        x = "",
        fill = ""
      )
  })


  # Cargar las entidades únicas desde la base de usuarias
  usuarias <- read_excel("00_base_usuarias.xlsx") %>% clean_names()
  entidades <- unique(usuarias$estado_desc)
  
  # Actualizar las opciones del selectInput
  updateSelectInput(session, "entidad", choices = entidades)
  
  # Ruta temporal para almacenar la ficha generada
  ficha_path <- reactiveVal()
  
  # Generar la ficha al hacer clic en "Generar"
  observeEvent(input$generar, {
    entidad <- input$entidad
    temp_html <- tempfile(fileext = ".html")
    
    # Renderizar el Rmd como HTML
    rmarkdown::render(
      input = "ficha_usuaria.Rmd",
      params = list(entidad_desc = entidad),
      output_file = temp_html
    )
    
    # Guardar la ruta generada
    ficha_path(temp_html)
  })
  
  # Mostrar el contenido HTML directamente en la interfaz
  output$vista_html <- renderUI({
    req(ficha_path())
    HTML(paste(readLines(ficha_path()), collapse = "\n"))  # Leer y mostrar el HTML
  })
  
  # Permitir la descarga del reporte en formato Word
  output$descargar <- downloadHandler(
    filename = function() {
      paste0("Ficha_", input$entidad, ".docx")
    },
    content = function(file) {
      rmarkdown::render(
        input = "ficha_usuaria.Rmd",
        params = list(entidad_desc = input$entidad),
        output_format = "word_document",
        output_file = file
      )
    }
  )






}
# Ejecutar la aplicación
shinyApp(ui = ui, server = server)
