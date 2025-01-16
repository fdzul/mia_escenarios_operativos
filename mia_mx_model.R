# Step 1. load the dataset #####
load("/Users/felipedzul/Library/CloudStorage/OneDrive-Personal/automatic_read_sinave/8.RData/epid_channel_data.RData")

# Step 2. extract the dengue cases of veracruz ####
x <- x |>
    dplyr::filter(DES_EDO_RES == "VERACRUZ") 

y <- tibble::tibble(SEM = x$SEM,
                    y = x$q25,
                    arm = "Nuevo Paradigma")
prob <- tibble::tibble(SEM = x$SEM,
                       prob = rep(0, length(x$SEM)))

# plot risk stratificaction  #####
ggplot2::ggplot() +
    ggplot2::geom_rect(ggplot2::aes(xmin = c(-Inf, 25), 
                                    xmax = c(25, Inf), 
                                    ymin = -Inf, 
                                    ymax = Inf),
                       fill = c("#2EB67D", "#E01E5A"),
                       alpha = .2) +
    ggplot2::geom_rect(ggplot2::aes(xmin = 20, 
                                    xmax = 25, 
                                    ymin = -Inf, 
                                    ymax = Inf),
                       fill = "#2EB67D",
                       alpha = 0.8) +
    ggplot2::geom_rect(ggplot2::aes(xmin = 38, 
                                    xmax = 40, 
                                    ymin = -Inf, 
                                    ymax = Inf),
                       fill = "#2EB67D",
                       alpha = 0.8) +
    ggplot2::geom_vline(xintercept = 25,
                        color = "gray", #"#36C5F0",
                        size = 1,
                        linetype = 3) +
    ggplot2::geom_vline(xintercept = 20,
                        color = "gray", #"#36C5F0",
                        size = 1,
                        linetype = 3) +
    ggplot2::geom_line(data = x, 
                       ggplot2::aes(x = SEM,
                                    y = q75-10),
                       col = "#E01E5A",
                       size = 3) +
    ggplot2::geom_line(data = y, 
                       ggplot2::aes(x = SEM,
                                    y = y),
                       color = "#2EB67D", #"#36C5F0",
                       size = 3) +
    ggplot2::ylab("Número de Casos") +
    ggplot2::xlab("Semanas Epidemiológicas") +
    ggplot2::annotate("text",
                      label = "Escenarios Operativos \nbasados \nen \nTransmisión Persistente",
                      x = 10, 
                      y = 360,
                      #text = "risk",
                      col = "#2EB67D",
                      size = 4) +
    ggplot2::annotate("text",
                      label = "Escenarios Operativos \nbasados \nen \nTransmisión Activa",
                      x = 48, 
                      y = 360,
                      #text = "risk",
                      col = "#E01E5A",
                      size = 4) +
    ggplot2::annotate("text",
                      label = "Acciones Específicas Básicas",
                      x = 10, 
                      y = 280,
                      col = "gray",
                      size = 4) +
    ggplot2::annotate("text",
                      label = "Control de Probables",
                      x = 10, 
                      y = 250,
                      col = "#36C5F0",
                      size = 4) +
    ggplot2::annotate("text",
                      label = "Hotspots del Vector",
                      x = 10, 
                      y = 230,
                      col = "#36C5F0",
                      size = 4) +
    ggplot2::annotate("text",
                      label = "Control de Criaderos Productivos",
                      x = 10, 
                      y = 205,
                      col = "#36C5F0",
                      size = 4) +
    ggplot2::annotate("text",
                      label = "Operativo Semana Santa",
                      x = 10, 
                      y = 180,
                      col = "#36C5F0",
                      size = 4) +
    ggplot2::annotate("text",
                      label = "1ra Jornada Nacional de Dengue",
                      x = 10, 
                      y = 160,
                      col = "#36C5F0",
                      size = 4) +
    ggplot2::annotate("text",
                      label = "Acciones Específicas de Soporte",
                      x = 10, 
                      y = 150-15,
                      col = "gray",
                      size = 4) +
    ggplot2::annotate("text",
                      label = "Termonebulización,\n Nebulización, & \nEliminación Masiva de Criaderos",
                      x = 10, 
                      y = 110-15,
                      col = "gray",
                      size = 3) +
    ggplot2::annotate("text",
                      label = "Sin Control",
                      x = 30, 
                      y = 410,
                      #text = "risk",
                      col = "#E01E5A",
                      size = 4) +
    ggplot2::annotate("text",
                      label = "Con Control",
                      x = 33, 
                      y = 105,
                      col = "#2EB67D",
                      size = 4) +
    ggplot2::annotate(geom ="text",
                      label = "Control Larvario, ULV Térmica & IRS",
                      x = 22, 
                      y = 220,
                      angle = 90,
                      col = "white",
                      size = 4) +
    ggplot2::annotate(geom ="text",
                      label = "Control Larvario & ULV Térmica",
                      x = 39, 
                      y = 220,
                      angle = 90,
                      col = "white",
                      size = 4) +
    ggplot2::geom_rect(ggplot2::aes(xmin = c(-Inf, 25), 
                                    xmax = c(25, Inf), 
                                    ymin = 435, 
                                    ymax = Inf),
                       fill = c("#2EB67D", "#E01E5A"),
                       alpha = .7) +
    ggplot2::annotate("text",
                      label = "Control Reactivo",
                      x = 42, 
                      y = 448,
                      vjust = 0.1,
                      #text = "risk",
                      col = "white",
                      size = 6) +
    ggplot2::annotate("text",
                      label = "Control Proactivo",
                      x = 10, 
                      y = 448,
                      vjust = 0.1,
                      #text = "risk",
                      col = "white",
                      size = 6) +
    ggplot2::geom_rect(ggplot2::aes(xmin = -Inf, 
                                    xmax =  Inf, 
                                    ymin = -Inf, 
                                    ymax = 0),
                       fill = "gray", 
                       alpha = 0.7) +
    ggplot2::annotate("text",
                      label = "Participación Comunitaria | Vigilancia Epidemiologica | Vigilancia Entomológica",
                      x = 28, 
                      y = -1,
                      #text = "risk",
                      vjust = 1.5,
                      col = "black",
                      size = 3.5) +
    ggplot2::geom_rect(ggplot2::aes(xmin = 25, 
                                    xmax = 50, 
                                    ymin = 0, 
                                    ymax = 20),
                       fill = "gray60", 
                       alpha = 0.7) +
    ggplot2::annotate("text",
                      label = "Atención Oportuna del Paciente",
                      x = 38, 
                      y = 7.5,
                      #text = "risk",
                      vjust = 0,
                      col = "black",
                      size = 3.5) +
    ggplot2::annotate("text",
                      label = "Acciones Básicas",
                      x = 49, 
                      y = 290,
                      #text = "risk",
                      vjust = 0,
                      col = "gray60",
                      size = 4) +
    ggplot2::annotate("text",
                      label = "Termonebulización \n(Riesgo Alto)",
                      x = 50, 
                      y = 240,
                      #text = "risk",
                      vjust = 0,
                      col = "#36C5F0",
                      size = 4) +
    ggplot2::annotate("text",
                      label = "Control Larvario",
                      x = 50, 
                      y = 225,
                      #text = "risk",
                      vjust = 0,
                      col = "#36C5F0",
                      size = 4)+
ggplot2::annotate("text",
                  label = "Nebulización Masiva",
                  x = 50, 
                  y = 205,
                  #text = "risk",
                  vjust = 0,
                  col = "#36C5F0",
                  size = 4) +
    ggplot2::annotate("text",
                      label = "Operativo Vacaciones",
                      x = 50, 
                      y = 185,
                      #text = "risk",
                      vjust = 0,
                      col = "#36C5F0",
                      size = 4) +
    ggplot2::annotate("text",
                      label = "Operativo Día de Muertos",
                      x = 50, 
                      y = 165,
                      #text = "risk",
                      vjust = 0,
                      col = "#36C5F0",
                      size = 4) +
    ggplot2::annotate("text",
                      label = "2a Jornada Nacional de Dengue",
                      x = 50, 
                      y = 145,
                      #text = "risk",
                      vjust = 0,
                      col = "#36C5F0",
                      size = 4) +
    ggplot2::annotate("text",
                      label = "Activación de Comite \nInterinstitucional de Salud",
                      x = 30, 
                      y = 200,
                      #text = "risk",
                      vjust = 0,
                      col = "Black",
                      size = 4) +
    ggplot2::geom_rect(ggplot2::aes(xmin = 25, 
                                    xmax = 35, 
                                    ymin = 20, 
                                    ymax = 45),
                       fill = "gray70", 
                       alpha = 0.7) +
    ggplot2::annotate("text",
                      label = "Diagnóstico Oportuno",
                      x = 30, 
                      y = 30,
                      #text = "risk",
                      vjust = 0,
                      col = "black",
                      size = 3.5)
ggplot2::theme(panel.border = ggplot2::element_rect(color = "black",
                                                        fill = NA,
                                                        linewidth = 1))


ggplot2::ggsave(filename = "mia_mx_model.jpg",
                dpi = 400) 
