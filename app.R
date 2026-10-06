# app.R — SAD rater planning application
# Aligns with index2.R (RSE, exact effects, feasible targets).
# Run: shiny::runApp("app.R") from the folder containing clr.csv.
suppressPackageStartupMessages({
  library(shiny); library(bslib); library(bsicons); library(dplyr)
  library(echarts4r); library(scales); library(DT)
})

# ---------- Statistical and input helpers ----------
validate_rater_data <- function(df) {
  required <- c("rater", "leaf", "actual", "unaided")
  if(!is.data.frame(df) || !nrow(df)) stop("The CSV must contain at least one data row.")
  if(!all(required %in% names(df))) stop("CSV must contain rater, leaf, actual and unaided; aided is optional.")
  if("study" %in% names(df) && (anyNA(df$study) || dplyr::n_distinct(df$study) != 1))
    stop("Upload one study at a time; raters from different studies must not be combined.")
  for(nm in c("rater", "leaf")) {
    df[[nm]] <- trimws(as.character(df[[nm]]))
    if(anyNA(df[[nm]]) || any(!nzchar(df[[nm]]))) stop("Rater and specimen IDs cannot be missing.")
  }
  if(!"aided" %in% names(df)) df$aided <- NA_real_
  for(nm in c("actual", "unaided", "aided")) {
    raw <- trimws(as.character(df[[nm]]))
    missing <- is.na(raw) | raw %in% c("", "NA", "NaN")
    value <- suppressWarnings(as.numeric(raw))
    if(any(!missing & !is.finite(value))) stop(paste("Non-numeric values in", nm, "— use decimal points in the comma-separated CSV."))
    value[missing] <- NA_real_
    if(any(value < 0 | value > 100, na.rm = TRUE)) stop(paste(nm, "must contain percentages from 0 to 100."))
    df[[nm]] <- value
  }
  if(anyNA(df$actual)) stop("Reference severity cannot be missing.")
  if(anyDuplicated(df[c("rater", "leaf")])) stop("Duplicate rater–specimen rows detected; use one assessment per condition.")
  refs <- df %>% group_by(leaf) %>% summarise(span = max(actual)-min(actual), .groups = "drop")
  if(any(refs$span > 1e-8)) stop("Reference severity must be consistent for each specimen.")
  df
}

ccc_lin <- function(x, y) {
  ok <- is.finite(x) & is.finite(y); x <- x[ok]; y <- y[ok]
  if(length(x) < 3L) return(NA_real_)
  denom <- var(x) + var(y) + (mean(x)-mean(y))^2
  if(!is.finite(denom) || denom <= 0) return(NA_real_)
  2*cov(x,y)/denom
}
per_rater_ccc <- function(df, cond = c("unaided", "aided")) {
  cond <- match.arg(cond)
  df %>% group_by(rater) %>% summarise(ccc = ccc_lin(.data[[cond]], actual), .groups = "drop") %>% filter(is.finite(ccc))
}
kmin_rse <- function(mu, sd, rse_star = .10, k_floor = 2) {
  if(!is.finite(mu) || !is.finite(sd) || mu <= 0 || sd < 0) return(NA_real_)
  max(k_floor, ceiling((sd/(mu*rse_star))^2))
}
paired_diff_z_tab <- function(df) {
  inner_join(per_rater_ccc(df,"unaided") %>% rename(ccc_u=ccc),
             per_rater_ccc(df,"aided") %>% rename(ccc_a=ccc), by="rater") %>%
    mutate(boundary = abs(ccc_u)>=1 | abs(ccc_a)>=1,
           z_u = ifelse(boundary, NA_real_, atanh(ccc_u)),
           z_a = ifelse(boundary, NA_real_, atanh(ccc_a)), d = z_a-z_u)
}
exact_ccc_effect <- function(baseline_ccc, delta_ccc) {
  if(!is.finite(baseline_ccc) || abs(baseline_ccc)>=1 || !is.finite(delta_ccc) || delta_ccc<=0)
    return(list(target_ccc=NA_real_, delta_z=NA_real_, feasible=FALSE, effect_status="invalid_baseline_or_effect"))
  target <- baseline_ccc + delta_ccc
  feasible <- target > -1 && target < 1
  list(target_ccc=target, delta_z=if(feasible) atanh(target)-atanh(baseline_ccc) else NA_real_,
       feasible=feasible, effect_status=if(feasible) "feasible" else "infeasible_target_at_or_above_1")
}
k_required_power <- function(df, alpha=.05, power=.80, delta_ccc=.10,
                             sided=c("two.sided","one.sided")) {
  sided <- match.arg(sided)
  pairs <- paired_diff_z_tab(df)
  tab <- filter(pairs,!boundary)
  n <- nrow(tab); baseline <- if(n) mean(tab$ccc_u) else NA_real_
  effect <- exact_ccc_effect(baseline,delta_ccc)
  sd_d <- if(n>=2) sd(tab$d) else NA_real_
  status <- effect$effect_status
  if(effect$feasible && n<4) status <- "insufficient_pairs"
  if(effect$feasible && n>=4 && (!is.finite(sd_d) || sd_d<=0)) status <- "invalid_difference_sd"
  k <- NA_real_
  if(status=="feasible") {
    fit <- tryCatch(power.t.test(delta=effect$delta_z, sd=sd_d, sig.level=alpha,
                  power=power,type="one.sample",alternative=sided,strict=TRUE),error=function(e) NULL)
    if(is.null(fit)) status <- "power_calculation_failed" else k <- ceiling(fit$n)
  }
  c(list(k_power=k,sd_diff_z=sd_d,n_pairs=n,boundary_pairs_excluded=sum(pairs$boundary),
         baseline_ccc=baseline,available_improvement=1-baseline,power_status=status),effect)
}

new_adaptive_controller <- function(rse_star=.10,k_floor=3,alpha=.05,power=.80,
                                    delta_ccc=.10,stability_m=2,sided="two.sided",
                                    power_from_full=FALSE,full_data=NULL) {
  stopifnot(is.finite(rse_star),rse_star>0,k_floor>=2,k_floor==floor(k_floor),
            alpha>0,alpha<1,power>.5,power<1,delta_ccc>0,
            stability_m>=1,stability_m==floor(stability_m),sided %in% c("two.sided","one.sided"))
  if(isTRUE(power_from_full)) full_data <- validate_rater_data(full_data)
  env <- new.env(); env$dat <- tibble(); env$history <- tibble(); env$hold_ok <- 0L
  env$settings <- list(rse_target=rse_star,k_floor=k_floor,alpha=alpha,power_target=power,
                       delta_ccc=delta_ccc,stability_m=stability_m,sided=sided,power_from_full=power_from_full)
  env$add_rater <- function(df_rater) {
    df_rater <- validate_rater_data(df_rater)
    if(n_distinct(df_rater$rater)!=1 || (nrow(env$dat)>0 && any(df_rater$rater %in% env$dat$rater)))
      stop("Add exactly one previously unused rater per step.")
    env$dat <- bind_rows(env$dat,df_rater)
    ccc_u <- per_rater_ccc(env$dat,"unaided"); k_cur <- nrow(ccc_u)
    mu <- if(k_cur) mean(ccc_u$ccc) else NA_real_; s <- if(k_cur>=2) sd(ccc_u$ccc) else NA_real_
    rse <- if(is.finite(mu) && mu>0 && is.finite(s)) s/(mu*sqrt(k_cur)) else NA_real_
    k_rse <- kmin_rse(mu,s,rse_star,k_floor)
    observed_pairs <- sum(!paired_diff_z_tab(env$dat)$boundary)
    pow <- k_required_power(if(isTRUE(power_from_full)) full_data else env$dat,
                            alpha,power,delta_ccc,sided)
    combined <- if(is.finite(k_rse) && is.finite(pow$k_power)) max(k_rse,pow$k_power) else NA_real_
    # Count usable collected pairs for the power requirement, even when the
    # full dataset supplies planning inputs. Invalid/missing CCCs cannot meet it.
    qualifies <- is.finite(combined) && k_cur>=k_rse && observed_pairs>=pow$k_power &&
                 n_distinct(env$dat$rater)>=k_floor
    env$hold_ok <- if(qualifies) env$hold_ok+1L else 0L
    stop_now <- env$hold_ok>=stability_m
    env$history <- bind_rows(env$history,tibble(
      step=nrow(env$history)+1L,raters_added=n_distinct(env$dat$rater),k=k_cur,
      mu=mu,sd=s,rse=rse,k_req_rse=k_rse,k_req_power=pow$k_power,k_final_req=combined,
      observed_pairs=observed_pairs,n_pairs=pow$n_pairs,
      baseline_ccc=pow$baseline_ccc,target_ccc=pow$target_ccc,delta_z=pow$delta_z,
      sd_diff_z=pow$sd_diff_z,boundary_pairs_excluded=pow$boundary_pairs_excluded,
      power_status=pow$power_status,qualifies=qualifies,stability_counter=env$hold_ok,
      stop=stop_now,rse_target=rse_star,delta_ccc=delta_ccc,alpha=alpha,
      power_target=power,sided=sided,k_floor=k_floor,stability_m=stability_m,
      power_from_full=power_from_full))
    list(k_current=k_cur,rse=rse,k_req_rse=k_rse,k_req_power=pow$k_power,
         k_final_req=combined,power_status=pow$power_status,stop=stop_now,
         stability_counter=env$hold_ok,history=env$history)
  }
  env$get_history <- function() env$history
  env$get_data <- function() env$dat
  env
}
# Generate a repeatable example order without changing other sessions' RNG.
ordered_raters <- function(ids,seed=123) {
  had <- exists(".Random.seed",envir=.GlobalEnv,inherits=FALSE)
  if(had) old <- get(".Random.seed",envir=.GlobalEnv)
  on.exit(if(had) assign(".Random.seed",old,envir=.GlobalEnv) else
    if(exists(".Random.seed",envir=.GlobalEnv,inherits=FALSE)) rm(".Random.seed",envir=.GlobalEnv))
  set.seed(seed); ids <- as.character(ids); ids[sample.int(length(ids))]
}

# ---------- UI ----------
ui <- page_navbar(
  title = "Minimum Raters SAD",
  theme = bs_theme(
    version = 5,
    bootswatch = "darkly", # Base dark theme
    bg = "#0A192F",        # Deep Navy Ocean
    fg = "#E6F1FF",        # Light blueish text
    primary = "#64FFDA",   # Cyan accent
    secondary = "#112240", # Soft navy
    base_font = "Arial, sans-serif"
  ) %>% bs_add_rules(list(
    "html { font-size: 0.85rem; }",
    ".kpi-container { display: flex; flex-wrap: wrap; gap: 10px; margin-bottom: 15px; }",
    ".info-icon { color: #64FFDA; cursor: pointer; margin-left: 5px; vertical-align: middle; }",
    ".kpi-box { flex: 1; padding: 10px; border-radius: 8px; color: #E6F1FF; display: flex; align-items: center; gap: 10px; min-height: 60px; box-shadow: 0 4px 6px rgba(0,0,0,0.3); border: 1px solid rgba(255,255,255,0.1); }",
    ".kpi-icon { font-size: 1.5rem; color: #64FFDA; }",
    ".kpi-content { display: flex; flex-direction: column; }",
    ".kpi-title { font-size: 0.65rem; text-transform: uppercase; font-weight: bold; color: #8892B0; letter-spacing: 0.5px; margin-bottom: 2px; }",
    ".kpi-value { font-size: 1.1rem; font-weight: 900; line-height: 1; }",
    ".bg-ocean-1 { background-color: #112240; }",
    ".bg-ocean-2 { background-color: #1d2d44; }",
    ".bg-ocean-3 { background-color: #233554; }",
    ".bg-ocean-stop { background-color: #3b4c68; }",
    ".bg-ocean-success { background-color: #053a2f !important; border-color: #64FFDA !important; }",
    ".card { background-color: #112240 !important; border: 1px solid #233554 !important; }",
    ".card-header { background-color: #1d2d44 !important; border-bottom: 1px solid #233554 !important; color: #64FFDA !important; font-weight: bold; }",
    ".sidebar { background-color: #0d1e36 !important; border-right: 1px solid #233554 !important; }",
    ".nav-tabs .nav-link.active { background-color: #112240 !important; border-color: #64FFDA !important; color: #64FFDA !important; }",
    "pre, code { background-color: #0d1e36 !important; color: #64FFDA !important; }",
    ".navbar { background-color: #053a2f !important; border-bottom: 2px solid #64FFDA !important; }",
    ".navbar-brand, .navbar-nav .nav-link { color: #ffffff !important; font-weight: 500 !important; }",
    ".navbar-nav .nav-link.active { color: #64FFDA !important; border-bottom: 2px solid #64FFDA !important; }"
  )),
  
  sidebar = sidebar(
    title = "Controls",
    span(
      "Upload CSV ",
      popover(
        bs_icon("info-circle", class = "info-icon"),
        title = "CSV Requirements",
        p("Your file must include these columns:"),
        tags$ul(
          tags$li(tags$b("rater: "), "ID or name of the evaluator"),
          tags$li(tags$b("leaf: "), "ID of the sample/leaf"),
          tags$li(tags$b("actual: "), "Reference severity value"),
          tags$li(tags$b("unaided: "), "Estimates without assistance"),
          tags$li(tags$b("aided: "), "(Optional) Estimates with assistance")
        ),
        p(style = "font-size: 0.7rem; color: #8892B0;", "Files should be in .csv format with comma separators.")
      )
    ),
    fileInput("file", NULL, accept = ".csv", placeholder = "Select file..."),
    actionButton("load_example", "Use Example (clr.csv)", icon = icon("database"), class = "btn-outline-info btn-sm w-100 mb-2"),
    tags$hr(),
    p(style="font-size: 0.75rem; color: #8892B0; margin-bottom: 5px;", "Execution:"),
    actionButton("add", "Add Random Rater", icon = icon("plus-circle"), class = "btn-success btn-sm w-100 mb-2"),
    actionButton("reset", "Reset All", icon = icon("undo"), class = "btn-outline-danger btn-sm w-100 mb-2")
  ),
  
  nav_panel(
    title = "Run",
    icon = bs_icon("play-circle"),
    
    # Custom KPIs Row
    htmlOutput("kpi_row"),
    uiOutput("planning_note"),
    p(class = "text-muted", "These are conditional planning estimates. Sequential stopping does not guarantee prospective power or measurement reproducibility."),
    
    # Main content in Tabs
    navset_card_tab(
      full_screen = TRUE,
      title = "Analytical Insights",
      nav_panel(
        title = "Visualizations",
        icon = bs_icon("graph-up-arrow"),
        layout_column_wrap(
          width = 1/2,
          card(
            card_header("Precision of Mean LCCC (RSE)"),
            echarts4rOutput("rse_plot", height = "380px")
          ),
          card(
            card_header("Power-based Sampling Requirement"),
            echarts4rOutput("power_plot", height = "380px")
          )
        )
      ),
      nav_panel(
        title = "Sequential History",
        icon = bs_icon("table"),
        DTOutput("hist_tbl")
      ),
      nav_panel(
        title = "Status Details",
        icon = bs_icon("info-circle"),
        verbatimTextOutput("status")
      ),
      nav_panel(
        title = "Raw Data",
        icon = bs_icon("database"),
        DTOutput("raw_data_tbl")
      )
    )
  ),
  
  nav_panel(
    title = "Settings",
    icon = bs_icon("sliders"),
    layout_column_wrap(
      width = 1/2,
      card(
        card_header("Target Parameters"),
        sliderInput("rse_target", "RSE target (%)", min = 2, max = 30, value = 10, step = 1),
        sliderInput("power_target","Power target",  min = 0.5, max = 0.95, value = 0.80, step = 0.01),
        sliderInput("delta_ccc",   "Target ΔLCCC", min = 0.03, max = 0.25, value = 0.10, step = 0.01),
        sliderInput("alpha",       "Significance level", min = 0.001, max = 0.10, value = 0.05, step = 0.001),
        selectInput("sided", "Test sidedness", choices = c("two.sided","one.sided"), selected = "two.sided"),
        tags$hr(),
        checkboxInput("power_full", "Estimate power inputs from the full dataset", FALSE),
        p("When selected, baseline and variability come from all available raters. These are empirical reference estimates; the dataset is not error-free ground truth.")
      ),
      card(
        card_header("Sequential Control"),
        numericInput("k_floor", "Minimum k (operational floor)", 3, min = 2, max = 10),
        numericInput("stability_m", "Consecutive steps to confirm stop", 2, min = 1, max = 5),
        p("Applying settings starts a new sampling history."),
        tags$hr(),
        actionButton("init", "Apply Settings & Start", icon = icon("play"), class = "btn-primary w-100")
      )
    )
  ),
  
  nav_panel(
    title = "Download",
    icon = bs_icon("download"),
    card(
      card_header("Export Data"),
      p("Download the complete sequential history in CSV format."),
      downloadButton("download_hist", "Download history (CSV)", class = "btn-success btn-sm")
    )
  )
)

# ---------- Server ----------
server <- function(input,output,session) {
  rv <- reactiveValues(ctrl=NULL,data=NULL,history=NULL,used=character(),order=character(),stop_notified=FALSE,
                       message="Load the example or upload a CSV and apply settings to begin.")
  read_data <- function(path) validate_rater_data(read.csv(path,stringsAsFactors=FALSE,check.names=FALSE))
  initialize <- function(df,message) {
    # Validate first, so invalid settings/data do not erase an active run.
    df <- validate_rater_data(df)
    ctrl <- new_adaptive_controller(rse_star=input$rse_target/100,k_floor=input$k_floor,
      alpha=input$alpha,power=input$power_target,delta_ccc=input$delta_ccc,
      stability_m=input$stability_m,sided=input$sided,power_from_full=input$power_full,full_data=df)
    rv$data <- df; rv$ctrl <- ctrl; rv$history <- NULL; rv$used <- character()
    rv$order <- ordered_raters(unique(df$rater)); rv$stop_notified <- FALSE; rv$message <- message
  }
  observeEvent(input$init, {
    tryCatch({
      if(!is.null(input$file)) df <- read_data(input$file$datapath)
      else if(!is.null(rv$data)) df <- rv$data
      else stop("Upload a CSV or load the example first.")
      initialize(df,"Settings applied. A new history has started; add raters to begin.")
    },error=function(e) showNotification(conditionMessage(e),type="error",duration=8))
  })
  observeEvent(input$load_example, {
    tryCatch({initialize(read_data("clr.csv"),"Example loaded with current settings. Add raters to begin.")
      showNotification("Example data loaded (clr.csv).",type="message")
    },error=function(e) showNotification(conditionMessage(e),type="error",duration=8))
  })
  observeEvent(input$add, {
    if(is.null(rv$ctrl)) {showNotification("Load data and apply settings first.",type="warning");return()}
    remaining <- setdiff(rv$order,rv$used)
    if(!length(remaining)) {showNotification("All available raters have been included.",type="warning");return()}
    next_rater <- remaining[1L]
    out <- rv$ctrl$add_rater(filter(rv$data,rater==next_rater))
    rv$used <- c(rv$used,next_rater); rv$history <- out$history
    rv$message <- paste("Added rater:",next_rater)
    if(isTRUE(out$stop) && !rv$stop_notified) {
      rv$stop_notified <- TRUE
      showModal(modalDialog(title="Planning criteria met",
        p("The current RSE and estimated power requirements have been met for the configured number of consecutive additions."),
        p(sprintf("Current valid raters: %d; combined estimated requirement: %d.",out$k_current,out$k_final_req)),
        p("This is a sequential planning result, not a guarantee of prospective power. You can continue sampling or download the history."),
        easyClose=TRUE,footer=modalButton("Continue")))
    }
  })
  observeEvent(input$reset, {
    rv$ctrl <- NULL; rv$data <- NULL; rv$history <- NULL; rv$used <- character(); rv$order <- character()
    rv$stop_notified <- FALSE; rv$message <- "Application reset. Load data to begin."
  })
  output$status <- renderPrint({
    cat(rv$message,"\n")
    if(!is.null(rv$ctrl)) {cat("\nActive settings (apply settings to start a new run):\n"); print(rv$ctrl$settings)}
    if(!is.null(rv$history)) {cat("\nLatest step:\n");print(tail(rv$history,1))}
  })
  output$planning_note <- renderUI({
    if(is.null(rv$history)) return(NULL)
    row <- tail(rv$history,1)
    msg <- switch(row$power_status,
      infeasible_target_at_or_above_1=sprintf("Infeasible improvement: baseline LCCC %.4f + Delta %.2f = %.4f. Choose a smaller improvement and apply settings. No combined requirement is reported.",row$baseline_ccc,row$delta_ccc,row$target_ccc),
      insufficient_pairs="At least four valid rater pairs are needed to estimate power inputs.",
      invalid_baseline_or_effect="Power is unavailable: no valid interior paired baseline has been estimated.",
      invalid_difference_sd="Power is unavailable because paired transformed differences have no estimable positive variability.",
      power_calculation_failed="The power calculation could not be estimated with these inputs.",
      feasible=if(row$target_ccc>.99) "The target is near perfect agreement; mathematical feasibility does not imply practical plausibility." else NULL)
    if(!is.null(msg)) div(class="alert alert-warning",msg)
    else if(length(rv$used)==length(rv$order) && !row$stop)
      div(class="alert alert-warning","All available raters are included; the configured stopping criteria have not been met.")
  })
  output$kpi_row <- renderUI({
    row <- if(is.null(rv$history)) NULL else tail(rv$history,1)
    fmt <- function(x) if(length(x)==1 && is.finite(x)) as.character(x) else "—"
    status <- if(is.null(row)) "Ready" else if(row$power_status=="infeasible_target_at_or_above_1") "Infeasible" else
      if(row$stop) "Criteria met" else if(length(rv$used)==length(rv$order)) "Exhausted" else "Sampling"
    box <- function(label,value,icon_name,cls) div(class=paste("kpi-box",cls),
      bs_icon(icon_name,class="kpi-icon"),div(class="kpi-content",div(class="kpi-title",label),div(class="kpi-value",value)))
    div(class="kpi-container",
      box("Current valid k",if(is.null(row)) "0" else fmt(row$k),"people-fill","bg-ocean-1"),
      box("Req k (RSE)",if(is.null(row)) "—" else fmt(row$k_req_rse),"bullseye","bg-ocean-2"),
      box("Req k (Power)",if(is.null(row)) "—" else fmt(row$k_req_power),"graph-up","bg-ocean-3"),
      box("Status",status,"info-circle",if(!is.null(row) && row$stop) "bg-ocean-success" else "bg-ocean-stop"))
  })
  output$rse_plot <- renderEcharts4r({
    req(rv$history)
    rv$history %>% e_charts(step) %>%
      e_line(rse,name="Relative standard error",symbol="circle",symbolSize=8) %>%
      e_mark_line(data=list(yAxis=rv$ctrl$settings$rse_target),title="RSE target",
        label=list(formatter="RSE target",position="insideEndTop",color="#FF4B2B"),
        lineStyle=list(color="#FF4B2B",type="dashed",width=2)) %>%
      e_tooltip(trigger="axis") %>% e_x_axis(name="Raters added",nameLocation="center",nameGap=35) %>%
      e_y_axis(name="RSE (%)",formatter=e_axis_formatter("percent"),splitLine=list(lineStyle=list(color="#233554"))) %>%
      e_theme_custom('{"color":["#64FFDA"],"backgroundColor":"#0d1e3c","textStyle":{"color":"#E6F1FF"}}') %>%
      e_legend(show=FALSE) %>% e_grid(left="15%",right="10%",bottom="20%",top="12%")
  })
  output$power_plot <- renderEcharts4r({
    req(rv$history)
    rv$history %>% e_charts(step) %>%
      e_line(k_req_power,name="Estimated requirement",symbol="rect",symbolSize=8) %>%
      e_line(observed_pairs,name="Valid pairs collected",lineStyle=list(type="dotted")) %>%
      e_tooltip(trigger="axis") %>% e_x_axis(name="Raters added",nameLocation="center",nameGap=35) %>%
      e_y_axis(name="Rater pairs",splitLine=list(lineStyle=list(color="#233554"))) %>%
      e_theme_custom('{"color":["#64FFDA","#F28E2B"],"backgroundColor":"#0d1e3c","textStyle":{"color":"#E6F1FF"}}') %>%
      e_legend(bottom=0,textStyle=list(color="#8892B0")) %>%
      e_grid(left="15%",right="8%",bottom="25%",top="12%")
  })
  output$hist_tbl <- renderDT({
    req(rv$history)
    datatable(rv$history,options=list(pageLength=10,scrollX=TRUE,dom='ftp'),class='display nowrap compact') %>%
      formatRound(columns=c("mu","sd","rse","baseline_ccc","target_ccc","sd_diff_z","delta_z"),digits=3)
  })
  output$raw_data_tbl <- renderDT({req(rv$data);datatable(rv$data,options=list(pageLength=10,scrollX=TRUE,dom='ftp'),class='display nowrap compact')})
  output$download_hist <- downloadHandler(
    filename=function() sprintf("adaptive_history_%s.csv",Sys.Date()),
    content=function(file){req(rv$history);write.csv(rv$history,file,row.names=FALSE)})
}

shinyApp(ui,server)
