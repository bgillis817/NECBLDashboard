# ============================================================================
#  NECBL LEAGUE DASHBOARD
#  All teams · 2023-2026 season toggle · Hitters + Pitchers
#  Google Drive data ingestion · Team → Player navigation
#  Dark theme · pitch sequencing · count & handedness heat maps
# ============================================================================

library(shiny)
library(dplyr)
library(readr)
library(ggplot2)
library(DT)
library(scales)
library(googledrive)
library(googlesheets4)

# ── Null coalesce ─────────────────────────────────────────────────────────────
`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b

# ── Google Drive folder ───────────────────────────────────────────────────────
DRIVE_FOLDER_ID <- "1haJdctNyLTx81GXlvCdBRDSWKrDAlKAw"

# Authenticate using service account JSON stored as env var
auth_drive <- function() {
  svc_json <- Sys.getenv("GOOGLE_SERVICE_ACCOUNT_JSON")
  if (nchar(svc_json) > 10) {
    tmp <- tempfile(fileext = ".json")
    writeLines(svc_json, tmp)
    googledrive::drive_auth(path = tmp)
    googlesheets4::gs4_auth(path = tmp)
  } else {
    googledrive::drive_deauth()
  }
}

# ── Load all CSVs from Drive folder ──────────────────────────────────────────
load_drive_data <- function() {
  tryCatch({
    auth_drive()
    files <- googledrive::drive_ls(googledrive::as_id(DRIVE_FOLDER_ID),
                                   type = "csv")
    if (nrow(files) == 0) return(NULL)

    all_dfs <- lapply(seq_len(nrow(files)), function(i) {
      tryCatch({
        tmp <- tempfile(fileext = ".csv")
        googledrive::drive_download(googledrive::as_id(files$id[i]),
                                    path = tmp, overwrite = TRUE)
        df <- readr::read_csv(tmp, show_col_types = FALSE)
        unlink(tmp)
        df
      }, error = function(e) { message("Skip file: ", files$name[i]); NULL })
    })

    dplyr::bind_rows(Filter(Negate(is.null), all_dfs))
  }, error = function(e) {
    message("Drive load error: ", e$message)
    NULL
  })
}

# ── Lookup tables ─────────────────────────────────────────────────────────────
woba_weights <- c(
  Walk=0.690, HitByPitch=0.690, Single=0.888,
  Double=1.271, Triple=1.616, HomeRun=2.101,
  Out=0, FieldersChoice=0, Sacrifice=0, Error=0
)

team_names <- c(
  UPP_VAL="Upper Valley Nighthawks", VAL_BLU="Valley Blue Sox",
  KEE_SWA="Keene Swampbats",         BRI_B="Bristol Blues",
  MAR_VIN="Martha's Vineyard Sharks",OCE_STA6="Ocean State Waves",
  NOR_ADA="North Adams Steeplecats", MYS_SCH="Mystic Schooners",
  NEW_GUL="Newport Gulls",           SAN_MAI="Sanford Mainers",
  VER_MOU="Vermont Mountaineers",    DAN_WES="Danbury Westerners",
  NSN="North Shore Navigators"
)

pitch_pal <- c(
  Fastball="#ff4655","4-Seam Fastball"="#ff4655","Four-Seam"="#ff4655",
  Sinker="#ff8c42","Two-Seam"="#ff8c42",
  Cutter="#ffd700", Slider="#00d4ff",
  Curveball="#a78bfa", Sweeper="#34d399",
  Changeup="#6ee7b7", Splitter="#f472b6",
  Other="#94a3b8"
)

team_pal <- c(
  "#ff4655","#4ECDC4","#45B7D1","#96CEB4","#FECA57",
  "#48D1CC","#FA8072","#DDA0DD","#98D8C8","#F7DC6F",
  "#85C1E5","#F8C471","#82E0AA","#D7BDE2","#A9DFBF"
)

heat_fills <- c("#141720","#1a3a5c","#1565c0","#ff4655","#ffeb3b")

# ── Count helpers ─────────────────────────────────────────────────────────────
count_situation <- function(b, s) {
  dplyr::case_when(
    b==0 & s==0 ~ "First Pitch (0-0)",
    b>=2 & s<=1 ~ "Hitter's Count",
    s==2 & b<=1 ~ "Pitcher's Count",
    b==s        ~ "Even Count",
    b>s         ~ "Hitter's Ahead",
    TRUE        ~ "Pitcher's Ahead"
  )
}

count_choices <- function(d_col) {
  indiv   <- sort(unique(d_col))
  grouped <- c("Hitter's Count","Pitcher's Count","Even Count",
                "First Pitch (0-0)","Hitter's Ahead","Pitcher's Ahead")
  c("All Counts",
    setNames("──grouped──","── Grouped ──"),
    setNames(grouped, grouped),
    setNames("──indiv──","── Individual ──"),
    setNames(indiv, indiv))
}

filter_by_count <- function(d, cnt_val, sit_col, indiv_col) {
  if (is.null(cnt_val) || cnt_val %in% c("All Counts","──grouped──","──indiv──"))
    return(d)
  grouped_lvls <- c("Hitter's Count","Pitcher's Count","Even Count",
                    "First Pitch (0-0)","Hitter's Ahead","Pitcher's Ahead")
  if (cnt_val %in% grouped_lvls) d %>% filter(.data[[sit_col]]   == cnt_val)
  else                            d %>% filter(.data[[indiv_col]] == cnt_val)
}

# ── Process hitters ───────────────────────────────────────────────────────────
process_hitters <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(NULL)
  tryCatch({
    df %>%
      mutate(
        Date = as.Date(Date, format="%m/%d/%Y"),
        Season = as.integer(format(Date, "%Y")),
        BatterTeamFull = ifelse(BatterTeam %in% names(team_names),
                                team_names[BatterTeam], BatterTeam),
        PitcherTeamFull = ifelse(PitcherTeam %in% names(team_names),
                                 team_names[PitcherTeam], PitcherTeam),
        wOBA_contribution = dplyr::case_when(
          PlayResult=="Walk"           ~ woba_weights["Walk"],
          PlayResult=="HitByPitch"     ~ woba_weights["HitByPitch"],
          PlayResult=="Single"         ~ woba_weights["Single"],
          PlayResult=="Double"         ~ woba_weights["Double"],
          PlayResult=="Triple"         ~ woba_weights["Triple"],
          PlayResult=="HomeRun"        ~ woba_weights["HomeRun"],
          PlayResult=="FieldersChoice" ~ woba_weights["FieldersChoice"],
          PlayResult=="Sacrifice"      ~ woba_weights["Sacrifice"],
          PlayResult=="Error"          ~ woba_weights["Error"],
          TRUE                         ~ woba_weights["Out"]
        ),
        Balls   = as.double(substr(as.character(Balls),   1, 10)),
        Strikes = as.double(substr(as.character(Strikes), 1, 10)),
        CountSit   = count_situation(Balls, Strikes),
        CountIndiv = paste0(Balls, "-", Strikes)
      ) %>%
      arrange(Batter, Date, PitchNo) %>%
      group_by(Batter, Season) %>%
      mutate(PA_count        = row_number(),
             cumulative_wOBA = cumsum(wOBA_contribution) / PA_count) %>%
      ungroup() %>%
      arrange(Batter, PitcherTeamFull, Date, PitchNo) %>%
      group_by(Batter, Season, PitcherTeamFull) %>%
      mutate(PA_count_opp        = row_number(),
             cumulative_wOBA_opp = cumsum(wOBA_contribution) / PA_count_opp) %>%
      ungroup()
  }, error = function(e) { message("Hitter process error: ", e$message); NULL })
}

# ── Process pitchers ──────────────────────────────────────────────────────────
process_pitchers <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(NULL)
  tryCatch({
    names(df) <- gsub("\\s+","_",names(df))
    names(df) <- gsub("[()]","",names(df))

    df <- df %>%
      mutate(
        Date             = as.Date(Date, format="%m/%d/%Y"),
        Season           = as.integer(format(Date, "%Y")),
        PitcherTeamFull  = ifelse(PitcherTeam %in% names(team_names),
                                  team_names[PitcherTeam], PitcherTeam),
        RelHeight        = as.double(RelHeight),
        RelSide          = as.double(RelSide),
        RelSpeed         = as.double(RelSpeed),
        SpinRate         = as.double(SpinRate),
        InducedVertBreak = as.double(InducedVertBreak),
        HorzBreak        = as.double(HorzBreak),
        PlateLocHeight   = as.double(PlateLocHeight),
        PlateLocSide     = as.double(PlateLocSide),
        Balls   = as.double(substr(as.character(Balls),   1, 10)),
        Strikes = as.double(substr(as.character(Strikes), 1, 10)),
        CountSit   = count_situation(Balls, Strikes),
        CountIndiv = paste0(Balls, "-", Strikes),
        SwingCheck     = PitchCall %in% c("FoulBall","StrikeSwinging","InPlay"),
        WhiffCheck     = PitchCall == "StrikeSwinging",
        CSWCheck       = PitchCall %in% c("StrikeSwinging","StrikeCalled"),
        ZoneCheck      = dplyr::between(PlateLocHeight,1.59,3.41) &
                         dplyr::between(PlateLocSide,-1,1),
        StrikeoutCheck = KorBB == "Strikeout",
        WalkCheck      = KorBB == "Walk",
        HBPCheck       = PitchCall == "HitByPitch",
        HCheck         = PlayResult %in% c("Single","Double","Triple","HomeRun"),
        SacCheck       = PlayResult == "Sacrifice",
        BIPCheck       = PlayResult != "Undefined",
        PACheck        = StrikeoutCheck + WalkCheck + HBPCheck + BIPCheck,
        ABCheck        = StrikeoutCheck + BIPCheck - SacCheck
      ) %>%
      arrange(Pitcher, Date, PitchNo) %>%
      group_by(Pitcher, Season) %>%
      mutate(OverallPitchCount = row_number()) %>%
      ungroup()

    seqs <- df %>%
      arrange(Pitcher, Date, PitchNo) %>%
      group_by(Pitcher, Date) %>%
      mutate(
        prev_type       = lag(TaggedPitchType),
        prev_loc_side   = lag(PlateLocSide),
        prev_loc_height = lag(PlateLocHeight)
      ) %>%
      ungroup() %>%
      filter(!is.na(prev_type))

    pairs <- seqs %>%
      group_by(Pitcher, PitcherTeamFull, prev_type, TaggedPitchType) %>%
      summarise(
        n_pairs    = n(),
        csw_rate   = mean(CSWCheck, na.rm=TRUE),
        whiff_rate = {sw=sum(SwingCheck,na.rm=TRUE);
                      if(sw>0) sum(WhiffCheck,na.rm=TRUE)/sw else NA_real_},
        zone_rate  = mean(ZoneCheck, na.rm=TRUE),
        chase_rate = {oz=sum(!ZoneCheck,na.rm=TRUE);
                      if(oz>0) sum(SwingCheck[!ZoneCheck],na.rm=TRUE)/oz else NA_real_},
        .groups="drop"
      )

    list(data=df, sequences=seqs, pairs=pairs)
  }, error = function(e) { message("Pitcher process error: ", e$message); NULL })
}

# ── Data stored as global reactive-ready objects (refreshed in server) ────────
# Initial load at startup
.raw_cache <- new.env(parent = emptyenv())
.raw_cache$raw_all            <- NULL
.raw_cache$hitters_data()  <- NULL
.raw_cache$pitchers_processed <- NULL
.raw_cache$p_data_r()         <- NULL
.raw_cache$p_seqs_r()         <- NULL
.raw_cache$p_pairs_r()        <- NULL
.raw_cache$last_loaded        <- NULL

load_and_cache <- function() {
  message("Loading data from Google Drive...")
  raw <- load_drive_data()
  .raw_cache$raw_all            <- raw
  .raw_cache$hitters_data()  <- process_hitters(raw)
  pit                           <- process_pitchers(raw)
  .raw_cache$pitchers_processed <- pit
  .raw_cache$p_data_r()         <- if (!is.null(pit)) pit$data      else NULL
  .raw_cache$p_seqs_r()         <- if (!is.null(pit)) pit$sequences else NULL
  .raw_cache$p_pairs_r()        <- if (!is.null(pit)) pit$pairs     else NULL
  .raw_cache$last_loaded        <- Sys.time()
  message("Data load complete: ", .raw_cache$last_loaded)
}

load_and_cache()

# ── Helpers ───────────────────────────────────────────────────────────────────
stat_card <- function(value, label) {
  tags$div(class="stat-card",
    tags$h2(value),
    tags$p(label)
  )
}

bb_card <- function(n, tot, label) {
  v <- if (tot > 0) n/tot else 0
  tags$div(class="stat-card",
    tags$h2(paste0(round(v*100,1),"%")),
    tags$p(style="color:#6b7280;font-size:11px;margin:2px 0;", paste0(n,"/",tot)),
    tags$p(label)
  )
}

slg_stats <- function(d) {
  d %>% summarise(
    PA   = n(),
    H    = sum(PlayResult %in% c("Single","Double","Triple","HomeRun")),
    `1B` = sum(PlayResult=="Single"), `2B`=sum(PlayResult=="Double"),
    `3B` = sum(PlayResult=="Triple"), HR=sum(PlayResult=="HomeRun"),
    BB   = sum(PlayResult=="Walk"),   HBP=sum(PlayResult=="HitByPitch"),
    AB   = PA-BB-HBP,
    TB   = `1B`+2*`2B`+3*`3B`+4*HR,
    BA   = ifelse(AB>0, round(H/AB,3),  NA),
    OBP  = ifelse(PA>0, round((H+BB+HBP)/PA,3), NA),
    SLG  = ifelse(AB>0, round(TB/AB,3), NA),
    wOBA = round(sum(wOBA_contribution,na.rm=TRUE)/PA,3)
  ) %>% filter(AB>0)
}

bb_stats_fn <- function(d, group_type="h") {
  d %>% summarise(
    BBE    = n(),
    GB     = sum(TaggedHitType=="GroundBall", na.rm=TRUE),
    FB     = sum(TaggedHitType=="FlyBall",    na.rm=TRUE),
    LD     = sum(TaggedHitType=="LineDrive",  na.rm=TRUE),
    PU     = sum(TaggedHitType=="Popup",      na.rm=TRUE),
    `GB%`  = ifelse(BBE>0, paste0(round(GB/BBE*100,1),"%"), "—"),
    `FB%`  = ifelse(BBE>0, paste0(round(FB/BBE*100,1),"%"), "—"),
    `LD%`  = ifelse(BBE>0, paste0(round(LD/BBE*100,1),"%"), "—"),
    `PU%`  = ifelse(BBE>0, paste0(round(PU/BBE*100,1),"%"), "—"),
    `GB/FB`= ifelse(FB>0,  as.character(round(GB/FB,2)), "—"),
    .groups="drop"
  ) %>% filter(BBE>0)
}

loc_heatmap <- function(d, title_str, subtitle_str="", flip_side=FALSE) {
  if (is.null(d) || nrow(d) < 5) {
    return(ggplot() +
      annotate("text",x=0,y=2.5,label="Insufficient data",color="#8892b0",size=5) +
      theme_navs() +
      theme(axis.text=element_blank(),axis.title=element_blank(),
            panel.grid=element_blank()))
  }
  x_col <- if (flip_side) -d$PlateLocSide else d$PlateLocSide
  ggplot(data.frame(x=x_col, y=d$PlateLocHeight), aes(x=x, y=y)) +
    stat_density_2d(aes(fill=after_stat(density)), geom="raster", contour=FALSE) +
    scale_fill_gradientn(colours=heat_fills, guide="none") +
    annotate("rect", xmin=-1,xmax=1,ymin=1.6,ymax=3.4,
             fill=NA, color="#ffffff", linewidth=.7) +
    geom_vline(xintercept=0, linewidth=.3, color="#2a2d3a", linetype="dashed") +
    geom_hline(yintercept=2.5, linewidth=.3, color="#2a2d3a", linetype="dashed") +
    ylim(1,4) + xlim(-2,2) +
    labs(title=title_str, subtitle=subtitle_str,
         x=if(flip_side)"Horizontal (Hitter's View)" else "Horizontal (Pitcher's View)",
         y="Vertical") +
    theme_navs()
}

dt_opts <- list(
  dom="ft", pageLength=25, scrollX=TRUE,
  initComplete=JS(
    "function(s,j){",
    "$(this.api().table().header()).css({'background':'#1e2235','color':'#b0b8d4'});",
    "}"
  )
)

# ── Dark CSS ──────────────────────────────────────────────────────────────────
dark_css <- "
body{background:#0f1117!important;color:#e8eaf0!important;
     font-family:'Segoe UI',-apple-system,BlinkMacSystemFont,sans-serif;}
.navbar{background:#141720!important;border-bottom:1px solid #2a2d3a!important;}
.navbar-brand{color:#fff!important;font-weight:700;font-size:18px;}
.navbar-nav>li>a{color:#b0b8d4!important;font-weight:500;padding:14px 18px!important;}
.navbar-nav>li.active>a,.navbar-nav>li>a:hover{color:#ff4655!important;
  border-bottom:2px solid #ff4655!important;}
.well,.panel,.sidebar-panel,.main-panel{background:#141720!important;border:none!important;}
.panel-default{background:#1a1e2e!important;border:1px solid #2a2d3a!important;border-radius:8px;}
.panel-default>.panel-heading{background:#1e2235!important;color:#fff!important;
  border-bottom:1px solid #2a2d3a;}
.form-control,select.form-control{background:#1e2235!important;color:#e8eaf0!important;
  border:1px solid #2e3350!important;border-radius:6px!important;}
.form-control:focus{border-color:#ff4655!important;
  box-shadow:0 0 0 2px rgba(255,70,85,.25)!important;}
label{color:#b0b8d4!important;font-size:12px;font-weight:600;
  letter-spacing:.5px;text-transform:uppercase;}
.btn-default{background:#1e2235!important;color:#e8eaf0!important;
  border:1px solid #2e3350!important;border-radius:6px!important;}
.btn-default:hover{background:#ff4655!important;border-color:#ff4655!important;
  color:#fff!important;}
.nav-tabs{border-bottom:1px solid #2a2d3a!important;}
.nav-tabs>li>a{background:#1a1e2e!important;color:#8892b0!important;
  border-color:#2a2d3a!important;border-radius:6px 6px 0 0!important;}
.nav-tabs>li.active>a,.nav-tabs>li>a:hover{background:#ff4655!important;
  color:#fff!important;border-color:#ff4655!important;}
.tab-content{background:#141720!important;border:1px solid #2a2d3a!important;
  border-top:none;padding:20px;border-radius:0 0 8px 8px;}
.dataTables_wrapper{color:#e8eaf0!important;}
table.dataTable thead th{background:#1e2235!important;color:#b0b8d4!important;
  border-bottom:1px solid #2a2d3a!important;}
table.dataTable tbody tr{background:#141720!important;color:#e8eaf0!important;}
table.dataTable tbody tr:nth-child(even){background:#1a1e2e!important;}
table.dataTable tbody tr:hover{background:#1e2235!important;}
.dataTables_filter input,.dataTables_length select{background:#1e2235!important;
  color:#e8eaf0!important;border:1px solid #2e3350!important;}
.stat-card{background:#1a1e2e;border:1px solid #2a2d3a;border-radius:10px;
  padding:18px 20px;margin-bottom:14px;}
.stat-card h2{color:#ff4655;margin:0 0 4px;font-size:28px;font-weight:700;}
.stat-card p{color:#8892b0;margin:0;font-size:12px;text-transform:uppercase;
  letter-spacing:.5px;}
.section-header{color:#fff;font-size:18px;font-weight:700;
  border-left:3px solid #ff4655;padding-left:12px;margin:20px 0 4px;}
.section-sub{color:#8892b0;font-size:12px;margin:0 0 16px 15px;}
.filter-bar{background:#1a1e2e;border:1px solid #2a2d3a;border-radius:8px;
  padding:14px 18px;margin-bottom:18px;}
.season-toggle{display:flex;gap:8px;margin-bottom:6px;}
.season-btn{padding:6px 16px;border-radius:20px;border:1px solid #2e3350;
  background:#1e2235;color:#8892b0;cursor:pointer;font-size:13px;font-weight:600;
  transition:all .2s;}
.season-btn.active{background:#ff4655;border-color:#ff4655;color:#fff;}
.checkbox label,.radio label{color:#b0b8d4!important;}
input[type=checkbox],input[type=radio]{accent-color:#ff4655;}
hr{border-color:#2a2d3a!important;}
h4,h5{color:#e8eaf0!important;}
.col-sm-3{background:#0f1117!important;}
::-webkit-scrollbar{width:6px;height:6px;}
::-webkit-scrollbar-track{background:#0f1117;}
::-webkit-scrollbar-thumb{background:#2a2d3a;border-radius:3px;}
"

# ── ggplot theme ──────────────────────────────────────────────────────────────
theme_navs <- function(base_size=12) {
  theme_minimal(base_size=base_size) %+replace% theme(
    plot.background  = element_rect(fill="#0f1117", color=NA),
    panel.background = element_rect(fill="#141720", color=NA),
    panel.grid.major = element_line(color="#2a2d3a", linewidth=.3),
    panel.grid.minor = element_blank(),
    axis.text        = element_text(color="#8892b0", size=9),
    axis.title       = element_text(color="#b0b8d4", size=10, face="bold"),
    plot.title       = element_text(color="#ffffff", size=14, face="bold", hjust=.5),
    plot.subtitle    = element_text(color="#8892b0", size=10, hjust=.5),
    legend.background= element_rect(fill="#1a1e2e", color=NA),
    legend.key       = element_rect(fill="#1a1e2e", color=NA),
    legend.text      = element_text(color="#b0b8d4", size=9),
    legend.title     = element_text(color="#e8eaf0", size=10, face="bold"),
    strip.background = element_rect(fill="#1e2235", color=NA),
    strip.text       = element_text(color="#e8eaf0", size=9, face="bold")
  )
}

# ============================================================================
#  UI
# ============================================================================
ui <- navbarPage(
  title = "NECBL Dashboard",
  id = "mainNav", collapsible = TRUE,
  header = tags$head(
    tags$style(HTML(dark_css)),
    tags$style(HTML("
      .refresh-bar{background:#141720;border-bottom:1px solid #2a2d3a;
        padding:6px 20px;display:flex;align-items:center;gap:16px;}
      .refresh-bar .btn{background:#1e2235;color:#e8eaf0;border:1px solid #2e3350;
        border-radius:6px;padding:4px 14px;font-size:12px;font-weight:600;}
      .refresh-bar .btn:hover{background:#ff4655;border-color:#ff4655;color:#fff;}
      .last-updated{color:#6b7280;font-size:12px;}
    ")),
    tags$script(HTML("
      Shiny.addCustomMessageHandler('setSeasonBtns', function(m) {
        var pfx = m.prefix; var s = m.season;
        document.getElementById(pfx+'_2023').classList.remove('active');
        document.getElementById(pfx+'_2024').classList.remove('active');
        document.getElementById(pfx+'_2025').classList.remove('active');
        document.getElementById(pfx+'_2026').classList.remove('active');
        document.getElementById(pfx+'_'+s).classList.add('active');
      });
    "))
  ),
  footer = tags$div(class="refresh-bar",
    actionButton("refresh_data", "Refresh Data", icon=icon("rotate"), class="btn"),
    tags$span(class="last-updated", textOutput("last_updated_txt", inline=TRUE))
  ),

  # ══════════════════════════════════════════════════════════════════════════
  # HITTERS TAB
  # ══════════════════════════════════════════════════════════════════════════
  tabPanel("Hitters",
    sidebarLayout(
      sidebarPanel(width=3,
        tags$div(class="section-header","Season"),
        tags$div(class="season-toggle",
          tags$button("2026",id="h_2026",class="season-btn active",
            onclick="Shiny.setInputValue('h_season','2026',{priority:'event'})"),
          tags$button("2025",id="h_2025",class="season-btn",
            onclick="Shiny.setInputValue('h_season','2025',{priority:'event'})"),
          tags$button("2024",id="h_2024",class="season-btn",
            onclick="Shiny.setInputValue('h_season','2024',{priority:'event'})"),
          tags$button("2023",id="h_2023",class="season-btn",
            onclick="Shiny.setInputValue('h_season','2023',{priority:'event'})")
        ),
        tags$hr(),
        uiOutput("h_teamSelect"),
        uiOutput("h_playerSelect"),
        uiOutput("h_dateRangeUI"),
        uiOutput("h_pitchTypeUI"),
        tags$hr(),
        uiOutput("h_playerInfo")
      ),
      mainPanel(width=9,
        tabsetPanel(
          tabPanel("Overview",
            br(),
            fluidRow(
              column(3,uiOutput("h_card_pa")),
              column(3,uiOutput("h_card_woba")),
              column(3,uiOutput("h_card_obp")),
              column(3,uiOutput("h_card_slg"))
            ),
            br(),
            tags$div(class="section-header","Spray Chart"),
            tags$div(class="section-sub","Colored by play result"),
            plotOutput("h_spray",height="500px")
          ),
          tabPanel("Plate Discipline",
            br(),
            tags$div(class="filter-bar",
              fluidRow(
                column(4, uiOutput("h_pd_pitch_ui")),
                column(4, uiOutput("h_pd_count_ui")),
                column(4,
                  radioButtons("h_pd_hand","Pitcher Throws",
                               choices=c("Combined","Right","Left"),
                               selected="Combined",inline=TRUE))
              )
            ),
            fluidRow(
              column(3,uiOutput("h_pd_card_zsw")),
              column(3,uiOutput("h_pd_card_zcon")),
              column(3,uiOutput("h_pd_card_whiff")),
              column(3,uiOutput("h_pd_card_chase"))
            ),
            br(),
            tags$div(class="section-header","Pitch Outcomes by Location"),
            tags$div(class="section-sub",
              HTML(paste0(
                "<span style='color:#4ECDC4;'>&#9679; Contact (zone)</span>&nbsp;&nbsp;",
                "<span style='color:#ff4655;'>&#x2715; Whiff (zone)</span>&nbsp;&nbsp;",
                "<span style='color:#ffd700;'>&#9679; Called Strike</span>&nbsp;&nbsp;",
                "<span style='color:#34d399;'>&#9679; Ball (zone)</span>&nbsp;&nbsp;",
                "<span style='color:#ff8c42;'>&#9679; Chase</span>&nbsp;&nbsp;",
                "<span style='color:#8892b0;'>&#9675; Take (out of zone)</span>"
              ))
            ),
            plotOutput("h_pd_plot",height="520px")
          ),
          tabPanel("wOBA Trends",
            br(),
            tags$div(class="section-header","Rolling Cumulative wOBA"),
            plotOutput("h_woba",height="340px"),
            br(),
            tags$div(class="section-header","wOBA by Opponent"),
            plotOutput("h_wobaOpp",height="340px")
          ),
          tabPanel("Splits",
            br(),
            tags$div(class="section-header","Stats by Pitch Type"),
            dataTableOutput("h_ptTable"),
            br(),
            tags$div(class="section-header","Righty / Lefty Splits"),
            dataTableOutput("h_lrTable"),
            br(),
            tags$div(class="section-header","Pitch Type \u00d7 Handedness"),
            dataTableOutput("h_ptLrTable")
          ),
          tabPanel("Batted Ball",
            br(),
            tags$div(class="filter-bar",
              fluidRow(
                column(4, uiOutput("h_bb_pitch_ui")),
                column(4,
                  radioButtons("h_bb_hand","Pitcher Throws",
                               choices=c("Combined","Right","Left"),
                               selected="Combined",inline=TRUE))
              )
            ),
            fluidRow(
              column(3,uiOutput("h_bb_card_gb")),
              column(3,uiOutput("h_bb_card_fb")),
              column(3,uiOutput("h_bb_card_ld")),
              column(3,uiOutput("h_bb_card_pu"))
            ),
            br(),
            tags$div(class="section-header","Cumulative"),
            dataTableOutput("h_bb_totalTable"),
            br(),
            tags$div(class="section-header","By Pitch Type"),
            dataTableOutput("h_bb_ptTable"),
            br(),
            tags$div(class="section-header","By Handedness"),
            dataTableOutput("h_bb_lrTable"),
            br(),
            tags$div(class="section-header","Pitch Type \u00d7 Handedness"),
            dataTableOutput("h_bb_ptLrTable")
          ),
          tabPanel("Heat Maps",
            br(),
            tags$div(class="section-header","Strike Zone Heat Maps"),
            tags$div(class="section-sub","Hitter's perspective (plate side flipped)"),
            tags$div(class="filter-bar",
              fluidRow(
                column(4, uiOutput("h_hm_pitch_ui")),
                column(4, uiOutput("h_hm_count_ui")),
                column(4,
                  radioButtons("h_hm_hand","Pitcher Throws",
                               choices=c("Combined","Right","Left"),
                               selected="Combined",inline=TRUE))
              )
            ),
            plotOutput("h_heatmap",height="460px")
          )
        )
      )
    )
  ),

  # ══════════════════════════════════════════════════════════════════════════
  # PITCHERS TAB
  # ══════════════════════════════════════════════════════════════════════════
  tabPanel("Pitchers",
    sidebarLayout(
      sidebarPanel(width=3,
        tags$div(class="section-header","Season"),
        tags$div(class="season-toggle",
          tags$button("2026",id="p_2026",class="season-btn active",
            onclick="Shiny.setInputValue('p_season','2026',{priority:'event'})"),
          tags$button("2025",id="p_2025",class="season-btn",
            onclick="Shiny.setInputValue('p_season','2025',{priority:'event'})"),
          tags$button("2024",id="p_2024",class="season-btn",
            onclick="Shiny.setInputValue('p_season','2024',{priority:'event'})"),
          tags$button("2023",id="p_2023",class="season-btn",
            onclick="Shiny.setInputValue('p_season','2023',{priority:'event'})")
        ),
        tags$hr(),
        uiOutput("p_teamSelect"),
        uiOutput("p_playerSelect"),
        uiOutput("p_dateUI"),
        uiOutput("p_pitchTypeUI"),
        selectInput("p_batterSide","Batter Side",choices=c("All","Right","Left")),
        tags$hr(),
        uiOutput("p_pitcherInfo")
      ),
      mainPanel(width=9,
        tabsetPanel(
          tabPanel("Summary",
            br(),
            fluidRow(
              column(3,uiOutput("p_card_n")),
              column(3,uiOutput("p_card_csw")),
              column(3,uiOutput("p_card_whiff")),
              column(3,uiOutput("p_card_zone"))
            ),
            br(),
            tags$div(class="section-header","Pitch Arsenal"),
            dataTableOutput("p_arsenal"),
            br(),
            tags$div(class="section-header","Results by Pitch Type"),
            dataTableOutput("p_results"),
            br(),
            tags$div(class="section-header","L / R Splits"),
            dataTableOutput("p_splits")
          ),
          tabPanel("Movement & Release",
            br(),
            tags$div(class="section-header","Pitch Movement"),
            plotOutput("p_movement",height="420px"),
            br(),
            tags$div(class="section-header","Release Points"),
            plotOutput("p_release",height="370px")
          ),
          tabPanel("Pitch Sequencing",
            br(),
            tags$div(class="section-header","Back-to-Back Pitch Matrix"),
            tags$div(class="filter-bar",
              fluidRow(
                column(4,
                  selectInput("p_seq_metric","Matrix Metric",
                    choices=c("CSW%"="csw_rate","Whiff%"="whiff_rate",
                              "Zone%"="zone_rate","Chase%"="chase_rate"))
                ),
                column(4,
                  selectInput("p_seq_hand","Batter Side",choices=c("All","Right","Left"))
                )
              )
            ),
            fluidRow(
              column(7, plotOutput("p_seqMatrix",height="520px",click="p_mat_click")),
              column(5,
                tags$div(class="section-header",style="font-size:14px;","Pair Locations"),
                fluidRow(
                  column(6,
                    tags$p(textOutput("p_seq_lbl1"),
                           style="color:#b0b8d4;font-size:11px;font-weight:600;text-align:center;"),
                    plotOutput("p_seq_loc1",height="230px")
                  ),
                  column(6,
                    tags$p(textOutput("p_seq_lbl2"),
                           style="color:#b0b8d4;font-size:11px;font-weight:600;text-align:center;"),
                    plotOutput("p_seq_loc2",height="230px")
                  )
                ),
                br(),
                uiOutput("p_seq_stats")
              )
            )
          ),
          tabPanel("Heat Maps",
            br(),
            tags$div(class="section-header","Pitch Location Heat Maps"),
            tags$div(class="filter-bar",
              fluidRow(
                column(4, uiOutput("p_hm_pitch_ui")),
                column(4, uiOutput("p_hm_count_ui")),
                column(4,
                  radioButtons("p_hm_hand","Batter Side",
                               choices=c("Combined","Right","Left"),
                               selected="Combined",inline=TRUE))
              )
            ),
            plotOutput("p_heatmap",height="480px")
          ),
          tabPanel("Velocity & Spin",
            br(),
            tags$div(class="section-header","Velocity Over Time"),
            plotOutput("p_velo",height="360px"),
            br(),
            tags$div(class="section-header","Spin Rate Over Time"),
            plotOutput("p_spin",height="360px")
          ),
          tabPanel("Count Splits",
            br(),
            tags$div(class="section-header","Results by Count"),
            dataTableOutput("p_countTable"),
            br(),
            tags$div(class="section-header","Pitch Type \u00d7 Batter Side"),
            dataTableOutput("p_ptHandTable")
          ),
          tabPanel("Batted Ball",
            br(),
            tags$div(class="filter-bar",
              fluidRow(
                column(4, uiOutput("p_bb_pitch_ui")),
                column(4,
                  radioButtons("p_bb_hand","Batter Side",
                               choices=c("Combined","Right","Left"),
                               selected="Combined",inline=TRUE))
              )
            ),
            fluidRow(
              column(3,uiOutput("p_bb_card_gb")),
              column(3,uiOutput("p_bb_card_fb")),
              column(3,uiOutput("p_bb_card_ld")),
              column(3,uiOutput("p_bb_card_pu"))
            ),
            br(),
            tags$div(class="section-header","Cumulative"),
            dataTableOutput("p_bb_totalTable"),
            br(),
            tags$div(class="section-header","By Pitch Type"),
            dataTableOutput("p_bb_ptTable"),
            br(),
            tags$div(class="section-header","By Batter Side"),
            dataTableOutput("p_bb_lrTable"),
            br(),
            tags$div(class="section-header","Pitch Type \u00d7 Batter Side"),
            dataTableOutput("p_bb_ptLrTable")
          )
        )
      )
    )
  ),

  # ══════════════════════════════════════════════════════════════════════════
  # LEADERBOARDS TAB
  # ══════════════════════════════════════════════════════════════════════════
  tabPanel("Leaderboards",
    br(),
    tags$div(class="filter-bar",
      fluidRow(
        column(3,
          tags$div(class="season-toggle",
            tags$button("2026",id="lb_2026",class="season-btn active",
              onclick="Shiny.setInputValue('lb_season','2026',{priority:'event'})"),
            tags$button("2025",id="lb_2025",class="season-btn",
              onclick="Shiny.setInputValue('lb_season','2025',{priority:'event'})"),
            tags$button("2024",id="lb_2024",class="season-btn",
              onclick="Shiny.setInputValue('lb_season','2024',{priority:'event'})"),
            tags$button("2023",id="lb_2023",class="season-btn",
              onclick="Shiny.setInputValue('lb_season','2023',{priority:'event'})")
          )
        ),
        column(3,
          numericInput("lb_min_pa","Min PA (Hitters)",value=20,min=1,max=500)
        ),
        column(3,
          numericInput("lb_min_bf","Min BF (Pitchers)",value=30,min=1,max=1000)
        )
      )
    ),
    tabsetPanel(
      tabPanel("Hitter Leaders",
        br(),
        tags$div(class="section-header","Qualified Hitters — Ranked by wOBA"),
        dataTableOutput("lb_hitters")
      ),
      tabPanel("Pitcher Leaders",
        br(),
        tags$div(class="section-header","Qualified Pitchers — Ranked by CSW%"),
        dataTableOutput("lb_pitchers")
      ),
      tabPanel("Team Hitting",
        br(),
        tags$div(class="section-header","Cumulative Team Hitting Stats"),
        tags$div(class="section-sub","Click any column header to sort"),
        dataTableOutput("lb_team_hitting")
      ),
      tabPanel("Team Pitching",
        br(),
        tags$div(class="section-header","Cumulative Team Pitching Stats"),
        tags$div(class="section-sub","Click any column header to sort"),
        dataTableOutput("lb_team_pitching")
      )
    )
  ),

  # ══════════════════════════════════════════════════════════════════════════
  # SCOUTING REPORTS TAB
  # ══════════════════════════════════════════════════════════════════════════
  tabPanel("Scouting Reports",
    sidebarLayout(
      sidebarPanel(width=3,
        tags$div(class="section-header","Report Settings"),
        tags$hr(),

        # Season
        tags$div(class="season-toggle",
          tags$button("2026",id="sr_2026",class="season-btn active",
            onclick="Shiny.setInputValue('sr_season','2026',{priority:'event'})"),
          tags$button("2025",id="sr_2025",class="season-btn",
            onclick="Shiny.setInputValue('sr_season','2025',{priority:'event'})"),
          tags$button("2024",id="sr_2024",class="season-btn",
            onclick="Shiny.setInputValue('sr_season','2024',{priority:'event'})"),
          tags$button("2023",id="sr_2023",class="season-btn",
            onclick="Shiny.setInputValue('sr_season','2023',{priority:'event'})")
        ),
        br(),

        # Report type
        radioButtons("sr_type","Report Type",
                     choices=c("Pitcher"="pitcher","Hitter"="hitter"),
                     selected="pitcher", inline=TRUE),
        tags$hr(),

        # Team + player selection
        uiOutput("sr_team_ui"),
        uiOutput("sr_players_ui"),
        fluidRow(
          column(6, actionButton("sr_sel_all","All",  class="btn-default btn-sm")),
          column(6, actionButton("sr_sel_none","Clear",class="btn-default btn-sm"))
        ),
        tags$hr(),

        # Section checkboxes
        tags$div(class="section-header",style="font-size:14px;","Sections to Include"),
        uiOutput("sr_sections_ui"),
        tags$hr(),

        # Generate button
        downloadButton("sr_download","Generate PDF",
                       class="btn-primary",style="width:100%;"),
        br(),br(),
        uiOutput("sr_status_ui")
      ),
      mainPanel(width=9,
        tags$div(class="section-header","Report Preview"),
        tags$div(class="section-sub",
                 "Preview shows first selected player. PDF will include all selected players."),
        uiOutput("sr_preview_ui")
      )
    )
  )
)

# ============================================================================
#  SERVER
# ============================================================================
server <- function(input, output, session) {

  # ── Refresh trigger ───────────────────────────────────────────────────────
  refresh_trigger <- reactiveVal(0)

  observeEvent(input$refresh_data, {
    showNotification("Refreshing data from Google Drive...",
                     type="message", duration=NULL, id="refreshing")
    load_and_cache()
    removeNotification("refreshing")
    showNotification("Data refreshed successfully!", type="message", duration=3)
    refresh_trigger(refresh_trigger() + 1)
  })

  output$last_updated_txt <- renderText({
    refresh_trigger()
    if (!is.null(.raw_cache$last_loaded))
      paste("Last updated:", format(.raw_cache$last_loaded, "%m/%d/%Y %I:%M %p"))
    else "Not yet loaded"
  })

  # ── Data accessors (read from cache, refresh on trigger) ──────────────────
  hitters_data <- reactive({
    refresh_trigger()
    .raw_cache$hitters_processed
  })
  p_data_r  <- reactive({ refresh_trigger(); .raw_cache$p_data_all  })
  p_seqs_r  <- reactive({ refresh_trigger(); .raw_cache$p_seqs_all  })
  p_pairs_r <- reactive({ refresh_trigger(); .raw_cache$p_pairs_all })

  h_season <- reactive({ input$h_season %||% "2026" })
  p_season <- reactive({ input$p_season %||% "2026" })
  lb_season <- reactive({ input$lb_season %||% "2026" })

  observeEvent(h_season(), {
    session$sendCustomMessage("setSeasonBtns",
      list(prefix="h", season=h_season()))
  })
  observeEvent(p_season(), {
    session$sendCustomMessage("setSeasonBtns",
      list(prefix="p", season=p_season()))
  })
  observeEvent(lb_season(), {
    session$sendCustomMessage("setSeasonBtns",
      list(prefix="lb", season=lb_season()))
  })

  # ── Hitter data slices ────────────────────────────────────────────────────
  h_raw <- reactive({
    req(!is.null(hitters_data()))
    hitters_data() %>%
      filter(Season == as.integer(h_season()))
  })

  output$h_teamSelect <- renderUI({
    req(!is.null(h_raw()), nrow(h_raw())>0)
    teams <- sort(unique(h_raw()$BatterTeamFull))
    selectInput("h_team","Select Team", choices=teams, selectize=TRUE)
  })

  output$h_playerSelect <- renderUI({
    req(input$h_team, !is.null(h_raw()))
    players <- h_raw() %>%
      filter(BatterTeamFull==input$h_team) %>%
      pull(Batter) %>% unique() %>% sort()
    prev <- isolate(input$h_batter)
    sel  <- if (!is.null(prev) && prev %in% players) prev else players[1]
    selectInput("h_batter","Select Batter", choices=players, selected=sel, selectize=TRUE)
  })

  output$h_dateRangeUI <- renderUI({
    req(input$h_batter, !is.null(h_raw()))
    d <- range(h_raw() %>% filter(Batter==input$h_batter) %>% pull(Date), na.rm=TRUE)
    dateRangeInput("h_dateRange","Date Range", start=d[1], end=d[2],
                   min=d[1], max=d[2], format="mm/dd/yyyy")
  })

  output$h_pitchTypeUI <- renderUI({
    req(!is.null(h_raw()))
    selectInput("h_pitchType","Pitch Type",
                choices=c("All",sort(unique(h_raw()$TaggedPitchType))))
  })

  output$h_playerInfo <- renderUI({
    d <- h_base(); if(is.null(d)||nrow(d)==0) return(NULL)
    tags$div(class="stat-card",
      tags$p(style="color:#8892b0;font-size:11px;",toupper(paste(h_season(),"season"))),
      tags$h2(style="font-size:20px;",max(d$PA_count,na.rm=TRUE)," PAs"),
      tags$p(input$h_team)
    )
  })

  h_base <- reactive({
    req(input$h_batter, !is.null(h_raw()))
    d <- h_raw() %>% filter(Batter==input$h_batter)
    if (!is.null(input$h_dateRange))
      d <- d %>% filter(Date>=input$h_dateRange[1], Date<=input$h_dateRange[2])
    d
  })

  h_filt <- reactive({
    d <- h_base()
    if (!is.null(input$h_pitchType) && input$h_pitchType!="All")
      d <- d %>% filter(TaggedPitchType==input$h_pitchType)
    d
  })

  h_stats <- reactive({
    d <- h_filt(); if(is.null(d)||nrow(d)==0) return(NULL)
    hits  <- sum(d$PlayResult %in% c("Single","Double","Triple","HomeRun"))
    walks <- sum(d$PlayResult=="Walk"); hbp <- sum(d$PlayResult=="HitByPitch")
    pa <- n_distinct(d$PA_count); ab <- pa-walks-hbp
    tb <- sum(d$PlayResult=="Single")+2*sum(d$PlayResult=="Double")+
          3*sum(d$PlayResult=="Triple")+4*sum(d$PlayResult=="HomeRun")
    list(pa=pa,
         woba=if(pa>0)round(sum(d$wOBA_contribution,na.rm=TRUE)/pa,3) else NA,
         obp =if(pa>0)round((hits+walks+hbp)/pa,3) else NA,
         slg =if(ab>0)round(tb/ab,3) else NA)
  })

  output$h_card_pa   <- renderUI({ s<-h_stats();req(s); stat_card(s$pa,"Plate Appearances") })
  output$h_card_woba <- renderUI({ s<-h_stats();req(s); stat_card(sprintf("%.3f",s$woba),"wOBA") })
  output$h_card_obp  <- renderUI({ s<-h_stats();req(s); stat_card(sprintf("%.3f",s$obp),"OBP") })
  output$h_card_slg  <- renderUI({ s<-h_stats();req(s); stat_card(sprintf("%.3f",s$slg),"SLG") })

  output$h_spray <- renderPlot({
    d <- h_filt() %>%
      filter(!is.na(LastTrackedDistance),!is.na(Bearing),!is.na(PlayResult)) %>%
      mutate(bearing_rad=Bearing*pi/180,
             x=LastTrackedDistance*sin(bearing_rad),
             y=LastTrackedDistance*cos(bearing_rad))
    req(nrow(d)>0)
    base <- 90/sqrt(2); foul_len <- 330; cf_dist <- 400
    dirt_r <- 95
    arc_df <- function(r,a1,a2,n=200){
      a <- seq(a1,a2,length.out=n)*pi/180
      data.frame(x=r*sin(a),y=r*cos(a))
    }
    fl_x <- foul_len*sin(45*pi/180); fl_y <- foul_len*cos(45*pi/180)
    wall_arc_l  <- arc_df(330,-45,-20)
    wall_arc_cf <- arc_df(cf_dist,-20,20)
    wall_arc_r  <- arc_df(330,20,45)
    wall_poly <- rbind(data.frame(x=0,y=0),data.frame(x=-fl_x,y=fl_y),
                       wall_arc_l,wall_arc_cf,wall_arc_r,data.frame(x=fl_x,y=fl_y))
    dirt_arc  <- arc_df(dirt_r,-45,45)
    dirt_poly <- rbind(data.frame(x=0,y=0),dirt_arc,data.frame(x=0,y=0))
    bases     <- data.frame(bx=c(0,base,0,-base,0),by=c(0,base,2*base,base,0))
    ggplot() +
      geom_polygon(data=wall_poly,aes(x=x,y=y),fill="#1a2e1a",color=NA) +
      geom_polygon(data=dirt_poly,aes(x=x,y=y),fill="#3d2b1f",color=NA) +
      geom_polygon(data=bases,aes(x=bx,y=by),fill="#1a2e1a",color=NA) +
      geom_path(data=rbind(data.frame(x=-fl_x,y=fl_y),wall_arc_l,wall_arc_cf,
                            wall_arc_r,data.frame(x=fl_x,y=fl_y)),
                aes(x=x,y=y),color="#6b7a8d",linewidth=1.4) +
      geom_segment(aes(x=0,y=0,xend=-fl_x,yend=fl_y),color="#6b7a8d",linewidth=.9) +
      geom_segment(aes(x=0,y=0,xend= fl_x,yend=fl_y),color="#6b7a8d",linewidth=.9) +
      geom_path(data=bases,aes(x=bx,y=by),color="#8892b0",linewidth=.8) +
      geom_point(data=data.frame(x=c(base,0,-base),y=c(base,2*base,base)),
                 aes(x=x,y=y),color="#ffffff",size=3,shape=15) +
      geom_point(aes(x=0,y=0),color="#ffffff",size=4,shape=18) +
      geom_point(aes(x=0,y=60.5),color="#5a3e2b",size=6,shape=16) +
      annotate("text",x=0,y=cf_dist+12,label="400",color="#6b7280",size=3) +
      annotate("text",x=-fl_x-18,y=fl_y+5,label="330",color="#6b7280",size=3) +
      annotate("text",x= fl_x+18,y=fl_y+5,label="330",color="#6b7280",size=3) +
      geom_point(data=d,aes(x=x,y=y,color=PlayResult),size=3.2,alpha=.85) +
      scale_color_manual(values=c(Single="#4ECDC4",Double="#45B7D1",HomeRun="#ff4655",
                                   Out="#8892b0",Triple="#a78bfa",FieldersChoice="#FECA57",
                                   Sacrifice="#48D1CC",Error="#FA8072"),name="Result") +
      coord_fixed(xlim=c(-280,280),ylim=c(-10,420)) +
      labs(title=paste(input$h_batter,"—",h_season(),"Spray Chart")) +
      theme_navs() +
      theme(axis.text=element_blank(),axis.title=element_blank(),
            panel.grid=element_blank(),panel.background=element_rect(fill="#0f1117",color=NA))
  }, bg="#0f1117")

  # ── Plate Discipline ──────────────────────────────────────────────────────
  h_pd_data <- reactive({
    req(input$h_batter, !is.null(h_raw()))
    d <- h_base()
    if (!is.null(input$h_pd_pitch) && input$h_pd_pitch!="All Pitches")
      d <- d %>% filter(TaggedPitchType==input$h_pd_pitch)
    d <- filter_by_count(d, input$h_pd_count, "CountSit", "CountIndiv")
    if (!is.null(input$h_pd_hand) && input$h_pd_hand!="Combined")
      d <- d %>% filter(PitcherThrows==input$h_pd_hand)
    d %>%
      filter(!is.na(PlateLocSide),!is.na(PlateLocHeight)) %>%
      mutate(
        InZone   = dplyr::between(PlateLocHeight,1.59,3.41) & dplyr::between(PlateLocSide,-1,1),
        IsSwing  = PitchCall %in% c("FoulBall","StrikeSwinging","InPlay"),
        IsWhiff  = PitchCall == "StrikeSwinging",
        IsContact= PitchCall %in% c("FoulBall","InPlay"),
        PD_side  = -PlateLocSide,
        Outcome  = dplyr::case_when(
          IsContact & InZone                                   ~ "Contact (zone)",
          IsWhiff   & InZone                                   ~ "Whiff (zone)",
          PitchCall == "StrikeCalled"                          ~ "Called Strike",
          PitchCall %in% c("BallCalled","HitByPitch") & InZone ~ "Ball (zone)",
          IsSwing   & !InZone                                  ~ "Chase",
          TRUE                                                  ~ "Take (out of zone)"
        )
      )
  })

  output$h_pd_pitch_ui <- renderUI({
    req(!is.null(h_raw()))
    selectInput("h_pd_pitch","Pitch Type",
                choices=c("All Pitches",sort(unique(h_raw()$TaggedPitchType))))
  })
  output$h_pd_count_ui <- renderUI({
    req(!is.null(h_raw()), input$h_batter)
    selectInput("h_pd_count","Count / Situation",
                choices=count_choices(h_base()$CountIndiv))
  })

  output$h_pd_card_zsw <- renderUI({
    d<-h_pd_data();req(d,nrow(d)>0)
    zp<-d%>%filter(InZone); v<-if(nrow(zp)>0)mean(zp$IsSwing,na.rm=TRUE) else 0
    tags$div(class="stat-card",tags$h2(paste0(round(v*100,1),"%")),
      tags$p(style="color:#6b7280;font-size:11px;margin:2px 0;",
             paste0(sum(zp$IsSwing,na.rm=TRUE),"/",nrow(zp))),
      tags$p("Zone Swing%"))
  })
  output$h_pd_card_zcon <- renderUI({
    d<-h_pd_data();req(d,nrow(d)>0)
    zs<-d%>%filter(InZone,IsSwing); v<-if(nrow(zs)>0)mean(zs$IsContact,na.rm=TRUE) else 0
    tags$div(class="stat-card",tags$h2(paste0(round(v*100,1),"%")),
      tags$p(style="color:#6b7280;font-size:11px;margin:2px 0;",
             paste0(sum(zs$IsContact,na.rm=TRUE),"/",nrow(zs))),
      tags$p("Zone Contact%"))
  })
  output$h_pd_card_whiff <- renderUI({
    d<-h_pd_data();req(d,nrow(d)>0)
    sw<-d%>%filter(IsSwing); v<-if(nrow(sw)>0)mean(sw$IsWhiff,na.rm=TRUE) else 0
    tags$div(class="stat-card",tags$h2(paste0(round(v*100,1),"%")),
      tags$p(style="color:#6b7280;font-size:11px;margin:2px 0;",
             paste0(sum(sw$IsWhiff,na.rm=TRUE),"/",nrow(sw))),
      tags$p("Whiff%"))
  })
  output$h_pd_card_chase <- renderUI({
    d<-h_pd_data();req(d,nrow(d)>0)
    oz<-d%>%filter(!InZone); v<-if(nrow(oz)>0)mean(oz$IsSwing,na.rm=TRUE) else 0
    tags$div(class="stat-card",tags$h2(paste0(round(v*100,1),"%")),
      tags$p(style="color:#6b7280;font-size:11px;margin:2px 0;",
             paste0(sum(oz$IsSwing,na.rm=TRUE),"/",nrow(oz))),
      tags$p("Chase%"))
  })

  output$h_pd_plot <- renderPlot({
    d<-h_pd_data();req(d,nrow(d)>0)
    outcome_pal <- c("Contact (zone)"="#4ECDC4","Whiff (zone)"="#ff4655",
                     "Called Strike"="#ffd700","Ball (zone)"="#34d399",
                     "Chase"="#ff8c42","Take (out of zone)"="#8892b0")
    outcome_shape <- c("Contact (zone)"=16,"Whiff (zone)"=4,"Called Strike"=16,
                       "Ball (zone)"=16,"Chase"=16,"Take (out of zone)"=1)
    d_ord <- d %>% mutate(Outcome=factor(Outcome,levels=c(
      "Take (out of zone)","Ball (zone)","Called Strike","Chase","Contact (zone)","Whiff (zone)")))
    ggplot(d_ord,aes(x=PD_side,y=PlateLocHeight,color=Outcome,shape=Outcome)) +
      annotate("rect",xmin=-1,xmax=1,ymin=1.59,ymax=3.41,fill=NA,color="#ffffff",linewidth=1) +
      annotate("segment",x=c(-1/3,1/3,-1,-1),xend=c(-1/3,1/3,1,1),
               y=c(1.59,1.59,(3.41-1.59)/3+1.59,(3.41-1.59)*2/3+1.59),
               yend=c(3.41,3.41,(3.41-1.59)/3+1.59,(3.41-1.59)*2/3+1.59),
               color="#2a2d3a",linewidth=.4) +
      geom_jitter(size=2.8,alpha=.80,width=.01,height=.01) +
      scale_color_manual(values=outcome_pal,name="Outcome") +
      scale_shape_manual(values=outcome_shape,name="Outcome") +
      xlim(-2.2,2.2)+ylim(0.8,4.2) +
      labs(title=paste(input$h_batter,"—",h_season(),"Plate Discipline"),
           x="Horizontal (Hitter's View)",y="Vertical Height (ft)") +
      theme_navs()
  }, bg="#0f1117")

  # ── wOBA trends ───────────────────────────────────────────────────────────
  output$h_woba <- renderPlot({
    d<-h_filt()%>%arrange(PA_count)%>%filter(is.finite(cumulative_wOBA));req(nrow(d)>0)
    lv<-tail(d$cumulative_wOBA,1)
    ggplot(d,aes(x=PA_count,y=cumulative_wOBA)) +
      geom_hline(yintercept=c(.320,.370,.420),linetype="dashed",color="#2a2d3a") +
      annotate("text",x=min(d$PA_count),y=c(.325,.375,.425),
               label=c("Average (.320)","Good (.370)","Great (.420)"),
               hjust=0,size=3,color="#8892b0") +
      geom_line(color="#ff4655",linewidth=1.6) +
      geom_point(color="#ff4655",size=2) +
      annotate("text",x=max(d$PA_count),y=lv,label=sprintf("%.3f",lv),
               hjust=-0.15,size=4,fontface="bold",color="#ff4655") +
      labs(title=paste(input$h_batter,"—",h_season(),"Rolling wOBA"),
           x="Plate Appearance",y="wOBA") +
      theme_navs()
  }, bg="#0f1117")

  output$h_wobaOpp <- renderPlot({
    d<-h_base()%>%filter(!is.na(PitcherTeamFull),PitcherTeamFull!="");req(nrow(d)>0)
    teams<-sort(unique(d$PitcherTeamFull))
    col_map<-setNames(team_pal[seq_along(teams)],teams)
    finals<-d%>%group_by(PitcherTeamFull)%>%
      summarise(fp=max(PA_count_opp),fw=last(cumulative_wOBA_opp),.groups="drop")
    p<-ggplot(d,aes(x=PA_count_opp,y=cumulative_wOBA_opp,color=PitcherTeamFull)) +
      geom_hline(yintercept=.320,linetype="dashed",color="#2a2d3a") +
      geom_line(linewidth=1.3,alpha=.85)+geom_point(size=1.8,alpha=.85) +
      scale_color_manual(values=col_map,name="Opponent") +
      labs(title=paste(input$h_batter,"— wOBA by Opponent"),
           x="PAs vs Opponent",y="Cumulative wOBA") +
      theme_navs()
    for(i in seq_len(nrow(finals)))
      p<-p+annotate("text",x=finals$fp[i],y=finals$fw[i],
                    label=sprintf("%.3f",finals$fw[i]),
                    hjust=-0.1,size=3,color=col_map[finals$PitcherTeamFull[i]])
    p
  }, bg="#0f1117")

  # ── Splits tables ─────────────────────────────────────────────────────────
  output$h_ptTable   <- renderDataTable({
    datatable(h_filt()%>%group_by(PitchType=TaggedPitchType)%>%slg_stats(),
              options=dt_opts,rownames=FALSE)
  })
  output$h_lrTable   <- renderDataTable({
    datatable(h_filt()%>%group_by(Throws=PitcherThrows)%>%slg_stats(),
              options=dt_opts,rownames=FALSE)
  })
  output$h_ptLrTable <- renderDataTable({
    datatable(h_filt()%>%group_by(Throws=PitcherThrows,PitchType=TaggedPitchType)%>%slg_stats(),
              options=dt_opts,rownames=FALSE)
  })

  # ── Batted Ball (hitter) ──────────────────────────────────────────────────
  h_bb_data <- reactive({
    d <- h_filt()
    if (!is.null(input$h_bb_pitch) && input$h_bb_pitch!="All Pitches")
      d <- d %>% filter(TaggedPitchType==input$h_bb_pitch)
    if (!is.null(input$h_bb_hand) && input$h_bb_hand!="Combined")
      d <- d %>% filter(PitcherThrows==input$h_bb_hand)
    d %>% filter(TaggedHitType %in% c("GroundBall","LineDrive","FlyBall","Popup"))
  })
  output$h_bb_pitch_ui <- renderUI({
    req(!is.null(h_raw()))
    selectInput("h_bb_pitch","Pitch Type",
                choices=c("All Pitches",sort(unique(h_raw()$TaggedPitchType))))
  })
  output$h_bb_card_gb <- renderUI({
    d<-h_bb_data();req(nrow(d)>0)
    bb_card(sum(d$TaggedHitType=="GroundBall",na.rm=TRUE),nrow(d),"GB%")
  })
  output$h_bb_card_fb <- renderUI({
    d<-h_bb_data();req(nrow(d)>0)
    bb_card(sum(d$TaggedHitType=="FlyBall",na.rm=TRUE),nrow(d),"FB%")
  })
  output$h_bb_card_ld <- renderUI({
    d<-h_bb_data();req(nrow(d)>0)
    bb_card(sum(d$TaggedHitType=="LineDrive",na.rm=TRUE),nrow(d),"LD%")
  })
  output$h_bb_card_pu <- renderUI({
    d<-h_bb_data();req(nrow(d)>0)
    bb_card(sum(d$TaggedHitType=="Popup",na.rm=TRUE),nrow(d),"PU%")
  })
  output$h_bb_totalTable <- renderDataTable({
    d<-h_bb_data();req(nrow(d)>0)
    datatable(d%>%mutate(Total="All")%>%group_by(Total)%>%bb_stats_fn(),
              options=dt_opts,rownames=FALSE)
  })
  output$h_bb_ptTable <- renderDataTable({
    d<-h_bb_data();req(nrow(d)>0)
    datatable(d%>%group_by(PitchType=TaggedPitchType)%>%bb_stats_fn(),
              options=dt_opts,rownames=FALSE)
  })
  output$h_bb_lrTable <- renderDataTable({
    d<-h_bb_data();req(nrow(d)>0)
    datatable(d%>%group_by(Throws=PitcherThrows)%>%bb_stats_fn(),
              options=dt_opts,rownames=FALSE)
  })
  output$h_bb_ptLrTable <- renderDataTable({
    d<-h_bb_data();req(nrow(d)>0)
    datatable(d%>%group_by(Throws=PitcherThrows,PitchType=TaggedPitchType)%>%bb_stats_fn(),
              options=dt_opts,rownames=FALSE)
  })

  # ── Heat maps (hitter) ────────────────────────────────────────────────────
  output$h_hm_pitch_ui <- renderUI({
    req(!is.null(h_raw()))
    selectInput("h_hm_pitch","Pitch Type",
                choices=c("All Pitches",sort(unique(h_raw()$TaggedPitchType))))
  })
  output$h_hm_count_ui <- renderUI({
    req(!is.null(h_raw()),input$h_batter)
    selectInput("h_hm_count","Count / Situation",
                choices=count_choices(h_base()$CountIndiv))
  })
  output$h_heatmap <- renderPlot({
    req(!is.null(h_raw()),input$h_batter)
    d <- h_base()
    if (!is.null(input$h_hm_pitch) && input$h_hm_pitch!="All Pitches")
      d <- d %>% filter(TaggedPitchType==input$h_hm_pitch)
    d <- filter_by_count(d, input$h_hm_count, "CountSit", "CountIndiv")
    if (!is.null(input$h_hm_hand) && input$h_hm_hand!="Combined")
      d <- d %>% filter(PitcherThrows==input$h_hm_hand)
    sub <- paste("Pitch:",input$h_hm_pitch%||%"All","| Count:",
                 input$h_hm_count%||%"All","| Throws:",input$h_hm_hand%||%"Combined")
    loc_heatmap(d, paste(input$h_batter,"—",h_season(),"Heat Map"), sub, flip_side=TRUE)
  }, bg="#0f1117")

  # ==========================================================================
  #  PITCHER REACTIVES
  # ==========================================================================
  p_raw <- reactive({
    req(!is.null(p_data_r()))
    p_data_r() %>% filter(Season == as.integer(p_season()))
  })
  p_seqs <- reactive({
    req(!is.null(p_seqs_r()))
    p_seqs_r() %>% filter(Season == as.integer(p_season()))
  })
  p_pairs <- reactive({
    req(!is.null(p_pairs_r()))
    p_pairs_r()
  })

  output$p_teamSelect <- renderUI({
    req(!is.null(p_raw()),nrow(p_raw())>0)
    teams <- sort(unique(p_raw()$PitcherTeamFull))
    selectInput("p_team","Select Team",choices=teams,selectize=TRUE)
  })

  output$p_playerSelect <- renderUI({
    req(input$p_team,!is.null(p_raw()))
    players <- p_raw()%>%filter(PitcherTeamFull==input$p_team)%>%
      pull(Pitcher)%>%unique()%>%sort()
    prev <- isolate(input$p_pitcher)
    sel  <- if(!is.null(prev)&&prev%in%players) prev else players[1]
    selectInput("p_pitcher","Select Pitcher",choices=players,selected=sel,selectize=TRUE)
  })

  output$p_dateUI <- renderUI({
    req(input$p_pitcher,!is.null(p_raw()))
    dates <- sort(unique(p_raw()%>%filter(Pitcher==input$p_pitcher)%>%pull(Date)))
    choices <- setNames(as.character(dates),format(dates,"%m/%d/%Y"))
    tagList(
      checkboxGroupInput("p_dates","Select Outing(s)",
                         choices=choices,selected=as.character(dates)),
      fluidRow(
        column(6,actionButton("p_selAll","All",class="btn-default btn-sm")),
        column(6,actionButton("p_selNone","Clear",class="btn-default btn-sm"))
      )
    )
  })

  output$p_pitchTypeUI <- renderUI({
    req(!is.null(p_raw()))
    selectInput("p_pitchType","Pitch Type",
                choices=c("All",sort(unique(p_raw()$TaggedPitchType))))
  })

  output$p_pitcherInfo <- renderUI({
    d<-p_filt();if(is.null(d)||nrow(d)==0) return(NULL)
    tags$div(class="stat-card",
      tags$p(style="color:#8892b0;font-size:11px;",toupper(paste(p_season(),"season"))),
      tags$h2(style="font-size:20px;",nrow(d)," pitches"),
      tags$p(n_distinct(d$Date)," outings — ",input$p_team)
    )
  })

  observeEvent(input$p_selAll,{
    req(input$p_pitcher,!is.null(p_raw()))
    dates<-sort(unique(p_raw()%>%filter(Pitcher==input$p_pitcher)%>%pull(Date)))
    updateCheckboxGroupInput(session,"p_dates",selected=as.character(dates))
  })
  observeEvent(input$p_selNone,{
    updateCheckboxGroupInput(session,"p_dates",selected=character(0))
  })

  p_filt <- reactive({
    req(input$p_pitcher,!is.null(p_raw()))
    d <- p_raw()%>%filter(Pitcher==input$p_pitcher)
    if(!is.null(input$p_dates)&&length(input$p_dates)>0)
      d <- d%>%filter(Date%in%as.Date(input$p_dates))
    else return(NULL)
    if(!is.null(input$p_pitchType)&&input$p_pitchType!="All")
      d <- d%>%filter(TaggedPitchType==input$p_pitchType)
    if(input$p_batterSide!="All")
      d <- d%>%filter(BatterSide==input$p_batterSide)
    d
  })

  output$p_card_n <- renderUI({
    d<-p_filt();req(d); stat_card(nrow(d),"Pitches")
  })
  output$p_card_csw <- renderUI({
    d<-p_filt();req(d)
    stat_card(paste0(round(mean(d$CSWCheck,na.rm=TRUE)*100,1),"%"),"CSW%")
  })
  output$p_card_whiff <- renderUI({
    d<-p_filt();req(d)
    sw<-sum(d$SwingCheck,na.rm=TRUE)
    stat_card(if(sw>0)paste0(round(sum(d$WhiffCheck,na.rm=TRUE)/sw*100,1),"%") else "—","Whiff%")
  })
  output$p_card_zone <- renderUI({
    d<-p_filt();req(d)
    stat_card(paste0(round(mean(d$ZoneCheck,na.rm=TRUE)*100,1),"%"),"Zone%")
  })

  output$p_arsenal <- renderDataTable({
    d<-p_filt();req(d,nrow(d)>0)
    tbl <- d%>%group_by(Pitch=TaggedPitchType)%>%
      summarise(
        Pitches=n(),
        `Avg Velo`=round(mean(RelSpeed,na.rm=TRUE),1),
        `Max Velo`=round(max(RelSpeed,na.rm=TRUE),1),
        Spin=round(mean(SpinRate,na.rm=TRUE),0),
        IVB=round(mean(InducedVertBreak,na.rm=TRUE),1),
        HB=round(mean(HorzBreak,na.rm=TRUE),1),
        `CSW%`=paste0(round(mean(CSWCheck,na.rm=TRUE)*100,1),"%"),
        `Zone%`=paste0(round(mean(ZoneCheck,na.rm=TRUE)*100,1),"%"),
        `Whiff%`={sw=sum(SwingCheck,na.rm=TRUE);
                  paste0(if(sw>0)round(sum(WhiffCheck,na.rm=TRUE)/sw*100,1)else 0,"%")},
        .groups="drop"
      )%>%
      mutate(Usage=scales::percent(Pitches/sum(Pitches),accuracy=0.1))%>%
      dplyr::select(Pitch,Pitches,Usage,`Avg Velo`,`Max Velo`,Spin,IVB,HB,`CSW%`,`Zone%`,`Whiff%`)
    datatable(tbl,options=dt_opts,rownames=FALSE)
  })

  output$p_results <- renderDataTable({
    d<-p_filt();req(d,nrow(d)>0)
    tbl <- d%>%group_by(Pitch=TaggedPitchType)%>%
      summarise(Pitches=n(),PA=sum(PACheck,na.rm=TRUE),AB=sum(ABCheck,na.rm=TRUE),
                H=sum(HCheck,na.rm=TRUE),
                `1B`=sum(PlayResult=="Single"),`2B`=sum(PlayResult=="Double"),
                `3B`=sum(PlayResult=="Triple"),HR=sum(PlayResult=="HomeRun"),
                SO=sum(StrikeoutCheck,na.rm=TRUE),BB=sum(WalkCheck,na.rm=TRUE),
                .groups="drop")%>%
      mutate(TB=`1B`+2*`2B`+3*`3B`+4*HR,
             AVG=sprintf("%.3f",ifelse(AB>0,H/AB,NA)),
             OBP=sprintf("%.3f",ifelse(PA>0,(H+BB)/PA,NA)),
             SLG=sprintf("%.3f",ifelse(AB>0,TB/AB,NA)))%>%
      dplyr::select(Pitch,Pitches,PA,SO,BB,H,HR,AVG,OBP,SLG)
    datatable(tbl,options=dt_opts,rownames=FALSE)
  })

  output$p_splits <- renderDataTable({
    d<-p_filt();req(d,nrow(d)>0)
    tbl <- d%>%group_by(Side=BatterSide)%>%
      summarise(Pitches=n(),PA=sum(PACheck,na.rm=TRUE),AB=sum(ABCheck,na.rm=TRUE),
                H=sum(HCheck,na.rm=TRUE),SO=sum(StrikeoutCheck,na.rm=TRUE),
                BB=sum(WalkCheck,na.rm=TRUE),
                `CSW%`=paste0(round(mean(CSWCheck,na.rm=TRUE)*100,1),"%"),
                .groups="drop")%>%
      mutate(AVG=sprintf("%.3f",ifelse(AB>0,H/AB,NA)),
             OBP=sprintf("%.3f",ifelse(PA>0,(H+BB)/PA,NA)))
    datatable(tbl,options=dt_opts,rownames=FALSE)
  })

  output$p_movement <- renderPlot({
    d<-p_filt();req(d,nrow(d)>0)
    ggplot(d,aes(x=HorzBreak,y=InducedVertBreak,color=TaggedPitchType))+
      geom_hline(yintercept=0,color="#2a2d3a",linewidth=.8)+
      geom_vline(xintercept=0,color="#2a2d3a",linewidth=.8)+
      geom_point(size=2.5,alpha=.7)+
      stat_ellipse(aes(group=TaggedPitchType),level=.68,linewidth=.6,linetype="dashed",alpha=.4)+
      scale_color_manual(values=pitch_pal,na.value="#94a3b8",name="Pitch Type")+
      xlim(-30,30)+ylim(-30,30)+
      labs(title=paste(input$p_pitcher,"—",p_season(),"Movement"),
           x="Horizontal Break (in)",y="Induced Vertical Break (in)")+
      theme_navs()
  }, bg="#0f1117")

  output$p_release <- renderPlot({
    d<-p_filt();req(d,nrow(d)>0)
    ggplot(d,aes(x=RelSide,y=RelHeight,color=TaggedPitchType))+
      geom_point(size=2.5,alpha=.7)+
      scale_color_manual(values=pitch_pal,na.value="#94a3b8",name="Pitch Type")+
      xlim(-4,4)+ylim(2,7)+
      labs(title=paste(input$p_pitcher,"—",p_season(),"Release Points"),
           x="Horizontal Release (ft)",y="Vertical Release (ft)")+
      theme_navs()
  }, bg="#0f1117")

  # ── Pitch Sequencing ──────────────────────────────────────────────────────
  seq_clicked <- reactiveValues(first=NULL,second=NULL)

  p_pairs_filt <- reactive({
    req(!is.null(p_pairs()),input$p_pitcher)
    d <- p_pairs()%>%filter(Pitcher==input$p_pitcher)
    hand <- input$p_seq_hand%||%"All"
    if(hand!="All"){
      d <- p_seqs()%>%filter(Pitcher==input$p_pitcher,BatterSide==hand)%>%
        group_by(Pitcher,PitcherTeamFull,prev_type,TaggedPitchType)%>%
        summarise(n_pairs=n(),
                  csw_rate=mean(CSWCheck,na.rm=TRUE),
                  whiff_rate={sw=sum(SwingCheck,na.rm=TRUE);if(sw>0)sum(WhiffCheck,na.rm=TRUE)/sw else NA_real_},
                  zone_rate=mean(ZoneCheck,na.rm=TRUE),
                  chase_rate={oz=sum(!ZoneCheck,na.rm=TRUE);if(oz>0)sum(SwingCheck[!ZoneCheck],na.rm=TRUE)/oz else NA_real_},
                  .groups="drop")
    }
    d
  })

  output$p_seqMatrix <- renderPlot({
    d<-p_pairs_filt();req(d,nrow(d)>0)
    metric <- input$p_seq_metric%||%"csw_rate"
    all_types <- sort(unique(c(d$prev_type,d$TaggedPitchType)))
    mat <- expand.grid(prev_type=all_types,TaggedPitchType=all_types,stringsAsFactors=FALSE)%>%
      left_join(d,by=c("prev_type","TaggedPitchType"))%>%
      mutate(val=.data[[metric]],n_pairs=ifelse(is.na(n_pairs),0L,n_pairs),
             label=ifelse(n_pairs>0,paste0(round(val*100,0),"%\n(n=",n_pairs,")"),""))
    metric_lbl <- c(csw_rate="CSW%",whiff_rate="Whiff%",zone_rate="Zone%",chase_rate="Chase%")[metric]
    ggplot(mat,aes(x=prev_type,y=TaggedPitchType,fill=val))+
      geom_tile(color="#0f1117",linewidth=1.2)+
      geom_text(aes(label=label),size=3.5,fontface="bold",color="#ffffff",lineheight=.85)+
      scale_fill_gradient2(low="#1565c0",mid="#1a1e2e",high="#ff4655",midpoint=.4,
                           na.value="#1a1e2e",limits=c(0,1),
                           labels=scales::percent_format(),name=metric_lbl)+
      labs(x="First Pitch (Previous)",y="Second Pitch (Current)",
           title=paste(input$p_pitcher,"—",p_season(),"Sequencing"))+
      theme_navs()+
      theme(panel.grid=element_blank(),
            axis.text.x=element_text(angle=30,hjust=1,size=10,face="bold"),
            axis.text.y=element_text(size=10,face="bold"))+
      coord_fixed()
  }, bg="#0f1117")

  observeEvent(input$p_mat_click,{
    req(input$p_pitcher,!is.null(p_pairs()))
    d<-p_pairs_filt()
    all_types<-sort(unique(c(d$prev_type,d$TaggedPitchType)))
    xi<-round(input$p_mat_click$x); yi<-round(input$p_mat_click$y)
    if(xi>=1&&xi<=length(all_types)&&yi>=1&&yi<=length(all_types)){
      seq_clicked$first  <- all_types[xi]
      seq_clicked$second <- all_types[yi]
    }
  })

  output$p_seq_lbl1 <- renderText({
    if(!is.null(seq_clicked$first)) paste("1st:",seq_clicked$first) else "1st pitch (click)"
  })
  output$p_seq_lbl2 <- renderText({
    if(!is.null(seq_clicked$second)) paste("2nd:",seq_clicked$second) else "2nd pitch (click)"
  })

  output$p_seq_loc1 <- renderPlot({
    req(seq_clicked$first,input$p_pitcher,!is.null(p_seqs()))
    loc <- p_seqs()%>%filter(Pitcher==input$p_pitcher,prev_type==seq_clicked$first)%>%
      transmute(PlateLocSide=prev_loc_side,PlateLocHeight=prev_loc_height)%>%
      filter(!is.na(PlateLocSide),!is.na(PlateLocHeight))
    loc_heatmap(loc,paste0(seq_clicked$first,"\n(n=",nrow(loc),")"))
  }, bg="#0f1117")

  output$p_seq_loc2 <- renderPlot({
    req(seq_clicked$first,seq_clicked$second,input$p_pitcher,!is.null(p_seqs()))
    loc <- p_seqs()%>%filter(Pitcher==input$p_pitcher,
                              prev_type==seq_clicked$first,
                              TaggedPitchType==seq_clicked$second)%>%
      dplyr::select(PlateLocSide,PlateLocHeight)%>%
      filter(!is.na(PlateLocSide),!is.na(PlateLocHeight))
    loc_heatmap(loc,paste0(seq_clicked$second,"\n(n=",nrow(loc),")"))
  }, bg="#0f1117")

  output$p_seq_stats <- renderUI({
    req(seq_clicked$first,seq_clicked$second)
    pair <- p_pairs_filt()%>%
      filter(prev_type==seq_clicked$first,TaggedPitchType==seq_clicked$second)
    if(nrow(pair)==0)
      return(tags$div(class="stat-card",tags$p(style="color:#8892b0;","No data.")))
    tags$div(class="stat-card",
      tags$p(style="color:#ff4655;font-weight:700;font-size:13px;",
             seq_clicked$first," \u2192 ",seq_clicked$second),
      tags$p(style="color:#8892b0;margin:4px 0;",strong(pair$n_pairs)," sequences"),
      tags$p(style="color:#8892b0;margin:4px 0;","CSW%: ",strong(paste0(round(pair$csw_rate*100,1),"%"))),
      tags$p(style="color:#8892b0;margin:4px 0;","Whiff%: ",strong(paste0(round(pair$whiff_rate*100,1),"%"))),
      tags$p(style="color:#8892b0;margin:4px 0;","Zone%: ",strong(paste0(round(pair$zone_rate*100,1),"%"))),
      tags$p(style="color:#8892b0;margin:4px 0;","Chase%: ",strong(paste0(round(pair$chase_rate*100,1),"%")))
    )
  })

  # ── Heat maps (pitcher) ───────────────────────────────────────────────────
  output$p_hm_pitch_ui <- renderUI({
    req(!is.null(p_raw()))
    selectInput("p_hm_pitch","Pitch Type",
                choices=c("All Pitches",sort(unique(p_raw()$TaggedPitchType))))
  })
  output$p_hm_count_ui <- renderUI({
    d<-p_filt();if(is.null(d)) return(NULL)
    selectInput("p_hm_count","Count / Situation",choices=count_choices(d$CountIndiv))
  })
  output$p_heatmap <- renderPlot({
    d<-p_filt();req(d,nrow(d)>0)
    if(!is.null(input$p_hm_pitch)&&input$p_hm_pitch!="All Pitches")
      d <- d%>%filter(TaggedPitchType==input$p_hm_pitch)
    d <- filter_by_count(d,input$p_hm_count,"CountSit","CountIndiv")
    if(!is.null(input$p_hm_hand)&&input$p_hm_hand!="Combined")
      d <- d%>%filter(BatterSide==input$p_hm_hand)
    sub <- paste("Count:",input$p_hm_count%||%"All","| Batter:",input$p_hm_hand%||%"Combined")
    if(is.null(input$p_hm_pitch)||input$p_hm_pitch=="All Pitches"){
      if(nrow(d)<5) return(ggplot()+annotate("text",x=0,y=0,label="Not enough data",color="#8892b0",size=6)+theme_navs())
      ggplot(d,aes(x=PlateLocSide,y=PlateLocHeight))+
        stat_density_2d(aes(fill=after_stat(density)),geom="raster",contour=FALSE)+
        scale_fill_gradientn(colours=heat_fills,guide="none")+
        annotate("rect",xmin=-1,xmax=1,ymin=1.6,ymax=3.4,fill=NA,color="#ffffff",linewidth=.7)+
        ylim(1,4)+xlim(-1.8,1.8)+
        facet_wrap(~TaggedPitchType,ncol=3)+
        labs(title=paste(input$p_pitcher,"—",p_season(),"Heat Maps"),subtitle=sub,
             x="Horizontal",y="Vertical")+
        theme_navs()
    } else {
      loc_heatmap(d,paste(input$p_pitcher,"—",p_season(),input$p_hm_pitch,"Heat Map"),sub)
    }
  }, bg="#0f1117")

  # ── Velocity / Spin ───────────────────────────────────────────────────────
  trend_plot <- function(d,y_col,y_lab,title_str){
    if(is.null(d)||nrow(d)==0) return(NULL)
    pd <- d%>%arrange(Date,PitchNo)%>%group_by(TaggedPitchType)%>%
      mutate(idx=row_number()-1)%>%ungroup()
    smry <- pd%>%group_by(TaggedPitchType)%>%
      summarise(avg=mean(.data[[y_col]],na.rm=TRUE),mn=min(.data[[y_col]],na.rm=TRUE),
                mx=max(.data[[y_col]],na.rm=TRUE),max_idx=max(idx),
                last_v=last(.data[[y_col]]),.groups="drop")%>%
      mutate(lbl=paste0(round(avg,1),if(y_col=="RelSpeed")" mph\n(" else " rpm\n(",
                        round(mn,0),"-",round(mx,0),")"))
    ggplot(pd,aes(x=idx,y=.data[[y_col]],color=TaggedPitchType))+
      geom_line(linewidth=1.2,alpha=.85)+geom_point(size=1.5,alpha=.4)+
      geom_text(data=smry,aes(x=max_idx,y=last_v,label=lbl,color=TaggedPitchType),
                hjust=-0.1,vjust=.5,size=2.8,lineheight=.85)+
      scale_color_manual(values=pitch_pal,na.value="#94a3b8")+
      scale_x_continuous(expand=expansion(mult=c(.02,.18)))+
      labs(title=title_str,x="Pitch Count",y=y_lab,color="Pitch Type")+
      theme_navs()
  }
  output$p_velo <- renderPlot({
    trend_plot(p_filt(),"RelSpeed","Velocity (MPH)",
               paste(input$p_pitcher,"—",p_season(),"Velocity"))
  }, bg="#0f1117")
  output$p_spin <- renderPlot({
    trend_plot(p_filt(),"SpinRate","Spin Rate (RPM)",
               paste(input$p_pitcher,"—",p_season(),"Spin Rate"))
  }, bg="#0f1117")

  # ── Count splits ──────────────────────────────────────────────────────────
  output$p_countTable <- renderDataTable({
    d<-p_filt();req(d,nrow(d)>0)
    tbl <- d%>%group_by(Count)%>%
      summarise(Pitches=n(),PA=sum(PACheck,na.rm=TRUE),AB=sum(ABCheck,na.rm=TRUE),
                H=sum(HCheck,na.rm=TRUE),SO=sum(StrikeoutCheck,na.rm=TRUE),
                BB=sum(WalkCheck,na.rm=TRUE),
                `CSW%`=paste0(round(mean(CSWCheck,na.rm=TRUE)*100,1),"%"),
                `Zone%`=paste0(round(mean(ZoneCheck,na.rm=TRUE)*100,1),"%"),
                .groups="drop")%>%
      mutate(AVG=sprintf("%.3f",ifelse(AB>0,H/AB,NA)),
             OBP=sprintf("%.3f",ifelse(PA>0,(H+BB)/PA,NA)))%>%
      arrange(Count)
    datatable(tbl,options=dt_opts,rownames=FALSE)
  })
  output$p_ptHandTable <- renderDataTable({
    d<-p_filt();req(d,nrow(d)>0)
    tbl <- d%>%group_by(Pitch=TaggedPitchType,Side=BatterSide)%>%
      summarise(Pitches=n(),
                `CSW%`=paste0(round(mean(CSWCheck,na.rm=TRUE)*100,1),"%"),
                `Zone%`=paste0(round(mean(ZoneCheck,na.rm=TRUE)*100,1),"%"),
                `Whiff%`={sw=sum(SwingCheck,na.rm=TRUE);
                           paste0(if(sw>0)round(sum(WhiffCheck,na.rm=TRUE)/sw*100,1)else 0,"%")},
                `Avg Velo`=round(mean(RelSpeed,na.rm=TRUE),1),.groups="drop")
    datatable(tbl,options=dt_opts,rownames=FALSE)
  })

  # ── Batted Ball (pitcher) ─────────────────────────────────────────────────
  p_bb_data <- reactive({
    d<-p_filt();req(d,nrow(d)>0)
    if(!is.null(input$p_bb_pitch)&&input$p_bb_pitch!="All Pitches")
      d <- d%>%filter(TaggedPitchType==input$p_bb_pitch)
    if(!is.null(input$p_bb_hand)&&input$p_bb_hand!="Combined")
      d <- d%>%filter(BatterSide==input$p_bb_hand)
    d%>%filter(TaggedHitType%in%c("GroundBall","LineDrive","FlyBall","Popup"))
  })
  output$p_bb_pitch_ui <- renderUI({
    req(!is.null(p_raw()))
    selectInput("p_bb_pitch","Pitch Type",
                choices=c("All Pitches",sort(unique(p_raw()$TaggedPitchType))))
  })
  output$p_bb_card_gb <- renderUI({
    d<-p_bb_data();req(nrow(d)>0)
    bb_card(sum(d$TaggedHitType=="GroundBall",na.rm=TRUE),nrow(d),"GB%")
  })
  output$p_bb_card_fb <- renderUI({
    d<-p_bb_data();req(nrow(d)>0)
    bb_card(sum(d$TaggedHitType=="FlyBall",na.rm=TRUE),nrow(d),"FB%")
  })
  output$p_bb_card_ld <- renderUI({
    d<-p_bb_data();req(nrow(d)>0)
    bb_card(sum(d$TaggedHitType=="LineDrive",na.rm=TRUE),nrow(d),"LD%")
  })
  output$p_bb_card_pu <- renderUI({
    d<-p_bb_data();req(nrow(d)>0)
    bb_card(sum(d$TaggedHitType=="Popup",na.rm=TRUE),nrow(d),"PU%")
  })
  output$p_bb_totalTable <- renderDataTable({
    d<-p_bb_data();req(nrow(d)>0)
    datatable(d%>%mutate(Total="All")%>%group_by(Total)%>%bb_stats_fn(),
              options=dt_opts,rownames=FALSE)
  })
  output$p_bb_ptTable <- renderDataTable({
    d<-p_bb_data();req(nrow(d)>0)
    datatable(d%>%group_by(Pitch=TaggedPitchType)%>%bb_stats_fn(),
              options=dt_opts,rownames=FALSE)
  })
  output$p_bb_lrTable <- renderDataTable({
    d<-p_bb_data();req(nrow(d)>0)
    datatable(d%>%group_by(Side=BatterSide)%>%bb_stats_fn(),
              options=dt_opts,rownames=FALSE)
  })
  output$p_bb_ptLrTable <- renderDataTable({
    d<-p_bb_data();req(nrow(d)>0)
    datatable(d%>%group_by(Pitch=TaggedPitchType,Side=BatterSide)%>%bb_stats_fn(),
              options=dt_opts,rownames=FALSE)
  })

  # ==========================================================================
  #  SCOUTING REPORTS
  # ==========================================================================

  sr_season <- reactive({ input$sr_season %||% "2026" })

  observeEvent(sr_season(), {
    session$sendCustomMessage("setSeasonBtns",
      list(prefix="sr", season=sr_season()))
  })

  # Pitcher / hitter section options
  pitcher_sections <- c(
    "Arsenal Table"          = "arsenal",
    "Movement Chart"         = "movement",
    "Release Points"         = "release",
    "Heat Maps (vs RHH)"     = "heatmap_r",
    "Heat Maps (vs LHH)"     = "heatmap_l",
    "Heat Maps (Combined)"   = "heatmap_all",
    "Velocity & Spin Table"  = "velo_spin",
    "L/R Splits"             = "splits",
    "Count Splits"           = "count_splits",
    "Batted Ball Rates"      = "batted_ball",
    "Pitch Sequencing Matrix"= "sequencing"
  )

  hitter_sections <- c(
    "Stat Summary"           = "summary",
    "Spray Chart"            = "spray",
    "Plate Discipline Scatter"= "pd_scatter",
    "Plate Discipline Stats" = "pd_stats",
    "Heat Map (vs RHP)"      = "heatmap_r",
    "Heat Map (vs LHP)"      = "heatmap_l",
    "Heat Map (Combined)"    = "heatmap_all",
    "L/R Splits Table"       = "splits",
    "Pitch Type Splits"      = "pt_splits",
    "Batted Ball Rates"      = "batted_ball",
    "wOBA Trend"             = "woba_trend"
  )

  output$sr_sections_ui <- renderUI({
    sections <- if (input$sr_type == "pitcher") pitcher_sections else hitter_sections
    checkboxGroupInput("sr_sections", NULL,
                       choices  = sections,
                       selected = sections[1:4])
  })

  output$sr_team_ui <- renderUI({
    req(!is.null(hitters_data()), !is.null(p_data_r()))
    if (input$sr_type == "pitcher") {
      teams <- p_data_r() %>%
        filter(Season == as.integer(sr_season())) %>%
        pull(PitcherTeamFull) %>% unique() %>% sort()
    } else {
      teams <- hitters_data() %>%
        filter(Season == as.integer(sr_season())) %>%
        pull(BatterTeamFull) %>% unique() %>% sort()
    }
    selectInput("sr_team","Select Team", choices=teams, selectize=TRUE)
  })

  output$sr_players_ui <- renderUI({
    req(input$sr_team)
    if (input$sr_type == "pitcher") {
      players <- p_data_r() %>%
        filter(Season==as.integer(sr_season()),
               PitcherTeamFull==input$sr_team) %>%
        pull(Pitcher) %>% unique() %>% sort()
    } else {
      players <- hitters_data() %>%
        filter(Season==as.integer(sr_season()),
               BatterTeamFull==input$sr_team) %>%
        pull(Batter) %>% unique() %>% sort()
    }
    checkboxGroupInput("sr_players","Select Players",
                       choices=players, selected=players)
  })

  observeEvent(input$sr_sel_all, {
    req(input$sr_team)
    if (input$sr_type == "pitcher") {
      players <- p_data_r() %>%
        filter(Season==as.integer(sr_season()),
               PitcherTeamFull==input$sr_team) %>%
        pull(Pitcher) %>% unique() %>% sort()
    } else {
      players <- hitters_data() %>%
        filter(Season==as.integer(sr_season()),
               BatterTeamFull==input$sr_team) %>%
        pull(Batter) %>% unique() %>% sort()
    }
    updateCheckboxGroupInput(session,"sr_players",selected=players)
  })
  observeEvent(input$sr_sel_none,{
    updateCheckboxGroupInput(session,"sr_players",selected=character(0))
  })

  output$sr_status_ui <- renderUI({
    req(input$sr_players)
    n <- length(input$sr_players)
    s <- length(input$sr_sections %||% character(0))
    tags$p(style="color:#8892b0;font-size:12px;",
           paste0(n," player(s) · ",s," section(s) selected"))
  })

  # ── Helper: build one pitcher page as ggplot list ─────────────────────────
  build_pitcher_plots <- function(pitcher_name, sections, p_df, p_seqs_df) {
    d <- p_df %>% filter(Pitcher == pitcher_name)
    if (nrow(d) == 0) return(list())

    pitch_pal <- c(
      Fastball="#333333","4-Seam Fastball"="#333333","Four-Seam"="#333333",
      Sinker="#555555","Two-Seam"="#555555",Cutter="#888800",
      Slider="#005588",Curveball="#660088",Sweeper="#006644",
      Changeup="#008844",Splitter="#880044",Other="#999999"
    )

    plots <- list()

    if ("arsenal" %in% sections) {
      tbl <- d %>% group_by(Pitch=TaggedPitchType) %>%
        summarise(
          N=n(), Usage=paste0(round(n()/nrow(d)*100,1),"%"),
          `Avg Velo`=round(mean(RelSpeed,na.rm=TRUE),1),
          `Max Velo`=round(max(RelSpeed,na.rm=TRUE),1),
          Spin=round(mean(SpinRate,na.rm=TRUE),0),
          IVB=round(mean(InducedVertBreak,na.rm=TRUE),1),
          HB=round(mean(HorzBreak,na.rm=TRUE),1),
          `CSW%`=paste0(round(mean(CSWCheck,na.rm=TRUE)*100,1),"%"),
          `Whiff%`={sw=sum(SwingCheck,na.rm=TRUE);paste0(if(sw>0)round(sum(WhiffCheck,na.rm=TRUE)/sw*100,1)else 0,"%")},
          .groups="drop"
        ) %>% arrange(desc(N))
      plots[["arsenal"]] <- list(type="table", data=tbl,
                                  title=paste(pitcher_name,"— Arsenal"))
    }

    if ("movement" %in% sections) {
      plots[["movement"]] <- list(type="plot",
        plot=ggplot(d,aes(x=HorzBreak,y=InducedVertBreak,color=TaggedPitchType))+
          geom_hline(yintercept=0,color="grey70",linewidth=.5)+
          geom_vline(xintercept=0,color="grey70",linewidth=.5)+
          geom_point(size=2,alpha=.6)+
          stat_ellipse(aes(group=TaggedPitchType),level=.68,linewidth=.5,linetype="dashed")+
          scale_color_manual(values=pitch_pal,na.value="grey60",name="Pitch")+
          xlim(-25,25)+ylim(-25,25)+
          labs(title=paste(pitcher_name,"— Movement"),
               x="Horizontal Break (in)",y="Induced Vertical Break (in)")+
          theme_bw(base_size=10)+theme(panel.grid.minor=element_blank())
      )
    }

    if ("release" %in% sections) {
      plots[["release"]] <- list(type="plot",
        plot=ggplot(d,aes(x=RelSide,y=RelHeight,color=TaggedPitchType))+
          geom_point(size=2,alpha=.6)+
          scale_color_manual(values=pitch_pal,na.value="grey60",name="Pitch")+
          xlim(-4,4)+ylim(2,7)+
          labs(title=paste(pitcher_name,"— Release Points"),
               x="Horizontal (ft)",y="Vertical (ft)")+
          theme_bw(base_size=10)+theme(panel.grid.minor=element_blank())
      )
    }

    # Heat maps
    hm_fn <- function(d_sub, title_str) {
      if (nrow(d_sub) < 5) return(NULL)
      ggplot(d_sub,aes(x=PlateLocSide,y=PlateLocHeight))+
        stat_density_2d(aes(fill=after_stat(density)),geom="raster",contour=FALSE)+
        scale_fill_gradient(low="white",high="#cc0000",guide="none")+
        annotate("rect",xmin=-1,xmax=1,ymin=1.6,ymax=3.4,
                 fill=NA,color="black",linewidth=.7)+
        ylim(1,4)+xlim(-1.8,1.8)+
        labs(title=title_str,x="Horizontal",y="Vertical")+
        theme_bw(base_size=10)+theme(panel.grid.minor=element_blank())
    }
    if ("heatmap_r"   %in% sections)
      plots[["heatmap_r"]]   <- list(type="plot",
        plot=hm_fn(d%>%filter(BatterSide=="Right"),paste(pitcher_name,"— Heat Map vs RHH")))
    if ("heatmap_l"   %in% sections)
      plots[["heatmap_l"]]   <- list(type="plot",
        plot=hm_fn(d%>%filter(BatterSide=="Left"), paste(pitcher_name,"— Heat Map vs LHH")))
    if ("heatmap_all" %in% sections)
      plots[["heatmap_all"]] <- list(type="plot",
        plot=hm_fn(d, paste(pitcher_name,"— Heat Map (Combined)")))

    if ("splits" %in% sections) {
      tbl <- d %>% group_by(Side=BatterSide) %>%
        summarise(
          BF=sum(PACheck,na.rm=TRUE),
          `K%`=paste0(round(sum(StrikeoutCheck,na.rm=TRUE)/max(BF,1)*100,1),"%"),
          `BB%`=paste0(round(sum(WalkCheck,na.rm=TRUE)/max(BF,1)*100,1),"%"),
          `CSW%`=paste0(round(mean(CSWCheck,na.rm=TRUE)*100,1),"%"),
          `Whiff%`={sw=sum(SwingCheck,na.rm=TRUE);paste0(if(sw>0)round(sum(WhiffCheck,na.rm=TRUE)/sw*100,1)else 0,"%")},
          `AVG`=sprintf("%.3f",ifelse(sum(ABCheck,na.rm=TRUE)>0,
                                       sum(HCheck,na.rm=TRUE)/sum(ABCheck,na.rm=TRUE),NA)),
          .groups="drop"
        )
      plots[["splits"]] <- list(type="table",data=tbl,
                                 title=paste(pitcher_name,"— L/R Splits"))
    }

    if ("count_splits" %in% sections) {
      tbl <- d %>% group_by(Count) %>%
        summarise(
          N=n(),
          `CSW%`=paste0(round(mean(CSWCheck,na.rm=TRUE)*100,1),"%"),
          `Zone%`=paste0(round(mean(ZoneCheck,na.rm=TRUE)*100,1),"%"),
          `Whiff%`={sw=sum(SwingCheck,na.rm=TRUE);paste0(if(sw>0)round(sum(WhiffCheck,na.rm=TRUE)/sw*100,1)else 0,"%")},
          .groups="drop"
        ) %>% arrange(Count)
      plots[["count_splits"]] <- list(type="table",data=tbl,
                                       title=paste(pitcher_name,"— Count Splits"))
    }

    if ("batted_ball" %in% sections) {
      bbe <- d %>% filter(TaggedHitType%in%c("GroundBall","FlyBall","LineDrive","Popup"))
      if (nrow(bbe) > 0) {
        tbl <- bbe %>% group_by(Side=BatterSide) %>%
          summarise(
            BBE=n(),
            `GB%`=paste0(round(sum(TaggedHitType=="GroundBall")/n()*100,1),"%"),
            `FB%`=paste0(round(sum(TaggedHitType=="FlyBall")/n()*100,1),"%"),
            `LD%`=paste0(round(sum(TaggedHitType=="LineDrive")/n()*100,1),"%"),
            `PU%`=paste0(round(sum(TaggedHitType=="Popup")/n()*100,1),"%"),
            `GB/FB`=as.character(round(sum(TaggedHitType=="GroundBall")/
                                        max(sum(TaggedHitType=="FlyBall"),1),2)),
            .groups="drop"
          )
        plots[["batted_ball"]] <- list(type="table",data=tbl,
                                        title=paste(pitcher_name,"— Batted Ball"))
      }
    }

    if ("velo_spin" %in% sections) {
      tbl <- d %>% group_by(Pitch=TaggedPitchType) %>%
        summarise(
          `Avg Velo`=round(mean(RelSpeed,na.rm=TRUE),1),
          `Max Velo`=round(max(RelSpeed,na.rm=TRUE),1),
          `Min Velo`=round(min(RelSpeed,na.rm=TRUE),1),
          `Avg Spin`=round(mean(SpinRate,na.rm=TRUE),0),
          `Max Spin`=round(max(SpinRate,na.rm=TRUE),0),
          .groups="drop"
        )
      plots[["velo_spin"]] <- list(type="table",data=tbl,
                                    title=paste(pitcher_name,"— Velocity & Spin"))
    }

    Filter(Negate(is.null), plots)
  }

  # ── Helper: build one hitter page ─────────────────────────────────────────
  build_hitter_plots <- function(batter_name, sections, h_df) {
    d <- h_df %>% filter(Batter == batter_name)
    if (nrow(d) == 0) return(list())

    plots <- list()

    if ("summary" %in% sections) {
      pa  <- n_distinct(d$PA_count)
      h   <- sum(d$PlayResult%in%c("Single","Double","Triple","HomeRun"))
      bb  <- sum(d$PlayResult=="Walk")
      hbp <- sum(d$PlayResult=="HitByPitch")
      ab  <- pa-bb-hbp
      tb  <- sum(d$PlayResult=="Single")+2*sum(d$PlayResult=="Double")+
             3*sum(d$PlayResult=="Triple")+4*sum(d$PlayResult=="HomeRun")
      tbl <- data.frame(
        Stat=c("PA","AB","H","2B","3B","HR","BB","HBP","BA","OBP","SLG","OPS","wOBA"),
        Value=c(pa,ab,h,
                sum(d$PlayResult=="Double"),sum(d$PlayResult=="Triple"),
                sum(d$PlayResult=="HomeRun"),bb,hbp,
                sprintf("%.3f",ifelse(ab>0,h/ab,NA)),
                sprintf("%.3f",ifelse(pa>0,(h+bb+hbp)/pa,NA)),
                sprintf("%.3f",ifelse(ab>0,tb/ab,NA)),
                sprintf("%.3f",ifelse(ab>0,(h+bb+hbp)/pa+tb/ab,NA)),
                sprintf("%.3f",sum(d$wOBA_contribution,na.rm=TRUE)/pa))
      )
      plots[["summary"]] <- list(type="table",data=tbl,
                                  title=paste(batter_name,"— Season Stats"))
    }

    if ("spray" %in% sections) {
      sd <- d %>%
        filter(!is.na(LastTrackedDistance),!is.na(Bearing),!is.na(PlayResult)) %>%
        mutate(bearing_rad=Bearing*pi/180,
               x=LastTrackedDistance*sin(bearing_rad),
               y=LastTrackedDistance*cos(bearing_rad))
      if (nrow(sd) > 0) {
        fl_x <- 330*sin(45*pi/180); fl_y <- fl_x
        arc_df <- function(r,a1,a2,n=200){
          a<-seq(a1,a2,length.out=n)*pi/180
          data.frame(x=r*sin(a),y=r*cos(a))
        }
        wall_poly <- rbind(
          data.frame(x=0,y=0),
          data.frame(x=-fl_x,y=fl_y),
          arc_df(330,-45,-20),arc_df(400,-20,20),arc_df(330,20,45),
          data.frame(x=fl_x,y=fl_y)
        )
        bases <- data.frame(bx=c(0,90/sqrt(2),0,-90/sqrt(2),0),
                             by=c(0,90/sqrt(2),2*90/sqrt(2),90/sqrt(2),0))
        col_map <- c(Single="#1a7a4a",Double="#1a4a7a",HomeRun="#aa0000",
                     Out="grey60",Triple="#6a1a7a",FieldersChoice="#aa6600",
                     Sacrifice="grey70",Error="#884400")
        plots[["spray"]] <- list(type="plot",
          plot=ggplot()+
            geom_polygon(data=wall_poly,aes(x=x,y=y),fill="#e8f5e9",color=NA)+
            geom_path(data=rbind(data.frame(x=-fl_x,y=fl_y),arc_df(330,-45,-20),
                                  arc_df(400,-20,20),arc_df(330,20,45),
                                  data.frame(x=fl_x,y=fl_y)),
                      aes(x=x,y=y),color="grey40",linewidth=1)+
            geom_segment(aes(x=0,y=0,xend=-fl_x,yend=fl_y),color="grey40")+
            geom_segment(aes(x=0,y=0,xend= fl_x,yend=fl_y),color="grey40")+
            geom_path(data=bases,aes(x=bx,y=by),color="grey30")+
            annotate("text",x=0,y=412,label="400",size=3,color="grey40")+
            annotate("text",x=-fl_x-12,y=fl_y+5,label="330",size=3,color="grey40")+
            annotate("text",x= fl_x+12,y=fl_y+5,label="330",size=3,color="grey40")+
            geom_point(data=sd,aes(x=x,y=y,color=PlayResult),size=3,alpha=.8)+
            scale_color_manual(values=col_map,name="Result")+
            coord_fixed(xlim=c(-280,280),ylim=c(-10,420))+
            labs(title=paste(batter_name,"— Spray Chart"))+
            theme_bw(base_size=10)+
            theme(axis.text=element_blank(),axis.title=element_blank(),
                  panel.grid=element_blank(),axis.ticks=element_blank())
        )
      }
    }

    if ("pd_stats" %in% sections) {
      pd <- d %>% filter(!is.na(PlateLocSide),!is.na(PlateLocHeight)) %>%
        mutate(InZone=dplyr::between(PlateLocHeight,1.59,3.41)&
                      dplyr::between(PlateLocSide,-1,1),
               IsSwing=PitchCall%in%c("FoulBall","StrikeSwinging","InPlay"),
               IsWhiff=PitchCall=="StrikeSwinging")
      if (nrow(pd) > 0) {
        tbl <- data.frame(
          Stat=c("Zone Swing%","Zone Contact%","Whiff%","Chase%","O-Swing%","Z-Contact%"),
          Value=c(
            paste0(round(mean(pd$IsSwing[pd$InZone],na.rm=TRUE)*100,1),"%"),
            {zs=pd%>%filter(InZone,IsSwing);paste0(round(mean(zs$PitchCall%in%c("FoulBall","InPlay"),na.rm=TRUE)*100,1),"%")},
            {sw=sum(pd$IsSwing);paste0(round(sum(pd$IsWhiff)/max(sw,1)*100,1),"%")},
            {oz=pd%>%filter(!InZone);paste0(round(mean(oz$IsSwing,na.rm=TRUE)*100,1),"%")},
            paste0(round(mean(pd$IsSwing[!pd$InZone],na.rm=TRUE)*100,1),"%"),
            {zs=pd%>%filter(InZone,IsSwing);paste0(round(mean(zs$PitchCall=="InPlay",na.rm=TRUE)*100,1),"%")}
          )
        )
        plots[["pd_stats"]] <- list(type="table",data=tbl,
                                     title=paste(batter_name,"— Plate Discipline"))
      }
    }

    if ("pd_scatter" %in% sections) {
      pd <- d %>%
        filter(!is.na(PlateLocSide),!is.na(PlateLocHeight)) %>%
        mutate(
          InZone  = dplyr::between(PlateLocHeight,1.59,3.41)&dplyr::between(PlateLocSide,-1,1),
          IsSwing = PitchCall%in%c("FoulBall","StrikeSwinging","InPlay"),
          IsWhiff = PitchCall=="StrikeSwinging",
          PD_side = -PlateLocSide,
          Outcome = dplyr::case_when(
            PitchCall%in%c("FoulBall","InPlay")&InZone ~ "Contact (zone)",
            IsWhiff&InZone                              ~ "Whiff (zone)",
            PitchCall=="StrikeCalled"                   ~ "Called Strike",
            PitchCall%in%c("BallCalled","HitByPitch")&InZone ~ "Ball (zone)",
            IsSwing&!InZone                             ~ "Chase",
            TRUE                                        ~ "Take (out of zone)"
          )
        )
      outcome_pal <- c("Contact (zone)"="#1a7a4a","Whiff (zone)"="#aa0000",
                       "Called Strike"="#886600","Ball (zone)"="#1a4a7a",
                       "Chase"="#aa5500","Take (out of zone)"="grey70")
      outcome_shape <- c("Contact (zone)"=16,"Whiff (zone)"=4,"Called Strike"=16,
                         "Ball (zone)"=16,"Chase"=16,"Take (out of zone)"=1)
      plots[["pd_scatter"]] <- list(type="plot",
        plot=ggplot(pd,aes(x=PD_side,y=PlateLocHeight,color=Outcome,shape=Outcome))+
          annotate("rect",xmin=-1,xmax=1,ymin=1.59,ymax=3.41,fill=NA,color="black",linewidth=.8)+
          annotate("segment",x=c(-1/3,1/3,-1,-1),xend=c(-1/3,1/3,1,1),
                   y=c(1.59,1.59,(3.41-1.59)/3+1.59,(3.41-1.59)*2/3+1.59),
                   yend=c(3.41,3.41,(3.41-1.59)/3+1.59,(3.41-1.59)*2/3+1.59),
                   color="grey80",linewidth=.3)+
          geom_jitter(size=2,alpha=.7,width=.01,height=.01)+
          scale_color_manual(values=outcome_pal,name="Outcome")+
          scale_shape_manual(values=outcome_shape,name="Outcome")+
          xlim(-2.2,2.2)+ylim(0.8,4.2)+
          labs(title=paste(batter_name,"— Plate Discipline"),
               x="Horizontal (Hitter's View)",y="Vertical (ft)")+
          theme_bw(base_size=10)+theme(panel.grid.minor=element_blank())
      )
    }

    hm_h_fn <- function(d_sub, title_str) {
      if (nrow(d_sub) < 5) return(NULL)
      ggplot(d_sub,aes(x=-PlateLocSide,y=PlateLocHeight))+
        stat_density_2d(aes(fill=after_stat(density)),geom="raster",contour=FALSE)+
        scale_fill_gradient(low="white",high="#cc0000",guide="none")+
        annotate("rect",xmin=-1,xmax=1,ymin=1.6,ymax=3.4,
                 fill=NA,color="black",linewidth=.7)+
        ylim(1,4)+xlim(-1.8,1.8)+
        labs(title=title_str,x="Horizontal (Hitter's View)",y="Vertical")+
        theme_bw(base_size=10)+theme(panel.grid.minor=element_blank())
    }
    if ("heatmap_r"   %in% sections)
      plots[["heatmap_r"]]   <- list(type="plot",
        plot=hm_h_fn(d%>%filter(!is.na(PlateLocSide),!is.na(PlateLocHeight),
                                  PitcherThrows=="Right"),
                     paste(batter_name,"— Heat Map vs RHP")))
    if ("heatmap_l"   %in% sections)
      plots[["heatmap_l"]]   <- list(type="plot",
        plot=hm_h_fn(d%>%filter(!is.na(PlateLocSide),!is.na(PlateLocHeight),
                                  PitcherThrows=="Left"),
                     paste(batter_name,"— Heat Map vs LHP")))
    if ("heatmap_all" %in% sections)
      plots[["heatmap_all"]] <- list(type="plot",
        plot=hm_h_fn(d%>%filter(!is.na(PlateLocSide),!is.na(PlateLocHeight)),
                     paste(batter_name,"— Heat Map (Combined)")))

    if ("splits" %in% sections) {
      tbl <- d %>% group_by(Throws=PitcherThrows) %>%
        summarise(
          PA=n(),H=sum(PlayResult%in%c("Single","Double","Triple","HomeRun")),
          HR=sum(PlayResult=="HomeRun"),BB=sum(PlayResult=="Walk"),
          AB=PA-BB-sum(PlayResult=="HitByPitch"),
          BA=sprintf("%.3f",ifelse(AB>0,H/AB,NA)),
          OBP=sprintf("%.3f",ifelse(PA>0,(H+BB+sum(PlayResult=="HitByPitch"))/PA,NA)),
          wOBA=sprintf("%.3f",sum(wOBA_contribution,na.rm=TRUE)/PA),
          .groups="drop"
        )
      plots[["splits"]] <- list(type="table",data=tbl,
                                 title=paste(batter_name,"— L/R Splits"))
    }

    if ("pt_splits" %in% sections) {
      tbl <- d %>% group_by(PitchType=TaggedPitchType) %>%
        summarise(
          PA=n(),H=sum(PlayResult%in%c("Single","Double","Triple","HomeRun")),
          HR=sum(PlayResult=="HomeRun"),BB=sum(PlayResult=="Walk"),
          AB=PA-BB-sum(PlayResult=="HitByPitch"),
          BA=sprintf("%.3f",ifelse(AB>0,H/AB,NA)),
          wOBA=sprintf("%.3f",sum(wOBA_contribution,na.rm=TRUE)/PA),
          .groups="drop"
        ) %>% arrange(desc(PA))
      plots[["pt_splits"]] <- list(type="table",data=tbl,
                                    title=paste(batter_name,"— Splits by Pitch Type"))
    }

    if ("batted_ball" %in% sections) {
      bbe <- d %>% filter(TaggedHitType%in%c("GroundBall","FlyBall","LineDrive","Popup"))
      if (nrow(bbe) > 0) {
        tbl <- data.frame(
          Stat=c("BBE","GB%","FB%","LD%","PU%","GB/FB"),
          Value=c(nrow(bbe),
                  paste0(round(sum(bbe$TaggedHitType=="GroundBall")/nrow(bbe)*100,1),"%"),
                  paste0(round(sum(bbe$TaggedHitType=="FlyBall")/nrow(bbe)*100,1),"%"),
                  paste0(round(sum(bbe$TaggedHitType=="LineDrive")/nrow(bbe)*100,1),"%"),
                  paste0(round(sum(bbe$TaggedHitType=="Popup")/nrow(bbe)*100,1),"%"),
                  as.character(round(sum(bbe$TaggedHitType=="GroundBall")/
                                     max(sum(bbe$TaggedHitType=="FlyBall"),1),2)))
        )
        plots[["batted_ball"]] <- list(type="table",data=tbl,
                                        title=paste(batter_name,"— Batted Ball"))
      }
    }

    Filter(Negate(is.null), plots)
  }

  # ── Preview (first selected player) ──────────────────────────────────────
  output$sr_preview_ui <- renderUI({
    req(input$sr_players, length(input$sr_players)>0,
        input$sr_sections, length(input$sr_sections)>0)
    player <- input$sr_players[1]
    sections <- input$sr_sections

    if (input$sr_type == "pitcher") {
      d <- p_data_r() %>% filter(Season==as.integer(sr_season()))
      plots <- build_pitcher_plots(player, sections, d, p_seqs_r())
    } else {
      d <- hitters_data() %>% filter(Season==as.integer(sr_season()))
      plots <- build_hitter_plots(player, sections, d)
    }

    if (length(plots) == 0)
      return(tags$p(style="color:#8892b0;","No data available for this player."))

    items <- lapply(plots, function(item) {
      if (item$type == "table") {
        tagList(
          tags$h4(style="color:#fff;font-weight:700;margin:16px 0 8px;",
                  item$title),
          renderTable(item$data, striped=TRUE, hover=TRUE)
        )
      } else if (item$type == "plot" && !is.null(item$plot)) {
        tagList(
          renderPlot(item$plot, height=320)
        )
      }
    })
    tagList(
      tags$h3(style="color:#ff4655;font-weight:700;margin-bottom:16px;", player),
      tagList(items)
    )
  })

  # ── PDF download ──────────────────────────────────────────────────────────
  output$sr_download <- downloadHandler(
    filename = function() {
      paste0("NECBL_ScoutingReport_",input$sr_team,"_",
             sr_season(),"_",format(Sys.Date(),"%Y%m%d"),".pdf")
    },
    content = function(file) {
      req(input$sr_players, length(input$sr_players)>0)

      sections <- input$sr_sections %||% character(0)
      if (length(sections) == 0) {
        showNotification("Select at least one section.", type="warning"); return()
      }

      if (input$sr_type == "pitcher") {
        d_all <- p_data_r() %>% filter(Season==as.integer(sr_season()))
      } else {
        d_all <- hitters_data() %>% filter(Season==as.integer(sr_season()))
      }

      # Build temp R Markdown and knit to PDF
      tmp_rmd <- tempfile(fileext=".Rmd")
      tmp_dir <- dirname(tmp_rmd)

      # Write Rmd
      rmd_lines <- c(
        "---",
        paste0('title: "NECBL Scouting Report — ',input$sr_team,' (',sr_season(),')"'),
        'output:',
        '  pdf_document:',
        '    latex_engine: xelatex',
        '    toc: false',
        '    fig_width: 6',
        '    fig_height: 3.5',
        'geometry: margin=1in',
        'fontsize: 10pt',
        "---",
        "",
        "```{r setup, include=FALSE}",
        "knitr::opts_chunk$set(echo=FALSE, warning=FALSE, message=FALSE,",
        "                      fig.width=6, fig.height=3.5)",
        "library(ggplot2)",
        "library(dplyr)",
        "library(knitr)",
        "```",
        ""
      )

      for (player in input$sr_players) {
        rmd_lines <- c(rmd_lines,
          paste0("\\newpage"),
          paste0("# ", player),
          paste0("**Team:** ", input$sr_team,
                 " | **Season:** ", sr_season()),
          ""
        )

        if (input$sr_type == "pitcher") {
          plots <- build_pitcher_plots(player, sections, d_all, p_seqs_r())
        } else {
          plots <- build_hitter_plots(player, sections, d_all)
        }

        for (nm in names(plots)) {
          item <- plots[[nm]]
          obj_name <- paste0("obj_", gsub("[^a-zA-Z0-9]","_",player), "_", nm)

          if (item$type == "table") {
            assign(obj_name, item$data, envir=.GlobalEnv)
            rmd_lines <- c(rmd_lines,
              paste0("## ", item$title),
              paste0("```{r}"),
              paste0("kable(", obj_name, ", format='latex', booktabs=TRUE)"),
              "```",
              ""
            )
          } else if (item$type == "plot" && !is.null(item$plot)) {
            assign(obj_name, item$plot, envir=.GlobalEnv)
            rmd_lines <- c(rmd_lines,
              paste0("```{r fig.", obj_name, ", fig.height=3.5}"),
              paste0("print(", obj_name, ")"),
              "```",
              ""
            )
          }
        }
      }

      writeLines(rmd_lines, tmp_rmd)

      tryCatch({
        rmarkdown::render(tmp_rmd, output_file=file, quiet=TRUE)
      }, error=function(e) {
        showNotification(paste("PDF error:", e$message), type="error", duration=8)
      })
    }
  )
  output$lb_hitters <- renderDataTable({
    req(!is.null(hitters_data()))
    d <- hitters_data()%>%filter(Season==as.integer(lb_season()))
    min_pa <- input$lb_min_pa%||%20
    tbl <- d%>%group_by(Batter,Team=BatterTeamFull)%>%
      summarise(
        PA=n_distinct(PA_count),
        H=sum(PlayResult%in%c("Single","Double","Triple","HomeRun")),
        `1B`=sum(PlayResult=="Single"),`2B`=sum(PlayResult=="Double"),
        `3B`=sum(PlayResult=="Triple"),HR=sum(PlayResult=="HomeRun"),
        BB=sum(PlayResult=="Walk"),HBP=sum(PlayResult=="HitByPitch"),
        AB=PA-BB-HBP,TB=`1B`+2*`2B`+3*`3B`+4*HR,
        BA=ifelse(AB>0,round(H/AB,3),NA),
        OBP=ifelse(PA>0,round((H+BB+HBP)/PA,3),NA),
        SLG=ifelse(AB>0,round(TB/AB,3),NA),
        wOBA=round(sum(wOBA_contribution,na.rm=TRUE)/PA,3),
        .groups="drop"
      )%>%
      filter(PA>=min_pa)%>%
      arrange(desc(wOBA))%>%
      dplyr::select(Batter,Team,PA,BA,OBP,SLG,wOBA,HR,`2B`,`3B`,BB)
    datatable(tbl,options=dt_opts,rownames=FALSE)
  })

  output$lb_pitchers <- renderDataTable({
    req(!is.null(p_data_r()))
    d <- p_data_r()%>%filter(Season==as.integer(lb_season()))
    min_bf <- input$lb_min_bf%||%30
    tbl <- d%>%group_by(Pitcher,Team=PitcherTeamFull)%>%
      summarise(
        Pitches=n(),BF=sum(PACheck,na.rm=TRUE),
        SO=sum(StrikeoutCheck,na.rm=TRUE),BB=sum(WalkCheck,na.rm=TRUE),
        H=sum(HCheck,na.rm=TRUE),
        `CSW%`=round(mean(CSWCheck,na.rm=TRUE)*100,1),
        `Zone%`=round(mean(ZoneCheck,na.rm=TRUE)*100,1),
        `Whiff%`={sw=sum(SwingCheck,na.rm=TRUE);
                  round(if(sw>0)sum(WhiffCheck,na.rm=TRUE)/sw*100 else 0,1)},
        `Avg Velo`=round(mean(RelSpeed,na.rm=TRUE),1),
        .groups="drop"
      )%>%
      filter(BF>=min_bf)%>%
      arrange(desc(`CSW%`))%>%
      dplyr::select(Pitcher,Team,Pitches,BF,SO,BB,`CSW%`,`Zone%`,`Whiff%`,`Avg Velo`)
    datatable(tbl,options=dt_opts,rownames=FALSE)
  })

  output$lb_team_hitting <- renderDataTable({
    req(!is.null(hitters_data()))
    d <- hitters_data()%>%filter(Season==as.integer(lb_season()))
    tbl <- d%>%group_by(Team=BatterTeamFull)%>%
      summarise(
        PA   = n(),
        H    = sum(PlayResult%in%c("Single","Double","Triple","HomeRun")),
        `1B` = sum(PlayResult=="Single"),
        `2B` = sum(PlayResult=="Double"),
        `3B` = sum(PlayResult=="Triple"),
        HR   = sum(PlayResult=="HomeRun"),
        BB   = sum(PlayResult=="Walk"),
        HBP  = sum(PlayResult=="HitByPitch"),
        AB   = PA-BB-HBP,
        TB   = `1B`+2*`2B`+3*`3B`+4*HR,
        R_PA = PA,
        BA   = ifelse(AB>0,  round(H/AB,3),  NA),
        OBP  = ifelse(PA>0,  round((H+BB+HBP)/PA,3), NA),
        SLG  = ifelse(AB>0,  round(TB/AB,3), NA),
        OPS  = ifelse(!is.na(OBP)&!is.na(SLG), round(OBP+SLG,3), NA),
        wOBA = round(sum(wOBA_contribution,na.rm=TRUE)/PA,3),
        `GB%`= {bbe=sum(TaggedHitType%in%c("GroundBall","FlyBall","LineDrive","Popup"),na.rm=TRUE);
                ifelse(bbe>0,paste0(round(sum(TaggedHitType=="GroundBall",na.rm=TRUE)/bbe*100,1),"%"),"—")},
        `FB%`= {bbe=sum(TaggedHitType%in%c("GroundBall","FlyBall","LineDrive","Popup"),na.rm=TRUE);
                ifelse(bbe>0,paste0(round(sum(TaggedHitType=="FlyBall",na.rm=TRUE)/bbe*100,1),"%"),"—")},
        `LD%`= {bbe=sum(TaggedHitType%in%c("GroundBall","FlyBall","LineDrive","Popup"),na.rm=TRUE);
                ifelse(bbe>0,paste0(round(sum(TaggedHitType=="LineDrive",na.rm=TRUE)/bbe*100,1),"%"),"—")},
        `K%` = ifelse(PA>0,paste0(round(sum(KorBB=="Strikeout",na.rm=TRUE)/PA*100,1),"%"),"—"),
        `BB%`= ifelse(PA>0,paste0(round(BB/PA*100,1),"%"),"—"),
        .groups="drop"
      )%>%
      arrange(desc(wOBA))%>%
      dplyr::select(Team,PA,BA,OBP,SLG,OPS,wOBA,HR,`2B`,BB,`K%`,`BB%`,`GB%`,`FB%`,`LD%`)
    datatable(
      tbl,
      options=c(dt_opts, list(dom="ft", order=list(list(6,"desc")))),
      rownames=FALSE
    )
  })

  output$lb_team_pitching <- renderDataTable({
    req(!is.null(p_data_r()))
    d <- p_data_r()%>%filter(Season==as.integer(lb_season()))
    tbl <- d%>%group_by(Team=PitcherTeamFull)%>%
      summarise(
        Pitches  = n(),
        BF       = sum(PACheck,na.rm=TRUE),
        SO       = sum(StrikeoutCheck,na.rm=TRUE),
        BB       = sum(WalkCheck,na.rm=TRUE),
        H        = sum(HCheck,na.rm=TRUE),
        HR       = sum(PlayResult=="HomeRun",na.rm=TRUE),
        `K%`     = ifelse(BF>0,paste0(round(SO/BF*100,1),"%"),"—"),
        `BB%`    = ifelse(BF>0,paste0(round(BB/BF*100,1),"%"),"—"),
        `K-BB%`  = ifelse(BF>0,paste0(round((SO-BB)/BF*100,1),"%"),"—"),
        `CSW%`   = paste0(round(mean(CSWCheck,na.rm=TRUE)*100,1),"%"),
        `Zone%`  = paste0(round(mean(ZoneCheck,na.rm=TRUE)*100,1),"%"),
        `Whiff%` = {sw=sum(SwingCheck,na.rm=TRUE);
                    paste0(if(sw>0)round(sum(WhiffCheck,na.rm=TRUE)/sw*100,1)else 0,"%")},
        `Chase%` = {oz=sum(!ZoneCheck,na.rm=TRUE);
                    paste0(if(oz>0)round(sum(SwingCheck[!ZoneCheck],na.rm=TRUE)/oz*100,1)else 0,"%")},
        `Avg Velo`= round(mean(RelSpeed,na.rm=TRUE),1),
        `GB%`    = {bbe=sum(TaggedHitType%in%c("GroundBall","FlyBall","LineDrive","Popup"),na.rm=TRUE);
                    ifelse(bbe>0,paste0(round(sum(TaggedHitType=="GroundBall",na.rm=TRUE)/bbe*100,1),"%"),"—")},
        `FB%`    = {bbe=sum(TaggedHitType%in%c("GroundBall","FlyBall","LineDrive","Popup"),na.rm=TRUE);
                    ifelse(bbe>0,paste0(round(sum(TaggedHitType=="FlyBall",na.rm=TRUE)/bbe*100,1),"%"),"—")},
        .groups="drop"
      )%>%
      arrange(desc(`CSW%`))%>%
      dplyr::select(Team,Pitches,BF,SO,BB,`K%`,`BB%`,`K-BB%`,
                    `CSW%`,`Zone%`,`Whiff%`,`Chase%`,`Avg Velo`,`GB%`,`FB%`)
    datatable(
      tbl,
      options=c(dt_opts, list(dom="ft", order=list(list(8,"desc")))),
      rownames=FALSE
    )
  })
}

shinyApp(ui=ui, server=server)
