required_pkgs <- c(
  "shiny","bslib","DT","ggplot2","dplyr","readr","data.table",
  "scales","jsonlite","plotly","leaflet","sf","htmltools",
  "rmarkdown","callr","tidyr"
)
missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, quietly=TRUE, FUN.VALUE=logical(1))]
if(length(missing_pkgs)){
  stop(
    paste0("Missing required packages: ",paste(missing_pkgs,collapse=", "),
           "\nRun source('install_packages.R') first."),
    call.=FALSE
  )
}

library(shiny)
library(bslib)
library(DT)
library(ggplot2)
library(dplyr)
library(readr)
library(data.table)
library(scales)
library(jsonlite)
library(plotly)
library(leaflet)
library(sf)
library(htmltools)

source(file.path("R","data.R"), local=FALSE)
source(file.path("R","helpers.R"), local=FALSE)
source(file.path("R","data_helpers.R"), local=FALSE)

theme <- bs_theme(
  version=5, bg="#071A2B", fg="#F4F8FC",
  primary="#1AB8FF", secondary="#2AD4C9",
  base_font=font_google("Source Sans 3"),
  heading_font=font_google("Merriweather Sans")
)

sidebar_ui <- sidebar(
  width=285,
  open="desktop",
  div(class="brand-block",
      tags$img(src="logo_unpad.png",class="brand-logo"),
      div(class="brand-univ","UNIVERSITAS"),
      div(class="brand-univ","PADJADJARAN"),
      div(class="brand-dept","Department of Statistics · FMIPA")),
  hr(class="side-separator"),
  navset_pill_list(
    id="main_nav",
    nav_panel("Overview",value="overview",icon=icon("house")),
    nav_panel("Live Spatial Map",value="live_map",icon=icon("map-location-dot")),
    nav_panel("Raw Data Explorer",value="explorer",icon=icon("table")),
    nav_panel("Analysis Runner",value="runner",icon=icon("play")),
    nav_panel("District Ranking",value="ranking",icon=icon("chart-column")),
    nav_panel("Sensitivity",value="sensitivity",icon=icon("sliders")),
    nav_panel("INLA Diagnostics",value="inla",icon=icon("chart-line")),
    nav_panel("Bayesian Hazard & Screening",value="bayesrisk",icon=icon("layer-group")),
    nav_panel("Methods & Limits",value="methods",icon=icon("book-open")),
    nav_panel("Downloads",value="downloads",icon=icon("download"))
  ),
  div(class="appearance-panel compact-appearance",
      div(class="appearance-title",icon("palette"),tags$span("Appearance")),
      selectInput("theme_choice",NULL,
                  choices=c("Midnight Blue"="midnight","Ocean Teal"="ocean",
                            "Sky Blue"="sky","Graphite Copper"="graphite","Light Academic"="light"),
                  selected="midnight"),
      tags$details(
        class="appearance-details",
        tags$summary("Advanced display controls"),
        sliderInput("font_scale","Text scale",min=.90,max=1.15,value=1,step=.05),
        sliderInput("hero_height","Hero height",min=280,max=420,value=330,step=10)
      )
  ),
  div(class="sidebar-footer",
      div(class="mini-rule"),
      div(class="small text-muted","Complete-data live analytics"),
      div(class="small text-muted",paste("Version",APP_VERSION)))
)

hero_ui <- div(
  class="hero-shell",
  div(class="hero-overlay"),
  div(class="hero-copy",
      h1(span("Lembang Fault Geo-Information",class="hero-title-main"),br(),
         span("Screening Dashboard",class="hero-title-accent")),
      div(class="hero-subtitle",
          "Bayesian Geo-Information Screening of Seismic Shaking, Exposure, and Vulnerability in the Lembang Fault Region, West Java, Indonesia"),
      div(class="hero-author",
          HTML("Salwa Azzah Imtiyaz, I Gede Nyoman Mindra Jaya<sup>*</sup>, and Yusep Suparman")),
      div(class="hero-affil",
          "Department of Statistics, Faculty of Mathematics and Natural Sciences, Universitas Padjadjaran, Jatinangor, Sumedang 45363, Indonesia"),
      div(class="hero-chip-row",
          div(class="hero-chip",icon("database"),tags$span("Complete datasets")),
          div(class="hero-chip",icon("map-location-dot"),tags$span("Runtime-generated maps")),
          div(class="hero-chip",icon("play"),tags$span("Reproducible analysis")))
  )
)

ui <- page_sidebar(
  title=div(class="top-title","IJGI Companion Dashboard · Professional Live Analytics"),
  sidebar=sidebar_ui,
  theme=theme,
  fillable=TRUE,
  tags$head(
    tags$link(rel="stylesheet",href="styles.css"),
    tags$link(rel="stylesheet",href="https://unpkg.com/maplibre-gl@4.7.1/dist/maplibre-gl.css"),
    tags$script(src="https://unpkg.com/maplibre-gl@4.7.1/dist/maplibre-gl.js"),
    tags$style(HTML("
      .ijgi-map-mode-toolbar{display:flex;align-items:flex-end;gap:12px;flex-wrap:wrap;}
      .ijgi-map3d-wrap{position:relative;width:100%;height:78vh;min-height:650px;border-radius:0 0 12px 12px;overflow:hidden;background:#0A1B2A;}
      #ijgi_map3d{position:absolute;inset:0;width:100%;height:100%;}
      .ijgi-map3d-badge{position:absolute;z-index:5;left:12px;bottom:12px;background:rgba(255,255,255,.93);color:#243746;padding:8px 10px;border-radius:9px;border:1px solid #d6e0e6;font-size:11px;line-height:1.4;box-shadow:0 3px 12px rgba(0,0,0,.18);pointer-events:none;}
      .ijgi-map3d-note{font-size:.78rem;color:var(--muted);margin-top:4px;line-height:1.35;}
      @media(max-width:900px){.ijgi-map3d-wrap{height:68vh;min-height:520px;}}
    ")),
    tags$script(HTML("
      Shiny.addCustomMessageHandler('ui-theme', function(x){
        document.documentElement.setAttribute('data-theme',x.theme);
        document.documentElement.style.setProperty('--font-scale',x.fontScale);
        document.documentElement.style.setProperty('--hero-height',x.heroHeight+'px');
      });

      function refreshIJGILeaflet(){
        setTimeout(function(){
          try{ var w=HTMLWidgets.find('#live_map'); if(w && w.getMap){w.getMap().invalidateSize(true);} }catch(e){}
          window.dispatchEvent(new Event('resize'));
        },120);
        setTimeout(function(){
          try{ var w=HTMLWidgets.find('#live_map'); if(w && w.getMap){w.getMap().invalidateSize(true);} }catch(e){}
          window.dispatchEvent(new Event('resize'));
        },500);
      }
      Shiny.addCustomMessageHandler('refreshLeaflet2D', function(x){refreshIJGILeaflet();});

      var ijgi3dMap = null;
      var ijgi3dLoaded = false;
      var ijgi3dPending = null;

      function ijgiSetGeoJSON(id, data){
        if(ijgi3dMap && ijgi3dMap.getSource(id)){ ijgi3dMap.getSource(id).setData(data); }
      }

      function initIJGI3D(payload){
        if(typeof maplibregl === 'undefined') return;
        var el = document.getElementById('ijgi_map3d');
        if(!el) return;
        if(ijgi3dMap) return;

        ijgi3dMap = new maplibregl.Map({
          container:'ijgi_map3d',
          center:[payload.centerLon,payload.centerLat],
          zoom:payload.zoom || 8.7,
          pitch:58,
          bearing:-18,
          maxPitch:85,
          style:{
            version:8,
            sources:{
              baseRaster:{type:'raster',tiles:[payload.tileUrl],tileSize:256,attribution:payload.attribution,maxzoom:payload.maxZoom || 19},
              terrainDEM:{type:'raster-dem',tiles:['https://s3.amazonaws.com/elevation-tiles-prod/terrarium/{z}/{x}/{y}.png'],tileSize:256,encoding:'terrarium',maxzoom:15}
            },
            layers:[
              {id:'base-raster',type:'raster',source:'baseRaster'},
              {id:'terrain-hillshade',type:'hillshade',source:'terrainDEM',paint:{'hillshade-exaggeration':0.36}}
            ]
          }
        });

        ijgi3dMap.addControl(new maplibregl.NavigationControl({visualizePitch:true}),'top-left');
        ijgi3dMap.addControl(new maplibregl.FullscreenControl(),'top-left');
        ijgi3dMap.addControl(new maplibregl.ScaleControl({maxWidth:120,unit:'metric'}),'bottom-right');

        ijgi3dMap.on('load',function(){
          ijgi3dLoaded = true;
          ijgi3dMap.setTerrain({source:'terrainDEM',exaggeration:payload.exaggeration || 1.6});

          ijgi3dMap.addSource('ijgi-boundaries',{type:'geojson',data:{type:'FeatureCollection',features:[]}});
          ijgi3dMap.addSource('ijgi-fault',{type:'geojson',data:{type:'FeatureCollection',features:[]}});
          ijgi3dMap.addSource('ijgi-points',{type:'geojson',data:{type:'FeatureCollection',features:[]}});

          ijgi3dMap.addLayer({id:'ijgi-boundary-line',type:'line',source:'ijgi-boundaries',paint:{'line-color':'#D7E7F0','line-width':1.0,'line-opacity':0.78}});
          ijgi3dMap.addLayer({id:'ijgi-fault-line',type:'line',source:'ijgi-fault',paint:{'line-color':'#FF3B30','line-width':5}});
          ijgi3dMap.addLayer({id:'ijgi-points-layer',type:'circle',source:'ijgi-points',paint:{
            'circle-radius':['interpolate',['linear'],['get','norm'],0,4,1,11],
            'circle-color':['interpolate',['linear'],['get','norm'],0,'#2C7BB6',0.25,'#00A6CA',0.5,'#F9D057',0.75,'#F29E2E',1,'#D7191C'],
            'circle-stroke-color':'#102A3B','circle-stroke-width':1.0,'circle-opacity':0.90
          }});

          ijgi3dMap.on('click','ijgi-points-layer',function(e){
            if(!e.features || !e.features.length) return;
            var p=e.features[0].properties || {};
            var html='<b>'+(p.label || 'Location')+'</b>';
            if(p.valueLabel) html += '<br>'+p.valueLabel;
            new maplibregl.Popup().setLngLat(e.lngLat).setHTML(html).addTo(ijgi3dMap);
          });
          ijgi3dMap.on('mouseenter','ijgi-points-layer',function(){ijgi3dMap.getCanvas().style.cursor='pointer';});
          ijgi3dMap.on('mouseleave','ijgi-points-layer',function(){ijgi3dMap.getCanvas().style.cursor='';});

          if(ijgi3dPending){ updateIJGI3D(ijgi3dPending); ijgi3dPending=null; }
          else updateIJGI3D(payload);
        });
      }

      function updateIJGI3D(payload){
        if(!ijgi3dMap){ initIJGI3D(payload); if(!ijgi3dMap) return; }
        if(!ijgi3dLoaded){ ijgi3dPending=payload; return; }

        var base=ijgi3dMap.getSource('baseRaster');
        if(base && base.setTiles) base.setTiles([payload.tileUrl]);
        ijgi3dMap.setTerrain({source:'terrainDEM',exaggeration:payload.exaggeration || 1.6});
        ijgiSetGeoJSON('ijgi-boundaries',{type:'FeatureCollection',features:payload.boundaries || []});
        ijgiSetGeoJSON('ijgi-fault',{type:'FeatureCollection',features:[{type:'Feature',properties:{name:'Lembang Fault'},geometry:{type:'LineString',coordinates:payload.fault || []}}]});
        ijgiSetGeoJSON('ijgi-points',{type:'FeatureCollection',features:(payload.points || []).map(function(p){
          return {type:'Feature',properties:{label:p.label,norm:p.norm,valueLabel:p.valueLabel},geometry:{type:'Point',coordinates:[p.lon,p.lat]}};
        })});
        ijgi3dMap.easeTo({center:[payload.centerLon,payload.centerLat],zoom:payload.zoom || 8.7,duration:500});
        setTimeout(function(){ if(ijgi3dMap) ijgi3dMap.resize(); },120);
      }

      function registerIJGI3DHandlers(){
        if(!window.Shiny || window.__ijgi3dHandlersRegistered) return;
        window.__ijgi3dHandlersRegistered=true;
        Shiny.addCustomMessageHandler('updateIJGI3D',function(payload){updateIJGI3D(payload);});
        Shiny.addCustomMessageHandler('resizeIJGI3D',function(){if(ijgi3dMap)setTimeout(function(){ijgi3dMap.resize();},120);});
      }
      document.addEventListener('shiny:connected',registerIJGI3DHandlers);
      setTimeout(registerIJGI3DHandlers,600);
    "))
  ),

  conditionalPanel(
    "input.main_nav == 'overview'",
    hero_ui,
    div(class="kpi-grid",
        div(class="kpi-card kpi-red",div(class="kpi-icon-wrap",icon("location-dot")),
            div(class="kpi-copy",div(class="kpi-title","Districts"),div(class="kpi-value","227"))),
        div(class="kpi-card kpi-blue",div(class="kpi-icon-wrap",icon("calendar-days")),
            div(class="kpi-copy",div(class="kpi-title","District-year records"),div(class="kpi-value","1,596"))),
        div(class="kpi-card kpi-teal",div(class="kpi-icon-wrap",icon("table-cells-large")),
            div(class="kpi-copy",div(class="kpi-title","Vs30 grid records"),div(class="kpi-value","78,000"))),
        div(class="kpi-card kpi-orange",div(class="kpi-icon-wrap",icon("database")),
            div(class="kpi-copy",div(class="kpi-title","Catalog source records"),div(class="kpi-value","217,807")))
    ),
    layout_columns(
      card(class="glass-card",
           card_header("Complete analytical application"),
           p("All analytical maps are generated at runtime from the bundled GeoPackage, KMZ, earthquake catalog, Vs30 grid, and BPS panel. The hero graphic is decorative only."),
           tags$ul(
             tags$li("Live Lembang Fault and district map"),
             tags$li("Live earthquake-event exploration"),
             tags$li("Live Vs30 map"),
             tags$li("Live BPS choropleth"),
             tags$li("Live scenario PGA surface"),
             tags$li("Full INLA workflow available from INLA Diagnostics")
           )),
      card(class="glass-card",
           card_header("Scientific interpretation"),
           p("The application is designed for reproducible geo-information screening and exploratory analysis."),
           div(class="note-box","The integrated score is comparative. It is not an earthquake early-warning product, occurrence forecast, casualty model, or economic-loss model.")),
      col_widths=c(7,5)
    )
  ),

  conditionalPanel(
    "input.main_nav == 'live_map'",
    div(class="page-heading",h2("Live Spatial Analytics"),
        p("All analytical map layers are generated at runtime from the bundled raw data.")),
    card(
      class="glass-card live-control-card",
      layout_columns(
        div(selectInput("live_layer","Map product",
                        choices=c(
                          "Study area + Lembang Fault"="study",
                          "Earthquake catalog"="earthquake",
                          "Vs30 grid points"="vs30",
                          "District mean Vs30"="vs30_district",
                          "BPS district indicator"="bps",
                          "Scenario PGA surface"="scenario",
                          "Paper: posterior hazard mean"="paper_hazard",
                          "Paper: exposure index"="paper_exposure",
                          "Paper: vulnerability index"="paper_vulnerability",
                          "Paper: integrated screening score"="paper_screening"
                        ),
                        selected="study")),
        div(selectInput("basemap_choice","Basemap",
                        choices=c(
                          "OpenStreetMap"="osm",
                          "Esri Street Map"="street",
                          "Esri Topographic"="topo",
                          "Esri Satellite"="sat",
                          "Esri National Geographic"="natgeo",
                          "Esri Gray Canvas"="gray",
                          "Esri Physical"="physical",
                          "Esri Ocean"="oceanmap",
                          "OpenTopoMap"="opentopo",
                          "None (offline safe)"="none"
                        ),
                        selected="osm")),
        div(selectInput("map_mode","Map mode",
                        choices=c("2D Interactive"="2d","3D Terrain"="3d"),
                        selected="2d")),
        conditionalPanel(
          "input.map_mode == '2d'",
          div(style="padding-top:24px;", actionButton("refresh_2d_map","Refresh 2D map",class="btn btn-info w-100"))
        ),
        conditionalPanel(
          "input.map_mode == '3d'",
          sliderInput("terrain_exaggeration","Terrain exaggeration",min=.5,max=3,value=1.6,step=.1),
          div(class="ijgi-map3d-note",
              "3D Terrain uses MapLibre and open elevation DEM tiles. No Google API key is required.")
        ),
        conditionalPanel(
          "input.live_layer == 'earthquake'",
          sliderInput("map_mag","Magnitude",0,9,c(4,9),.1),
          sliderInput("map_depth","Depth (km)",0,700,c(0,100),10)
        ),
        conditionalPanel(
          "input.live_layer == 'earthquake'",
          numericInput("map_eq_n","Maximum events",3000,min=250,max=12000,step=250)
        ),
        conditionalPanel(
          "input.live_layer == 'vs30'",
          sliderInput("map_vs30","Vs30 (m/s)",0,1500,c(100,1000),25),
          numericInput("map_vs30_n","Maximum grid points",5000,min=500,max=15000,step=500)
        ),
        conditionalPanel(
          "input.live_layer == 'bps'",
          selectInput("map_bps_var","Indicator",
                      choices=c("Health facilities"="Faskes",
                                "Population density"="Kepadatan",
                                "Telecommunication"="Sinyal",
                                "Education facilities"="Pendidikan")),
          sliderInput("map_bps_year","Year",2019,2025,2024,1,sep="")
        ),
        conditionalPanel(
          "input.live_layer == 'scenario'",
          sliderInput("scenario_mw","Scenario Mw",5.5,7.5,6.8,.1),
          sliderInput("scenario_depth","Depth (km)",1,30,10,1)
        ),
        conditionalPanel(
          "input.live_layer == 'scenario'",
          numericInput("scenario_threshold","Threshold (g)",.092,min=.001,max=2,step=.001)
        ),
        conditionalPanel(
          "['paper_hazard','paper_exposure','paper_vulnerability','paper_screening'].includes(input.live_layer)",
          div(class="paper-map-note",
              icon("circle-info"),
              tags$span("These layers use manuscript outputs generated by the full INLA workflow. Run it from the INLA Diagnostics tab if needed."))
        ),
        col_widths=c(3,3,3,3)
      )
    ),
    card(
      class="glass-card full-map-card",
      full_screen=TRUE,
      card_header(
        div(class="map-header-row",
            textOutput("live_map_title"),
            div(class="live-badge",icon("circle-dot")," Runtime-generated"))
      ),
      conditionalPanel(
        "input.map_mode == '2d'",
        leafletOutput("live_map",height="78vh")
      ),
      conditionalPanel(
        "input.map_mode == '3d'",
        uiOutput("map3d_status"),
        div(class="ijgi-map3d-wrap",
            tags$div(id="ijgi_map3d"),
            div(class="ijgi-map3d-badge",
                HTML("<b>3D Terrain</b><br>Drag / Ctrl+drag: rotate & tilt<br>Scroll: zoom<br>Red line: Lembang Fault"))
        )
      )
    )
  ),

  conditionalPanel(
    "input.main_nav == 'explorer'",
    div(class="page-heading",h2("Raw Data Explorer"),
        p("Inspect the complete source datasets with sensible dataset-specific default plots.")),
    card(
      class="glass-card explorer-control-card",
      layout_columns(
        div(selectInput("explore_dataset","Dataset",
                        choices=c("BPS socioeconomic panel"="BPS",
                                  "BMKG earthquake catalog"="BMKG",
                                  "Vs30 grid"="VS30"))),
        div(uiOutput("explore_filters")),
        div(sliderInput("preview_n","Preview rows",25,500,100,25)),
        div(actionButton("refresh_explorer","Apply / Refresh",class="btn btn-info w-100")),
        col_widths=c(3,5,2,2)
      ),
      uiOutput("variable_selectors")
    ),
    layout_columns(
      card(class="glass-card scroll-card",full_screen=TRUE,
           card_header("Interactive data table"),
           div(class="horizontal-scroll",
               div(class="wide-content wide-table-content", DTOutput("raw_table")))),
      card(class="glass-card scroll-card",
           card_header("Missingness"),
           div(class="horizontal-scroll",
               div(class="wide-content compact-table-content", DTOutput("missing_table")))),
      col_widths=c(8,4)
    ),
    layout_columns(
      card(class="glass-card full-plot-card scroll-card",full_screen=TRUE,
           card_header("Dynamic exploratory plot"),
           div(class="horizontal-scroll plot-scroll",
               div(class="wide-content plot-wide-content",
                   plotlyOutput("explore_plot",height="590px")))),
      card(class="glass-card scroll-card",full_screen=TRUE,
           card_header("Numeric summary"),
           div(class="horizontal-scroll",
               div(class="wide-content summary-wide-content",
                   DTOutput("numeric_summary_table")))),
      col_widths=c(7,5)
    )
  ),

  conditionalPanel(
    "input.main_nav == 'runner'",
    div(class="page-heading",h2("Analysis Runner"),
        p("Run data audits and exploratory summaries. The full INLA/SPDE model is now managed in the dedicated INLA Diagnostics tab.")),
    layout_columns(
      card(class="glass-card",
           card_header("Raw-data audit"),
           actionButton("run_audit","Run audit",class="btn btn-info w-100"),
           br(),br(),uiOutput("audit_status")),
      card(class="glass-card",
           card_header("Earthquake analysis"),
           sliderInput("runner_mag","Minimum magnitude",0,8,4,.1),
           sliderInput("runner_depth","Maximum depth (km)",10,700,100,10),
           actionButton("run_eq","Run event analysis",class="btn btn-info w-100")),
      card(class="glass-card",
           card_header("BPS indicator analysis"),
           selectInput("bps_indicator",NULL,choices=c("Faskes","Kepadatan","Sinyal","Pendidikan")),
           sliderInput("bps_years","Year range",2019,2025,c(2019,2025),1,sep=""),
           actionButton("run_bps","Run BPS analysis",class="btn btn-info w-100")),
      col_widths=c(4,4,4)
    ),
    card(
      class="glass-card scroll-card",
      full_screen=TRUE,
      card_header("Audit results"),
      div(
        class="horizontal-scroll",
        div(
          class="wide-content wide-table-content",
          DTOutput("audit_table")
        )
      )
    ),
    layout_columns(
      card(class="glass-card full-plot-card scroll-card",full_screen=TRUE,
           card_header("Earthquake analysis"),
           div(class="horizontal-scroll plot-scroll",
               div(class="wide-content plot-medium-content",
                   plotlyOutput("runner_eq_plot",height="520px")))),
      card(class="glass-card full-plot-card scroll-card",full_screen=TRUE,
           card_header("BPS indicator analysis"),
           div(class="horizontal-scroll plot-scroll",
               div(class="wide-content plot-medium-content",
                   plotlyOutput("runner_bps_plot",height="520px")))),
      col_widths=c(6,6)
    )
  ),

  conditionalPanel(
    "input.main_nav == 'ranking'",
    div(class="page-heading",h2("District Ranking"),p("Manuscript-ready integrated screening ranking regenerated by the current full workflow.")),
    layout_columns(
      card(class="glass-card compact-control-card",
           sliderInput("top_n","Number of districts",5,10,10,1),
           downloadButton("download_top10","Download ranking CSV",class="btn btn-info w-100")),
      card(class="glass-card full-plot-card scroll-card",full_screen=TRUE,
           div(class="horizontal-scroll plot-scroll",
               div(class="wide-content plot-wide-content",
                   plotOutput("ranking_plot",height="620px",width="1400px")))),
      col_widths=c(3,9)
    ),
    card(class="glass-card scroll-card",
         div(class="horizontal-scroll",
             div(class="wide-content wide-table-content",DTOutput("ranking_table"))))
  ),

  conditionalPanel(
    "input.main_nav == 'sensitivity'",
    div(class="page-heading",h2("Sensitivity Analysis"),p("Ranking sensitivity regenerated from the same paper-aligned workflow.")),
    layout_columns(
      card(class="glass-card compact-control-card",
           selectInput("sens_factor","Sensitivity factor",choices=unique(sensitivity$sensitivity_factor))),
      card(class="glass-card full-plot-card scroll-card",full_screen=TRUE,
           div(class="horizontal-scroll plot-scroll",
               div(class="wide-content plot-wide-content",
                   plotOutput("sensitivity_plot",height="600px",width="1400px")))),
      col_widths=c(3,9)
    ),
    card(class="glass-card scroll-card",
         div(class="horizontal-scroll",
             div(class="wide-content wide-table-content",DTOutput("sensitivity_table"))))
  ),

  conditionalPanel(
    "input.main_nav == 'inla'",
    div(class="page-heading",h2("INLA Diagnostics"),
        p("Run and inspect the full manuscript INLA/SPDE workflow from this dedicated model-diagnostics workspace.")),
    div(class="note-box mb-3", tags$b("Exceedance-model revision: "),
        "the current workflow smooths the continuous annual exceedance fraction on the logit scale. It no longer rounds small fractions to binomial pseudo-counts, which previously produced zero posterior exceedance values."),
    card(
      class="glass-card inla-run-card",
      card_header("Run Full INLA / SPDE Manuscript Model"),
      p("Run the bundled reproducible R Markdown here. The diagnostics below are refreshed only after the model has completed successfully."),
      layout_columns(
        div(
          actionButton("run_inla","Run Full INLA Workflow",class="btn btn-warning w-100"),
          actionButton("cancel_inla","Cancel INLA Run",class="btn btn-outline-danger w-100 mt-2"),
          tags$div(class="small text-muted mt-2",
                   "INLA runs in a separate R process, so this dashboard remains responsive and shows live status.")
        ),
        div(
          uiOutput("inla_progress_ui"),
          uiOutput("inla_run_status")
        ),
        col_widths=c(4,8)
      ),
      tags$details(
        class="inla-log-details mt-3",
        tags$summary("Show INLA run log"),
        verbatimTextOutput("inla_log_tail",placeholder=TRUE)
      )
    ),
    uiOutput("inla_gate"),
    conditionalPanel(
      "output.inla_ready",
      layout_columns(
        card(class="glass-card scroll-card",card_header("Model diagnostics"),
             div(class="horizontal-scroll",
                 div(class="wide-content compact-table-content",DTOutput("inla_live_table")))),
        card(class="glass-card full-plot-card scroll-card",card_header("DIC and WAIC"),
             div(class="horizontal-scroll plot-scroll",
                 div(class="wide-content plot-medium-content",
                     plotlyOutput("inla_metric_plot",height="430px")))),
        col_widths=c(6,6)
      )
    )
  ),

  conditionalPanel(
    "input.main_nav == 'bayesrisk'",
    div(
      class="page-heading",
      h2("Bayesian Hazard & Screening"),
      p("Paper-aligned posterior exceedance screening, uncertainty, and spatial surfaces from the completed INLA/SPDE workflow.")
    ),
    div(
      class="note-box mb-3",
      strong("Interpretation boundary: "),
      "the posterior quantity is the smoothed weighted logit-Gaussian shaking-exceedance screening probability used in the manuscript. ",
      "It is not an earthquake-occurrence probability, calibrated physical risk probability, casualty probability, or loss probability."
    ),
    card(
      class="glass-card compact-control-card",
      layout_columns(
        div(selectInput(
          "bayes_map_product","Posterior map",
          choices=c(
            "District posterior exceedance — average 2019–2025"="posterior_avg",
            "District posterior exceedance — selected year"="posterior_year",
            "Posterior uncertainty — 95% CI width"="posterior_uncertainty",
            "High-resolution SPDE posterior surface (supplementary)"="posterior_highres",
            "Integrated relative seismic screening score"="screening"
          ),
          selected="posterior_avg"
        )),
        conditionalPanel(
          "input.bayes_map_product == 'posterior_year'",
          sliderInput("bayes_year","Model year",2019,2025,2024,1,sep="")
        ),
        div(selectInput(
          "bayes_basemap","Basemap",
          choices=c(
            "OpenStreetMap"="osm",
            "Esri Street Map"="street",
            "Esri Topographic"="topo",
            "Esri Satellite"="sat",
            "OpenTopoMap"="opentopo",
            "None (offline safe)"="none"
          ),
          selected="osm"
        )),
        col_widths=c(5,3,4)
      )
    ),
    layout_columns(
      card(
        class="glass-card full-map-card",
        full_screen=TRUE,
        card_header(textOutput("bayes_map_title")),
        leafletOutput("bayes_map",height="650px")
      ),
      card(
        class="glass-card",
        card_header("Posterior interpretation"),
        uiOutput("bayes_summary_ui"),
        hr(),
        card_header("Highest posterior exceedance"),
        DTOutput("bayes_top_table")
      ),
      col_widths=c(8,4)
    ),
    layout_columns(
      card(
        class="glass-card full-plot-card",
        card_header("Posterior exceedance by year"),
        plotlyOutput("bayes_year_plot",height="430px")
      ),
      card(
        class="glass-card full-plot-card",
        card_header("Posterior uncertainty"),
        plotlyOutput("bayes_uncertainty_plot",height="430px")
      ),
      col_widths=c(6,6)
    )
  ),

  conditionalPanel(
    "input.main_nav == 'methods'",
    div(class="page-heading",h2("Methods & Limits")),
    layout_columns(
      card(class="glass-card",card_header("Baseline analysis"),uiOutput("methods_table")),
      card(class="glass-card",card_header("Interpretation boundaries"),
           tags$ul(class="limit-list",
                   tags$li("Ground motion is a study-specific deterministic proxy."),
                   tags$li("The second-stage response is weighted logit-Gaussian and model-derived."),
                   tags$li("Vulnerability is a screening proxy based on available indicators."),
                   tags$li("No casualty, structural-loss, or economic-loss model is included."))),
      col_widths=c(6,6))
  ),

  conditionalPanel(
    "input.main_nav == 'downloads'",
    div(class="page-heading",h2("Downloads & Source Files")),
    layout_columns(
      card(class="glass-card",card_header("Summary exports"),
           downloadButton("download_top10_2","Top-10 screening",class="btn btn-outline-info w-100"),br(),br(),
           downloadButton("download_sensitivity","Sensitivity analysis",class="btn btn-outline-info w-100"),br(),br(),
           downloadButton("download_models","Reported model comparison",class="btn btn-outline-info w-100")),
      card(class="glass-card",card_header("Manuscript synchronization"),
           p("After the Full INLA Workflow completes, manuscript-ready tables and figures are generated automatically."),
           downloadButton("download_synced_paper","Synchronized manuscript",class="btn btn-info w-100"),br(),br(),
           downloadButton("download_manuscript_bundle","Manuscript results bundle",class="btn btn-outline-info w-100")),
      card(class="glass-card",card_header("Complete bundled raw data"),
           tags$ul(tags$li("Data BPS.csv"),tags$li("BMKG_Earthquake_Catalog.csv"),tags$li("DATA_VS30_LENGKAP.csv"),tags$li("Batas_Desa_Kelurahan_JABAR.gpkg"),tags$li("titik_sesar_lembang.kmz")),
           p("All raw files are physically included under data/raw/.")),
      col_widths=c(4,4,4))
  )
)

server <- function(input,output,session){

  observe({
    send_clean_message("ui-theme",list(
      theme=input$theme_choice %||% "midnight",
      fontScale=input$font_scale %||% 1,
      heroHeight=input$hero_height %||% 330
    ))
  })

  is_light <- reactive(identical(input$theme_choice,"light"))
  axis_color <- reactive(if(is_light()) "#243746" else "#CFE3EF")
  grid_color <- reactive(if(is_light()) "#C8D6E0" else "#315167")

  transparent_theme <- reactive({
    theme_minimal(base_size=13) +
      theme(
        plot.background=element_rect(fill="transparent",colour=NA),
        panel.background=element_rect(fill="transparent",colour=NA),
        legend.background=element_rect(fill="transparent",colour=NA),
        legend.key=element_rect(fill="transparent",colour=NA),
        panel.grid.major=element_line(colour=grid_color(),linewidth=.3),
        panel.grid.minor=element_blank(),
        axis.text=element_text(color=axis_color()),
        axis.title=element_text(color=axis_color()),
        plot.title=element_text(color=axis_color())
      )
  })

  plotly_transparent <- function(p) {
    ggplotly(p) %>% layout(
      autosize=TRUE,
      paper_bgcolor="rgba(0,0,0,0)",
      plot_bgcolor="rgba(0,0,0,0)",
      font=list(color=axis_color()),
      margin=list(l=70,r=30,t=25,b=65)
    )
  }

  # Remove names from atomic vectors before Shiny/jsonlite serialization.
  # Named lists remain named, so JSON objects are preserved correctly.
  strip_atomic_names <- function(x){
    if(is.list(x)){
      return(lapply(x, strip_atomic_names))
    }
    if(is.atomic(x) && !is.null(names(x))){
      return(unname(x))
    }
    x
  }

  send_clean_message <- function(type, payload){
    session$sendCustomMessage(type, strip_atomic_names(payload))
  }

  # Lazy raw-data caches
  bps_cache <- reactiveVal(NULL)
  bmkg_cache <- reactiveVal(NULL)
  vs30_cache <- reactiveVal(NULL)
  district_cache <- reactiveVal(NULL)
  fault_cache <- reactiveVal(NULL)
  district_vs30_cache <- reactiveVal(NULL)

  get_bps <- function(){ if(is.null(bps_cache())) bps_cache(load_bps()); bps_cache() }
  get_bmkg <- function(){ if(is.null(bmkg_cache())) bmkg_cache(load_bmkg()); bmkg_cache() }
  get_vs30 <- function(){ if(is.null(vs30_cache())) vs30_cache(load_vs30()); vs30_cache() }
  get_districts <- function(){
    if(is.null(district_cache())){
      withProgress(message="Preparing district polygons...",value=.2,{
        district_cache(build_districts()); incProgress(.8)
      })
    }
    district_cache()
  }
  get_fault <- function(){ if(is.null(fault_cache())) fault_cache(read_fault_kmz()); fault_cache() }
  get_district_vs30 <- function(){
    if(is.null(district_vs30_cache())){
      withProgress(message="Aggregating raw Vs30 to districts...",value=.15,{
        district_vs30_cache(aggregate_vs30_to_districts(get_districts(),get_vs30()))
        incProgress(.85)
      })
    }
    district_vs30_cache()
  }

  # 3D map helpers adapted from the supplied reference app.
  map3d_tile_config <- function(style="osm"){
    switch(
      style,
      sat=list(url="https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}",
               attribution="Tiles © Esri — Source: Esri, Maxar, Earthstar Geographics",max_zoom=19),
      topo=list(url="https://a.tile.opentopomap.org/{z}/{x}/{y}.png",
                attribution="Map data © OpenStreetMap contributors, SRTM | OpenTopoMap",max_zoom=17),
      opentopo=list(url="https://a.tile.opentopomap.org/{z}/{x}/{y}.png",
                    attribution="Map data © OpenStreetMap contributors, SRTM | OpenTopoMap",max_zoom=17),
      street=list(url="https://server.arcgisonline.com/ArcGIS/rest/services/World_Street_Map/MapServer/tile/{z}/{y}/{x}",
                  attribution="Tiles © Esri — World Street Map",max_zoom=19),
      natgeo=list(url="https://server.arcgisonline.com/ArcGIS/rest/services/NatGeo_World_Map/MapServer/tile/{z}/{y}/{x}",
                  attribution="Tiles © Esri — National Geographic",max_zoom=16),
      gray=list(url="https://server.arcgisonline.com/ArcGIS/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/{z}/{y}/{x}",
                attribution="Tiles © Esri — Light Gray Canvas",max_zoom=16),
      physical=list(url="https://server.arcgisonline.com/ArcGIS/rest/services/World_Physical_Map/MapServer/tile/{z}/{y}/{x}",
                    attribution="Tiles © Esri — World Physical Map",max_zoom=8),
      oceanmap=list(url="https://server.arcgisonline.com/ArcGIS/rest/services/Ocean/World_Ocean_Base/MapServer/tile/{z}/{y}/{x}",
                    attribution="Tiles © Esri — World Ocean Base",max_zoom=16),
      list(url="https://tile.openstreetmap.org/{z}/{x}/{y}.png",
           attribution="© OpenStreetMap contributors",max_zoom=19)
    )
  }

  normalize01 <- function(x){
    x <- suppressWarnings(as.numeric(x))
    good <- is.finite(x)
    if(!any(good)) return(rep(.5,length(x)))
    rng <- range(x[good],na.rm=TRUE)
    if(!all(is.finite(rng)) || diff(rng)==0) return(rep(.5,length(x)))
    out <- rep(.5,length(x))
    out[good] <- pmax(0,pmin(1,(x[good]-rng[1])/diff(rng)))
    out
  }

  boundary_features_3d <- reactiveVal(NULL)

  geometry_to_linestring_features <- function(x){
    feats <- list()
    k <- 1L

    g <- if(inherits(x,"sf")) sf::st_geometry(x) else x
    g <- tryCatch(sf::st_make_valid(g), error=function(e) g)

    for(i in seq_along(g)){
      gi <- g[i]
      gi_type <- as.character(sf::st_geometry_type(gi,by_geometry=TRUE))

      if(any(gi_type=="GEOMETRYCOLLECTION")){
        gi <- tryCatch(
          sf::st_collection_extract(gi,"POLYGON",warn=FALSE),
          error=function(e) gi
        )
      }

      bi <- tryCatch(sf::st_boundary(gi), error=function(e) NULL)
      if(is.null(bi) || length(bi)==0) next

      li <- tryCatch(
        suppressWarnings(sf::st_cast(bi,"LINESTRING",warn=FALSE)),
        error=function(e) bi
      )

      for(j in seq_along(li)){
        cc <- tryCatch(sf::st_coordinates(li[j]), error=function(e) NULL)
        if(is.null(cc) || nrow(cc)<2) next

        feats[[k]] <- list(
          type="Feature",
          properties=list(),
          geometry=list(
            type="LineString",
            coordinates=lapply(seq_len(nrow(cc)), function(r){
              c(as.numeric(cc[r,1]),as.numeric(cc[r,2]))
            })
          )
        )
        k <- k+1L
      }
    }

    feats
  }

  get_boundary_features_3d <- function(){
    if(!is.null(boundary_features_3d())) return(boundary_features_3d())

    d <- get_districts()

    ds <- tryCatch({
      d |>
        sf::st_transform(32748) |>
        sf::st_simplify(dTolerance=400,preserveTopology=TRUE) |>
        sf::st_transform(4326)
    }, error=function(e){
      sf::st_transform(d,4326)
    })

    feats <- tryCatch(
      geometry_to_linestring_features(ds),
      error=function(e) list()
    )

    boundary_features_3d(feats)
    feats
  }

  get_fault_coords_3d <- function(){
    f <- sf::st_transform(get_fault(),4326)
    g <- if(inherits(f,"sf")) sf::st_geometry(f) else f
    g <- tryCatch(sf::st_make_valid(g), error=function(e) g)

    if(any(as.character(sf::st_geometry_type(g,by_geometry=TRUE))=="GEOMETRYCOLLECTION")){
      g <- tryCatch(
        sf::st_collection_extract(g,"LINESTRING",warn=FALSE),
        error=function(e) g
      )
    }

    lines <- tryCatch(
      suppressWarnings(sf::st_cast(g,"LINESTRING",warn=FALSE)),
      error=function(e) g
    )

    coords <- list()
    k <- 1L

    for(i in seq_along(lines)){
      cc <- tryCatch(sf::st_coordinates(lines[i]), error=function(e) NULL)
      if(is.null(cc) || nrow(cc)<2) next

      for(r in seq_len(nrow(cc))){
        coords[[k]] <- c(as.numeric(cc[r,1]),as.numeric(cc[r,2]))
        k <- k+1L
      }
    }

    coords
  }

  make_3d_points <- function(z){
    label <- value_label <- NULL
    if(z$type=="earthquake"){
      d <- as.data.frame(z$points)
      if(!nrow(d)) return(list())
      val <- d$Magnitude
      return(lapply(seq_len(nrow(d)),function(i) list(
        lon=d$Longitude[i],lat=d$Latitude[i],norm=normalize01(val)[i],
        label=paste0("M ",d$Magnitude[i]),
        valueLabel=paste0("Depth: ",d[["Depth (km)"]][i]," km")
      )))
    }
    if(z$type=="vs30"){
      d <- as.data.frame(z$points)
      if(!nrow(d)) return(list())
      val <- d$vs30_mosaic
      return(lapply(seq_len(nrow(d)),function(i) list(
        lon=d$longitude[i],lat=d$latitude[i],norm=normalize01(val)[i],
        label="Vs30 grid",valueLabel=paste0("Vs30: ",round(val[i],1)," m/s")
      )))
    }
    if(z$type %in% c("study","vs30_district","bps","scenario")){
      p <- if(z$type=="study") z$districts else z$polygons
      cent <- suppressWarnings(sf::st_coordinates(sf::st_centroid(sf::st_geometry(p))))
      val <- switch(z$type,
                    study=seq_len(nrow(p)),
                    vs30_district=p$Vs30_mean,
                    bps=p$map_value,
                    scenario=p$PGA_scenario)
      labs <- if(all(c("kabupaten_kota","kecamatan") %in% names(p)))
        paste(p$kabupaten_kota,p$kecamatan,sep=" — ") else paste0("District ",seq_len(nrow(p)))
      vlab <- switch(z$type,
                     study="Study district",
                     vs30_district=paste0("Mean Vs30: ",round(val,1)," m/s"),
                     bps=paste0(z$indicator,": ",round(val,2)),
                     scenario=paste0("PGA proxy: ",round(val,3)," g"))
      nr <- normalize01(val)
      return(lapply(seq_len(nrow(p)),function(i) list(
        lon=cent[i,1],lat=cent[i,2],norm=nr[i],label=labs[i],valueLabel=vlab[i]
      )))
    }
    if(z$type=="paper"){
      screening_path <- file.path("data","derived","screening_map_live.csv")
      if(!file.exists(screening_path)) return(list())
      dd <- readr::read_csv(screening_path,show_col_types=FALSE)
      dd$key <- paste(clean_key(dd$kabupaten),clean_key(dd$kecamatan),sep="__")
      p <- dplyr::left_join(get_districts(),dd,by="key")
      vals <- p[[z$variable]]
      cent <- suppressWarnings(sf::st_coordinates(sf::st_centroid(sf::st_geometry(p))))
      nr <- normalize01(vals)
      labs <- paste(p$kabupaten_kota,p$kecamatan,sep=" — ")
      return(lapply(seq_len(nrow(p)),function(i) list(
        lon=cent[i,1],lat=cent[i,2],norm=nr[i],label=labs[i],
        valueLabel=paste0(z$title,": ",ifelse(is.na(vals[i]),"NA",sprintf("%.3f",vals[i])))
      )))
    }
    list()
  }

  # -------- LIVE MAP: fully reactive; no Build button required --------
  live_map_data <- reactive({
    switch(
      input$live_layer,
      study=list(type="study",districts=get_districts(),fault=get_fault()),
      earthquake={
        x <- data.table::copy(get_bmkg())
        x <- x[
          is.finite(Latitude) & is.finite(Longitude) & is.finite(Magnitude) & is.finite(`Depth (km)`) &
          Longitude>=105 & Longitude<=110 & Latitude>=-9 & Latitude<=-5 &
          Magnitude>=input$map_mag[1] & Magnitude<=input$map_mag[2] &
          `Depth (km)`>=input$map_depth[1] & `Depth (km)`<=input$map_depth[2]
        ]
        list(type="earthquake",points=safe_sample(x,input$map_eq_n),fault=get_fault())
      },
      vs30={
        x <- data.table::copy(get_vs30())
        x <- x[
          is.finite(longitude)&is.finite(latitude)&is.finite(vs30_mosaic) &
          longitude>=105&longitude<=110&latitude>=-9&latitude<=-5 &
          vs30_mosaic>=input$map_vs30[1]&vs30_mosaic<=input$map_vs30[2]
        ]
        list(type="vs30",points=safe_sample(x,input$map_vs30_n),fault=get_fault())
      },
      vs30_district={
        list(type="vs30_district",polygons=get_district_vs30(),fault=get_fault())
      },
      bps=list(type="bps",
               polygons=join_bps_to_districts(get_districts(),get_bps(),input$map_bps_year,input$map_bps_var),
               indicator=input$map_bps_var,year=input$map_bps_year,fault=get_fault()),
      scenario={
        s <- compute_scenario_surface(
          get_district_vs30(),get_fault(),
          input$scenario_mw,input$scenario_depth,input$scenario_threshold
        )
        list(type="scenario",polygons=s,fault=get_fault(),
             mw=input$scenario_mw,depth=input$scenario_depth,threshold=input$scenario_threshold)
      },
      paper_hazard={
        list(type="paper",variable="p_mean_avg",title="Posterior hazard mean",fault=get_fault())
      },
      paper_exposure={
        list(type="paper",variable="Exposure_Index",title="Exposure index",fault=get_fault())
      },
      paper_vulnerability={
        list(type="paper",variable="Vulnerability_Index",title="Vulnerability index",fault=get_fault())
      },
      paper_screening={
        list(type="paper",variable="Screening_Score",title="Integrated screening score",fault=get_fault())
      }
    )
  })

  output$live_map_title <- renderText({
    switch(input$live_layer,