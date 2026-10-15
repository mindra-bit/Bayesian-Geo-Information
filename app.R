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
      study="Study Area and Lembang Fault",
      earthquake="BMKG Earthquake Catalog",
      vs30="Vs30 Grid Points",
      vs30_district="District Mean Vs30",
      bps=paste("BPS Choropleth —",input$map_bps_var,input$map_bps_year),
      scenario=paste0("Scenario PGA — Mw ",input$scenario_mw,", depth ",input$scenario_depth," km"),
      paper_hazard="Paper Output — Posterior Hazard Mean",
      paper_exposure="Paper Output — Exposure Index",
      paper_vulnerability="Paper Output — Vulnerability Index",
      paper_screening="Paper Output — Integrated Screening Score"
    )
  })

  outputOptions(output,"live_map",suspendWhenHidden=FALSE)

  refresh_leaflet_2d <- function(){ send_clean_message("refreshLeaflet2D",list()) }
  observeEvent(input$main_nav,{ if(identical(input$main_nav,"live_map") && identical(input$map_mode,"2d")) refresh_leaflet_2d() },ignoreInit=TRUE)
  observeEvent(input$map_mode,{ if(identical(input$map_mode,"2d")) refresh_leaflet_2d() },ignoreInit=TRUE)
  observeEvent(input$refresh_2d_map,{ refresh_leaflet_2d() },ignoreInit=TRUE)
  observeEvent(input$live_layer,{ if(identical(input$main_nav,"live_map") && identical(input$map_mode,"2d")) refresh_leaflet_2d() },ignoreInit=TRUE)

  output$live_map <- renderLeaflet({
    z <- live_map_data()

    m <- leaflet(options=leafletOptions(
      preferCanvas=TRUE,
      minZoom=5,
      zoomControl=TRUE
    ))
    m <- add_basemap(m,input$basemap_choice)

    # Always start from the study region so a meaningful map is visible.
    m <- m |> setView(lng=107.55,lat=-6.92,zoom=8)

    if(z$type=="study"){
      pal <- colorFactor(
        c("#1B6A85","#21516B","#2D7D78","#365B80","#5B617A",
          "#31766A","#477F92","#3B6E8D","#536D73"),
        domain=as.character(z$districts$kabupaten_kota)
      )
      bb <- sf::st_bbox(z$districts)
      m <- m |>
        addPolygons(
          data=z$districts,
          fillColor=~pal(as.character(kabupaten_kota)),
          fillOpacity=.40,
          color="#D8E8F2",
          weight=.8,
          popup=~paste0("<b>",kabupaten_kota,"</b><br>",kecamatan),
          group="Districts"
        ) |>
        addPolylines(
          data=z$fault,
          color="#FF4838",
          weight=5,
          opacity=1,
          group="Lembang Fault"
        ) |>
        addLegend(
          "bottomright",
          pal=pal,
          values=as.character(z$districts$kabupaten_kota),
          title="Regency / City"
        ) |>
        fitBounds(
          as.numeric(unname(bb["xmin"])),
          as.numeric(unname(bb["ymin"])),
          as.numeric(unname(bb["xmax"])),
          as.numeric(unname(bb["ymax"]))
        )
    }

    if(z$type=="earthquake"){
      x <- z$points

      if(is.null(x) || nrow(x)==0){
        m <- m |>
          addControl(
            html="<div class='map-empty-note'><b>No earthquake events</b><br>Adjust magnitude or depth filters.</div>",
            position="topright"
          )
      } else {
        pal <- colorNumeric(
          c("#FFE66D","#FF9F43","#FF5D4A","#B5173D"),
          domain=x$Magnitude
        )
        m <- m |>
          addCircleMarkers(
            data=x,
            lng=~Longitude,
            lat=~Latitude,
            radius=~pmax(2,pmin(9,(Magnitude-2)*1.2)),
            stroke=FALSE,
            fillOpacity=.78,
            color=~pal(Magnitude),
            popup=~paste0(
              "<b>M ",Magnitude,"</b><br>",
              Date,
              "<br>Depth: ",`Depth (km)`," km",
              "<br>Type: ",`Mag Type`
            ),
            group="Earthquakes"
          ) |>
          addPolylines(
            data=z$fault,
            color="#FF4838",
            weight=4,
            opacity=.95,
            group="Lembang Fault"
          ) |>
          addLegend(
            "bottomright",
            pal=pal,
            values=x$Magnitude,
            title="Magnitude"
          ) |>
          fitBounds(
            min(x$Longitude,na.rm=TRUE),
            min(x$Latitude,na.rm=TRUE),
            max(x$Longitude,na.rm=TRUE),
            max(x$Latitude,na.rm=TRUE)
          )
      }
    }

    if(z$type=="vs30"){
      x <- z$points

      if(is.null(x) || nrow(x)==0){
        m <- m |>
          addControl(
            html="<div class='map-empty-note'><b>No Vs30 grid points</b><br>Adjust the Vs30 range.</div>",
            position="topright"
          )
      } else {
        pal <- colorNumeric(
          c("#3F007D","#2C7FB8","#41B6C4","#A1DAB4","#FFFFCC"),
          domain=x$vs30_mosaic
        )
        m <- m |>
          addCircleMarkers(
            data=x,
            lng=~longitude,
            lat=~latitude,
            radius=3,
            stroke=FALSE,
            fillOpacity=.76,
            color=~pal(vs30_mosaic),
            popup=~paste0("<b>Vs30:</b> ",round(vs30_mosaic,1)," m/s"),
            group="Vs30"
          ) |>
          addPolylines(
            data=z$fault,
            color="#FF4838",
            weight=4,
            opacity=.95,
            group="Lembang Fault"
          ) |>
          addLegend(
            "bottomright",
            pal=pal,
            values=x$vs30_mosaic,
            title="Vs30 (m/s)"
          ) |>
          fitBounds(
            min(x$longitude,na.rm=TRUE),
            min(x$latitude,na.rm=TRUE),
            max(x$longitude,na.rm=TRUE),
            max(x$latitude,na.rm=TRUE)
          )
      }
    }

    if(z$type=="bps"){
      p <- z$polygons

      pal <- colorNumeric(
        c("#081D58","#225EA8","#41B6C4","#C7E9B4","#FFFFD9"),
        domain=p$map_value,
        na.color="#46525C"
      )

      bb <- sf::st_bbox(p)
      m <- m |>
        addPolygons(
          data=p,
          fillColor=~pal(map_value),
          fillOpacity=.74,
          color="#D9E7EF",
          weight=.65,
          popup=~paste0(
            "<b>",kabupaten_kota," — ",kecamatan,"</b><br>",
            z$indicator,": ",
            ifelse(is.na(map_value),"NA",format(round(map_value,2),big.mark=","))
          ),
          group="BPS indicator"
        ) |>
        addPolylines(
          data=z$fault,
          color="#FF4838",
          weight=4,
          opacity=1,
          group="Lembang Fault"
        ) |>
        addLegend(
          "bottomright",
          pal=pal,
          values=p$map_value,
          title=paste(z$indicator,z$year)
        ) |>
        fitBounds(
          as.numeric(unname(bb["xmin"])),
          as.numeric(unname(bb["ymin"])),
          as.numeric(unname(bb["xmax"])),
          as.numeric(unname(bb["ymax"]))
        )
    }


    if(z$type=="vs30_district"){
      p <- z$polygons
      pal <- colorNumeric(
        c("#440154","#31688E","#35B779","#FDE725"),
        domain=p$Vs30_mean,
        na.color="#46525C"
      )
      bb <- st_bbox(p)
      m <- m |>
        addPolygons(
          data=p,
          fillColor=~pal(Vs30_mean),
          fillOpacity=.78,
          color="#D9E7EF",
          weight=.65,
          popup=~paste0(
            "<b>",kabupaten_kota," — ",kecamatan,"</b><br>",
            "Mean Vs30: ",ifelse(is.na(Vs30_mean),"NA",sprintf("%.1f",Vs30_mean))," m/s<br>",
            "Grid cells: ",ifelse(is.na(n_grid),"NA",n_grid)
          ),
          group="District mean Vs30"
        ) |>
        addPolylines(data=z$fault,color="#FF4838",weight=4,opacity=1) |>
        addLegend("bottomright",pal=pal,values=p$Vs30_mean,title="Mean Vs30 (m/s)") |>
        fitBounds(
          as.numeric(unname(bb["xmin"])),as.numeric(unname(bb["ymin"])),
          as.numeric(unname(bb["xmax"])),as.numeric(unname(bb["ymax"]))
        )
    }

    if(z$type=="paper"){
      if(!file.exists(live_screening_path)){
        d <- get_districts()
        bb <- st_bbox(d)
        m <- m |>
          addPolygons(data=d,fillColor="#243746",fillOpacity=.18,
                      color="#8EA9B9",weight=.65) |>
          addPolylines(data=z$fault,color="#FF4838",weight=4,opacity=1) |>
          addControl(
            html="<div class='map-empty-note'><b>Paper-derived map not available yet.</b><br>Run INLA Diagnostics → Run Full INLA Workflow to generate this layer.</div>",
            position="topright"
          ) |>
          fitBounds(
            as.numeric(unname(bb["xmin"])),as.numeric(unname(bb["ymin"])),
            as.numeric(unname(bb["xmax"])),as.numeric(unname(bb["ymax"]))
          )
      } else {
        dd <- readr::read_csv(live_screening_path,show_col_types=FALSE)
        dd$key <- paste(clean_key(dd$kabupaten),clean_key(dd$kecamatan),sep="__")
        p <- dplyr::left_join(get_districts(),dd,by="key")
        vals <- p[[z$variable]]
        pal <- colorNumeric(
          c("#081D58","#225EA8","#41B6C4","#FDE725"),
          domain=vals,
          na.color="#46525C"
        )
        bb <- st_bbox(p)
        m <- m |>
          addPolygons(
            data=p,
            fillColor=pal(vals),
            fillOpacity=.78,
            color="#D9E7EF",
            weight=.65,
            popup=paste0(
              "<b>",p$kabupaten_kota," — ",p$kecamatan,"</b><br>",
              z$title,": ",
              ifelse(is.na(vals),"NA",ifelse(abs(vals)<0.001,format(vals,scientific=TRUE,digits=4),sprintf("%.4f",vals)))
            )
          ) |>
          addPolylines(data=z$fault,color="#FF4838",weight=4,opacity=1) |>
          addLegend("bottomright",pal=pal,values=vals,title=z$title) |>
          fitBounds(
            as.numeric(unname(bb["xmin"])),as.numeric(unname(bb["ymin"])),
            as.numeric(unname(bb["xmax"])),as.numeric(unname(bb["ymax"]))
          )
      }
    }

    if(z$type=="scenario"){
      p <- z$polygons

      pal <- colorNumeric(
        c("#081D58","#225EA8","#41B6C4","#FDE725","#FF5A36"),
        domain=p$PGA_scenario,
        na.color="#46525C"
      )

      bb <- sf::st_bbox(p)
      m <- m |>
        addPolygons(
          data=p,
          fillColor=~pal(PGA_scenario),
          fillOpacity=.80,
          color="#D9E7EF",
          weight=.6,
          popup=~paste0(
            "<b>",kabupaten_kota," — ",kecamatan,"</b><br>",
            "PGA proxy: ",sprintf("%.3f",PGA_scenario)," g<br>",
            "Vs30 mean: ",sprintf("%.1f",Vs30_mean)," m/s<br>",
            "Distance to fault: ",sprintf("%.1f",Distance_to_fault_km)," km<br>",
            "Above ",z$threshold," g: ",ifelse(Above_threshold,"Yes","No")
          ),
          group="Scenario PGA"
        ) |>
        addPolylines(
          data=z$fault,
          color="#FF3028",
          weight=5,
          opacity=1,
          group="Lembang Fault"
        ) |>
        addLegend(
          "bottomright",
          pal=pal,
          values=p$PGA_scenario,
          title="PGA proxy (g)"
        ) |>
        fitBounds(
          as.numeric(unname(bb["xmin"])),
          as.numeric(unname(bb["ymin"])),
          as.numeric(unname(bb["xmax"])),
          as.numeric(unname(bb["ymax"]))
        )
    }

    m
  })


  # -------- 3D TERRAIN MAP --------
  map3d_state <- reactiveVal("idle")
  map3d_message <- reactiveVal(
    "Switch to 3D Terrain to initialize the terrain map. Internet access is required for MapLibre and terrain tiles."
  )

  observe({
    req(input$main_nav=="live_map",input$map_mode=="3d")

    map3d_state("loading")
    map3d_message("Preparing 3D terrain data...")

    tryCatch({
      z <- live_map_data()
      tc <- map3d_tile_config(input$basemap_choice)

      fault_coords <- tryCatch(
        get_fault_coords_3d(),
        error=function(e) list()
      )

      pts <- make_3d_points(z)

      if(length(pts)>5000){
        pts <- pts[
          unique(
            round(
              seq(1,length(pts),length.out=5000)
            )
          )
        ]
      }

      boundaries <- tryCatch(
        get_boundary_features_3d(),
        error=function(e) list()
      )

      send_clean_message(
        "updateIJGI3D",
        list(
          centerLon=107.60,
          centerLat=-6.88,
          zoom=8.7,
          exaggeration=input$terrain_exaggeration %||% 1.6,
          tileUrl=tc$url,
          attribution=tc$attribution,
          maxZoom=tc$max_zoom,
          boundaries=boundaries,
          fault=fault_coords,
          points=pts
        )
      )

      map3d_state("ready")
      map3d_message(
        paste0(
          "3D payload ready · ",
          length(pts)," analytical points · ",
          length(boundaries)," boundary segments. ",
          "If the terrain canvas remains blank, check internet access to unpkg.com and elevation tile services."
        )
      )
    },error=function(e){
      map3d_state("error")
      map3d_message(
        paste("3D terrain preparation failed:",conditionMessage(e))
      )
    })
  })

  output$map3d_status <- renderUI({
    st <- map3d_state()
    cls <- if(identical(st,"error")){
      "map3d-status map3d-status-error"
    } else if(identical(st,"loading")){
      "map3d-status map3d-status-loading"
    } else {
      "map3d-status map3d-status-ready"
    }
    div(class=cls,map3d_message())
  })

  observeEvent(input$map_mode,{
    if(identical(input$map_mode,"3d")){
      send_clean_message("resizeIJGI3D",list())
    }
  },ignoreInit=TRUE)

  # -------- RAW EXPLORER --------
  current_data <- eventReactive(input$refresh_explorer,{
    switch(input$explore_dataset,
      BPS={
        x <- data.table::copy(get_bps())
        if(!is.null(input$filter_bps_year)) x <- x[Tahun>=input$filter_bps_year[1]&Tahun<=input$filter_bps_year[2]]
        x
      },
      BMKG={
        x <- data.table::copy(get_bmkg())
        if(!is.null(input$filter_mag)) x <- x[Magnitude>=input$filter_mag[1]&Magnitude<=input$filter_mag[2]]
        if(!is.null(input$filter_depth)) x <- x[`Depth (km)`>=input$filter_depth[1]&`Depth (km)`<=input$filter_depth[2]]
        x
      },
      VS30={
        x <- data.table::copy(get_vs30())
        if(!is.null(input$filter_vs30)) x <- x[vs30_mosaic>=input$filter_vs30[1]&vs30_mosaic<=input$filter_vs30[2]]
        x
      }
    )
  },ignoreNULL=FALSE)

  output$explore_filters <- renderUI({
    switch(input$explore_dataset,
      BPS=sliderInput("filter_bps_year","Year",2019,2025,c(2019,2025),1,sep=""),
      BMKG=tagList(
        sliderInput("filter_mag","Magnitude",0,9,c(0,9),.1),
        sliderInput("filter_depth","Depth (km)",0,700,c(0,700),10)
      ),
      VS30=sliderInput("filter_vs30","Vs30",0,1600,c(0,1600),25)
    )
  })

  output$variable_selectors <- renderUI({
    x <- current_data()
    nums <- names(x)[vapply(x,is.numeric,logical(1))]
    alln <- names(x)

    defaults <- switch(input$explore_dataset,
      BPS=list(type="line",x="Tahun",y="Faskes",color=""),
      BMKG=list(type="scatter",x="Longitude",y="Latitude",color="Magnitude"),
      VS30=list(type="scatter",x="longitude",y="latitude",color="vs30_mosaic")
    )

    div(class="explorer-selector-grid",
        selectInput("plot_type","Plot type",
                    choices=c("Histogram"="hist","Scatter"="scatter","Boxplot"="box","Line / trend"="line"),
                    selected=defaults$type),
        selectInput("x_var","X variable",choices=alln,selected=defaults$x),
        selectInput("y_var","Y variable",choices=c("(none)"="",nums),selected=defaults$y),
        selectInput("color_var","Color / group",choices=c("(none)"="",alln),selected=defaults$color)
    )
  })

  output$raw_table <- renderDT({
    datatable(as.data.frame(head(current_data(),input$preview_n)),rownames=FALSE,
              class="stripe hover compact",options=list(scrollX=TRUE,pageLength=25))
  })
  output$missing_table <- renderDT({
    datatable(missingness_table(as.data.frame(current_data())),rownames=FALSE,
              options=list(dom="t",pageLength=20)) |> formatRound("missing_pct",1)
  })
  output$numeric_summary_table <- renderDT({
    z <- numeric_summary(as.data.frame(current_data()))
    dt <- datatable(z,rownames=FALSE,options=list(scrollX=TRUE,pageLength=20))
    if(nrow(z)) dt <- dt |> formatRound(setdiff(names(z),c("variable","n")),3)
    dt
  })

  output$explore_plot <- renderPlotly({
    x <- as.data.frame(safe_sample(current_data(),20000))
    req(input$x_var)
    blank <- function(msg) {
      plot_ly() %>% layout(
        paper_bgcolor="rgba(0,0,0,0)",plot_bgcolor="rgba(0,0,0,0)",
        font=list(color=axis_color()),
        annotations=list(list(text=msg,x=.5,y=.5,xref="paper",yref="paper",showarrow=FALSE,font=list(size=16)))
      )
    }

    if(input$plot_type=="hist"){
      if(!is.numeric(x[[input$x_var]])) return(blank("Select a numeric X variable"))
      p <- ggplot(x,aes(x=.data[[input$x_var]]))+
        geom_histogram(bins=35,fill="#27B7FF",alpha=.85)
    } else if(input$plot_type=="scatter"){
      if(input$y_var=="" || !is.numeric(x[[input$x_var]]) || !is.numeric(x[[input$y_var]]))
        return(blank("Scatter plot requires numeric X and Y variables"))
      if(input$color_var!="" && is.numeric(x[[input$color_var]])){
        p <- ggplot(x,aes(x=.data[[input$x_var]],y=.data[[input$y_var]],color=.data[[input$color_var]]))+
          geom_point(alpha=.65,size=1.8)+scale_color_viridis_c()
      } else if(input$color_var!=""){
        p <- ggplot(x,aes(x=.data[[input$x_var]],y=.data[[input$y_var]],color=as.factor(.data[[input$color_var]])))+
          geom_point(alpha=.65,size=1.8)
      } else {
        p <- ggplot(x,aes(x=.data[[input$x_var]],y=.data[[input$y_var]]))+
          geom_point(alpha=.60,size=1.8,color="#27B7FF")
      }
    } else if(input$plot_type=="box"){
      if(input$y_var=="" || !is.numeric(x[[input$y_var]]))
        return(blank("Boxplot requires a numeric Y variable"))
      p <- ggplot(x,aes(x=as.factor(.data[[input$x_var]]),y=.data[[input$y_var]]))+
        geom_boxplot(fill="#2AD4C9",alpha=.72)+coord_flip()
    } else {
      if(input$y_var=="" || !is.numeric(x[[input$y_var]]))
        return(blank("Trend plot requires a numeric Y variable"))
      x_plot <- x[is.finite(x[[input$y_var]]) & !is.na(x[[input$x_var]]),,drop=FALSE]
      if(nrow(x_plot)==0) return(blank("No finite values available for this trend plot"))
      p <- ggplot(x_plot,aes(x=.data[[input$x_var]],y=.data[[input$y_var]]))+
        stat_summary(fun=mean,geom="line",color="#27B7FF",linewidth=1.0,na.rm=TRUE)+
        stat_summary(fun=mean,geom="point",color="#2AD4C9",size=2.5,na.rm=TRUE)
    }
    p <- p + transparent_theme() + labs(x=input$x_var,y=if(input$y_var=="")NULL else input$y_var)
    plotly_transparent(p)
  })

  # -------- RUNNER --------
  audit_result <- eventReactive(input$run_audit,{
    b <- get_bps(); e <- get_bmkg(); v <- get_vs30()
    data.frame(
      dataset=c("BPS socioeconomic panel","BMKG earthquake catalog","Vs30 grid"),
      rows=c(nrow(b),nrow(e),nrow(v)),
      columns=c(ncol(b),ncol(e),ncol(v)),
      missing_cells=c(sum(is.na(b)),sum(is.na(e)),sum(is.na(v)))
    )
  })
  output$audit_table <- renderDT({
    req(audit_result())
    datatable(
      audit_result(),
      rownames=FALSE,
      class="stripe hover compact",
      options=list(
        dom="t",
        scrollX=TRUE,
        autoWidth=TRUE,
        columnDefs=list(
          list(width="46%",targets=0),
          list(width="18%",targets=1),
          list(width="18%",targets=2),
          list(width="18%",targets=3)
        )
      )
    )
  })
  output$audit_status <- renderUI({
    req(input$run_audit)
    div(class="note-box","Audit completed directly from data/raw/.")
  })

  eq_analysis <- eventReactive(input$run_eq,{
    x <- data.table::copy(get_bmkg())
    x <- x[Magnitude>=input$runner_mag & `Depth (km)`<=input$runner_depth & !is.na(Date_parsed)]
    x[,year:=as.integer(format(Date_parsed,"%Y"))]
    x[year>=1998 & year<=2024,.N,by=year]
  })
  output$runner_eq_plot <- renderPlotly({
    d <- eq_analysis(); req(nrow(d))
    p <- ggplot(d,aes(year,N))+geom_col(fill="#27B7FF")+
      labs(x="Year",y="Events")+transparent_theme()
    plotly_transparent(p)
  })

  bps_analysis <- eventReactive(input$run_bps,{
    x <- data.table::copy(get_bps())
    x <- x[Tahun>=input$bps_years[1]&Tahun<=input$bps_years[2]]
    x[,value:=suppressWarnings(as.numeric(get(input$bps_indicator)))]
    x[,.(mean_value=mean(value,na.rm=TRUE)),by=Tahun]
  })
  output$runner_bps_plot <- renderPlotly({
    d <- bps_analysis()
    req(nrow(d))
    p <- ggplot(d,aes(Tahun,mean_value))+
      geom_line(color="#2AD4C9",linewidth=1.1)+
      geom_point(color="#2AD4C9",size=2.8)+
      labs(y=paste("Mean",input$bps_indicator))+
      transparent_theme()
    plotly_transparent(p)
  })

  # -------- TRUE INLA RUN GATE --------
  live_diag_path <- file.path("data","derived","model_comparison_live.csv")
  live_screening_path <- file.path("data","derived","screening_map_live.csv")
  inla_log_path <- file.path("data","derived","inla_run.log")
  inla_err_path <- file.path("data","derived","inla_error.log")

  inla_metrics <- reactiveVal(
    if(file.exists(live_diag_path)){
      readr::read_csv(live_diag_path,show_col_types=FALSE)
    } else NULL
  )
  inla_status <- reactiveVal(
    if(file.exists(live_diag_path)){
      "Existing diagnostics were found from a previous completed run."
    } else {
      "INLA has not been run in this project session."
    }
  )
  inla_state <- reactiveVal(if(file.exists(live_diag_path)) "done" else "idle")
  inla_started_at <- reactiveVal(NULL)
  inla_process <- reactiveVal(NULL)
  inla_pid <- reactiveVal(NA_integer_)

  tail_inla_log <- function(n=16){
    read_one <- function(path){
      if(!file.exists(path)) return(character())
      tryCatch(
        readLines(path,warn=FALSE,encoding="UTF-8"),
        error=function(e) character()
      )
    }

    out <- read_one(inla_log_path)
    err <- read_one(inla_err_path)

    zz <- c(
      if(length(out)) c("[stdout]",out) else character(),
      if(length(err)) c("[stderr]",err) else character()
    )

    if(!length(zz)) return("INLA process started; waiting for log output...")
    paste(utils::tail(zz,n),collapse="\n")
  }

  inla_error_summary <- function(n=6){
    read_one <- function(path){
      if(!file.exists(path)) return(character())
      tryCatch(
        readLines(path,warn=FALSE,encoding="UTF-8"),
        error=function(e) character()
      )
    }

    err <- read_one(inla_err_path)
    out <- read_one(inla_log_path)
    z <- c(err,out)
    z <- z[nzchar(trimws(z))]
    if(!length(z)) return("No error text was written by the child R process.")

    # Prefer explicit error/failure lines when present.
    hit <- grep(
      "Error|Execution halted|Quitting from lines|failed|cannot|could not|not found|there is no package",
      z,
      ignore.case=TRUE
    )
    if(length(hit)){
      j <- max(hit)
      lo <- max(1,j-2)
      hi <- min(length(z),j+2)
      return(paste(z[lo:hi],collapse="\n"))
    }

    paste(utils::tail(z,n),collapse="\n")
  }

  observeEvent(input$run_inla,{
    current <- inla_process()
    if(!is.null(current)){
      alive <- tryCatch(current$is_alive(),error=function(e) FALSE)
      if(isTRUE(alive)){
        inla_status("INLA is already running. Cancel the current run before starting another one.")
        return()
      }
    }

    if(!requireNamespace("INLA",quietly=TRUE)){
      inla_state("error")
      inla_status("INLA is not installed. Run source('install_inla.R') once, restart R, then try again.")
      return()
    }
    if(!requireNamespace("rmarkdown",quietly=TRUE)){
      inla_state("error")
      inla_status("Package 'rmarkdown' is not installed. Run source('install_packages.R') first.")
      return()
    }
    if(!requireNamespace("callr",quietly=TRUE)){
      inla_state("error")
      inla_status("Package 'callr' is not installed. Run source('install_packages.R') first.")
      return()
    }

    dir.create(file.path("data","derived"),showWarnings=FALSE,recursive=TRUE)

    # Prevent stale diagnostics from being mistaken for the current run.
    if(file.exists(live_diag_path)) unlink(live_diag_path)
    if(file.exists(live_screening_path)) unlink(live_screening_path)
    if(file.exists(inla_log_path)) unlink(inla_log_path)
    if(file.exists(inla_err_path)) unlink(inla_err_path)

    inla_metrics(NULL)
    inla_state("running")
    inla_started_at(Sys.time())
    inla_status("Starting a separate R process for the full INLA/SPDE workflow...")

    project_dir <- normalizePath(".",winslash="/",mustWork=TRUE)
    rmd_file <- normalizePath(
      file.path("analysis","Lembang_IJGI_Reproducible_Analysis.Rmd"),
      winslash="/",
      mustWork=TRUE
    )
    raw_dir <- normalizePath(
      file.path("data","raw"),
      winslash="/",
      mustWork=TRUE
    )
    log_file <- normalizePath(
      file.path("data","derived"),
      winslash="/",
      mustWork=TRUE
    )
    log_file <- file.path(log_file,"inla_run.log")
    err_file <- file.path(dirname(log_file),"inla_error.log")

    parent_libpaths <- .libPaths()

    proc <- tryCatch(
      callr::r_bg(
        func=function(project_dir,rmd_file,raw_dir,parent_libpaths){
          setwd(project_dir)
          .libPaths(unique(c(parent_libpaths,.libPaths())))
          options(warn=1)

          cat("Background R version:",R.version.string,"\n")
          cat("Library paths:\n",paste(.libPaths(),collapse="\n"),"\n")
          cat("INLA available:",requireNamespace("INLA",quietly=TRUE),"\n")
          cat("rmarkdown available:",requireNamespace("rmarkdown",quietly=TRUE),"\n")
          cat("Starting manuscript computation without Pandoc conversion...\n")

          # run_pandoc=FALSE executes all R Markdown chunks but avoids a
          # separate document-conversion failure after the scientific results
          # have already been produced.
          rmarkdown::render(
            input=rmd_file,
            output_format="html_document",
            params=list(data_dir=raw_dir),
            envir=new.env(parent=globalenv()),
            quiet=FALSE,
            clean=FALSE,
            run_pandoc=FALSE
          )

          cat("R Markdown computation finished.\n")
          sync_script <- file.path(project_dir,"scripts","sync_manuscript.py")
          py <- Sys.which("python3"); if(!nzchar(py)) py <- Sys.which("python")
          if(file.exists(sync_script) && nzchar(py)){
            cat("Synchronizing manuscript from regenerated outputs...\n")
            sync_status <- tryCatch(system2(py,c(sync_script),stdout=TRUE,stderr=TRUE),error=function(e) paste("SYNC ERROR:",conditionMessage(e)))
            cat(paste(sync_status,collapse="\n"),"\n")
          } else {
            cat("Manuscript auto-sync skipped: Python or sync script not available.\n")
          }
          invisible(TRUE)
        },
        args=list(
          project_dir=project_dir,
          rmd_file=rmd_file,
          raw_dir=raw_dir,
          parent_libpaths=parent_libpaths
        ),
        libpath=parent_libpaths,
        stdout=log_file,
        stderr=err_file,
        supervise=TRUE
      ),
      error=function(e) e
    )

    if(inherits(proc,"error")){
      inla_state("error")
      inla_status(paste("Could not start the INLA background process:",conditionMessage(proc)))
      inla_process(NULL)
      return()
    }

    inla_process(proc)
    inla_pid(tryCatch(proc$get_pid(),error=function(e) NA_integer_))
    inla_status(
      paste0(
        "INLA is running in a background R process",
        if(is.finite(inla_pid())) paste0(" (PID ",inla_pid(),")") else "",
        ". You can continue using the dashboard."
      )
    )
  })

  observeEvent(input$cancel_inla,{
    proc <- inla_process()
    if(is.null(proc)){
      inla_status("There is no active INLA process to cancel.")
      return()
    }
    alive <- tryCatch(proc$is_alive(),error=function(e) FALSE)
    if(isTRUE(alive)){
      try(proc$kill(),silent=TRUE)
      inla_state("cancelled")
      inla_status("INLA run was cancelled by the user.")
    } else {
      inla_status("The INLA process is no longer running.")
    }
  })

  inla_poll <- reactiveTimer(1000,session)

  observe({
    inla_poll()
    req(identical(inla_state(),"running"))

    proc <- inla_process()
    if(is.null(proc)) return()

    alive <- tryCatch(proc$is_alive(),error=function(e) FALSE)
    elapsed_sec <- if(!is.null(inla_started_at())){
      as.numeric(difftime(Sys.time(),inla_started_at(),units="secs"))
    } else 0

    if(isTRUE(alive)){
      elapsed_txt <- if(elapsed_sec < 60){
        paste0(round(elapsed_sec)," s")
      } else {
        paste0(round(elapsed_sec/60,1)," min")
      }
      inla_status(
        paste0(
          "INLA is RUNNING",
          if(is.finite(inla_pid())) paste0(" (PID ",inla_pid(),")") else "",
          " · elapsed ",elapsed_txt,
          ". The dashboard remains responsive."
        )
      )
      return()
    }

    exit_status <- tryCatch(proc$get_exit_status(),error=function(e) NA_integer_)

    # Scientific success is determined from the generated diagnostic file,
    # not only from the child process exit code. A later report-formatting
    # failure must not discard an already completed INLA fit.
    if(file.exists(live_diag_path)){
      new_metrics <- tryCatch(
        readr::read_csv(live_diag_path,show_col_types=FALSE),
        error=function(e) NULL
      )

      if(!is.null(new_metrics) && nrow(new_metrics)>0){
        inla_metrics(new_metrics)
        elapsed_min <- round(elapsed_sec/60,1)

        if(identical(exit_status,0L)){
          inla_state("done")
          inla_status(
            paste0(
              "INLA completed successfully",
              if(is.finite(elapsed_min)) paste0(" in ",elapsed_min," min") else "",
              ". Diagnostics and manuscript-derived map layers were refreshed."
            )
          )
        } else {
          inla_state("done_warning")
          inla_status(
            paste0(
              "The core INLA models completed and diagnostics were recovered",
              if(is.finite(elapsed_min)) paste0(" after ",elapsed_min," min") else "",
              ". The background report process later returned exit status ",
              ifelse(is.na(exit_status),"unknown",exit_status),
              ". Results are available; see the log for the post-processing warning."
            )
          )
        }
        return()
      }
    }

    inla_state("error")
    inla_status(
      paste0(
        "INLA stopped before producing diagnostics (exit status ",
        ifelse(is.na(exit_status),"unknown",exit_status),
        "). ",
        inla_error_summary(5)
      )
    )
  })

  output$inla_progress_ui <- renderUI({
    inla_poll()
    st <- inla_state()

    if(identical(st,"running")){
      elapsed_sec <- if(!is.null(inla_started_at())){
        as.numeric(difftime(Sys.time(),inla_started_at(),units="secs"))
      } else 0
      elapsed_txt <- if(elapsed_sec < 60){
        paste0(round(elapsed_sec)," seconds")
      } else {
        paste0(round(elapsed_sec/60,1)," minutes")
      }

      div(
        class="inla-progress-wrap",
        div(
          class="inla-status-row",
          span(class="status-dot running-dot"),
          strong("INLA IS RUNNING"),
          span(class="inla-elapsed",paste0(" · ",elapsed_txt))
        ),
        div(
          class="inla-progress-track",
          div(class="inla-progress-bar inla-progress-animated")
        ),
        div(
          class="small text-muted mt-1",
          "Background process is active. Progress is indeterminate because INLA runtime depends on model complexity and convergence."
        )
      )
    } else if(identical(st,"done")){
      div(
        class="inla-status-row",
        span(class="status-dot done-dot"),
        strong("INLA COMPLETED")
      )
    } else if(identical(st,"done_warning")){
      div(
        class="inla-status-row",
        span(class="status-dot done-dot"),
        strong("INLA COMPLETED — REPORT WARNING")
      )
    } else if(identical(st,"cancelled")){
      div(
        class="inla-status-row",
        span(class="status-dot idle-dot"),
        strong("INLA CANCELLED")
      )
    } else if(identical(st,"error")){
      div(
        class="inla-status-row",
        span(class="status-dot error-dot"),
        strong("INLA ERROR")
      )
    } else {
      div(
        class="inla-status-row",
        span(class="status-dot idle-dot"),
        strong("INLA NOT STARTED")
      )
    }
  })

  output$inla_run_status <- renderUI({
    st <- inla_state()
    if(identical(st,"error")){
      div(
        class="note-box",
        strong("INLA error"),
        tags$p(inla_status()),
        tags$pre(
          style="white-space:pre-wrap;max-height:220px;overflow:auto;margin-top:8px;",
          inla_error_summary(8)
        )
      )
    } else {
      div(class="note-box",inla_status())
    }
  })

  output$inla_log_tail <- renderText({
    inla_poll()
    tail_inla_log(18)
  })

  output$inla_ready <- reactive(!is.null(inla_metrics()))
  outputOptions(output,"inla_ready",suspendWhenHidden=FALSE)

  output$inla_gate <- renderUI({
    if(is.null(inla_metrics())){
      div(
        class="inla-empty-state",
        icon("chart-line"),
        h4("No diagnostics from a completed INLA run yet"),
        p("Use 'Run Full INLA Workflow' above. While it is running, the status indicator and log update in this tab.")
      )
    } else {
      div(
        class="note-box",
        "Live diagnostics are available from the most recent completed INLA run."
      )
    }
  })

  output$inla_live_table <- renderDT({
    req(inla_metrics())
    datatable(
      inla_metrics(),
      rownames=FALSE,
      options=list(dom="t",scrollX=TRUE)
    )
  })

  output$inla_metric_plot <- renderPlotly({
    d <- inla_metrics()
    req(d)

    blank_plotly <- function(msg){
      plot_ly() %>%
        layout(
          paper_bgcolor="rgba(0,0,0,0)",
          plot_bgcolor="rgba(0,0,0,0)",
          font=list(color=axis_color()),
          xaxis=list(visible=FALSE),
          yaxis=list(visible=FALSE),
          annotations=list(list(
            text=msg,
            x=.5,y=.5,
            xref="paper",yref="paper",
            showarrow=FALSE,
            font=list(size=15,color=axis_color())
          )),
          margin=list(l=20,r=20,t=20,b=20)
        )
    }

    d <- as.data.frame(d)

    # Detect DIC/WAIC case-insensitively because exported diagnostics may
    # use DIC/WAIC, dic/waic, or mixed-case names.
    nms <- names(d)
    metric_idx <- which(toupper(nms) %in% c("DIC","WAIC"))

    if(!length(metric_idx)){
      return(blank_plotly(
        "DIC and WAIC are not available in the current completed diagnostics."
      ))
    }

    metric_cols <- nms[metric_idx]

    # Model label is also detected case-insensitively. If absent, generate
    # a stable row label rather than failing the plot.
    model_idx <- which(tolower(nms)=="model")
    if(length(model_idx)){
      d$Model_display <- as.character(d[[model_idx[1]]])
    } else {
      d$Model_display <- paste0("Model ",seq_len(nrow(d)))
    }

    for(nm in metric_cols){
      d[[nm]] <- suppressWarnings(as.numeric(d[[nm]]))
    }

    long <- tidyr::pivot_longer(
      d,
      cols=all_of(metric_cols),
      names_to="metric",
      values_to="value"
    ) %>%
      dplyr::mutate(metric=toupper(metric)) %>%
      dplyr::filter(is.finite(value))

    if(!nrow(long)){
      return(blank_plotly(
        "DIC/WAIC columns were found, but they contain no finite numeric values."
      ))
    }

    p <- ggplot(long,aes(x=Model_display,y=value,fill=metric))+
      geom_col(position="dodge",width=.7,na.rm=TRUE)+
      scale_fill_manual(
        values=unname(c("#27B7FF","#2AD4C9")),
        breaks=c("DIC","WAIC")
      )+
      labs(x=NULL,y="Criterion",fill=NULL)+
      transparent_theme()

    plotly_transparent(p)
  })

  # -------- BAYESIAN HAZARD & SCREENING --------
  posterior_year_path <- file.path("data","derived","posterior_exceedance_year_live.csv")
  posterior_summary_path <- file.path("data","derived","posterior_exceedance_summary_live.csv")
  posterior_highres_path <- file.path("data","derived","posterior_spde_highres_live.csv")

  posterior_year_data <- reactive({
    if(!file.exists(posterior_year_path)) return(NULL)
    tryCatch(
      readr::read_csv(posterior_year_path,show_col_types=FALSE),
      error=function(e) NULL
    )
  })

  posterior_summary_data <- reactive({
    if(!file.exists(posterior_summary_path)) return(NULL)
    tryCatch(
      readr::read_csv(posterior_summary_path,show_col_types=FALSE),
      error=function(e) NULL
    )
  })

  posterior_highres_data <- reactive({
    if(!file.exists(posterior_highres_path)) return(NULL)
    tryCatch(
      readr::read_csv(posterior_highres_path,show_col_types=FALSE),
      error=function(e) NULL
    )
  })

  output$bayes_map_title <- renderText({
    switch(
      input$bayes_map_product,
      posterior_avg="Posterior Smoothed Exceedance Fraction — District Average",
      posterior_year=paste("Posterior Smoothed Exceedance Fraction —",input$bayes_year),
      posterior_uncertainty="Posterior Uncertainty — Mean 95% Credible-Interval Width",
      posterior_highres="High-Resolution SPDE Posterior Screening Surface",
      screening="Integrated Relative Seismic Screening Score"
    )
  })

  output$bayes_map <- renderLeaflet({
    m <- leaflet(options=leafletOptions(preferCanvas=TRUE,minZoom=5)) %>%
      add_basemap(input$bayes_basemap)

    fault <- get_fault()
    d <- get_districts()

    m <- m %>%
      addPolylines(data=fault,color="#FF3B30",weight=5,opacity=.95)

    product <- input$bayes_map_product

    if(product=="posterior_highres"){
      hr <- posterior_highres_data()
      if(is.null(hr) || !nrow(hr)){
        return(
          m %>%
            addPolygons(data=d,fill=FALSE,color="#8EA9B9",weight=.6) %>%
            addControl(
              html="<div class='map-empty-note'><b>High-resolution SPDE surface not available.</b><br>Run the Full INLA Workflow in INLA Diagnostics.</div>",
              position="topright"
            )
        )
      }

      pal <- colorNumeric(
        c("#081D58","#225EA8","#41B6C4","#FDE725"),
        domain=hr$posterior_exceedance_hr,
        na.color="#46525C"
      )

      m <- m %>%
        addCircleMarkers(
          data=hr,
          lng=~longitude,
          lat=~latitude,
          radius=3,
          stroke=FALSE,
          fillOpacity=.78,
          color=~pal(posterior_exceedance_hr),
          popup=~paste0(
            "<b>SPDE posterior screening surface</b><br>",
            "Posterior exceedance: ",format(posterior_exceedance_hr,scientific=TRUE,digits=4)
          )
        ) %>%
        addPolygons(data=d,fill=FALSE,color="#E3EEF4",weight=.45,opacity=.75) %>%
        addLegend(
          "bottomright",
          pal=pal,
          values=hr$posterior_exceedance_hr,
          title="Posterior exceedance"
        )

      bb <- sf::st_bbox(d)
      return(m %>% fitBounds(
        as.numeric(bb["xmin"]),as.numeric(bb["ymin"]),
        as.numeric(bb["xmax"]),as.numeric(bb["ymax"])
      ))
    }

    if(product=="screening"){
      if(!file.exists(live_screening_path)){
        return(
          m %>% addPolygons(data=d,fill=FALSE,color="#8EA9B9",weight=.6) %>%
            addControl(
              html="<div class='map-empty-note'><b>Screening output not available.</b><br>Run the Full INLA Workflow.</div>",
              position="topright"
            )
        )
      }

      dd <- readr::read_csv(live_screening_path,show_col_types=FALSE)
      dd$key <- paste(clean_key(dd$kabupaten),clean_key(dd$kecamatan),sep="__")
      p <- dplyr::left_join(d,dd,by="key")
      vals <- p$Screening_Score

      pal <- colorNumeric(
        c("#081D58","#225EA8","#41B6C4","#FDE725"),
        domain=vals,
        na.color="#46525C"
      )

      bb <- sf::st_bbox(p)
      return(
        m %>%
          addPolygons(
            data=p,
            fillColor=pal(vals),
            fillOpacity=.78,
            color="#D9E7EF",
            weight=.65,
            popup=paste0(
              "<b>",p$kabupaten_kota," — ",p$kecamatan,"</b><br>",
              "Screening score: ",sprintf("%.2f",vals)
            )
          ) %>%
          addLegend("bottomright",pal=pal,values=vals,title="Screening score") %>%
          fitBounds(
            as.numeric(bb["xmin"]),as.numeric(bb["ymin"]),
            as.numeric(bb["xmax"]),as.numeric(bb["ymax"])
          )
      )
    }

    if(product=="posterior_year"){
      dd <- posterior_year_data()
      if(is.null(dd)) return(
        m %>% addPolygons(data=d,fill=FALSE,color="#8EA9B9",weight=.6) %>%
          addControl(
            html="<div class='map-empty-note'><b>Posterior output not available.</b><br>Run the Full INLA Workflow.</div>",
            position="topright"
          )
      )
      dd <- dd %>% filter(Tahun==input$bayes_year)
      var <- "posterior_exceedance"
      title <- paste("Posterior exceedance",input$bayes_year)
    } else {
      dd <- posterior_summary_data()
      if(is.null(dd)) return(
        m %>% addPolygons(data=d,fill=FALSE,color="#8EA9B9",weight=.6) %>%
          addControl(
            html="<div class='map-empty-note'><b>Posterior output not available.</b><br>Run the Full INLA Workflow.</div>",
            position="topright"
          )
      )
      if(product=="posterior_uncertainty"){
        var <- "ci_width_avg"
        title <- "Mean 95% CI width"
      } else {
        var <- "p_mean_avg"
        title <- "Posterior exceedance"
      }
    }

    dd$key <- paste(clean_key(dd$kabupaten),clean_key(dd$kecamatan),sep="__")
    p <- dplyr::left_join(d,dd,by="key")
    vals <- suppressWarnings(as.numeric(p[[var]]))

    pal <- colorNumeric(
      c("#081D58","#225EA8","#41B6C4","#FDE725"),
      domain=vals,
      na.color="#46525C"
    )

    bb <- sf::st_bbox(p)
    m %>%
      addPolygons(
        data=p,
        fillColor=pal(vals),
        fillOpacity=.80,
        color="#D9E7EF",
        weight=.65,
        popup=paste0(
          "<b>",p$kabupaten_kota," — ",p$kecamatan,"</b><br>",
          title,": ",
          ifelse(is.na(vals),"NA",format(vals,scientific=TRUE,digits=4))
        )
      ) %>%
      addLegend("bottomright",pal=pal,values=vals,title=title) %>%
      fitBounds(
        as.numeric(bb["xmin"]),as.numeric(bb["ymin"]),
        as.numeric(bb["xmax"]),as.numeric(bb["ymax"])
      )
  })

  output$bayes_summary_ui <- renderUI({
    dd <- posterior_summary_data()
    if(is.null(dd)){
      return(div(
        class="note-box",
        "Run the Full INLA Workflow first. Posterior maps are generated from the completed full spatial-temporal model."
      ))
    }

    vals <- suppressWarnings(as.numeric(dd$p_mean_avg))
    vals <- vals[is.finite(vals)]
    if(!length(vals)) return(div(class="note-box","No finite posterior values are available."))

    tagList(
      div(class="note-box",
          tags$b("Paper quantity: "),
          "district-level posterior fitted exceedance-screening probability, averaged over model years."),
      tags$ul(
        tags$li(paste0("Median: ",format(stats::median(vals),scientific=TRUE,digits=4))),
        tags$li(paste0("Maximum: ",format(max(vals),scientific=TRUE,digits=4))),
        tags$li(paste0("Minimum: ",format(min(vals),scientific=TRUE,digits=4))),
        tags$li("Values may round to 0.000 at three decimals; use scientific notation to inspect spatial differences.")
      )
    )
  })

  output$bayes_top_table <- renderDT({
    dd <- posterior_summary_data()
    req(dd)
    z <- dd %>%
      arrange(desc(p_mean_avg)) %>%
      transmute(
        Regency=kabupaten,
        District=kecamatan,
        Posterior_Exceedance=p_mean_avg,
        CI_Width=ci_width_avg
      ) %>%
      slice_head(n=12)

    datatable(z,rownames=FALSE,options=list(dom="t",scrollX=TRUE)) %>%
      formatSignif(c("Posterior_Exceedance","CI_Width"),4)
  })

  output$bayes_year_plot <- renderPlotly({
    dd <- posterior_year_data()
    req(dd)

    z <- dd %>%
      group_by(Tahun) %>%
      summarise(
        median=median(posterior_exceedance,na.rm=TRUE),
        q25=quantile(posterior_exceedance,.25,na.rm=TRUE),
        q75=quantile(posterior_exceedance,.75,na.rm=TRUE),
        .groups="drop"
      )

    p <- ggplot(z,aes(Tahun,median))+
      geom_ribbon(aes(ymin=q25,ymax=q75),alpha=.20)+
      geom_line(linewidth=1.1,color="#27B7FF")+
      geom_point(size=2.5,color="#2AD4C9")+
      scale_y_continuous(labels=scales::label_scientific())+
      labs(x="Year",y="Posterior smoothed exceedance fraction")+
      transparent_theme()

    plotly_transparent(p)
  })

  output$bayes_uncertainty_plot <- renderPlotly({
    dd <- posterior_summary_data()
    req(dd)

    z <- dd %>%
      filter(is.finite(p_mean_avg),is.finite(ci_width_avg))

    p <- ggplot(z,aes(p_mean_avg,ci_width_avg))+
      geom_point(alpha=.65,color="#27B7FF")+
      scale_x_continuous(labels=scales::label_scientific())+
      scale_y_continuous(labels=scales::label_scientific())+
      labs(
        x="Mean posterior exceedance",
        y="Mean 95% credible-interval width"
      )+
      transparent_theme()

    plotly_transparent(p)
  })

  # -------- PAPER-SYNCHRONIZED RANKING / SENSITIVITY --------
  top10_live_path <- file.path("data","derived","top10_screening_live.csv")
  sensitivity_live_path <- file.path("data","derived","sensitivity_analysis_live.csv")

  live_top10 <- reactive({
    if(!file.exists(top10_live_path)) return(NULL)
    tryCatch(readr::read_csv(top10_live_path,show_col_types=FALSE),error=function(e) NULL)
  })

  live_sensitivity <- reactive({
    if(!file.exists(sensitivity_live_path)) return(NULL)
    tryCatch(readr::read_csv(sensitivity_live_path,show_col_types=FALSE),error=function(e) NULL)
  })

  ranking_filtered <- reactive({
    d <- live_top10()
    req(d)
    n <- min(input$top_n,nrow(d))
    d %>% arrange(Rank) %>% slice_head(n=n)
  })

  output$ranking_plot <- renderPlot({
    d <- live_top10()
    if(is.null(d)){
      plot.new(); text(.5,.5,"Run the Full INLA Workflow to generate paper-synchronized ranking results.",cex=1.1)
      return(invisible())
    }
    d <- ranking_filtered()
    ggplot(d,aes(x=reorder(kecamatan,Screening_Score),y=Screening_Score))+
      geom_col(fill="#27B7FF",width=.72)+coord_flip()+
      labs(x=NULL,y="Screening score")+transparent_theme()
  },res=120,bg="transparent")

  output$ranking_table <- renderDT({
    d <- live_top10()
    if(is.null(d)){
      return(datatable(
        data.frame(Status="Run the Full INLA Workflow to generate manuscript-ready ranking results."),
        rownames=FALSE,options=list(dom="t")
      ))
    }
    datatable(ranking_filtered(),rownames=FALSE,options=list(scrollX=TRUE,pageLength=10)) |>
      formatRound(c("Screening_Score","p_mean_avg"),6)
  })

  sensitivity_filtered <- reactive({
    d <- live_sensitivity()
    req(d)
    d %>% filter(sensitivity_factor==input$sens_factor)
  })

  observe({
    d <- live_sensitivity()
    if(!is.null(d) && nrow(d)){
      updateSelectInput(session,"sens_factor",choices=unique(d$sensitivity_factor),selected=unique(d$sensitivity_factor)[1])
    }
  })

  output$sensitivity_plot <- renderPlot({
    d0 <- live_sensitivity()
    if(is.null(d0)){
      plot.new(); text(.5,.5,"Run the Full INLA Workflow to regenerate sensitivity analysis under the aligned vulnerability definition.",cex=1.0)
      return(invisible())
    }
    d <- sensitivity_filtered() %>% mutate(setting=factor(setting,levels=rev(setting)))
    ggplot(d,aes(spearman_rho,setting))+
      geom_segment(aes(x=0,xend=spearman_rho,yend=setting),linewidth=2.8,color="#35566A")+
      geom_point(size=4,color="#2AD4C9")+
      labs(x="Spearman rho",y=NULL)+transparent_theme()
  },res=120,bg="transparent")

  output$sensitivity_table <- renderDT({
    d <- live_sensitivity()
    if(is.null(d)){
      return(datatable(
        data.frame(Status="Run the Full INLA Workflow to generate manuscript-ready sensitivity results."),
        rownames=FALSE,options=list(dom="t")
      ))
    }
    datatable(sensitivity_filtered(),rownames=FALSE,options=list(dom="t",scrollX=TRUE))
  })

  output$methods_table <- renderUI({
    table_ui(c(
      "Common support"="227 districts (kecamatan)",
      "Exposure indicators"="Population density, health facilities, education facilities",
      "Vulnerability indicator"="2024 inverse BPS Sinyal/connectivity screening proxy",
      "Bayesian layer"="INLA latent Gaussian model",
      "Spatial structure"="Matérn SPDE",
      "Temporal structure"="RW1",
      "Baseline weighting"="60% shaking, 30% exposure, 10% vulnerability"
    ))
  })

  dl <- function(data,filename)
    downloadHandler(filename=function()filename,content=function(file)readr::write_csv(data,file))
  output$download_top10 <- downloadHandler(
    filename=function() "top10_screening_live.csv",
    content=function(file){
      d <- live_top10(); validate(need(!is.null(d),"Run Full INLA Workflow first.")); readr::write_csv(d,file)
    }
  )
  output$download_top10_2 <- downloadHandler(
    filename=function() "top10_screening_live.csv",
    content=function(file){
      d <- live_top10(); validate(need(!is.null(d),"Run Full INLA Workflow first.")); readr::write_csv(d,file)
    }
  )
  output$download_sensitivity <- downloadHandler(
    filename=function() "sensitivity_analysis_live.csv",
    content=function(file){
      d <- live_sensitivity(); validate(need(!is.null(d),"Run Full INLA Workflow first.")); readr::write_csv(d,file)
    }
  )
  output$download_models <- dl(models,"reported_model_comparison.csv")
  manuscript_dir_live <- file.path("data","manuscript")
  synced_paper_live <- file.path(manuscript_dir_live,"Lembang_IJGI_FINAL_SYNCED.docx")
  output$download_synced_paper <- downloadHandler(filename=function() "Lembang_IJGI_FINAL_SYNCED.docx",content=function(file){validate(need(file.exists(synced_paper_live),"Run Full INLA Workflow first."));file.copy(synced_paper_live,file,overwrite=TRUE)})
  output$download_manuscript_bundle <- downloadHandler(filename=function() "Lembang_IJGI_Manuscript_Outputs.zip",content=function(file){validate(need(dir.exists(manuscript_dir_live),"Run Full INLA Workflow first.")); candidates <- list.files(manuscript_dir_live,full.names=TRUE,pattern="\\.(csv|png|txt|docx)$",ignore.case=TRUE); validate(need(length(candidates)>0,"No manuscript outputs are available yet.")); oldwd<-getwd(); on.exit(setwd(oldwd),add=TRUE); setwd(manuscript_dir_live); utils::zip(zipfile=normalizePath(file,mustWork=FALSE),files=basename(candidates),flags="-r9X")})
}

shinyApp(ui,server)