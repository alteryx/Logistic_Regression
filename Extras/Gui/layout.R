library(jeeves)
library(htmltools)
pluginDir <- '.'
config <- renderPluginWidgets(pluginDir, wrapInDiv = TRUE)

yml <- 'Setup:
  - "Model Name"
  - "Y Var"
  - "X Vars"
Customize:
  - Modelsadfasfa:
    - Link
    - "Use Weights"
    - "Weight Vec"
  - Plotsafasdf:
    - graph.resolution
'

spec <- yaml::yaml.load(yml)

makeLayoutFromSpec <- function(spec, config){
  page1 <- spec[[1]]
  setupPage <- jvSetupPage(title = names(spec)[1],
    config[page1]
  )
  page2 <- spec[[2]]
  sectionNames <- sapply(page2, function(x){names(x)})
  sections <- lapply(page2, makeSection, config = config)
  customizePage <- jvCustomizePage(title = names(spec)[2],
    do.call(jvTabbedContent, sections)
  )
  ui <- tagList(setupPage, customizePage)
  di <- jvMakeDataItemsToInitialize(
    curPage = 'Home', 
    curTab = makeHtmlId(sectionNames[1])
  )
  displayRules = list(
    `div-weight-vec` = "Use Weights"
  )
  jsonDisplayRules <- jsonlite::toJSON(
    displayRules, auto_unbox = TRUE, pretty = TRUE
  )
  
  myPage <- tagList(ui, tags$script(
    makeJsVariable(di, 'items'), 
    makeJsVariable(jsonDisplayRules, 'displayRules')
  ))
  
  writeLines(as.character(myPage), file.path(pluginDir, 'Extras/Gui/layout.html'))
}

makeSubsection <- function()

# makeSection <- function(title, nms, config){
#   jvTabPage(title = title, config[nms])
# }
makeSection <- function(section, config){
  jvTabPage(title = names(section), config[section[[1]]])
}



setupPage <- jvSetupPage(title = 'Setup',
  config$`Model Name`, config$`Y Var`, config$`X Vars`              
)

tabContent <- jvTabbedContent(
  jvTabPage(title = 'Model', id = 'advanced',
    config$Link, 
    config$`Use Weights`, 
    div(class = 'indent-one', config$`Weight Vec`)      
  ),
  jvTabPage(title = 'Plots', id = 'graphics',
    config$graph.resolution
  )
)
customizePage <- jvCustomizePage(title = 'Customize', tabContent)

ui <- tagList(setupPage, customizePage)
di <- jvMakeDataItemsToInitialize(
  curPage = 'Home', 
  curTab = 'advanced'
)
displayRules = list(
  `div-weight-vec` = "Use Weights"
)
jsonDisplayRules <- jsonlite::toJSON(
  displayRules, auto_unbox = TRUE, pretty = TRUE
)


myPage <- tagList(ui, tags$script(
  makeJsVariable(di, 'items'), 
  makeJsVariable(jsonDisplayRules, 'displayRules')
))

writeLines(as.character(myPage), file.path(pluginDir, 'Extras/Gui/layout.html'))
updatePlugin()
