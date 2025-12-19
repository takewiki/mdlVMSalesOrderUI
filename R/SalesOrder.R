library(shiny)
library(DT)
library(shinyWidgets)
SalesOrder_all_columns_ui <- c(
  'Sales OrderID',
  'PN',
  'Product Name',
  'Serial Number',
  'Delivery Date',
  'Delivery Location',
  'Sales OrderQty',
  'Production Date'
 # 'Sales OrderID2',

  #'Total DeliveryQty'
)
#设置默认值
SalesOrder_default_columns_ui <- c(
  'Sales OrderID',
  'PN',
  'Product Name',
  'Serial Number',
  'Delivery Date',
  'Delivery Location'


)
#' 序列号查询生成器界面
#'
#' @param colTitles  主页标题
#' @param widthRates 左右比例
#' @param func_left 左函数
#' @param func_right 右函数
#' @param tabTitle 标题
#' @param func_bottom  下面一栏
#'
#' @return 返回值
#' @import tsui
#' @export
#'
#' @examples
#' SalesOrderUI()
SalesOrderUI <- function(tabTitle ='Sales Order',
                           colTitles =c('Operation Area','Column Setting','Result Area'),
                           widthRates =c(6,6,12),
                           func_left = SalesOrderUI_left,
                           func_right =SalesOrderUI_right,
                           func_bottom = SalesOrderUI_bottom
) {

  #三栏式设置，可以复制
  res = tsui::uiGen3(tabTitle = tabTitle,colTitles =colTitles,widthRates = widthRates,func_left = func_left,func_right = func_right,func_bottom = func_bottom )
  return(res)

}





#' 请输入文件
#'
#' @return 返回值
#' @export
#'
#' @examples
#' SalesOrderUI_left()
SalesOrderUI_left <- function() {


  res <- tagList(
    tsui::mdl_text2(id ='text_SalesOrder' ,label ='Sales Order',value = ''),
    shiny::actionButton(inputId = 'btn_SalesOrder_view',label = 'Search'),

    tsui::mdl_download_button(id = 'dl_SalesOrder',label = 'Download')








  )
  return(res)

}


#' 生成区域
#'
#' @return 返回值
#' @export
#'
#' @examples
#' SalesOrderUI_bottom()
SalesOrderUI_right <- function() {
  res <- tagList(
    pickerInput(
      inputId = "SalesOrder_column_selector",
      label = "Selected Columns:",
      choices = SalesOrder_all_columns_ui,
      selected = SalesOrder_default_columns_ui,
      options = list(
        `actions-box` = TRUE,
        `selected-text-format` = "count > 3",
       # `count-selected-text` = "{0} Columns Seleced",
        `count-selected-text` = "Default Value",
        size = 10,
        `live-search` = TRUE,
        `live-search-style` = "contains"
      ),
      multiple = TRUE,
      choicesOpt = list(
        style = rep(("color: black;"), 100)
      )
    ),
    # 全选/取消全选按钮
    actionButton("btn_SalesOrder_select_all", "Select All",
                 class = "btn-primary btn-sm",
                 style = "margin-right: 5px;"),
    # actionButton("btn_SalesOrder_deselect_all", "Deselect All",
    #              class = "btn-secondary btn-sm"),
    actionButton("btn_SalesOrder_defaultValue", "Default Value",
                 class = "btn-primary btn-sm")
    #verbatimTextOutput("SalesOrder_selection_info")






  )
  return(res)

}


#' 生成区域
#'
#' @return 返回值
#' @export
#'
#' @examples
#' SalesOrderUI_bottom()
SalesOrderUI_bottom <- function() {
  res <- tagList(
    tsui::uiScrollX(tsui::mdl_dataTable(id = 'SalesOrder_resultView',label ='结果显示' ))

  )
  return(res)

}
