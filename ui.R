# ⓒ 2020 지성. all rights reserved.
# <llllllllll@kakao.com>
# GNU General Public License v3.0

# 패키지가 존재하는지 확인
packages <- 'rstudioapi'
if(length(setdiff(packages, rownames(installed.packages()))) > 0) {
    install.packages(setdiff(packages, rownames(installed.packages())))
}
library('rstudioapi')
current_dir <- dirname(rstudioapi::getSourceEditorContext()$path)


package_in <- function(p_name, option=1) {
    packages <- p_name
    if(length(setdiff(packages, rownames(installed.packages()))) > 0) {
        install.packages(setdiff(packages, rownames(installed.packages())))
    }
    if(option == 1) {
        library(p_name, character.only=T)
    }
}

###############################################################################

# 패키지가 존재하지 않을 경우 설치
# shiny
package_in('shinydashboard')
package_in('shinydashboardPlus')
package_in('shiny')
package_in('shinythemes')

# data load
package_in('DT')

# summary
package_in('data.table')
package_in('grid')
package_in('ggplot2')

# decision tree
package_in('C50')
package_in('gmodels')

# JRip
#package_in('RWeka')

# knn
package_in('class')
package_in('gmodels')

# Naive
package_in('e1071')
package_in('data.table')

# Neuralnet
package_in('neuralnet')

# Graph
package_in('ggplot2')
package_in('dplyr')
package_in('plotly')
package_in('lattice')
package_in('GGally')

###############################################################################

# GUI
ui <- navbarPage(
    # 테마 설정
    'Data Aanalyst',
    theme = shinytheme('cyborg'),
    
    # Tab
    tabPanel(
        'Home',
        'Data Analyst ver 1.0.0',
        # Data Load
        dropdownBlock(
            id='opendata',
            title='Open Data',
            icon='file',
            fileInput(
                'file',
                'Choose Your Data',
                multiple=F,
                accept=c(
                    'text/csv',
                    '.xlsx',
                    '.txt',
                    'text/comma-separated-values,text/plain',
                    '.csv'
                )
            )
        )
    ),
    tabPanel(
        'Table',
        DT::dataTableOutput('table')
    ),
    navbarMenu(
        'Statistics',
        tabPanel(
            'Summary',
            fluidRow(
                box(
                    title='Summary',
                    solidHeader=T,
                    collapsible=T,
                    width=6,
                    verbatimTextOutput('stat_summary')
                ),
                box(
                    title='Quantile',
                    solidHeader=T,
                    collapsible=T,
                    width=6,
                    verbatimTextOutput('quan')
                ),
                box(
                    title='Quantile Graph',
                    solidHeader=T,
                    collapsible=T,
                    width=12,
                    plotOutput('plot_quan', height='auto')
                ),
                box(
                    title='Distribution Graph',
                    solidHeader=T,
                    collapsible=T,
                    width=12,
                    plotOutput('plot_dist', height='auto')
                )
            )
        ),
        tabPanel(
            'Regression',
            fluidRow(
                box(
                    title='Data',
                    solidHeader=T,
                    collapsible=T,
                    width=9,
                    verbatimTextOutput('submit_input_sample_regression')
                ),
                box(
                    width=2,
                    uiOutput('dependents_delcol_regression',
                             style='overflow-y:scroll;  
                                max-height:
                                100px;
                                background: ghostwhite;'
                    ),
                    uiOutput('dependents_selcol_regression'), 
                    uiOutput('dependents_button_regression') 
                ),
                box(
                    title='ANOVA Table',
                    solidHeader=T,
                    collapsible=T,
                    width=9,
                    verbatimTextOutput('anova_Render_regression')
                ),
                box(
                    title='Regression',
                    solidHeader=T,
                    collapsible=T,
                    width=9,
                    verbatimTextOutput('coefficient_Render_regression')
                ),
                box(
                    title='Parameter Confidence Interval',
                    solidHeader=T,
                    collapsible=T,
                    width=9,
                    verbatimTextOutput('interval_Render_regression')
                ),
                box(
                    title='Model Plot',
                    solidHeader=T,
                    collapsible=T,
                    width=9,
                    plotOutput('plot_reg', height='auto')
                )
            )
        ),
        tabPanel(
            'Decision Tree',
            fluidRow(
                box(title='Data',
                    solidHeader=T,
                    collapsible=T,
                    width=8,
                    verbatimTextOutput('submit_input_sample_decision')
                ),
                box(
                    width=2, 
                    uiOutput('dependents_delcol_decision',
                             style='overflow-y:scroll;
                                max-height: 100px;  
                                background: ghostwhite;'
                    )
                ),
                box(
                    width=2,
                    uiOutput('dependents_selcol_decision'),
                    uiOutput('dependents_selmodel_decision'),
                    uiOutput('dependents_selwinnow_decision'),
                    numericInput('trials', 'Trials:', 1, min=1),
                    numericInput('seed', 'Seed:', 1),
                    uiOutput('dependents_button_decision')
                ),
                box(title='Cross Table',
                    solidHeader=T,
                    collapsible=T,
                    width=9,
                    verbatimTextOutput('TestTableRender_decision')
                ),
                box(title='Model Summary',
                    solidHeader=T,
                    collapsible=T,
                    width=9,
                    verbatimTextOutput('TestSummaryRender_decision')
                )
            )
        )
    ),
    navbarMenu(
        'Machine Learning',
        tabPanel(
            'JRip',
            fluidRow(
                box(title='Data',
                    solidHeader=T,
                    collapsible=T,
                    width=9,
                    verbatimTextOutput('submit_input_sample_jrip')
                ),
                box(width=2,   
                    uiOutput('dependents_delcol_jrip',
                             style='overflow-y:scroll;
                                  max-height: 100px;  
                                  background: ghostwhite;'
                    ), 
                    uiOutput('dependents_selcol_jrip'),
                    numericInput('numopt', 'NumOpt', 1, min=1),
                    numericInput('seed', 'Seed:', 1),
                    uiOutput('dependents_button_jrip')
                ),   
                box(title='Cross Table',
                    solidHeader=T,
                    collapsible=T,
                    width=9,
                    verbatimTextOutput('TestTableRender_jrip')
                ),
                box(title='Model Summary',
                    solidHeader=T,
                    collapsible=T,
                    widtd=9,
                    verbatimTextOutput('TestSummaryRender_jrip')
                )
            )
        ),
        tabPanel(
            'KMeans',
            fluidRow(
                box(title='Data',
                    solidHeader=T,
                    collapsible=T,
                    width=9,
                    verbatimTextOutput('submit_input_sample_kmeans')
                ),
                box(title='Summary',
                    solidHeader=T,
                    collapsible=T,
                    width=9,
                    verbatimTextOutput('TestSummaryRender_kmeans')
                ),
                box(width=2,   
                    uiOutput('dependents_delcol_kmeans',
                             style='overflow-y:scroll;
                                  max-height: 100px;
                                  background: ghostwhite;'
                    ),
                    uiOutput('dependents_selcol_kmeans'), 
                    numericInput('seed', 'Seed', 1),
                    numericInput('k_mv', 'Centers', 2, min=2),
                    numericInput('k_nstart', 'nstart', 1),
                    uiOutput('dependents_button_kmeans')
                ),
                box(title='Cross Table',
                    solidHeader=T,
                    collapsible=T,
                    width=9,
                    verbatimTextOutput('TestTableRender_kmeans')
                ),
                box(title='Model Centers',
                    solidHeader=T,
                    collapsible=T,
                    width=9,
                    verbatimTextOutput('TestCenterRender_kmeans')
                ),
                box(title='Model Plot',
                    solidHeader=T,
                    collapsible=T,
                    collapsed=T,
                    width=9,
                    plotOutput('plot_kmeans', height='auto')
                )
            )
        ),
        tabPanel(
            'KNN',
            fluidRow(
                box(
                    title='Data',
                    solidHeader=T,
                    collapsible=T,
                    width=9,
                    verbatimTextOutput('submit_input_sample_knn')
                ),
                box(
                    title='Summary',
                    solidHeader=T,
                    collapsible=T,
                    width=9,
                    verbatimTextOutput('TestSummaryRender_knn')
                ),
                box(
                    width=2,
                    uiOutput('dependents_delcol_knn',
                             style='overflow-y:scroll;
                                max-height: 100px;
                                background: ghostwhite;'),
                    uiOutput('dependents_selcol_knn'),
                    numericInput('knn_k', 'k', 'Default', min=2),
                    uiOutput('dependents_button_knn')
                ),
                box(
                    title='Cross Table',
                    solidHeader=T,
                    collapsible=T,
                    width=9,
                    verbatimTextOutput('TestTableRender_knn')
                )
            )
        ),
        tabPanel(
            'Naive bayes',
            fluidRow(
                box(
                    title='Data',
                    solidHeader=T,
                    collapsible=T,
                    width=9,
                    verbatimTextOutput('submit_input_sample_naive')
                ),
                box(
                    width=2,   
                    uiOutput('dependents_delcol_naive',
                             style='overflow-y:scroll;
                                max-height: 100px;
                                background: ghostwhite;'),
                    uiOutput('dependents_selcol_naive'),
                    numericInput('laplace', 'Laplace', 0,
                                 min = 0, max = 1
                    ),
                    numericInput('seed', 'Seed', 1),
                    uiOutput('dependents_button_naive')
                ),
                box(
                    title='Cross Table',
                    solidHeader=T,
                    collapsible=T,
                    width=9,
                    verbatimTextOutput('TestTableRender_naive')
                ),
                box(
                    title='Model',
                    solidHeader=T,
                    collapsible=T,
                    width=9,
                    verbatimTextOutput('TestModelRender_naive')
                )
            )
        ),
        tabPanel(
            'Neural Network',
            fluidRow(
                box(
                    title='Data',
                    solidHeader=T,
                    collapsible=T,
                    width=9,
                    verbatimTextOutput('submit_input_sample_neuralnet')
                ),
                box(
                    width=2, 
                    uiOutput('dependents_delcol_neuralnet',
                             style='overflow-y:scroll;
                                max-height: 100px;
                                background: ghostwhite;'),
                    uiOutput('dependents_selcol_neuralnet'), 
                    uiOutput('dependents_type_neuralnet'), 
                    numericInput('seed', 'Seed:', 1),
                    numericInput('hidden_1', 'Hidden Layer 1:', 3, min=1),
                    numericInput('hidden_2', 'Hidden Layer 2:', 2, min=1),
                    uiOutput('dependents_button_neuralnet')  
                ),                              
                box(
                    title='Accuracy',
                    solidHeader=T,
                    collapsible=T,
                    width=2,
                    verbatimTextOutput('TestAccuracyRender_neuralnet')
                ),
                box(
                    title='Predict',
                    solidHeader=T,
                    collapsible=T,
                    width=7,
                    verbatimTextOutput('TestTableRender_neuralnet')
                ),
                box(
                    title='Model Plot',
                    solidHeader=T,
                    collapsible=T,
                    width=9,
                    plotOutput('plot_neural', height='auto')
                )
            )
        )
    ),
    navbarMenu(
        'Graph',
        tabPanel(
            'Bar Plot',
            sidebarPanel(
                selectInput('in_sel_bar_yVar',
                            'y Variable:',
                            choices=NULL),
                selectInput('in_sel_bar_xVar',
                            'x Variable:',
                            choices=NULL)
            ),
            mainPanel(
                plotOutput('plot_bar')
            )
        ),
        tabPanel(
            'Pie Chart',
            sidebarPanel(
                selectInput('in_sel_pie_xVar',
                            'x Variable:',
                            choices=NULL),
                selectInput('in_sel_pie_yVar',
                            'y Variable:',
                            choices=NULL)
            ),
            mainPanel(
                plotlyOutput('plot_pie')
            )
        ),
        tabPanel(
            'Line Plot',
            sidebarPanel(
                selectInput('in_sel_line_yVar',
                            'y Variable:',
                            choices=NULL),
                selectInput('in_sel_line_xVar',
                            'x Variable:',
                            choices=NULL)
            ),
            mainPanel(
                plotlyOutput('plot_line')
            )
        ),
        tabPanel(
            'Scatter Plot',
            sidebarPanel(
                selectInput('in_sel_scatter_yVar',
                            'y Variable:',
                            choices=NULL),
                selectInput('in_sel_scatter_xVar',
                            'x Variable:',
                            choices=NULL)
            ),
            mainPanel(
                plotOutput('plot_scatter'),
                textOutput('text_scatter')
            )
        ),
        tabPanel(
            'Box Plot',
            sidebarPanel(
                selectInput('in_sel_box_xVar',
                            'x Variable:',
                            choices=NULL)
            ),
            mainPanel(
                plotOutput('plot_box'),
                verbatimTextOutput('text_box')
            )
        ),
        tabPanel(
            'Pairs',
            sidebarPanel(
                uiOutput('dependents_delcol_pairs', 
                         style='overflow-y:scroll;
                              max-height: 100px;
                              background: ghostwhite;'
                ), 
                uiOutput('dependents_selcol_pairs',
                         title='Select Colour'), 
                numericInput('alpha', 'Alpha', 0.5),
                uiOutput('dependents_button_pairs'),
                width=2
            ),
            mainPanel(plotOutput('plot_pairs'))
        )
    ),
    footer=dashboardFooter(left_text='ⓒ  2020. 지성. all rights reserved.')
)