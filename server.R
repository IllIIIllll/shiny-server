# ¨Ï 2020 Áö¼º. all rights reserved.
# <llllllllll@kakao.com>
# GNU General Public License v3.0

server <- function(input, output, session) {
    options(warn=-1)
    options(shiny.maxRequestSize=30 * 1024 ^ 2)
    
    dataload <- reactive({
        req(input$file)
        
        file=input$file
        data=read.csv(file$datapath)
        
        # Bar Plot
        updateSelectInput(session,
                          'in_sel_bar_xVar',
                          choices=colnames(data))
        updateSelectInput(session,
                          'in_sel_bar_yVar',
                          choices=colnames(data))
        # Pie Chart
        updateSelectInput(session,
                          'in_sel_pie_xVar',
                          choices=colnames(data))
        updateSelectInput(session,
                          'in_sel_pie_yVar',
                          choices=colnames(data))
        # Line Plot
        updateSelectInput(session,
                          'in_sel_line_xVar',
                          choices=colnames(data))
        updateSelectInput(session,
                          'in_sel_line_yVar',
                          choices=colnames(data))
        # Scatter Plot
        updateSelectInput(session,
                          'in_sel_scatter_xVar',
                          choices=colnames(data))
        updateSelectInput(session,
                          'in_sel_scatter_yVar',
                          choices=colnames(data))
        # Box Plot
        num <- c()
        for(i in seq(ncol(data))) {
            if(is.numeric(data[1, i]) == T) {
                num <- c(num, colnames(data)[i])
            }
        }
        updateSelectInput(session,
                          'in_sel_box_xVar',
                          choices=num)
        
        return(data)
    })
    
    len <- function() {
        dl <- dataload()
        return(ceiling(ncol(dl) / 3) * 400)
    }
    
    # Table
    output$table <- DT::renderDataTable(
        DT::datatable({
            req(input$file)
            
            file <- input$file
            data <- read.csv(file$datapath)
        }) %>% formatStyle(
            colnames(data),
            color='black'
        )
    )
    
    # Statistics - Summery
    quant <- function(dt){
        q <- c()
        name <- c()
        for(i in seq(ncol(dt))) {
            if(is.numeric(dt[1, i]) == T) {
                name <- c(name, colnames(dt[i]))
                quan <- quantile(unlist(dt[i]),
                                 probs=seq(0.1, 1, 0.1),
                                 na.rm=T
                )
                
                sm <- c()
                for(j in seq(length(quan))) {
                    if(j == 1) {
                        sm <- rbind(sm, sum(unlist(dt[i]) <= quan[j]))
                    } else {
                        sm <- rbind(sm, sum(unlist(dt[i]) > quan[j-1]
                                            & unlist(dt[i]) <= quan[j]))
                    }
                }
                q <- cbind(q, sm)
            }
        }
        
        df <- data.frame(q)
        colnames(df) <- name
        rownames(df) <- c('Q01', 'Q02', 'Q03', 'Q04', 'Q05',
                          'Q06', 'Q07', 'Q08', 'Q09', 'Q10')
        
        return(df)
    }
    
    output$stat_summary <- renderPrint({
        dl <- dataload()
        stat.summary <- function(dt) {
            v <- c()
            for(i in seq(ncol(dt))) {
                if(is.numeric(dt[1, i]) == T) {
                    a <- paste('Min    :', sprintf('%.4f', min(dt[i])))
                    b <- paste('Mean   :', sprintf('%.4f', mean(unlist(dt[i])), 4))
                    c <- paste('Median :', sprintf('%.4f', median(unlist(dt[i]))))
                    d <- paste('Max    :', sprintf('%.4f', max(dt[i])))
                    e <- paste('SD     :', sprintf('%.4f', sd(unlist(dt[i]))))
                    g <- paste('NA     :', sum(is.na(dt[i])))
                    f <- rbind(a, b, c, d, e, g)
                } else {
                    na <- paste('NA     :', round(sum(is.na(dt[i]))))
                    f <- c(summary(dt[i]), na)
                }
                
                w <- max(nchar(f))
                
                if(i == 1) {
                    v <- format(f, width=w)
                } else{
                    if(length(v) < length(f)){
                        v <- rbind(v,
                                   matrix('',
                                          nrow=nrow(f) - nrow(v),
                                          ncol=ncol(v)
                                   )
                        )
                    } else if(nrow(v) > length(f)) {
                        f <- c(f, rep('', nrow(v) - length(f)))
                    }
                    
                    v <- cbind(v, format(f, width=w))
                }
            }
            r <- data.frame(v)
            colnames(r) <- c(colnames(dt))
            rownames(r) <- c()
            
            return(data.frame(r))
        }
        data.table(stat.summary(dl))
    })
    output$quan <- renderPrint({
        dl <- dataload()
        data.table(quant(dl))
    })
    output$plot_quan <- renderPlot({
        dl <- dataload()
        bar <- function(rdt) {
            par(mfrow=c(ceiling(ncol(rdt) / 3), 3),
                mar=c(3, 3, 3, 3),
                pty='s')
            colfunc <- colorRampPalette(c('cadetblue1', 'darkseagreen1'))
            dt <- quant(rdt)
            k <- 1
            for(i in seq(ncol(rdt))){
                dumb <- c()
                if(is.numeric(unlist(rdt[i])) == T){
                    for(j in seq(nrow(dt[k]))){
                        dumb <- c(dumb, 
                                  rep(rownames(dt[k])[j],
                                      unlist(dt[k])[j] + 1
                                  ))
                    }
                    tb <- table(dumb)
                    tt <-paste(colnames(dt)[k], 'Quantile Dist')
                    barplot(tb,
                            ylim=c(0, max(tb) + 1),
                            main=tt,
                            col=colfunc(10),
                            border=F,
                            cex.main=2,
                            cex.names=1.2,
                            axes=F)
                    k <- k + 1
                } else {
                    dumb <- c()
                    unqn <- length(unique(unlist(rdt[i])))
                    chr <- unique(rdt[i])[1]
                    for(j in seq(unqn)) {
                        unq <- unique(rdt[i])[j, 1]
                        dumb <- c(dumb,
                                  rep(as.character(chr[j, 1]),
                                      sum(unlist(rdt[i]) == unq)
                                  ))
                    }
                    tb <- table(dumb)
                    tt <- paste(colnames(rdt)[i], 'Dist')
                    barplot(tb,
                            ylim=c(0, max(tb) + 1),
                            main=tt,
                            col=colfunc(unqn),
                            border=F,
                            cex.main=2,
                            cex.names=2,
                            axes=F)
                }
            }
        }
        bar(dl)
    },
    height=len)
    
    output$plot_dist <- renderPlot({
        dl <- dataload()
        distplot <- function(dt) {
            
            grid.newpage()
            pushViewport(viewport(layout=grid.layout(ceiling(ncol(dt) / 3), 3)))
            
            vplayout <- function(x, y) {
                viewport(layout.pos.row=x,
                         layout.pos.col=y
                )
            }
            k <- 1
            l <- 1
            for(i in seq(ncol(dt))) {
                if(is.numeric(unlist(dt[i])) == T) {
                    plt <- ggplot(dt, aes_string(colnames(dt)[i]))
                    + geom_histogram(aes(y=..density..),
                                     binwidth=.5,
                                     colour='cadetblue4',
                                     fill='white')
                    + geom_density(alpha=.2,
                                   fill='indianred1')
                    + theme_bw()
                } else {
                    plt <- ggplot(dt, aes_string(colnames(dt)[i]))
                    + geom_bar(aes(y=..count..),
                               stat='count',
                               colour='cadetblue4',
                               fill='white')
                    + theme_bw()
                }
                
                print(plt, vp=vplayout(k, l))
                l <- l + 1
                if(i %% 3 == 0) {
                    k <- k + 1
                    l <- 1
                }
            }
        }
        distplot(dl)
        
    },
    height=len)
    
    # Statistics - Regression
    output$dependents_delcol_regression <- renderUI({
        data <- dataload()
        if(is.null(data)) {
            return(NULL)
        }
        checkboxGroupInput(inputId='in_che_delcol_regression',
                           label='Delete colmun',
                           choices=colnames(data),
                           selected='null',
                           inline=F
        )
    })
    output$dependents_selcol_regression <- renderUI({
        data <- dataload()      
        if(is.null(data)) {
            return(NULL)
        }
        
        if(length(input$in_che_delcol_regression) == 0) {
            ch <- colnames(data)
        } else {
            ch <- colnames(data)[-which(colnames(data) == input$in_che_delcol_regression)]
        }
        selectInput('in_sel_label_regression',
                    'Target',
                    choices=ch
        )
    })
    output$dependents_button_regression <- renderUI({
        data <- dataload()       
        if(is.null(data)) {
            return(NULL)
        }
        actionButton('in_btn_submit_regression','Submit')
    })
    
    subinput_table_regression <- eventReactive(input$in_btn_submit_regression, {
        req(input$file)
        file <- input$file
        data <- read.csv(file$datapath, stringsAsFactors = T)
        data <- data[, !(colnames(data) %in% input$in_che_delcol_regression )]
        reg_data <- na.omit(data)
        input_label <-as.formula(paste(colnames(reg_data[input$in_sel_label_regression]), '~.'))  
        
        model <- lm(input_label, data=reg_data)
        return(model)
    })
    output$submit_input_sample_regression <- renderPrint({
        req(input$file)         
        file <- input$file
        data <- read.csv(file$datapath)
        data <- data[, !(colnames(data) %in% input$in_che_delcol_regression)]
        return(head(data, 7))
    })
    output$anova_Render_regression <- renderPrint({
        anova(subinput_table_regression())
    })
    output$coefficient_Render_regression <- renderPrint({
        summary(subinput_table_regression())
    })
    output$interval_Render_regression <- renderPrint({
        confint(subinput_table_regression())
    })
    output$plot_reg <- renderPlot({
        par(mfrow=c(2, 2), pty = 's')
        plot(subinput_table_regression())
    },
    height=800)
    
    # Statistics - Decision Tree
    output$dependents_delcol_decision <- renderUI({
        # ë¶ˆí•„?š”?•œ ì»¬ëŸ¼ ?‚­? œ ?‹œ ?ž‘?™
        data <- dataload()
        if(is.null(data)) {
            return(NULL)
        }
        checkboxGroupInput(inputId='in_che_delcol_decision',
                           label='Delete colmun',
                           choices=colnames(data),
                           selected='null',
                           inline=F
        )
    })
    output$dependents_selcol_decision <- renderUI({
        data <- dataload()
        if (is.null(data)) {
            return(NULL)
        }
        
        if (length(input$in_che_delcol_decision) == 0) {
            ch <- colnames(data)
        } else {
            ch <- colnames(data)[-which(colnames(data) == input$in_che_delcol_decision)]
        }
        selectInput('in_sel_label_decision',
                    'Target',
                    choices=ch)
    })
    output$dependents_selmodel_decision <- renderUI({
        data <- dataload()
        if (is.null(data)) {
            return(NULL)
        }
        selectInput('in_sel_model_decision',
                    'Model',
                    choices=c('Tree', 'Rules'))
    })
    output$dependents_selwinnow_decision <- renderUI({
        data <- dataload()      
        if (is.null(data)) {
            return(NULL)
        }
        selectInput('in_sel_winnow_decision',
                    'Winnow',
                    choices=c(F,T))
    })
    output$dependents_button_decision <- renderUI({
        data <- dataload()
        if (is.null(data)) {
            return(NULL)
        }
        actionButton('in_btn_submit_decision', 'submit')
    })
    
    output$submit_input_sample_decision <- renderPrint({
        req(input$file)         
        file <- input$file
        data <- read.csv(file$datapath)
        data <- data[, !(colnames(data) %in% input$in_che_delcol_decision)]
        return(head(data, 5))
    })
    subinput_table_decision <- eventReactive(input$in_btn_submit_decision, {
        req(input$file)
        file <- input$file
        data <- read.csv(file$datapath, stringsAsFactors=T)
        data <- data[, !(colnames(data) %in% input$in_che_delcol_decision)]
        data <- na.omit(data)
        
        train_cnt <- round(0.9 * nrow(data))
        set.seed(input$seed)
        train_index <- sample(1:nrow(data), train_cnt, replace=F)
        train <- data[train_index, ]
        test <- data[-train_index, ]
        test_label <- factor(data[-train_index, input$in_sel_label_decision])
        
        if(input$in_sel_model_decision == 'Tree') {
            model <- C5.0(train[, -which(colnames(data) == input$in_sel_label_decision)],
                          factor(train[, input$in_sel_label_decision]),
                          trials=input$trials,
                          control=C5.0Control(winnow=input$dependents_selwinnow_decision, seed=input$seed)
            )
        } else {
            model <- C5.0(train[,-which(colnames(data) == input$in_sel_label_decision)],
                          factor(train[, input$in_sel_label_decision]),
                          trials=input$trials,
                          rules=T,
                          control=C5.0Control(winnow=input$dependents_selwinnow_decision, seed=input$seed))
        }
        
        out <- list(model, test, test_label, colnames(data))
        
        return(out)
    })
    
    output$TestTableRender_decision <- renderPrint({
        result <- subinput_table_decision()
        model <- result[[1]]
        test <- result[[2]]
        test_label <- result[[3]]
        coln <- result[[4]]
        predict_result <- predict(model, test[, -which(coln == input$in_sel_label_decision)])
        CrossTable(test_label, predict_result)
    })
    output$TestSummaryRender_decision <- renderPrint({
        model <- subinput_table_decision()
        summary(model[[1]])
    })
    
    # Machine Learning - JRip
    output$dependents_delcol_jrip <- renderUI({
        data <- dataload()
        if(is.null(data)) {
            return(NULL)
        }
        checkboxGroupInput(inputId='in_che_delcol_jrip',
                           label='Delete Colmun',
                           choices=colnames(data),
                           selected='null',
                           inline =F
        )
    })
    output$dependents_selcol_jrip <- renderUI({
        data <- dataload()
        if(is.null(data)) {
            return(NULL)
        }
        
        if(length(input$in_che_delcol_jrip) == 0) {
            ch <- colnames(data)
        } else {
            ch <- colnames(data)[-which(colnames(data) == input$in_che_delcol_jrip)]
        }
        selectInput('in_sel_label_jrip',
                    'Target',
                    choices=ch)
    })
    output$dependents_button_jrip <- renderUI({
        # sumit ?‹œ ?ž‘?™
        data <- dataload()
        if (is.null(data)) {
            return(NULL)
        }
        actionButton('in_btn_submit_jrip', 'Submit')
    })
    
    output$submit_input_sample_jrip <- renderPrint({
        req(input$file)        
        file <- input$file
        data <- read.csv(file$datapath)
        data <- data[,!(colnames(data) %in% input$in_che_delcol_jrip)]
        
        return(head(data, 5))
    })
    subinput_table_jrip <- eventReactive(input$in_btn_submit_jrip, {
        req(input$file)
        file <- input$file
        data <- read.csv(file$datapath, stringsAsFactors=T)
        data <- data[, !(colnames(data) %in% input$in_che_delcol_jrip)]
        data <- na.omit(data)
        
        input_label <- as.formula(paste(colnames(data[input$in_sel_label_jrip]), '~.'))  
        
        train_cnt <- round(0.75*dim(data)[1])
        set.seed(input$seed)
        train_index <- sample(1:dim(data)[1], train_cnt, replace = F)
        train <- data[train_index, ]
        test <- data[-train_index, ]
        test_label <- factor(data[-train_index, input$in_sel_label_jrip])
        
        model <- JRip(input_label,
                      data=train,
                      control=Weka_control(O=input$numopt))
        
        out <- list(model,
                    test,
                    test_label,
                    colnames(data))
        
        return(out)
    })
    output$TestTableRender_jrip <- renderPrint({
        result <- subinput_table_jrip()  
        model <- result[[1]]
        test <- result[[2]]
        test_label <- result[[3]]
        coln <- result[[4]]
        predict_result <- predict(model, test[,-which(coln == input$in_sel_label_jrip)])
        CrossTable(test_label , predict_result)
    })
    output$TestSummaryRender_jrip <- renderPrint({
        model <- subinput_table_jrip()
        summary(model[[1]])
    })
    
    # Machine Learning - kmeans
    output$dependents_delcol_kmeans <- renderUI({
        data <- dataload()
        if(is.null(data)) {
            return(NULL)
        }
        checkboxGroupInput(inputId='in_che_delcol_kmeans',
                           label='Delete Colmun',
                           choices=colnames(data),
                           selected='null',
                           inline =F
        )
    })
    output$dependents_selcol_kmeans <- renderUI({
        data <- dataload()      
        if(is.null(data)) {
            return(NULL)
        }
        
        if(length(input$in_che_delcol_kmeans) == 0) {
            ch <- colnames(data)
        } else {
            ch <- colnames(data)[-which(colnames(data) == input$in_che_delcol_kmeans)]
        }
        selectInput('in_sel_label_kmeans',
                    'Target',
                    choices=ch)
    })
    output$dependents_button_kmeans <- renderUI({
        data <- dataload()     
        if(is.null(data)) {
            return(NULL)
        }
        actionButton('in_btn_submit_kmeans', 'Submit')
    })
    
    output$submit_input_sample_kmeans <- renderPrint({
        req(input$file)         
        file=input$file
        data <- read.csv(file$datapath)
        data <- data[, !(colnames(data) %in% input$in_che_delcol_kmeans)]
        
        return(head(data,5))
    })
    output$TestSummaryRender_kmeans <- renderPrint({
        req(input$file)
        file <- input$file
        data <- read.csv(file$datapath, stringsAsFactors=T)
        data <- data[, !(colnames(data) %in% input$in_che_delcol_kmeans )]
        data <- na.omit(data)
        summary(data)
    })
    subinput_table_kmeans <- eventReactive(input$in_btn_submit_kmeans, {
        
        set.seed(input$seed)
        
        req(input$file)
        file <- input$file
        data <- read.csv(file$datapath,stringsAsFactors=T)
        data <- data[, !(colnames(data) %in% input$in_che_delcol_kmeans)]
        data <- na.omit(data)
        
        input_label <-colnames(data[input$in_sel_label_kmeans])
        
        data_n <- data[, -which(colnames(data) == input$in_sel_label_kmeans)]
        
        like_model <- kmeans(data_n, input$k_mv, nstart=input$k_nstart)
        
        out <- list(data, like_model)
        
        return(out)
    })
    output$TestTableRender_kmeans <- renderPrint({
        result <- subinput_table_kmeans()
        data <- result[[1]]
        model <- result[[2]]
        x <- cbind(data[ , which(colnames(data) == input$in_sel_label_kmeans)], model$cluster)
        x2 <- data.frame(x)
        table(x2, dnn=c('Test Data', 'Predict Result'))
    })
    output$TestCenterRender_kmeans <- renderPrint({
        result <- subinput_table_kmeans()
        model <- result[[2]]
        model$centers
    })
    output$plot_kmeans <- renderPlot({
        result <- subinput_table_kmeans() 
        data <- result[[1]]
        model <- result[[2]]
        data_wo_label <- data[, -which(colnames(data) == input$in_sel_label_kmeans)]
        plot(data_wo_label, pch=model$cluster, col=model$cluster)
    },
    height=len)
    
    # Machine Learning - KNN
    output$dependents_delcol_knn <- renderUI({
        data <- dataload()
        if(is.null(data)) {
            return(NULL)
        }
        checkboxGroupInput(inputId='in_che_delcol_knn',
                           label='Delete Colmun',
                           choices=colnames(data),
                           selected='null',
                           inline=F
        )
    })
    output$dependents_selcol_knn <- renderUI({
        data <- dataload()
        if(is.null(data)) {
            return(NULL)
        }
        
        if(length(input$in_che_delcol_knn) == 0) {
            ch <- colnames(data)
        } else {
            ch <- colnames(data)[-which(colnames(data) == input$in_che_delcol_knn)]
        }
        selectInput('in_sel_label_knn',
                    'Target',
                    choices=ch)
    })
    output$dependents_button_knn <- renderUI({
        data <- dataload()
        if(is.null(data)) {
            return(NULL)
        }
        actionButton('in_btn_submit_knn', 'Submit')
    })
    
    output$submit_input_sample_knn <- renderPrint({
        req(input$file)
        file <- input$file
        data <- read.csv(file$datapath)
        data <- data[, !(colnames(data) %in% input$in_che_delcol_knn)]
        return(head(data, 5))
    })
    output$TestSummaryRender_knn <- renderPrint({
        req(input$file)
        file <- input$file
        data <- read.csv(file$datapath,stringsAsFactors=T)
        data <- data[, !(colnames(data) %in% input$in_che_delcol_knn)]
        data <- na.omit(data)
        summary(data)
    })
    normalize <- function(x) {
        return((x - min(x)) / (max(x) - min(x))) 
    }
    subinput_table_knn <- eventReactive(input$in_btn_submit_knn, {
        req(input$file)
        file <- input$file
        data <- read.csv(file$datapath, stringsAsFactors=F)
        set.seed(1)
        data <- data[sample(nrow(data)), ]
        data <- data[, !(colnames(data) %in% input$in_che_delcol_kmeans)]
        data <- na.omit(data)
        
        data <- as.data.frame(lapply(data[, -which(colnames(data) == input$in_sel_label_knn)], normalize))
        
        train_index <- as.integer(trunc(nrow(data) * 0.8))
        
        train <- data[1:as.integer(train_index), ]
        
        test <- data[as.integer(train_index+1):as.integer(nrow(data)), ]
        
        train_label <- data[1:as.integer(train_index), which(colnames(data) == input$in_sel_label_knn)] 
        test_label <- data[as.integer(train_index+1):as.integer(nrow(data)), which(colnames(data) == input$in_sel_label_knn)]
        
        train_label <- factor(train_label)
        
        if(is.numeric(input$knn_k) == F) {
            kk <- round(sqrt(length(train)))
        } else {
            kk <- input$knn_k
        }
        
        result <- knn(train=train,
                      test=test,
                      cl=train_label,
                      k=kk
        )
        
        cross_table <- CrossTable(test_label,
                                  result,
                                  prop.chisq=F
        )
        
        return(cross_table)
    })
    output$TestTableRender_knn <- renderPrint({
        subinput_table_knn()
    })
    
    # Machine Learning - Naive Bayes
    output$dependents_delcol_naive <- renderUI({
        data <- dataload()
        if(is.null(data)) {
            return(NULL)
        }
        checkboxGroupInput(inputId='in_che_delcol_naive',
                           label='Delete Colmun',
                           choices=colnames(data),
                           selected='null',
                           inline=F
        )
    })
    output$dependents_selcol_naive <- renderUI({
        data <- dataload()
        if(is.null(data)) {
            return(NULL)
        }
        if(length(input$in_che_delcol_naive) == 0) {
            ch <- colnames(data)
        } else {
            ch <- colnames(data)[-which(colnames(data) == input$in_che_delcol_naive)]
        }
        selectInput('in_sel_label_naive',
                    'Target',
                    choices=ch)
    })
    output$dependents_button_naive <- renderUI({
        data <- dataload()
        if(is.null(data)) {
            return(NULL)
        }
        actionButton('in_btn_submit_naive', 'Submit')
    })
    
    output$submit_input_sample_naive <- renderPrint({
        req(input$file)
        file <- input$file
        data <- read.csv(file$datapath)
        data <- data[, !(colnames(data) %in% input$in_che_delcol_naive)]
        
        return(head(data, 5))
    })
    subinput_table_naive <- eventReactive(input$in_btn_submit_naive, {
        req(input$file)
        file <- input$file
        data <- read.csv(file$datapath, stringsAsFactors=T)
        data <- data[, !(colnames(data) %in% input$in_che_delcol_naive)]
        data <- na.omit(data)
        
        input_label <- as.formula(paste(colnames(data[input$in_sel_label_naive]), '~.'))  
        
        train_cnt <- round(0.7 * dim(data)[(colnames(data) %in% input$in_sel_label_naive)])
        set.seed(input$seed)
        
        train_index <- sample(1:dim(data)[(colnames(data) %in% input$in_sel_label_naive)],
                              train_cnt,
                              replace=F
        )
        train <- data[train_index, ]
        test <- data[-train_index, ]
        
        model <- naiveBayes(input_label,
                            data=train,
                            laplace=input$laplace
        )
        out <- list(model,
                    colnames(data),
                    test
        )
        
        return(out)
    })
    output$TestTableRender_naive <- renderPrint({
        result <- subinput_table_naive()
        model <- result[[1]]
        coln <- result[[2]]
        test <- result[[3]]
        test_label <- test[, (coln %in% input$in_sel_label_naive)]
        predict_result <- predict(model,
                                  test[, !(coln %in% input$in_sel_label_naive)]
        )
        CrossTable(test_label, predict_result)
    })
    output$TestModelRender_naive <- renderPrint({
        result <- subinput_table_naive()
        result[[1]]
    })
    
    # Machine Learning - Neural Net
    output$dependents_delcol_neuralnet <- renderUI({
        data <- dataload()
        if(is.null(data)) {
            return(NULL)
        }
        checkboxGroupInput(inputId='in_che_delcol_neuralnet',
                           label='Delete Colmun',
                           choices=colnames(data),
                           selected='null',
                           inline=F
        )
    })
    output$dependents_selcol_neuralnet <- renderUI({
        data <- dataload()       
        if(is.null(data)) {
            return(NULL)
        }
        if(length(input$in_che_delcol_naive) == 0) {
            ch <- colnames(data)
        } else {
            ch <- colnames(data)[-which(colnames(data) == input$in_che_delcol_naive)]
        }
        selectInput('in_sel_label_neuralnet',
                    'Target',
                    choices=ch)
    })
    output$dependents_type_neuralnet <- renderUI({
        selectInput('in_sel_type_neuralnet',
                    'Target Type',
                    choices=c('Numeric', 'Character')
        )
    })
    output$dependents_button_neuralnet <- renderUI({
        data <- dataload()       
        if(is.null(data)) {
            return(NULL)
        }
        
        actionButton('in_btn_submit_neuralnet', 'Submit')
    })
    
    output$submit_input_sample_neuralnet <- renderPrint({
        req(input$file)        
        file <- input$file
        data <- read.csv(file$datapath)
        data <- data[, !(colnames(data) %in% input$in_che_delcol_neuralnet)]
        return(head(data, 5))
    })
    subinput_table_neuralnet <- eventReactive(input$in_btn_submit_neuralnet, {
        req(input$file)
        file <- input$file
        data <- read.csv(file$datapath, stringsAsFactors=T)
        data <- data[, !(colnames(data) %in% input$in_che_delcol_neuralnet)]
        data <- na.omit(data)
        
        input_label <- as.formula(paste(colnames(data[input$in_sel_label_neuralnet]), '~.'))  
        
        train_cnt <- round(0.75 * dim(data)[1])
        set.seed(input$seed)
        train_index <- sample(1:dim(data)[1],
                              train_cnt,
                              replace=F
        )
        
        normalize <- function(x) {
            return ((x - min(x)) / (max(x) - min(x)))
        }
        
        if(input$in_sel_type_neuralnet == 'Numeric') {
            data_norm <- as.data.frame(lapply(data, normalize))
        } else {
            data_norm <- data.frame(data[, input$in_sel_label_neuralnet],
                                     scale(data[, !(colnames(data) %in% input$in_sel_label_neuralnet)])
            )
            colnames(data_norm)[1] <- c(input$in_sel_label_neuralnet)
        }
        
        train <- data_norm[train_index, ]
        test <- data_norm[-train_index, ]
        
        test_label <- factor(data_norm[-train_index, input$in_sel_label_neuralnet])
        
        model <- neuralnet(formula=input_label,
                           data=train,
                           hidden=c(input$hidden_1,input$hidden_2)
        )
        out <- list(model, test, data)
        
        return(out)
    })
    output$TestAccuracyRender_neuralnet <- renderPrint({
        result <- subinput_table_neuralnet()
        model <- result[[1]]
        test <- result[[2]]
        data <- result[[3]]
        if(input$in_sel_type_neuralnet == 'Numeric') {
            model_results <- neuralnet::compute(model, test[, !(colnames(test) %in% input$in_sel_label_neuralnet)])
            results <- data.frame(actual=test[, input$in_sel_label_neuralnet], prediction=model_results$net.result)
            predicted <- results$prediction * abs(diff(range(data[, input$in_sel_label_neuralnet]))) + min(data[, input$in_sel_label_neuralnet])
            actual <- results$actual * abs(diff(range(data[, input$in_sel_label_neuralnet]))) + min(data[, input$in_sel_label_neuralnet])
            comparison <- data.frame(predicted, actual)
            deviation <- ((actual - predicted) / actual)
            comparison <- data.frame(predicted, actual, deviation)
            accuracy <- 1 - abs(mean(deviation))
            data.frame(Accuracy=accuracy, row.names='')
        } else {
            model_results <- neuralnet::compute(model, test[, !(colnames(test) %in% input$in_sel_label_neuralnet)])
            predicted <- model_results$net.result
            predicted_label <- 0
            correct <- 0
            for(i in seq(nrow(predicted))) {
                predicted_label <- which(predicted[i, ] == max(predicted[i, ]))
                if (as.numeric(as.factor(test[,input$in_sel_label_neuralnet]))[i]==predicted_label) correct <- correct + 1
            }
            accuracy <- correct/nrow(test)
            data.frame(Accuracy=accuracy, row.names='')
        }
    })
    output$TestTableRender_neuralnet <- renderPrint({
        result <- subinput_table_neuralnet()
        model <- result[[1]]
        test <- result[[2]]
        data <- result[[3]]
        if(input$in_sel_type_neuralnet == 'Numeric'){
            model_results <- neuralnet::compute(model, test[,!(colnames(test) %in% input$in_sel_label_neuralnet)])
            results <- data.frame(actual = test[, input$in_sel_label_neuralnet], prediction=model_results$net.result)
            results
        } else {
            model_results <- neuralnet::compute(model, test[, !(colnames(test) %in% input$in_sel_label_neuralnet)])
            predicted <- model_results$net.result
            
            predicted_label <- c()
            for(i in seq(nrow(predicted))){
                predicted_label <- c(predicted_label, which(predicted[i, ] == max(predicted[i, ])))
            }
            
            CrossTable(unlist(test[, input$in_sel_label_neuralnet]),
                       predicted_label,
                       dnn=c('test_label', 'predicted_label')
            )
        }
        
    })
    output$plot_neural <- renderPlot({
        result <- subinput_table_neuralnet()
        model <- result[[1]]
        dev.off()
        plot(model)
    },
    height=600)
    
    # Graph - Bar Plot
    output$plot_bar <- renderPlot({
        table_in <- dataload()
        
        xdata <- as.factor(table_in[, input$in_sel_bar_xVar])
        ydata <- as.factor(table_in[, input$in_sel_bar_yVar])
        fdata <- data.frame(x=xdata, y=ydata)
        
        ggplot(fdata) + geom_bar(aes_string(x='x', y='y', fill='x'),
                                 stat='identity', show.legend=F
        )
    })
    
    # Graph - Pie Chart
    output$plot_pie <- renderPlotly({
        table_in <- dataload()
        
        plot_ly(table_in, labels=~table_in[, input$in_sel_pie_xVar],
                values=~table_in[, input$in_sel_pie_yVar], type='pie')
    })
    
    # Graph - Line Plot
    output$plot_line <- renderPlotly({
        table_in <- dataload()
        
        x <- list(title=input$in_sel_line_xVar)
        y <- list(title=input$in_sel_line_yVar)
        
        plot_ly(data=table_in,
                x=~table_in[, input$in_sel_line_xVar], 
                y=~table_in[, input$in_sel_line_yVar],
                type='scatter',
                mode='dot'
        ) %>% layout(xaxis=x, yaxis=y)
    })
    
    # Graph - Scatter Plot
    output$plot_scatter <- renderPlot({
        table_in <- dataload()
        
        xyplot(table_in[, input$in_sel_scatter_yVar] ~ table_in[, input$in_sel_scatter_xVar],
               grid=T,
               type=c('p'),
               col.line='darkorange',
               lwd=2,
               xlab=input$in_sel_scatter_xVar,
               ylab=input$in_sel_scatter_yVar
        )
        
    })
    
    # Graph - Scatter Cov
    output$text_scatter <- renderText({
        table_in <- dataload()
        paste('The correlation between the two is: ',
              cor(table_in[, input$in_sel_scatter_yVar],
                  table_in[, input$in_sel_scatter_xVar]
              )
        )
    })
    
    # Graph - Box Plot
    output$plot_box <- renderPlot({
        table_in <- dataload()
        bwplot(table_in[, input$in_sel_box_xVar],
               xlab=input$in_sel_box_xVar
        )
    })
    
    # Graph - Box Summary
    output$text_box <- renderPrint({
        table_in <- dataload()
        dataset <- table_in[, input$in_sel_box_xVar]
        summary(dataset)
    })
    
    # Graph - Pairs
    output$dependents_delcol_pairs <- renderUI({
        data <- dataload()
        if(is.null(data)) {
            return(NULL)
        }
        checkboxGroupInput(inputId='in_che_delcol_pairs',
                           label='Delete colmun',
                           choices=colnames(data),
                           selected='null',
                           inline=F
        )
    })
    output$dependents_selcol_pairs <- renderUI({
        data <- dataload()
        if(is.null(data)) {
            return(NULL)
        }
        ch <- c('None')
        for(i in seq(colnames(data))) {
            if(is.numeric(data[1, i]) == F) {
                ch <- c(ch,colnames(data)[i])
            }
        }
        selectInput('in_sel_label_pairs',
                    'Submit',
                    label='Select Colour',
                    choices=ch
        )
    })
    output$dependents_button_pairs <- renderUI({
        data <- dataload()
        if(is.null(data)) {
            return(NULL)
        }
        actionButton('in_btn_submit_pairs', 'Submit')
    })
    subinput_pairs <- eventReactive(input$in_btn_submit_pairs, {
        req(input$file)
        file <- input$file
        data <- read.csv(file$datapath)
        data <- data[, !(colnames(data) %in% input$in_che_delcol_pairs)]
        
        if(input$in_sel_label_pairs == 'None') {
            pa <- ggpairs(data, aes_string(alpha=input$alpha))
        } else {
            pa <- ggpairs(data, aes_string(colour=input$in_sel_label_pairs, alpha=input$alpha))
        }
        
        return(pa)
        
    })
    output$plot_pairs <- renderPlot({
        subinput_pairs()
    },
    height=len)
}