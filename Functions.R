# Functions

## Variance Inflation Factor
vif = function(model){
        x = model$model[-1]
        n = length(resid(model))
        X = as.matrix(cbind(rep(1,n), x))
        C = solve(t(X) %*% X)
        
        Cjj = diag(C)
        se_2 = (summary(model)$sigma)^2
        Sxx = apply(x, 2, function(x) sum((x - mean(x))^2))
        
        Cjj[-1] * Sxx
        
}


## Sensitivity and Specificity
make_conf_mat = function(predicted, actual) {
        table(predicted = predicted, actual = actual)
}

get_sens = function(conf_mat) {
        conf_mat[2, 2] / sum(conf_mat[, 2])
}

get_spec = function(conf_mat) {
        conf_mat[1, 1] / sum(conf_mat[, 1])
}





############# LOOCV  - RMSE

calc_loocv_rmse = function(model){
        sqrt(mean((resid(model) / (1 - hatvalues(model)))^2))
}


############## Diagnostics fitted vs. residual values, 

diagnostics = function(model, pcol = "grey", lcol = "dodgerblue", alpha = 0.05, plotit = TRUE, testit = TRUE){
        p_val = "no test executed"
        decision = "no test executed"
        
        if (testit == TRUE) {
                p_val = signif(shapiro.test(resid(model))$p.value, 5)
                if (p_val < alpha) decision_sh  = "Reject" else decision_sh = "Fail to Reject"
                
                p_val_bpt = signif(bptest(model)$p.value, 5)
                if (p_val_bpt < alpha) decision_bp  = "Reject" else decision_bp = "Fail to Reject"
        }
        
        # plot function
        myplot = function(mod=model, pcl = pcol, lcl = lcol, 
                          pval = p_val, dec_sh = decision_sh, dec_bp = decision_bp){
                
                par(mfrow = c(1, 2))
                plot(fitted(mod), resid(mod), col = pcl, pch = 20,
                     xlab = "Fitted", ylab = "Residuals", main = "Fitted versus Residuals")
                abline(h = 0, col = lcl, lwd = 2)
                y = max(resid(mod))
                x = mean(fitted(mod))
                text(x=x, y = y,paste("Assessing Const. Variance"), col = "darkorange")
                text(x=x, y = y-0.1*y,paste("p value:", p_val_bpt), col = "grey")
                text(x=x, y = y-0.2*y,paste("Decision: ", dec_bp),col = "grey")
                
                
                qqnorm(resid(mod), main = "Normal Q-Q Plot", col = pcl,  ylab = "Simulated Quantiles")
                qqline(resid(mod), col = lcl, lwd = 2)
                y = max(qqnorm(resid(mod), plot.it = FALSE)$y)
                text(x=0, y = y,paste("Assessing Normality"), col = "darkorange")
                text(x=0, y = y-0.1*y,paste("p value:", pval), col = "grey")
                text(x=0, y = y-0.2*y,paste("Decision: ", dec_sh),col = "grey")
        }
        
        if (plotit == TRUE) myp = myplot() else myp = NULL
        #if (testit == TRUE) list(p_val = p_val, decision = decision)
        
}     
