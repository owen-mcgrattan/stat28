vioplot2<-function (x, fac, range = 1.5, h = NULL, ylim = NULL, names = NULL, 
    horizontal = FALSE, col = "magenta", border = "black", lty = 1, 
    lwd = 1, rectCol = "black", colMed = "white", pchMed = 19, 
    at, add = FALSE, wex = 1, drawRect = TRUE,clip=TRUE,las=par("las"),...) 
{
	if(!require(vioplot)) stop("Must intall the vioplot package")
    paramList<-list(...)
	if(!missing(fac)){
    	if(!is.factor(fac)) fac<-factor(fac)
		datas<-tapply(x,fac,function(x){x},simplify=FALSE)
		if(is.null(names)) names<-levels(fac)
    }
	else datas<-list(x)
	n <- length(datas)
	getVectorValue<-function(value,i){rep(value,length=n)[i]}
    if (missing(at)) 
        at <- 1:n
    upper <- vector(mode = "numeric", length = n)
    lower <- vector(mode = "numeric", length = n)
    q1 <- vector(mode = "numeric", length = n)
    q3 <- vector(mode = "numeric", length = n)
    med <- vector(mode = "numeric", length = n)
    base <- vector(mode = "list", length = n)
    height <- vector(mode = "list", length = n)
    baserange <- c(Inf, -Inf)
    args <- list(display = "none")
    if (!(is.null(h))) 
        args <- c(args, h = h)
    for (i in 1:n) {
        data <- datas[[i]]
        data.min <- min(data)
        data.max <- max(data)
        q1[i] <- quantile(data, 0.25)
        q3[i] <- quantile(data, 0.75)
        med[i] <- median(data)
        iqd <- q3[i] - q1[i]
        upper[i] <- min(q3[i] + range * iqd, data.max)
        lower[i] <- max(q1[i] - range * iqd, data.min)
        est.xlim <- c(min(lower[i], data.min), max(upper[i], 
            data.max))
        smout <- do.call("sm.density", c(list(data, xlim = est.xlim), 
            args))
        hscale <- 0.4/max(smout$estimate) * wex
        base[[i]] <- smout$eval.points
        height[[i]] <- smout$estimate * hscale
        t <- range(base[[i]])
        baserange[1] <- min(baserange[1], t[1])
        baserange[2] <- max(baserange[2], t[2])
    }
    if (!add) {
        xlim <- if (n == 1) 
            at + c(-0.5, 0.5)
        else range(at) + min(diff(at))/2 * c(-1, 1)
        if (is.null(ylim)) {
            ylim <- baserange
        }
    }
    if (is.null(names)) {
        label <- 1:n
    }
    else {
        label <- names
    }
    boxwidth <- 0.05 * wex
	if (!horizontal) {
        if (!add) {
			if(!"xlab" %in% names(paramList)) paramList$xlab<-"" 
    		do.call(plot,c(list(x=0,y=0,xlim = xlim, ylim = ylim,type="n",xaxt="n",las=las),paramList))
            axis(2,las=las)
            axis(1, at = at, label = label,las=las)
        }
        box()
        for (i in 1:n) {
            polygon(c(at[i] - height[[i]], rev(at[i] + height[[i]])), 
                c(base[[i]], rev(base[[i]])), col = getVectorValue(col,i), border = getVectorValue(border,i), 
                lty = getVectorValue(lty,i), lwd = getVectorValue(lwd,i))
            if (drawRect) {
                lines(at[c(i, i)], c(lower[i], upper[i]), lwd = getVectorValue(lwd,i), 
                  lty = getVectorValue(lty,i))
                rect(at[i] - boxwidth/2, q1[i], at[i] + boxwidth/2, 
                  q3[i], col = getVectorValue(rectCol,i))
                points(at[i], med[i], pch = getVectorValue(pchMed,i), col = getVectorValue(colMed,i))
            }
        }
    }
    else {
        if (!add) {
			if(!"ylab" %in% names(paramList)) paramList$ylab<-"" 
    		do.call(plot,c(list(x=0,y=0,xlim = ylim, ylim = xlim,type="n",xaxt="n"),paramList))
            axis(1)
            axis(2, at = at, label = label)
        }
        box()
        for (i in 1:n) {
            polygon(c(base[[i]], rev(base[[i]])), c(at[i] - height[[i]], 
                rev(at[i] + height[[i]])), col = getVectorValue(col,i), border = getVectorValue(border,i), 
                lty = getVectorValue(lty,i), lwd = getVectorValue(lwd,i))
            if (drawRect) {
                lines(c(lower[i], upper[i]), at[c(i, i)], lwd = getVectorValue(lwd,i), 
                  lty = getVectorValue(lty,i))
                rect(q1[i], at[i] - boxwidth/2, q3[i], at[i] + 
                  boxwidth/2, col = getVectorValue(rectCol,i))
                points(med[i], at[i], pch = getVectorValue(pchMed,i), col = getVectorValue(colMed,i))
            }
        }
    }
    invisible(list(upper = upper, lower = lower, median = med, q1 = q1, q3 = q3))
}