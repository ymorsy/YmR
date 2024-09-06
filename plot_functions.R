# add function for the alpha diversity boxplot ----
ym_boxplot_shape <- function(df, x, y, color_fct, color_vec, shape_fct, shape_vec, facet_fct1, facet_fct2, plot_title) {
    plot <- ggplot(df, aes(x = !!sym(x), y = !!sym(y), color = !!sym(color_fct), shape = !!sym(shape_fct))) +
        geom_jitter(width = 0.1, size = 2.5) +
        stat_boxplot(geom = "errorbar", width = 0.12) +
        geom_boxplot(aes(fill = !!sym(color_fct)), width = 0.26, alpha = 0.5, outlier.shape = NA) +
        scale_color_manual(values = color_vec) +
        scale_fill_manual(values = color_vec) +
        scale_shape_manual(values = shape_vec) +
        theme_bw() +
        labs(title = plot_title) +
        theme(
            legend.position = "right",
            legend.key.size = unit(12, "pt"),
            plot.title = element_text(size = 18),
            legend.title = element_blank(),
            legend.text = element_text(size = rel(1.6)),
            axis.text.x = element_text(size = rel(1.6), angle = 45, hjust = 1),
            axis.text.y = element_text(size = rel(1.6)),
            axis.title = element_text(size = rel(1.6))
        )

    if (is.null(facet_fct1)) {
        return(plot)
    } else {
        plot <- plot + facet_wrap(as.formula(paste("~", facet_fct1)), scales = "free_x")
        return(plot)
    }
}
ym_boxplot_color <- function(df, x, y, color_fct, color_vec, shape_fct, facet_fct1, facet_fct2, plot_title) {
    plot <- ggplot(df, aes(x = !!sym(x), y = !!sym(y), color = !!sym(color_fct))) +
        geom_jitter(width = 0.1, size = 2.5) +
        stat_boxplot(geom = "errorbar", width = 0.12) +
        geom_boxplot(aes(fill = !!sym(color_fct)), width = 0.26, alpha = 0.5, outlier.shape = NA) +
        scale_color_manual(values = color_vec) +
        scale_fill_manual(values = color_vec) +
        theme_bw() +
        labs(title = plot_title) +
        theme(
            legend.position = "right",
            legend.key.size = unit(12, "pt"),
            plot.title = element_text(size = 18),
            legend.title = element_blank(),
            legend.text = element_text(size = rel(1.6)),
            axis.text.x = element_text(size = rel(1.6), angle = 45, hjust = 1),
            axis.text.y = element_text(size = rel(1.6)),
            axis.title = element_text(size = rel(1.6))
        )

    if (is.null(facet_fct1)) {
        return(plot)
    } else {
        plot <- plot + facet_wrap(as.formula(paste("~", facet_fct1)), scales = "free_x")
        return(plot)
    }
}

ym_boxplot_without_outlier <- function(df, x, y, color_fct, color_vec, shape_fct, shape_vec, facet_fct1, facet_fct2, plot_title) {
    plot <- ggplot(df, aes(x = !!sym(x), y = !!sym(y), color = !!sym(color_fct), shape = !!sym(shape_fct))) +
        geom_jitter(width = 0.1, size = 2.5) +
        stat_boxplot(geom = "errorbar", width = 0.12) +
        geom_boxplot(aes(fill = !!sym(color_fct)), width = 0.26, alpha = 0.5, outlier.shape = NA) +
        scale_color_manual(values = color_vec) +
        scale_fill_manual(values = color_vec) +
        scale_shape_manual(values = shape_vec) +
        theme_bw() +
        labs(title = plot_title) +
        theme(
            legend.position = "right",
            legend.key.size = unit(12, "pt"),
            plot.title = element_text(size = 18),
            legend.title = element_blank(),
            legend.text = element_text(size = rel(1.6)),
            axis.text.x = element_text(size = rel(1.6), angle = 45, hjust = 1),
            axis.text.y = element_text(size = rel(1.6)),
            axis.title = element_text(size = rel(1.6))
        )

    if (is.null(facet_fct1)) {
        return(plot)
    } else {
        plot <- plot + facet_wrap(as.formula(paste("~", facet_fct1)), scales = "free_x")
        return(plot)
    }
}

#' Add grids to a scatterplot3d ----
#'
#' @description The goal of this function is to add grids on an existing
#'  plot created using the package scatterplot3d
#' @param x,y,z numeric vectors specifying the x, y, z coordinates of points.
#'  x can be a matrix or a data frame containing 3 columns corresponding to
#'  the x, y and z coordinates. In this case the arguments y and z are optional
#' @param grid specifies the facet(s) of the plot on which grids should be drawn.
#'  Possible values are the combination of "xy", "xz" or "yz".
#'  Example: grid = c("xy", "yz"). The default value is TRUE to add grids only on xy facet.
#' @param col.grid,lty.grid color and line type to be used for grids
#' @param lab a numerical vector of the form c(x, y, len).
#'  The values of x and y give the (approximate) number of tickmarks on the x and y axes.
#' @param lab.z the same as lab, but for z axis
#' @param scale.y of y axis related to x- and z axis
#' @param angle angle between x and y axis
#' @param "xlim, ylim, zlim" the x, y and z limits (min, max) of the plot.
#'
#' @note
#' Users who want to extend an existing scatterplot3d graphic with the
#'  function addgrids3d, should consider to set the arguments scale.y, angle, ...,
#'  to the value used in scatterplot3d.
#'
#' @author Alboukadel Kassambara \email{alboukadel.kassambara@@gmail.com}
#' @references http://www.sthda.com
#'
#' @example
#' library(scatterplot3d)
#' data(iris)
#' scatterplot3d(iris[, 1:3], pch = 16, grid=T, box=F)
#' addgrids3d(iris[, 1:3], grid = c("xy", "xz", "yz"))
addgrids3d <- function(x, y = NULL, z = NULL, grid = TRUE,
                       col.grid = "grey", lty.grid = par("lty"),
                       lab = par("lab"), lab.z = mean(lab[1:2]),
                       scale.y = 1, angle = 40,
                       xlim = NULL, ylim = NULL, zlim = NULL) {
    if (inherits(x, c("matrix", "data.frame"))) {
        x <- as.data.frame(x)
        y <- unlist(x[, 2])
        z <- unlist(x[, 3])
        x <- unlist(x[, 1])
    }

    p.lab <- par("lab")

    angle <- (angle %% 360) / 90
    yz.f <- scale.y * abs(if (angle < 1) angle else if (angle > 3) angle - 4 else 2 - angle)
    yx.f <- scale.y * (if (angle < 2) 1 - angle else angle - 3)


    # x axis range
    x.range <- range(x[is.finite(x)], xlim)
    x.prty <- pretty(x.range, n = lab[1], min.n = max(1, min(0.5 * lab[1], p.lab[1])))
    x.scal <- round(diff(x.prty[1:2]), digits = 12)
    x <- x / x.scal
    x.range <- range(x.prty) / x.scal
    x.max <- ceiling(x.range[2])
    x.min <- floor(x.range[1])
    if (!is.null(xlim)) {
        x.max <- max(x.max, ceiling(xlim[2] / x.scal))
        x.min <- min(x.min, floor(xlim[1] / x.scal))
    }
    x.range <- range(x.min, x.max)

    # y axis range
    y.range <- range(y[is.finite(y)], ylim)
    y.prty <- pretty(y.range, n = lab[2], min.n = max(1, min(0.5 * lab[2], p.lab[2])))
    y.scal <- round(diff(y.prty[1:2]), digits = 12)
    y.add <- min(y.prty)
    y <- (y - y.add) / y.scal
    y.max <- (max(y.prty) - y.add) / y.scal
    if (!is.null(ylim)) {
        y.max <- max(y.max, ceiling((ylim[2] - y.add) / y.scal))
    }

    # Z axis range
    z.range <- range(z[is.finite(z)], zlim)
    z.prty <- pretty(z.range, n = lab.z, min.n = max(1, min(0.5 * lab.z, p.lab[2])))
    z.scal <- round(diff(z.prty[1:2]), digits = 12)
    z <- z / z.scal
    z.range <- range(z.prty) / z.scal
    z.max <- ceiling(z.range[2])
    z.min <- floor(z.range[1])
    if (!is.null(zlim)) {
        z.max <- max(z.max, ceiling(zlim[2] / z.scal))
        z.min <- min(z.min, floor(zlim[1] / z.scal))
    }
    z.range <- range(z.min, z.max)

    # Add grid
    if ("xy" %in% grid || grid == TRUE) {
        i <- x.min:x.max
        segments(i, z.min, i + (yx.f * y.max), yz.f * y.max +
            z.min, col = col.grid, lty = lty.grid)
        i <- 0:y.max
        segments(x.min + (i * yx.f), i * yz.f + z.min, x.max +
            (i * yx.f), i * yz.f + z.min, col = col.grid, lty = lty.grid)
    }

    if ("xz" %in% grid) {
        i <- x.min:x.max
        segments(i + (yx.f * y.max), yz.f * y.max + z.min,
            i + (yx.f * y.max), yz.f * y.max + z.max,
            col = col.grid, lty = lty.grid
        )
        temp <- yx.f * y.max
        temp1 <- yz.f * y.max
        i <- z.min:z.max
        segments(x.min + temp, temp1 + i,
            x.max + temp, temp1 + i,
            col = col.grid, lty = lty.grid
        )
    }

    if ("yz" %in% grid) {
        i <- 0:y.max
        segments(x.min + (i * yx.f), i * yz.f + z.min,
            x.min + (i * yx.f), i * yz.f + z.max,
            col = col.grid, lty = lty.grid
        )
        temp <- yx.f * y.max
        temp1 <- yz.f * y.max
        i <- z.min:z.max
        segments(x.min + temp, temp1 + i,
            x.min, i,
            col = col.grid, lty = lty.grid
        )
    }
}
# function to summerize the level of GP for stacked bar plot ----
stacked_bar_prep <- function(phyloseq_obj,
                             tax_level = String,
                             cutoff = numeric,
                             meta_fct = String,
                             meta_fct2 = String) {
    cutoff <- as.numeric(as.character(cutoff))
    if (is.null(meta_fct2) || is.na(meta_fct2)) {
        df <- tax_glom(phyloseq_obj, taxrank = tax_level) %>%
            transform_sample_counts(function(x) x / sum(x)) %>%
            psmelt() %>%
            as_tibble() %>%
            mutate(
                OTU = as.factor(!!as.symbol(tax_level))
            ) %>%
            arrange(OTU) %>%
            group_by(!!as.symbol(meta_fct), OTU) %>%
            summarise(mean_rel_abund = mean(Abundance, na.rm = TRUE) * 100, .groups = "drop") %>%
            group_by(OTU) %>%
            mutate(pool = max(mean_rel_abund) < cutoff) %>%
            mutate(OTU = if_else(pool, "Other", as.character(OTU))) %>%
            ungroup(OTU) %>%
            arrange(desc(mean_rel_abund)) %>%
            mutate(OTU = factor(OTU) %>%
                fct_reorder(mean_rel_abund, max, .desc = TRUE) %>%
                fct_relevel("Other", after = Inf)) %>%
            select(ASV = OTU, !!as.symbol(meta_fct), mean_rel_abund)

        return(df)
    } else {
        df2 <- tax_glom(phyloseq_obj, taxrank = tax_level) %>%
            transform_sample_counts(function(x) x / sum(x)) %>%
            psmelt() %>%
            as_tibble() %>%
            mutate(
                OTU = as.factor(!!as.symbol(tax_level))
            ) %>%
            arrange(OTU) %>%
            group_by(!!as.symbol(meta_fct), !!as.symbol(meta_fct2), , OTU) %>%
            summarise(mean_rel_abund = mean(Abundance, na.rm = TRUE) * 100, .groups = "drop") %>%
            group_by(OTU) %>%
            mutate(pool = max(mean_rel_abund) < cutoff) %>%
            mutate(OTU = if_else(pool, "Other", as.character(OTU))) %>%
            ungroup(OTU) %>%
            arrange(desc(mean_rel_abund)) %>%
            mutate(OTU = factor(OTU) %>%
                fct_reorder(mean_rel_abund, max, .desc = TRUE) %>%
                fct_relevel("Other", after = Inf)) %>%
            select(ASV = OTU, !!as.symbol(meta_fct), !!as.symbol(meta_fct2), , mean_rel_abund)

        return(df2)
    }
}
# function to plot the stacked bar plot ----
plot_stacked_bar <- function(df, x, y, fill, palette, l) {
    ggplot(df, aes_string(x = x, y = y, fill = fill)) +
        geom_col(width = 0.6, color = "white") +
        scale_fill_manual(values = palette) +
        labs(
            title = l,
            y = "Mean Relative Abundance (%)"
        ) +
        theme_classic() +
        geom_text(aes(label = paste(round(mean_rel_abund, 1), "%")),
            position = position_stack(vjust = 0.5), size = 2, color = "white"
        ) +
        theme(
            legend.text = element_text(face = "italic"),
            legend.key.size = unit(10, "pt"),
            axis.text.y = element_text(size = rel(2)),
            axis.title.y = element_text(size = rel(1.5)),
            axis.text.x = element_text(size = rel(1.5), angle = 45, hjust = 1),
        )
}
# with flow
plot_stacked_bar_flow <- function(df, x, y, fill, palette, l) {
    ggplot(df, aes_string(x = x, y = y, fill = fill)) +
        geom_flow(aes(alluvium = !!sym(fill)),
            alpha = 0.5,
            curve_type = "xspline",
            width = 0.49
        ) +
        geom_col(width = 0.5, color = "white") +
        scale_fill_manual(values = palette) +
        labs(
            title = l,
            y = "Mean Relative Abundance (%)"
        ) +
        theme_classic() +
        geom_text(aes(label = paste(round(mean_rel_abund, 1), "%")),
            position = position_stack(vjust = 0.5), size = 2, color = "white"
        ) +
        theme(
            legend.text = element_text(face = "italic"),
            legend.key.size = unit(10, "pt"),
            axis.text.y = element_text(size = rel(2)),
            axis.title.y = element_text(size = rel(1.5)),
            axis.text.x = element_text(size = rel(1.5), angle = 45, hjust = 1),
        )
}
#prepare chrod table input 
prepare_chrod_df <- function(df_taxa_long, meta_fct, tax_lvl, abundance_col) {
    df <- df_taxa_long %>%
        group_by(!!sym(meta_fct), !!sym(tax_lvl)) %>%
        summarise(mean_abundance = mean(!!sym(abundance_col), na.rm = TRUE) * 100, .groups = "drop") %>%
        group_by(!!sym(tax_lvl)) %>%
        mutate(pool = max(mean_abundance) < 1) %>%
        mutate(!!sym(tax_lvl) := if_else(pool, "Other", as.character(!!sym(tax_lvl)))) %>%
        ungroup(!!sym(tax_lvl)) %>%
        arrange(desc(mean_abundance)) %>%
        mutate(!!sym(tax_lvl) := factor(!!sym(tax_lvl)) %>%
            fct_reorder(mean_abundance, max, .desc = TRUE) %>%
            fct_relevel("Other", after = Inf)) %>%
        group_by(!!sym(meta_fct), !!sym(tax_lvl)) %>%
        summarise(mean_abundance = sum(mean_abundance), .groups = "drop") %>%
        pivot_wider(id_cols = !!sym(tax_lvl), names_from = !!sym(meta_fct), values_from = mean_abundance)
    return(df)
}
                                    
