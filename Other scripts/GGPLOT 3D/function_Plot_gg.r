plot_gg = function(ggobj, width = 3, height = 3, height_aes = NULL,invert = FALSE, shadow_intensity = 0.5,
                   units = c("in", "cm", "mm"), scale=150, pointcontract = 0.7, preview = FALSE,
                   raytrace = TRUE, sunangle = 315, anglebreaks = seq(30,40,0.1), multicore=FALSE, lambert=TRUE,
                   save_shadow_matrix = FALSE, saved_shadow_matrix=NULL, ...) {
  flipud = function(x) {
    x[nrow(x):1,]
  }
  fliplr = function(x) {
    x[,ncol(x):1]
  }
  heightmaptemp = tempfile()
  colormaptemp = tempfile()
  if(class(ggobj) == "list" && length(ggobj) == 2) {
    ggplotobj2 = unserialize(serialize(ggobj[[2]], NULL))
    ggsave(paste0(colormaptemp,".png"),ggobj[[1]],width = width,height = height)
  } else {
    ggplotobj2 = unserialize(serialize(ggobj, NULL))
    ggsave(paste0(colormaptemp,".png"),ggplotobj2,width = width,height = height)
  }
  isfill = FALSE
  iscolor = FALSE
  if(is.null(height_aes)) {
    for(i in seq_len(length(ggplotobj2$layers))) {
      if("fill" %in% names(ggplotobj2$layers[[i]]$mapping)) {
        isfill = TRUE
      }
      if(any(c("color","colour") %in% names(ggplotobj2$layers[[i]]$mapping))) {
        iscolor = TRUE
      }
    }
    if(isfill && !iscolor) {
      height_aes = "fill"
    } else if (!isfill && iscolor) {
      height_aes = "colour"
    } else if (isfill && iscolor) {
      height_aes = "fill"
    } else {
      height_aes = "fill"
    }
  }
  if(height_aes == "color") {
    height_aes = "colour"
  }
  other_height_type = ifelse(height_aes == "colour", "fill", "colour")
  mapcolor = png::readPNG(paste0(colormaptemp,".png"))
  colortheme = c("line","rect","text","axis.title", "axis.title.x",
                 "axis.title.x.top","axis.title.y","axis.title.y.right","axis.text",
                 "axis.text.x" ,"axis.text.x.top","axis.text.y","axis.text.y.right",
                 "axis.ticks" ,"axis.ticks.length","axis.line"  ,"axis.line.x",
                 "axis.line.y","legend.background","legend.margin","legend.spacing",
                 "legend.spacing.x","legend.spacing.y","legend.key" ,"legend.key.size",
                 "legend.key.height","legend.key.width","legend.text","legend.text.align",
                 "legend.title","legend.title.align","legend.position","legend.direction",
                 "legend.justification" ,"legend.box","legend.box.margin","legend.box.background",
                 "legend.box.spacing","panel.background","panel.border","panel.spacing",
                 "panel.spacing.x","panel.spacing.y","panel.grid" ,"panel.grid.minor",
                 "panel.ontop","plot.background","plot.title" ,"plot.subtitle",
                 "plot.caption","plot.tag","plot.tag.position","plot.margin",
                 "strip.background","strip.placement","strip.text" ,"strip.text.x",
                 "strip.text.y","strip.switch.pad.grid","strip.switch.pad.wrap","panel.grid.major",
                 "title","axis.ticks.length.x","axis.ticks.length.y","axis.ticks.length.x.top",
                 "axis.ticks.length.x.bottom","axis.ticks.length.y.left","axis.ticks.length.y.right")
  
  key_theme_elements = c("text", "line", "axis.line", "axis.title", "axis.text", 
                         "axis.ticks", "strip.background", "strip.text", "legend.text", 
                         "legend.background", "legend.title", "panel.background")
  theme_bool = rep(TRUE,length(key_theme_elements))
  names(theme_bool) = key_theme_elements
  
  typetheme = c("line","rect","text","text","text", 
                "text","text","text","text",
                "text","text","text","text",
                "line","unit","line","line",
                "line","rect","margin","unit",
                "unit","unit","rect","unit", 
                "unit","unit","text","none",
                "text","none","none","none", 
                "none","rect","margin","rect",
                "unit","rect","rect","unit",
                "unit","unit","line","line", 
                "none","rect","text","text",
                "text","text","none","margin",
                "rect","none","text","text",
                "text","unit","unit","line",
                "text","line","line","line",
                "line","line","line")
  black_white_pal = function(x) {
    grDevices::colorRampPalette(c("white", "black"))(255)[x * 254 + 1]
  }
  white_white_pal = function(x) {
    grDevices::colorRampPalette(c("white", "white"))(255)[x * 254 + 1]
  }
  ifelsefxn = function(entry) {
    if(!is.null(entry)) {
      return(entry)
    }
  }
  
  #aes_with_guides = c("size","shape","colour","fill","alpha","linetype")
  #Shift all continuous palettes of height_aes to black/white, and set all discrete key colors to white.
  if(ggplotobj2$scales$n() != 0) {
    anyfound = FALSE
    #Check to see if same guide being used for both color and fill aesthetics
    if(ggplotobj2$scales$has_scale("colour") && ggplotobj2$scales$has_scale("fill")) {
      fillscale = ggplotobj2$scales$get_scales("fill")
      colorscale = ggplotobj2$scales$get_scales("colour")
      same_limits = FALSE
      same_breaks = FALSE
      same_labels = FALSE
      same_calls = FALSE
      if((!is.null(fillscale$limits) && !is.null(colorscale$limits))) {
        if(fillscale$limits == colorscale$limits) {
          same_limits = TRUE
        }
      } else if (is.null(fillscale$limits) && is.null(colorscale$limits)) {
        same_limits = TRUE
      }
      if((!is.null(fillscale$breaks) && !is.null(colorscale$breaks))) {
        if(all(fillscale$breaks == colorscale$breaks)) {
          same_breaks = TRUE
        }
      } else if (is.null(fillscale$breaks) && is.null(colorscale$breaks)) {
        same_breaks = TRUE
      }
      if((class(fillscale$labels) != "waiver" && class(colorscale$labels) != "waiver")) {
        if(all(fillscale$labels == colorscale$labels)) {
          same_labels = TRUE
        }
      } else if ((class(fillscale$labels) == "waiver" && class(colorscale$labels) == "waiver")) {
        same_labels = TRUE
      }
      if(fillscale$call == colorscale$call) {
        same_calls = TRUE
      }
      if(same_limits && same_breaks && same_labels && same_calls) {
        if(height_aes == "fill") {
          ggplotobj2 = ggplotobj2 + guides(color = "none")
        } else {
          ggplotobj2 = ggplotobj2 + guides(fill = "none")
        }
      }
    }
    #Now check for scales and change to the b/w palette, but preserve guide traits.
    for(i in seq_len(ggplotobj2$scales$n())) {
      if(height_aes %in% ggplotobj2$scales$scales[[i]]$aesthetics) {
        ggplotobj2$scales$scales[[i]]$palette = black_white_pal
        ggplotobj2$scales$scales[[i]]$na.value = "white"
        if(!any("guide" %in% class(ggplotobj2$scales$scales[[i]]$guide))) {
          if(height_aes == "fill") {
            if(is.null(ggplotobj2$guides$fill)) {
              ggplotobj2 = ggplotobj2 + guides(fill = guide_colourbar(ticks = FALSE,nbin = 1000,order=i))
            } else {
              if(any(ggplotobj2$guides$fill != "none")) {
                copyguide = ggplotobj2$guides$fill
                copyguide$frame.linewidth = 0
                copyguide$ticks = FALSE
                copyguide$nbin = 1000
                ggplotobj2 = ggplotobj2 + 
                  guides(fill = guide_colourbar(ticks = FALSE,nbin = 1000))
                ggplotobj2$guides$fill = copyguide
              }
            }
            for(j in seq_len(length(ggplotobj2$layers))) {
              if("colour" %in% names(ggplotobj2$layers[[j]]$mapping)) {
                ggplotobj2$layers[[j]]$geom$draw_key = drawkeyfunction_points
              }
            }
          } else {
            if(is.null(ggplotobj2$guides$colour)) {
              ggplotobj2 = ggplotobj2 + guides(colour = guide_colourbar(ticks = FALSE,nbin = 1000,order=i))
            } else {
              if(any(ggplotobj2$guides$colour != "none")) {
                copyguide = ggplotobj2$guides$colour
                copyguide$frame.linewidth = 0
                copyguide$ticks = FALSE
                copyguide$nbin = 1000
                ggplotobj2 = ggplotobj2 + 
                  guides(colour = guide_colourbar(ticks = FALSE,nbin = 1000))
                ggplotobj2$guides$colour = copyguide
              }
            }
          }
        }
        anyfound = TRUE
      } else if(other_height_type %in% ggplotobj2$scales$scales[[i]]$aesthetics) {
        #change guides for other height_aes to be the all white palette
        ggplotobj2$scales$scales[[i]]$palette = white_white_pal
        ggplotobj2$scales$scales[[i]]$na.value = "white"
      } 
    }
    #If no scales found, just add one to the ggplot object.
    if(!anyfound) {
      if(height_aes == "colour") {
        ggplotobj2 = ggplotobj2 + 
          scale_color_gradientn(colours = grDevices::colorRampPalette(c("white","black"))(256), na.value = "white") +
          guides(colour = guide_colourbar(ticks = FALSE,nbin = 1000))
      }
      if(height_aes == "fill") {
        ggplotobj2 = ggplotobj2 + 
          scale_fill_gradientn(colours = grDevices::colorRampPalette(c("white","black"))(256), na.value = "white") +
          guides(fill = guide_colourbar(ticks = FALSE,nbin = 1000))
      }
    }
  } else {
    #If no scales found, just add one to the ggplot object.
    if(ggplotobj2$scales$n() == 0) {
      if(height_aes == "fill") {
        ggplotobj2 = ggplotobj2 + scale_fill_gradientn(colours = grDevices::colorRampPalette(c("white","black"))(256), na.value = "white") +
          guides(fill = guide_colourbar(ticks = FALSE,nbin = 1000))
      } else {
        ggplotobj2 = ggplotobj2 + scale_color_gradientn(colours = grDevices::colorRampPalette(c("white","black"))(256), na.value = "white") +
          guides(colour = guide_colourbar(ticks = FALSE,nbin = 1000))
      }
    } else {
      if(height_aes == "fill") {
        ggplotobj2 = ggplotobj2 + scale_fill_gradientn(colours = grDevices::colorRampPalette(c("white","black"))(256), na.value = "white") +
          guides(fill = guide_colourbar(ticks = FALSE,nbin = 1000))
      } else {
        ggplotobj2 = ggplotobj2 + scale_color_gradientn(colours = grDevices::colorRampPalette(c("white","black"))(256), na.value = "white") +
          guides(colour = guide_colourbar(ticks = FALSE,nbin = 1000))
      }
    }
  }
  #Set all elements to white if custom theme passed, and color aesthetic geoms to size = 0 if height_aes == "fill"
  if(length(ggplotobj2$theme) > 0) {
    if(height_aes == "fill") {
      for(layer in seq_along(1:length(ggplotobj2$layers))) {
        if("colour" %in% names(ggplotobj2$layers[[layer]]$mapping) || 
           0 == length(names(ggplotobj2$layers[[layer]]$mapping))) {
          ggplotobj2$layers[[layer]]$aes_params$colour = "white"
        }
        if("fill" %in% names(ggplotobj2$layers[[layer]]$mapping)) {
          ggplotobj2$layers[[layer]]$aes_params$size = 0
        }
        if("shape" %in% names(ggplotobj2$layers[[layer]]$mapping)) {
          shapedata = layer_data(ggplotobj2)
          numbershapes = length(unique(shapedata$shape))
          if(numbershapes > 3) {
            warning("Non-solid shapes will not be projected to 3D.")
          }
          ggplotobj2$layers[[layer]]$geom$draw_key = drawkeyfunction_points
        }
        if("size" %in% names(ggplotobj2$layers[[layer]]$mapping)) {
          ggplotobj2$layers[[layer]]$geom$draw_key = drawkeyfunction_points
        }
        if("alpha" %in% names(ggplotobj2$layers[[layer]]$mapping)) {
          ggplotobj2$layers[[layer]]$geom$draw_key = drawkeyfunction_points
          for(j in seq_len(length(ggplotobj2$layers))) {
            if("stroke" %in% names(ggplotobj2$layers[[j]]$geom$default_aes)) {
              ggplotobj2$layers[[j]]$geom$default_aes$stroke = 0
            }
          }
          ggplotobj2 = suppressMessages({ggplotobj2 + scale_alpha_continuous(range=c(1,1))})
        }
        if("linetype" %in% names(ggplotobj2$layers[[layer]]$mapping)) {
          ggplotobj2$layers[[layer]]$geom$draw_key = drawkeyfunction_lines
        }
      }
    } else {
      for(layer in seq_along(1:length(ggplotobj2$layers))) {
        if("fill" %in% names(ggplotobj2$layers[[layer]]$mapping) || 
           0 == length(names(ggplotobj2$layers[[layer]]$mapping))) {
          ggplotobj2$layers[[layer]]$aes_params$fill = "white"
        }
        if("shape" %in% names(ggplotobj2$layers[[layer]]$mapping)) {
          shapedata = layer_data(ggplotobj2)
          numbershapes = length(unique(shapedata$shape))
          if(numbershapes > 3) {
            warning("Non-solid shapes will not be projected to 3D.")
          }
          ggplotobj2$layers[[layer]]$geom$draw_key = drawkeyfunction_points
        }
        if("size" %in% names(ggplotobj2$layers[[layer]]$mapping)) {
          ggplotobj2$layers[[layer]]$geom$draw_key = drawkeyfunction_points
        }
        if("alpha" %in% names(ggplotobj2$layers[[layer]]$mapping)) {
          ggplotobj2$layers[[layer]]$geom$draw_key = drawkeyfunction_points
          for(j in seq_len(length(ggplotobj2$layers))) {
            if("stroke" %in% names(ggplotobj2$layers[[j]]$geom$default_aes)) {
              ggplotobj2$layers[[j]]$geom$default_aes$stroke = 0
            }
          }
          ggplotobj2 = suppressMessages({ggplotobj2 + scale_alpha_continuous(range=c(1,1))})
        }
        if("linetype" %in% names(ggplotobj2$layers[[layer]]$mapping)) {
          ggplotobj2$layers[[layer]]$geom$draw_key = drawkeyfunction_lines
        }
      }
    }
    #switch all elements to white
    for(i in 1:length(ggplotobj2$theme)) {
      tempname = names(ggplotobj2$theme[i])
      if(tempname %in% key_theme_elements) {
        theme_bool[tempname] = FALSE
      } else if (substr(tempname,1,nchar(tempname)-2) %in% key_theme_elements) {
        theme_bool[substr(tempname,1,nchar(tempname)-2)] = FALSE
      }
      whichtype = typetheme[which(tempname == colortheme)]
      if(whichtype %in% c("text","line")) {
        if(!is.null(ggplotobj2$theme[[i]])) {
          ggplotobj2$theme[[i]]$colour = "white"
        }
      } else if(whichtype == "rect") {
        if(!(tempname %in% c("panel.border","rect"))) {
          ggplotobj2$theme[[i]]$colour = "white"
          ggplotobj2$theme[[i]]$fill = "white"
        } else {
          ggplotobj2$theme[[i]]$colour = "white"
          ggplotobj2$theme[[i]]$fill = NA
        }
      } 
    }
    if(ggplotobj2$scales$n() > 0) {
      for(i in 1:ggplotobj2$scales$n()) {
        if(length(ggplotobj2$scales$scales[[i]]$guide) > 1) {
          ggplotobj2$scales$scales[[i]]$guide$frame.colour = "white"
          ggplotobj2$scales$scales[[i]]$guide$ticks = FALSE
          ggplotobj2$scales$scales[[i]]$guide$nbin = 256
          ggplotobj2$scales$scales[[i]]$guide$draw.llim = FALSE
          ggplotobj2$scales$scales[[i]]$na.value = "white"
        }
      }
    }
    if(theme_bool["text"]) ggplotobj2 = ggplotobj2 + theme(text = element_text(color="white"))
    if(theme_bool["line"]) ggplotobj2 = ggplotobj2 + theme(line = element_line(color="white"))
    if(theme_bool["axis.line"]) ggplotobj2 = ggplotobj2 + theme(axis.line = element_line(color="white"))
    if(theme_bool["axis.title"]) ggplotobj2 = ggplotobj2 + theme(axis.title = element_text(color="white"))
    if(theme_bool["axis.text"]) ggplotobj2 = ggplotobj2 + theme(axis.text = element_text(color="white"))
    if(theme_bool["axis.ticks"]) ggplotobj2 = ggplotobj2 + theme(axis.ticks = element_line(color="white"))
    if(theme_bool["strip.background"]) ggplotobj2 = ggplotobj2 + theme(strip.background = element_rect(fill = "white", color = "white"))
    if(theme_bool["strip.text"]) ggplotobj2 = ggplotobj2 + theme(strip.text = element_text(color="white"))
    if(theme_bool["legend.text"]) ggplotobj2 = ggplotobj2 + theme(legend.text = element_text(color="white"))
    if(theme_bool["legend.background"]) ggplotobj2 = ggplotobj2 + theme(legend.background = element_rect(fill = "white", color = "white"))
    if(theme_bool["legend.title"]) ggplotobj2 = ggplotobj2 + theme(legend.title = element_text(color="white"))
    if(theme_bool["panel.background"]) ggplotobj2 = ggplotobj2 + theme(panel.background = element_rect(fill = "white", color = "white"))
  } else {
    if(height_aes == "fill") {
      for(layer in seq_along(1:length(ggplotobj2$layers))) {
        if("colour" %in% names(ggplotobj2$layers[[layer]]$mapping) || 
           0 == length(names(ggplotobj2$layers[[layer]]$mapping))) {
          ggplotobj2$layers[[layer]]$aes_params$colour = "white"
        }
        if("fill" %in% names(ggplotobj2$layers[[layer]]$mapping)) {
          ggplotobj2$layers[[layer]]$aes_params$size = 0
        }
        if("shape" %in% names(ggplotobj2$layers[[layer]]$mapping)) {
          shapedata = layer_data(ggplotobj2)
          numbershapes = length(unique(shapedata$shape))
          if(numbershapes > 3) {
            warning("Non-solid shapes will not be projected to 3D.")
          }
          ggplotobj2$layers[[layer]]$geom$draw_key = drawkeyfunction_points
        }
        if("size" %in% names(ggplotobj2$layers[[layer]]$mapping)) {
          ggplotobj2$layers[[layer]]$geom$draw_key = drawkeyfunction_points
        }
        if("alpha" %in% names(ggplotobj2$layers[[layer]]$mapping)) {
          ggplotobj2$layers[[layer]]$geom$draw_key = drawkeyfunction_points
          for(j in seq_len(length(ggplotobj2$layers))) {
            if("stroke" %in% names(ggplotobj2$layers[[j]]$geom$default_aes)) {
              ggplotobj2$layers[[j]]$geom$default_aes$stroke = 0
            }
          }
          ggplotobj2 = suppressMessages({ggplotobj2 + scale_alpha_continuous(range=c(1,1))})
        }
        if("linetype" %in% names(ggplotobj2$layers[[layer]]$mapping)) {
          ggplotobj2$layers[[layer]]$geom$draw_key = drawkeyfunction_lines
        }
      }
    } else {
      for(layer in seq_len(length(ggplotobj2$layers))) {
        if("fill" %in% names(ggplotobj2$layers[[layer]]$mapping) || 
           0 == length(names(ggplotobj2$layers[[layer]]$mapping))) {
          ggplotobj2$layers[[layer]]$aes_params$fill = "white"
        }
        if("shape" %in% names(ggplotobj2$layers[[layer]]$mapping)) {
          shapedata = layer_data(ggplotobj2)
          numbershapes = length(unique(shapedata$shape))
          if(numbershapes > 3) {
            warning("Non-solid shapes will not be projected to 3D.")
          }
          ggplotobj2$layers[[layer]]$geom$draw_key = drawkeyfunction_points
        }
        if("size" %in% names(ggplotobj2$layers[[layer]]$mapping)) {
          ggplotobj2$layers[[layer]]$geom$draw_key = drawkeyfunction_points
        }
        if("alpha" %in% names(ggplotobj2$layers[[layer]]$mapping)) {
          ggplotobj2$layers[[layer]]$geom$draw_key = drawkeyfunction_points
          for(j in seq_len(length(ggplotobj2$layers))) {
            if("stroke" %in% names(ggplotobj2$layers[[j]]$geom$default_aes)) {
              ggplotobj2$layers[[j]]$geom$default_aes$stroke = 0
            }
          }
          ggplotobj2 = suppressMessages({ggplotobj2 + scale_alpha_continuous(range=c(1,1))})
        }
        if("linetype" %in% names(ggplotobj2$layers[[layer]]$mapping)) {
          ggplotobj2$layers[[layer]]$geom$draw_key = drawkeyfunction_lines
        }
      }
    }
    #No custom theme passed, just create one.
    ggplotobj2 = ggplotobj2 +
      theme(text = element_text(color="white"),
            line = element_line(color="white"),
            axis.line = element_line(color="white"),
            axis.title = element_text(color="white"),
            axis.text = element_text(color="white"),
            axis.ticks = element_line(color="white"),
            strip.background = element_rect(fill = "white", color = "white"),
            strip.text = element_text(color="white"),
            legend.key = element_rect(fill = "white", color = "white"),
            legend.text = element_text(color="white"),
            legend.background = element_rect(fill = "white", color = "white"),
            legend.title = element_text(color="white"),
            panel.background = element_rect(fill = "white", color = "white"))
  }
  if(height_aes == "fill") {
    if(length(ggplotobj2$layers) > 0) {
      for(i in seq_along(1:length(ggplotobj2$layers))) {
        ggplotobj2$layers[[i]]$aes_params$size = 0
      }
    }
  } else {
    if(length(ggplotobj2$layers) > 0) {
      for(i in seq_along(1:length(ggplotobj2$layers))) {
        ggplotobj2$layers[[i]]$aes_params$fill = "white"
      }
      if(pointcontract != 1) {
        for(i in 1:length(ggplotobj2$layers)) {
          if(!is.null(ggplotobj2$layers[[i]]$aes_params$size)) {
            ggplotobj2$layers[[i]]$aes_params$size = ggplotobj2$layers[[i]]$aes_params$size * pointcontract
          } else {
            ggplotobj2$layers[[i]]$geom$default_aes$size = ggplotobj2$layers[[i]]$geom$default_aes$size * pointcontract
          }
        }
      }
    }
  }
  tryCatch({
    ggsave(paste0(heightmaptemp,".png"),ggplotobj2,width = width,height = height)
  }, error = function(e) {
    if(any(grepl("Error: Discrete value supplied to continuous scale", as.character(e),fixed = TRUE))) {
      stop(paste0("Error: Discrete variable cannot be mapped to 3D. Did you mean to choose `",ifelse(height_aes == "fill","color","fill"), "` as the `height_aes`?"),call.=FALSE)
    }
  })
  mapheight = png::readPNG(paste0(heightmaptemp,".png"))
  if(invert) {
    mapheight[,,1] = 1 - mapheight[,,1]
  }
  if(raytrace) {
    if(is.null(saved_shadow_matrix)) {
      raylayer = ray_shade(1-t(mapheight[,,1]),maxsearch = 600,sunangle = sunangle,anglebreaks = anglebreaks,
                           zscale=1/scale,multicore = multicore,lambert = lambert, ...)
      if(!preview) {
        mapcolor %>%
          add_shadow(raylayer,shadow_intensity) %>%
          plot_3d((t(1-mapheight[,,1])),zscale=1/scale, ... )
      } else {
        mapcolor %>%
          add_shadow(raylayer,shadow_intensity) %>%
          plot_map()
      }
    } else {
      raylayer = saved_shadow_matrix
      if(!preview) {
        mapcolor %>%
          add_shadow(raylayer,shadow_intensity) %>%
          plot_3d((t(1-mapheight[,,1])),zscale=1/scale, ... )
      } else {
        mapcolor %>%
          add_shadow(raylayer,shadow_intensity) %>%
          plot_map()
      }
    }
  } else {
    if(!preview) {
      plot_3d(mapcolor, (t(1-mapheight[,,1])), zscale=1/scale, ...)
    } else {
      plot_map(mapcolor)
    }
  }
  if(save_shadow_matrix) {
    return(raylayer)
  }
}