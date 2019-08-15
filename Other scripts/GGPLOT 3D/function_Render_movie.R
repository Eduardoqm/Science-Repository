render_movie = function(filename, type = "orbit", frames = 360, fps = 30, 
                        phi = 30, theta = 0, zoom = NULL, fov = NULL, 
                        title_text = NULL, title_offset = c(20,20), 
                        title_color = "black", title_size = 30, title_font = "sans",
                        image_overlay = NULL, audio=NULL, progbar = interactive(), ...) {
  if(!("av" %in% rownames(utils::installed.packages()))) {
    stop("`av` package required for render_movie()")
  }
  if(is.null(filename)) {
    stop("render_movie requires a filename")
  }
  if(!is.null(title_text)) {
    has_title = TRUE
  } else {
    has_title = FALSE
  }
  if(length(title_offset) != 2) {
    stop("`title_offset` needs to be length-2 vector")
  }
  if(!is.null(image_overlay)) {
    if("character" %in% class(image_overlay)) {
      image_overlay_file = image_overlay
      has_overlay = TRUE
    } else if("array" %in% class(image_overlay)) {
      image_overlay_file = tempfile()
      png::writePNG(image_overlay_file)
      has_overlay = TRUE
    }
  } else {
    has_overlay = FALSE
  }
  if(substring(filename, nchar(filename)-3,nchar(filename)) != ".mp4") {
    filename = paste0(filename,".mp4")
  }
  windowsize = rgl::par3d()$viewport
  if(is.null(fov)) {
    fov = rgl::par3d()$FOV
  }
  if(is.null(zoom)) {
    zoom = rgl::par3d()$zoom
  }
  png_files = file.path(tempdir(), sprintf("image%d.png", seq_len(frames)))
  on.exit(unlink(png_files))
  if(type == "orbit") {
    theta_vector = seq(0,360,length.out = frames+1)[-(frames+1)]
    for(i in seq_len(frames)) {
      render_camera(theta = theta_vector[i], phi = phi, zoom = zoom, fov = fov)
      rgl::snapshot3d(filename = png_files[i])
    }
  } else if (type == "oscillate") {
    theta_vector = theta + 45 * sin(seq(0,360,length.out = frames+1)[-(frames+1)]*pi/180)
    for(i in seq_len(frames)) {
      render_camera(theta = theta_vector[i], phi = phi, zoom = zoom, fov = fov)
      rgl::snapshot3d(filename = png_files[i])
    }
  } else if (type == "custom") {
    if(length(theta) == 1) theta = rep(theta, frames)
    if(length(phi) == 1) phi = rep(phi, frames)
    if(length(zoom) == 1) zoom = rep(zoom, frames)
    if(length(fov) == 1) fov = rep(fov, frames)
    if(!all(c(length(theta), length(phi), length(zoom),length(fov)) == frames)) {
      stop("All camera vectors must be the same length (or fixed values)")
    }
    for(i in seq_len(frames)) {
      render_camera(theta = theta[i], phi = phi[i], zoom = zoom[i], fov = fov[i])
      rgl::snapshot3d(filename = png_files[i])
    }
  } else {
    stop("Unknown type: ", type)
  }
  temp = png::readPNG(png_files[1])
  dimensions = dim(temp)
  if(dimensions[1] %% 2 != 0) {
    dimensions[1] = dimensions[1] - 1
  }
  if(has_overlay) {
    if(!("magick" %in% rownames(utils::installed.packages()))) {
      stop("`magick` package required for adding overlay")
    }
    if(progbar) {
      pb = progress::progress_bar$new(
        format = "  Adding overlay image [:bar] :percent eta: :eta",
        total = frames, width= 60)
    }
    for(i in seq_len(frames)) {
      if(progbar) {
        pb$tick()
      }
      magick::image_read(png_files[i]) %>%
        magick::image_composite(
          magick::image_scale(magick::image_read(image_overlay_file),
                              paste0(dimensions[1],"x",dimensions[2]))
        ) %>%
        magick::image_write(path = png_files[i], format = "png")
    }
  }
  if(has_title) {
    if(!("magick" %in% rownames(utils::installed.packages()))) {
      stop("`magick` package required for adding title")
    }
    if(progbar) {
      pb = progress::progress_bar$new(
        format = "  Adding title text [:bar] :percent eta: :eta",
        total = frames, width= 60)
    }
    for(i in seq_len(frames)) {
      if(progbar) {
        pb$tick()
      }
      magick::image_read(png_files[i]) %>%
        magick::image_annotate(title_text, 
                               location = paste0("+", title_offset[1],"+",title_offset[2]),
                               size = title_size, color = title_color, 
                               font = title_font, ...) %>%
        magick::image_write(path = png_files[i], format = "png")
    }
  }
  av::av_encode_video(png_files, output = filename, framerate = fps, 
                      vfilter = paste0("scale=",dimensions[1],":-2"), audio=audio)
}