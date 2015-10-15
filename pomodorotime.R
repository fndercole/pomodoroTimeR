#!/usr/bin/r
# ---
# author: Favio D'Ercole
# license: GPLv3
# ---

require("RGtk2")
require(gWidgets2)
options(guiToolkit="RGtk2")

# Parameters
## default duration for each timer in seconds
refTime <- c(
  workingTime = 25*60,
  shortBreak = 5*60,
  longBreak = 15*60,
  custom = 1*60
)

## default number of short breaks before a long break
nwork <- 3

# Variables
ws <- 0
sh <- 0
st <- 1
min <- 0
sec <- 0
previousTime <- 0
elapsedTime <- 0

# Actions
timer <- gtimer(1000, 
  FUN = function(data, ...) {
    currentTime <- Sys.time();
    elapsedTime <<- as.numeric(difftime(currentTime, startTime, units="secs")) + previousTime
    restTime <- refTime[st] - elapsedTime
    min <- floor(restTime/60)
    sec <- round(restTime - min*60)
    frac <- 100*elapsedTime/refTime[st]
    svalue(progress) <- frac
    svalue(clock) <- paste(formatC(min, width=2, flag="0"), formatC(sec, width=2, flag="0"), sep=":")
    if (elapsedTime >= refTime[st]) {
      timer$stop_timer()
      enabled(startWorkingTime) <- TRUE
      enabled(pauseWorkingTime) <- FALSE
      enabled(stopWorkingTime) <- FALSE
      focus(W) <- TRUE
      if (st == 1) {
        ws <<- ws + 1
        if (sh < nwork) {
          st <<- 2
        } else {
          sh <<- 0
          st <<- 3
        }
      } else if (st == 2) {
        sh <<- sh + 1 
        st <<- 1
      } else if (st == 3) {
        sh <<- 0
        st <<- 1
      } else {
        st <<- st
        sh <<- sh + 1
      }
      svalue(opts, index=TRUE) <- st
      enabled(GG) <- TRUE
      svalue(progress) <- 100
      svalue(clock) <- paste(formatC(0, width=2, flag="0"), formatC(0, width=2, flag="0"), sep=":")
      focus(W) <- TRUE
      cmd <- 'notify-send -i gtk-ok "Pomodoro TimeR" "Time out!"'
      system(cmd, ignore.stdout=TRUE, ignore.stderr=TRUE, wait=FALSE)
      cmd <- "mplayer /usr/share/sounds/ubuntu/stereo/desktop-logout.ogg"
      system(cmd, ignore.stdout=TRUE, ignore.stderr=TRUE, wait=FALSE)
      startTime <<- Sys.time()
      enabled(clock) <- FALSE
      redTimer$start_timer()
    }
  },
  start = FALSE
)

redTimer <- gtimer(1000, 
  FUN = function(data, ...) {
    currentTime <- Sys.time();
    elapsedTime <<- as.numeric(difftime(currentTime, startTime, units="secs")) + previousTime
    min <- floor(elapsedTime/60)
    sec <- round(elapsedTime - min*60)
    svalue(clock) <- paste(formatC(min, width=2, flag="0"), formatC(sec, width=2, flag="0"), sep=":")
  },
  start = FALSE
)

startWorkingTime <- gaction("start",
  icon = "gtk-media-play",
  key.accel = "w",
  handler = function(h,...) {
    if (redTimer$started) {
      redTimer$stop_timer()
    }
    if (st == 4) {
      imin <- as.numeric(svalue(gmin))
      isec <- as.numeric(svalue(gsec))
      if (is.na(imin)) {
        imin <- 0
        svalue(gmin) <- "0"
      }
      if (is.na(isec)) {
        isec <- 0
        svalue(gsec) <- "0"
      }
      refTime["custom"] <<- imin*60 + isec
    }
    min <- floor(refTime[st]/60)
    sec <- round(refTime[st] - min*60)    
    svalue(clock) <- paste(formatC(min, width=2, flag="0"), formatC(sec, width=2, flag="0"), sep=":")
    enabled(clock) <- TRUE
    startTime <<- Sys.time()
    timer$start_timer()
    enabled(startWorkingTime) <- FALSE
    enabled(pauseWorkingTime) <- TRUE
    enabled(stopWorkingTime) <- TRUE
    enabled(GG) <- FALSE
  }
)

stopWorkingTime <- gaction("stop",
  icon = "gtk-media-stop",
  handler = function(h,...) {
    timer$stop_timer()
    previousTime <<- 0
    enabled(startWorkingTime) <- TRUE
    enabled(pauseWorkingTime) <- FALSE
    enabled(stopWorkingTime) <- FALSE
    enabled(GG) <- TRUE
  }
)

pauseWorkingTime <- gaction("pause",
  icon = "gtk-media-pause",
  handler = function(h,...) {
    timer$stop_timer()
    previousTime <<- elapsedTime
    enabled(startWorkingTime) <- TRUE
    enabled(pauseWorkingTime) <- FALSE
    enabled(stopWorkingTime) <- TRUE
    enabled(GG) <- FALSE
  }
)

# GUI
opts <- gradio(c("Pomodoro", "Short Break", "Long Break", "Custom"), horizontal = TRUE,
  handler = function(h, ...) {
    st <<- svalue(opts, index=TRUE)
    if (st == 4) {
      enabled(gmin) <- TRUE
      enabled(gsec) <- TRUE
    } else {
      enabled(gmin) <- FALSE
      enabled(gsec) <- FALSE   
    }
  }
)

gmin <- gedit(text="1", width=4)
gsec <- gedit(text="0", width=4)
enabled(gmin) <- FALSE
enabled(gsec) <- FALSE
GG <- ggroup(spacing=10)

progress <- gprogressbar(0)

enabled(startWorkingTime) <- TRUE
enabled(pauseWorkingTime) <- FALSE
enabled(stopWorkingTime) <- FALSE
enabled(GG) <- TRUE

clock <- glabel("00:00")
  font(clock) <- list(weight="light", size=72)

toolBar <- gtoolbar(list(startWorkingTime, pauseWorkingTime, gseparator(), stopWorkingTime), style="icon")

W <- gwindow("Pomodoro TimeR", visible=FALSE, 
      handler = function(h, ...) {
        gtkMainQuit()
      }
    )

G <- ggroup(horizontal=FALSE, spacing=10)

add(G, clock, expand=TRUE)
  add(GG, opts, expand=TRUE)
  add(GG, gmin, expand=FALSE)
  add(GG, glabel(":"), expand=FALSE)
  add(GG, gsec, expand=FALSE)
add(G, GG, expand=FALSE)
add(G, progress, expand=FALSE)

add(W, toolBar)
add(W, G)

visible(W) <- TRUE
focus(W) <- TRUE

gtkMain()
