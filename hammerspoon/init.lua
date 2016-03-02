-----------------------------------------------
-- Set up
-----------------------------------------------

-- local hyper = {"shift", "cmd", "alt", "ctrl"}
local hyper = {"shift", "alt"}
hs.window.animationDuration = 0

require("hs.application")
require("hs.window")

-----------------------------------------------
-- Reload config on write
-----------------------------------------------

function reload_config(files)
  hs.reload()
end
hs.pathwatcher.new(os.getenv("HOME") .. "/.hammerspoon/", reload_config):start()
hs.alert.show("Config loaded")

-----------------------------------------------
-- Handy functions
-----------------------------------------------

function focusedWindow (fn)
  return function()
    -- Workaround to select windows without a title bar
    local win = hs.window.focusedWindow() or hs.window.frontmostWindow()
    if win == nil then
      hs.alert.show("No active window")
    else
      fn(win, win:frame(), win:screen():frame())
    end
  end
end
-----------------------------------------------
-- hyper d for left one half window
-----------------------------------------------

hs.hotkey.bind(hyper, 'd', focusedWindow(function(win, f, max)
  f.x = max.x
  f.y = max.y
  f.w = max.w / 2
  f.h = max.h
  win:setFrame(f)
end))

-----------------------------------------------
-- hyper g for right one half window
-----------------------------------------------

hs.hotkey.bind(hyper, 'g', focusedWindow(function(win, f, max)
  f.x = max.x + (max.w / 2)
  f.y = max.y
  f.w = max.w / 2
  f.h = max.h
  win:setFrame(f)
end))

-----------------------------------------------
-- hyper f for fullscreen
-----------------------------------------------

hs.hotkey.bind(hyper, 'f', focusedWindow(function(win, f, max)
  f.x = max.x
  f.y = max.y
  f.w = max.w
  f.h = max.h
  win:setFrame(f)
end))

-----------------------------------------------
-- hyper r for top left one quarter window
-----------------------------------------------

hs.hotkey.bind(hyper, 'r', focusedWindow(function(win, f, max)
  f.x = max.x
  f.y = max.y
  f.w = max.w / 2
  f.h = max.h / 2
  win:setFrame(f)
end))

-----------------------------------------------
-- hyper t for top right one quarter window
-----------------------------------------------

hs.hotkey.bind(hyper, 't', focusedWindow(function(win, f, max)
  f.x = max.x + (max.w / 2)
  f.y = max.y
  f.w = max.w / 2
  f.h = max.h / 2
  win:setFrame(f)
end))

-----------------------------------------------
-- hyper v for bottom left one quarter window
-----------------------------------------------

hs.hotkey.bind(hyper, 'v', focusedWindow(function(win, f, max)
  f.x = max.x + (max.w / 2)
  f.y = max.y + (max.h / 2)
  f.w = max.w / 2
  f.h = max.h / 2
  win:setFrame(f)
end))

-----------------------------------------------
-- hyper c for bottom right one quarter window
-----------------------------------------------

hs.hotkey.bind(hyper, 'c', focusedWindow(function(win, f, max)
  f.x = max.x
  f.y = max.y + (max.h / 2)
  f.w = max.w / 2
  f.h = max.h / 2
  win:setFrame(f)
end))

-----------------------------------------------
-- Hyper hjkl to switch window focus
-----------------------------------------------

hs.hotkey.bind(hyper, 'k', focusedWindow(function(win)
  win:focusWindowNorth()
end))

hs.hotkey.bind(hyper, 'j', focusedWindow(function(win)
  win:focusWindowSouth()
end))

hs.hotkey.bind(hyper, 'l', focusedWindow(function(win)
  win:focusWindowEast()
end))

hs.hotkey.bind(hyper, 'h', focusedWindow(function(win)
  win:focusWindowWest()
end))
