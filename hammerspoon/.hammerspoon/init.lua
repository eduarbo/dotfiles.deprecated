-----------------------------------------------
-- Set up
-----------------------------------------------

local MEGA = {"cmd", "alt", "ctrl"}
local HYPER = {"cmd", "alt", "ctrl", "shift"}
hs.window.animationDuration = 0

require("hs.application")
require("hs.window")
require("hs.layout")
require("hs.caffeinate")

-----------------------------------------------
-- Reload config on write
-----------------------------------------------

hs.pathwatcher.new(os.getenv("HOME") .. "/.hammerspoon/", hs.reload):start()
hs.alert("Config loaded")

-----------------------------------------------
-- Launch Screensaver
-----------------------------------------------

hs.hotkey.bind("alt", '`', function()
  hs.caffeinate.startScreensaver()
end)

-----------------------------------------------
-- Date & Time
-----------------------------------------------

hs.hotkey.bind("alt", '/', function()
  local date = os.date('%A, %h %e')
  local time = os.date('%I:%M%p'):gsub('^0',''):lower()
  hs.alert(time..' - '..date, 2.5)
end)
