-----------------------------------------------
-- Set up
-----------------------------------------------

local MEGA = {"alt", "shift"}
local HYPER = {"cmd", "alt", "ctrl", "shift"}
hs.window.animationDuration = 0

require("hs.application")
require("hs.window")
require("hs.layout")
require("hs.caffeinate")

-----------------------------------------------
-- Reload config on write
-----------------------------------------------

hs.hotkey.bind(MEGA, 'delete', hs.reload)

hs.pathwatcher.new(os.getenv("HOME") .. "/.hammerspoon/", hs.reload):start()
hs.alert("Config loaded")

-----------------------------------------------
-- Handy functions
-----------------------------------------------

function focusedWindow (fn)
  return function()
    -- Workaround to select windows without a title bar
    local win = hs.window.focusedWindow() or hs.window.frontmostWindow()
    if win == nil then
      hs.alert("No active window")
    else
      fn(win)
    end
  end
end

-----------------------------------------------
-- Move and Resize windows with modifier Shift+CMD
-----------------------------------------------

local winmod = {
  -- Halves
  { 'h', hs.layout.left50 },  -- left half
  { 'l', hs.layout.right50 }, -- right half
  { 'k', {0,0,1,0.5} },       -- top half
  { 'j', {0,0.5,1,0.5} },     -- bottom half
  --quadrants
  { 'i', {0,0,1/2,1/2} },     -- left top corner
  { 'o', {1/2,0,1/2,1/2} },   -- right top corner
  { 'u', {0,1/2,1/2,1/2} },   -- left bottom corner
  { 'p', {1/2,1/2,1/2,1/2} }, -- right bottom corner
}

for i,item in ipairs(winmod) do
  hs.hotkey.bind(HYPER, item[1], focusedWindow(function(win)
    win:move(item[2])
  end))
end

-----------------------------------------------
-- fullscreen
-----------------------------------------------

local frameCache = {}
hs.hotkey.bind(HYPER, 'f', focusedWindow(function(win)
  if frameCache[win:id()] then
    win:setFrame(frameCache[win:id()])
    frameCache[win:id()] = nil
  else
    frameCache[win:id()] = win:frame()
    win:move(hs.layout.maximized)
  end
end))

-----------------------------------------------
-- Launcher with mega modifier
-----------------------------------------------
-- In the following example, the list `{ 'e', "Emacs", true }` holds the key
-- to be used with the modifier to launch the application described in the 2nd
-- position. The 3rd position (optional) is used as workaround to bring
-- apps compiled without a title bar (like my emacs version <3) at front.
-----------------------------------------------
local apps = {
  { 'a', "Calendar" },
  { 'c', "Google Chrome" },
  { 'b', "/Applications/FirefoxDeveloperEdition.app" },
  { 'e', "Emacs", true },
  { 'i', "iTerm" },
  { 's', "Skype" },
  { 'v', "VLC" },
  { 'f', "Finder" },
  { 'm', "Mail" },
  { 'd', "Slack" },
  { 'p', "Spotify" },
  { 'r', "Karabiner" },
}

for i,item in ipairs(apps) do
  hs.hotkey.bind(MEGA, item[1], function()
    local app = hs.application.open(item[2])
    if app then app:activate(true) end
  end)
end

-- Open Hammerspoon Console
hs.hotkey.bind(MEGA, 'o', function() hs.openConsole() end)

-- Launch Screensaver
hs.hotkey.bind(MEGA, '`', function()
  hs.caffeinate.startScreensaver()
end)

-----------------------------------------------
-- Date & Time
-----------------------------------------------

hs.hotkey.bind(MEGA, '/', function()
  local date = os.date('%A, %h %e')
  local time = os.date('%I:%M%p'):gsub('^0',''):lower()
  hs.alert(time..' - '..date, 2.5)
end)

-----------------------------------------------
-- Toggle screen for focused window
-----------------------------------------------

hs.hotkey.bind(MEGA, "tab", function ()
  moveWindowToToNextScreen(hs.window.focusedWindow())
end)

function moveWindowToToNextScreen(win)
  if win:screen():toWest() then
    win:moveOneScreenWest(0)
  else
    win:moveOneScreenEast(0)
  end
end
