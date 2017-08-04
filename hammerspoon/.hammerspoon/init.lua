-- adjust hotkey logging... info as the default is too much.
local utils = require("utils")

-- Reload config on write
hs.pathwatcher.new(os.getenv("HOME") .. "/.hammerspoon/", hs.reload):start()

utils.tempNotify(2, hs.notify.new({
    title = "Hammerspoon",
    subTitle = "Config reloaded",
}))

hs.hotkey.setLogLevel("warning")
hs.logger.historySize(1000)
log = hs.logger.new('init','debug')
log.i('Initializing')

hs.alert.defaultStyle.strokeColor = { white = 0, alpha = 0 }
hs.alert.defaultStyle.fillColor = { white = 0, alpha = 0.75 }
hs.alert.defaultStyle.textStyle = { paragraphStyle = { alignment = "center" } }
hs.alert.defaultStyle.textSize = 25

-----------------------------------------------
-- Press Cmd+Q twice to quit
-----------------------------------------------

local quitModal = hs.hotkey.modal.new('cmd','q')

function quitModal:entered()
    hs.alert.closeAll()
    hs.alert.show("Press Cmd+Q again to quit", 1)
    hs.timer.doAfter(1, function() quitModal:exit() end)
end

local function doQuit()
    local res = hs.application.frontmostApplication():selectMenuItem("^Quit.*$")
    quitModal:exit()
end

quitModal:bind('cmd', 'q', doQuit)
quitModal:bind('', 'escape', function() quitModal:exit() end)

-----------------------------------------------
-- Modules
-----------------------------------------------

require("hyper")
require("netspeed")
require("window")
