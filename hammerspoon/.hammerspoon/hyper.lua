local HYPER = {"rightalt"}

-----------------------------------------------
-- Launcher with HYPER modifier
-----------------------------------------------

-- In the following example, the list `{ 'e', "Emacs", true }` holds the key
-- to be used with the modifier to launch the application described in the 2nd
-- position. The 3rd position (optional) is used as workaround to bring
-- apps compiled without a title bar (like my emacs version <3) at front.
-----------------------------------------------
local apps = {
    { 'a', "Calendar" },
    { 'c', "Google Chrome" },
    { 'o', "Opera" },
    { 'e', "Emacs", true },
    { 'i', "iTerm", true },
    { 's', "Slack" },
    { 'f', "Finder" },
    { 'm', "Mail" },
    { 'p', "Spotify" },
    { 'r', "Activity Monitor" },
    { 'b', "Karabiner-Elements" },
    { ';', "Alfred 3" },
    { 'w', "1Password 6" },
}

for _,item in ipairs(apps) do
    hs.hotkey.bind(HYPER, item[1], function()
        local app = hs.application.open(item[2])
        if app then app:activate(true) end
    end)
end
