
-- show a notification any time this config file is reloaded
hs.notify.new({title="Hammerspoon", informativeText="Configuration Reloaded"}):send()

--
local hyper = {"control", "option"}

-- reload config
hs.hotkey.bind(hyper, "R", function()
    hs.reload()
end)

-- sleep displays
hs.hotkey.bind(hyper, "delete", function()
    os.execute("pmset displaysleepnow")
end)

-- easy fullscreening
hs.hotkey.bind(hyper, "F", function()
    hs.window.focusedWindow():toggleFullScreen()
end)

-- open iTerm
hs.hotkey.bind(hyper, "J", function()
    hs.application.launchOrFocus("iTerm")
end)

-- open Firefox
hs.hotkey.bind(hyper, "K", function()
    hs.application.launchOrFocus("Firefox")
end)

-- open Slack
hs.hotkey.bind(hyper, "L", function()
    hs.application.launchOrFocus("Slack")
end)

