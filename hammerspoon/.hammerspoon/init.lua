
-- show a notification any time this config file is reloaded
hs.notify.new({title="Hammerspoon", informativeText="Configuration Reloaded"}):send()

--
local hyper = {"control", "option"}

-- reload config
hs.hotkey.bind(hyper, "R", function()
    hs.reload()
end)

-- test
hs.hotkey.bind(hyper, "W", function()
    hs.notify.new({title="Hammerspoon", informativeText="Hello World"}):send()
end)

-- easy fullscreening
hs.hotkey.bind(hyper, "F", function()
    hs.window.focusedWindow():toggleFullScreen()
end)

-- show Empty desktop
hs.hotkey.bind(hyper, "H", function()
    hs.window.desktop():focus()
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

