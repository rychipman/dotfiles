-- show a notification any time this config file is reloaded
hs.notify.new({title="Hammerspoon", informativeText="Configuration Reloaded"}):send()

--
local hyper = {"command", "control", "option", "shift"}

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

-- open Spotify
hs.hotkey.bind(hyper, "G", function()
    hs.application.launchOrFocus("Spotify")
end)

-- open iTerm
hs.hotkey.bind(hyper, "H", function()
    hs.application.launchOrFocus("iTerm")
end)

-- open emacs
hs.hotkey.bind(hyper, "J", function()
    hs.application.launchOrFocus("emacs")
end)

-- open Firefox
hs.hotkey.bind(hyper, "K", function()
    hs.application.launchOrFocus("Firefox")
end)
local ff_modal = hs.hotkey.modal.new()
ff_modal:bind(hyper, "[", function() hs.eventtap.keyStroke({"command", "option"}, "Left") end)
ff_modal:bind(hyper, "]", function() hs.eventtap.keyStroke({"command", "option"}, "Right") end)
hs.window.filter.new('Firefox')
    :subscribe(hs.window.filter.windowFocused,function()
        ff_modal:enter()
    end)
    :subscribe(hs.window.filter.windowUnfocused,function()
        ff_modal:exit()
    end)

-- open Slack
hs.hotkey.bind(hyper, "L", function()
    hs.application.launchOrFocus("Slack")
end)

-- open Todoist
hs.hotkey.bind(hyper, "O", function()
    hs.application.launchOrFocus("Todoist")
end)

-- open Quicken
hs.hotkey.bind(hyper, "I", function()
    hs.application.launchOrFocus("Quicken")
end)

-- open Zoom
hs.hotkey.bind(hyper, "M", function()
    hs.application.launchOrFocus("zoom.us")
end)
hs.hotkey.bind(hyper, "Z", function()
    hs.application.launchOrFocus("zoom.us")
end)
