
-- show a notification any time this config file is reloaded
hs.notify.new({title="Hammerspoon", informativeText="Configuration Reloaded"}):send()

-- reload config
hs.hotkey.bind({"control", "option"}, "R", function()
    hs.reload()
end)

-- test
hs.hotkey.bind({"control", "option"}, "W", function()
    hs.notify.new({title="Hammerspoon", informativeText="Hello World"}):send()
end)

hs.hotkey.bind({"control"}, "Tab", function()
    hs.notify.new({title="Hammerspoon", informativeText="Hello World"}):send()
end)


