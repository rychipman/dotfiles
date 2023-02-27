-- show a notification any time this config file is reloaded
hs.notify.new({title="Hammerspoon", informativeText="Configuration Reloaded"}):send()

--
local hyper = {"command", "control", "option", "shift"}

-- reload config
hs.hotkey.bind(hyper, "R", function()
    hs.reload()
end)

apps = {
  {
    name = "Firefox",
    hyperKey = "K",
    localBindings = {
      {
        description = "tab left",
        from = {modifiers = hyper, key = "["},
        to = {modifiers = {"command", "option"}, key = "Left"},
      },
      {
        description = "tab right",
        from = {modifiers = hyper, key = "]"},
        to = {modifiers = {"command", "option"}, key = "Right"},
      },
    },
  },
  {
    name = "Slack",
    hyperKey = "L",
    localBindings = {
      {
        description = "next unread",
        from = {modifiers = {"command"}, key = "J"},
        to = {modifiers = {"shift", "option"}, key = "Down"},
      },
      {
        description = "prev unread",
        from = {modifiers = {"command"}, key = "K"},
        to = {modifiers = {"shift", "option"}, key = "Up"},
      },
    },
  },
  {
    name = "Todoist",
    hyperKey = "O",
    localBindings = {
      {
        description = "C-j down",
        from = {modifiers = {"command"}, key = "J"},
        to = {modifiers = {}, key = "Down"},
      },
      {
        description = "C-n down",
        from = {modifiers = {"command"}, key = "N"},
        to = {modifiers = {}, key = "Down"},
      },
      {
        description = "C-k up",
        from = {modifiers = {"command"}, key = "K"},
        to = {modifiers = {}, key = "Up"},
      },
      {
        description = "C-p up",
        from = {modifiers = {"command"}, key = "P"},
        to = {modifiers = {}, key = "Up"},
      },
    },
  },
  {
    name = "Quicken",
    hyperKey = "I",
    localBindings = {
      {
        description = "C-j down",
        from = {modifiers = {"command"}, key = "J"},
        to = {modifiers = {}, key = "Down"},
      },
      {
        description = "C-k up",
        from = {modifiers = {"command"}, key = "K"},
        to = {modifiers = {}, key = "Up"},
      },
    },
  },
  {
    name = "Zoom",
    hyperKey = "Z",
  },
  {
    name = "Messages",
    hyperKey = "M",
    localBindings = {
      {
        description = "Next Conversation",
        from = {modifiers = {"command"}, key = "J"},
        to = {modifiers = {"control"}, key = "Tab"},
      },
      {
        description = "Prev Conversation",
        from = {modifiers = {"command"}, key = "K"},
        to = {modifiers = {"control", "shift"}, key = "Tab"},
      },
    },
  },
  {
    name = "iTerm",
    hyperKey = "H",
  },
  {
    name = "emacs",
    hyperKey = "J",
  },
  {
    name = "Spotify",
    hyperKey = "G",
  },
}

function doAppHyperKey(appName)
  local app = hs.application.get(appName)
  if app then
    if app:isFrontmost() then
      app:hide()
    else
      app:activate()
    end
  else
    hs.application.launchOrFocus(appName)
  end
end

for _, app in pairs(apps) do
  if app.hyperKey ~= nil then
    hs.hotkey.bind(hyper, app.hyperKey, function() doAppHyperKey(app.name) end)
  end

  if app.localBindings ~= nil then
    local modal = hs.hotkey.modal.new()
    for _, bind in pairs(app.localBindings) do
        modal:bind(bind.from.modifiers, bind.from.key, function()
          hs.eventtap.keyStroke(bind.to.modifiers, bind.to.key)
        end)
    end
    hs.window.filter.new(app.name)
        :subscribe(hs.window.filter.windowFocused, function() modal:enter() end)
        :subscribe(hs.window.filter.windowUnfocused, function() modal:exit() end)
  end
end
