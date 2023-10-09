local wezterm = require 'wezterm'

local config = {}

if wezterm.config_builder then
    config = wezterm.config_builder()
end

config.color_scheme = 'rose-pine-moon'

config.font = wezterm.font('JetBrains Mono', { weight = 'Bold', italic = true })
config.font_size = 15.0

return config
