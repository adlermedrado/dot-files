local wezterm = require 'wezterm'

local config = {}

config.scrollback_lines = 9999

if wezterm.config_builder then
    config = wezterm.config_builder()
end

config.color_scheme = 'Tokyo Night'
config.window_background_opacity = 0.9

config.font = wezterm.font('JetBrains Mono', { weight = 'Bold', italic = true })
config.font_size = 16.0

return config
