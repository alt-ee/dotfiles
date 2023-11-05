local wezterm = require 'wezterm'
local config = {}

if wezterm.config_builder then
    config = wezterm.config_builder()
end

config.font = wezterm.font 'Iosevka Comfy'
config.color_scheme = 'Borland'

config.font_size = 14
config.use_fancy_tab_bar = false
config.hide_tab_bar_if_only_one_tab = true
config.audible_bell = 'Disabled'

return config