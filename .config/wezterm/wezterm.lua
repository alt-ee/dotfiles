local wezterm = require 'wezterm'
local config = {}

if wezterm.config_builder then
    config = wezterm.config_builder()
end

config.font_size = 12
config.use_fancy_tab_bar = false
config.hide_tab_bar_if_only_one_tab = true
config.audible_bell = 'Disabled'
config.check_for_updates = false
config.show_update_window = false
config.font = wezterm.font 'Iosevka Term'
config.default_prog = { '/usr/bin/fish', '-i' }

return config
