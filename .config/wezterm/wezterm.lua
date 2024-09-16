local wezterm = require("wezterm")
local config = {}

if wezterm.config_builder then
	config = wezterm.config_builder()
end

function set_font(window, fontname)
	local overrides = window:get_config_overrides() or {}
	overrides.font = wezterm.font(fontname)
	window:set_config_overrides(overrides)
end

function set_font_size(window, size)
	local overrides = window:get_config_overrides() or {}
	overrides.font_size = size
	window:set_config_overrides(overrides)
end

wezterm.on('big-font', function(window, pane)
	set_font_size(window, 16)
end)
	
wezterm.on('small-font', function(window, pane)
	set_font_size(window, 11)
end)

config.font_size = 16
config.use_fancy_tab_bar = false
config.hide_tab_bar_if_only_one_tab = true
config.audible_bell = "Disabled"
config.check_for_updates = false
config.show_update_window = false
config.font = wezterm.font(
	"Tamzen",
	{ weight = 'Regular'}
)
config.color_scheme = "Seti"

config.keys = {
	{ key = "=", mods = "CTRL", action = wezterm.action.EmitEvent 'big-font' },
	{ key = "-", mods = "CTRL", action = wezterm.action.EmitEvent 'small-font' }
}

return config
