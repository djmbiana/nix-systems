-- Vague theme for NvChad
-- Based on https://github.com/vague-theme/vague-alacritty

local M = {}

M.base_30 = {
  white = "#cdcdcd",
  darker_black = "#0f0f10",
  black = "#141415", --  nvim bg
  black2 = "#1a1a1b",
  one_bg = "#1e1e1f",
  one_bg2 = "#252530",
  one_bg3 = "#2d2d38",
  grey = "#3d3d48",
  grey_fg = "#4d4d58",
  grey_fg2 = "#5d5d68",
  light_grey = "#606079",
  red = "#d8647e",
  baby_pink = "#e08398",
  pink = "#bb9dbd",
  line = "#1e1e1f", -- for lines like vertsplit
  green = "#7fa563",
  vibrant_green = "#99b782",
  nord_blue = "#6e94b2",
  blue = "#6e94b2",
  yellow = "#f3be7c",
  sun = "#f5cb96",
  purple = "#bb9dbd",
  dark_purple = "#aa8cac",
  teal = "#aeaed1",
  orange = "#f3be7c",
  cyan = "#aeaed1",
  statusline_bg = "#1a1a1b",
  lightbg = "#252530",
  pmenu_bg = "#7fa563",
  folder_bg = "#6e94b2",
}

M.base_16 = {
  base00 = "#141415", -- Default Background
  base01 = "#1e1e1f", -- Lighter Background
  base02 = "#252530", -- Selection Background
  base03 = "#606079", -- Comments, Invisibles
  base04 = "#9d9daf", -- Dark Foreground
  base05 = "#cdcdcd", -- Default Foreground
  base06 = "#d7d7d7", -- Light Foreground
  base07 = "#e1e1e1", -- Light Background
  base08 = "#d8647e", -- Variables, Tags
  base09 = "#f3be7c", -- Integers, Booleans, Constants
  base0A = "#f5cb96", -- Classes, Search Text Background
  base0B = "#7fa563", -- Strings, Inherited Class
  base0C = "#aeaed1", -- Support, Regular Expressions
  base0D = "#6e94b2", -- Functions, Methods
  base0E = "#bb9dbd", -- Keywords, Storage
  base0F = "#e08398", -- Deprecated, Embedded
}

M.type = "dark"

M = require("base46").override_theme(M, "vague")

return M
