-- Custom NVChad theme
-- Save this as: lua/themes/mytheme.lua

local M = {}

M.base_30 = {
  white = "#d3c6aa",
  darker_black = "#1e252a",
  black = "#2d353b", -- Main background
  black2 = "#353d44",
  one_bg = "#3c444a",
  one_bg2 = "#454c52",
  one_bg3 = "#4c5359",
  grey = "#5a6268",
  grey_fg = "#6b7379",
  grey_fg2 = "#7c848a",
  light_grey = "#8d959b",
  red = "#e67e80",
  baby_pink = "#e9959c",
  pink = "#d699b6",
  line = "#3c444a", -- for lines like vertsplit
  green = "#a7c080",
  vibrant_green = "#b0c78c",
  nord_blue = "#7fbbb3",
  blue = "#7fbbb3",
  yellow = "#dbbc7f",
  sun = "#e0c57f",
  purple = "#d699b6",
  dark_purple = "#b699c0",
  teal = "#83c092",
  orange = "#e79f80",
  cyan = "#83c092",
  statusline_bg = "#353d44",
  lightbg = "#454c52",
  pmenu_bg = "#a7c080",
  folder_bg = "#7fbbb3",
}

M.base_16 = {
  base00 = "#2d353b", -- Default Background
  base01 = "#353d44", -- Lighter Background (status line, etc.)
  base02 = "#454c52", -- Selection Background
  base03 = "#5a6268", -- Comments, Invisibles
  base04 = "#6b7379", -- Dark Foreground (status line)
  base05 = "#d3c6aa", -- Default Foreground
  base06 = "#e0d5b3", -- Light Foreground
  base07 = "#f0e8d9", -- Lightest Foreground
  base08 = "#e67e80", -- Variables, Tags
  base09 = "#e79f80", -- Integers, Constants
  base0A = "#dbbc7f", -- Classes, Search
  base0B = "#a7c080", -- Strings
  base0C = "#83c092", -- Support, Regex
  base0D = "#7fbbb3", -- Functions, Methods
  base0E = "#d699b6", -- Keywords, Storage
  base0F = "#d699b0", -- Deprecated
}

M.polish_hl = {
  treesitter = {
    ["@variable"] = { fg = M.base_30.white },
    ["@property"] = { fg = M.base_30.blue },
    ["@variable.builtin"] = { fg = M.base_30.red },
    ["@variable.parameter"] = { fg = M.base_30.blue },
    ["@keyword"] = { fg = M.base_30.red },
    ["@keyword.function"] = { fg = M.base_30.red },
    ["@keyword.return"] = { fg = M.base_30.red },
    ["@function"] = { fg = M.base_30.green },
    ["@function.builtin"] = { fg = M.base_30.orange },
    ["@function.macro"] = { fg = M.base_30.orange },
    ["@string"] = { fg = M.base_30.teal },
    ["@number"] = { fg = M.base_30.purple },
    ["@boolean"] = { fg = M.base_30.purple },
    ["@constant"] = { fg = M.base_30.purple },
    ["@type"] = { fg = M.base_30.yellow },
    ["@type.builtin"] = { fg = M.base_30.yellow },
    ["@comment"] = { fg = M.base_30.grey },
    ["@operator"] = { fg = M.base_30.orange },
    ["@punctuation.bracket"] = { fg = M.base_30.white },
  },
}

M.type = "dark"

return M
