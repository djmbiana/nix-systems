-- Gruvbox Material Black theme for NvChad
-- Save this as: lua/themes/gruvbox-material-black.lua

local M = {}

M.base_30 = {
  white = "#d4be98",
  darker_black = "#0a0908",
  black = "#141311", -- Main background
  black2 = "#1a1816",
  one_bg = "#1d1b19",
  one_bg2 = "#262321",
  one_bg3 = "#2e2b29",
  grey = "#7c6f64",
  grey_fg = "#928374",
  grey_fg2 = "#a89984",
  light_grey = "#bdae93",
  red = "#ea6962",
  baby_pink = "#d3869b",
  pink = "#d3869b",
  line = "#1d1b19", -- for lines like vertsplit
  green = "#a9b665",
  vibrant_green = "#b8bb26",
  nord_blue = "#7daea3",
  blue = "#7daea3",
  yellow = "#d8a657",
  sun = "#fabd2f",
  purple = "#d3869b",
  dark_purple = "#b16286",
  teal = "#89b482",
  orange = "#e78a4e",
  cyan = "#89b482",
  statusline_bg = "#1a1816",
  lightbg = "#262321",
  pmenu_bg = "#a9b665",
  folder_bg = "#7daea3",
}

M.base_16 = {
  base00 = "#141311", -- Default Background
  base01 = "#1a1816", -- Lighter Background (status line, etc.)
  base02 = "#262321", -- Selection Background
  base03 = "#3c3836", -- Comments, Invisibles
  base04 = "#665c54", -- Dark Foreground (status line)
  base05 = "#d4be98", -- Default Foreground
  base06 = "#ddc7a1", -- Light Foreground
  base07 = "#ebdbb2", -- Lightest Foreground
  base08 = "#ea6962", -- Variables, Tags
  base09 = "#e78a4e", -- Integers, Constants
  base0A = "#d8a657", -- Classes, Search
  base0B = "#a9b665", -- Strings
  base0C = "#89b482", -- Support, Regex
  base0D = "#7daea3", -- Functions, Methods
  base0E = "#d3869b", -- Keywords, Storage
  base0F = "#d65d0e", -- Deprecated
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
