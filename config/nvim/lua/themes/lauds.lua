local M = {}

M.base_30 = {
  white = "#1a1d21",
  darker_black = "#e5e3e0",
  black = "#f0efeb", --  nvim bg
  black2 = "#efeeed",
  one_bg = "#e5e3e0",
  one_bg2 = "#d8d6d3",
  one_bg3 = "#b8b5b0",
  grey = "#9a9791",
  grey_fg = "#7d7a75",
  grey_fg2 = "#5f5c58",
  light_grey = "#4A4D51",
  red = "#8B6666",
  baby_pink = "#8B7E52",
  pink = "#7A6D5A",
  line = "#e5e3e0", -- for lines like vertsplit
  green = "#5A6B5A",
  vibrant_green = "#4D6B6B",
  nord_blue = "#5A6B7A",
  blue = "#5A6B7A",
  yellow = "#8B7E52",
  sun = "#7A6D5A",
  purple = "#546470",
  dark_purple = "#64757d",
  teal = "#4D6B6B",
  orange = "#7A6D5A",
  cyan = "#64757d",
  statusline_bg = "#efeeed",
  lightbg = "#d8d6d3",
  pmenu_bg = "#5A6B5A",
  folder_bg = "#5A6B7A",
}

M.base_16 = {
  base00 = "#f0efeb",
  base01 = "#e5e3e0",
  base02 = "#d8d6d3",
  base03 = "#9a9791",
  base04 = "#5f5c58",
  base05 = "#1a1d21",
  base06 = "#2d2a27",
  base07 = "#1a1d21",
  base08 = "#8B6666",
  base09 = "#7A6D5A",
  base0A = "#8B7E52",
  base0B = "#5A6B5A",
  base0C = "#64757d",
  base0D = "#5A6B7A",
  base0E = "#546470",
  base0F = "#8B7E52",
}

M.type = "light"

M.polish_hl = {
  ["@comment"] = { fg = M.base_30.grey_fg2, italic = true },
  Keyword = { fg = M.base_16.base0D, bold = true },
  Operator = { fg = M.base_16.base0B, bold = true },
  Type = { fg = M.base_16.base0D, bold = true },
}

return M
