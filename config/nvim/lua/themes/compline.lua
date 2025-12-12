local M = {}

M.base_30 = {
  white = "#e0dcd4",
  darker_black = "#16191d",
  black = "#1a1d21", --  nvim bg
  black2 = "#1f2226",
  one_bg = "#24272b",
  one_bg2 = "#2e3135",
  one_bg3 = "#383b3f",
  grey = "#515761",
  grey_fg = "#5b6169",
  grey_fg2 = "#656971",
  light_grey = "#6f7379",
  red = "#CDACAC",
  baby_pink = "#c8beb8",
  pink = "#c0b8bc",
  line = "#24272b", -- for lines like vertsplit
  green = "#b8c4b8",
  vibrant_green = "#b4beb4",
  nord_blue = "#b4bcc4",
  blue = "#b4bcc4",
  yellow = "#DBCDB3",
  sun = "#ccc4b0",
  purple = "#c0b8bc",
  dark_purple = "#c4beb8",
  teal = "#b0bcc8",
  orange = "#DBCDB3",
  cyan = "#b0c0b8",
  statusline_bg = "#1f2226",
  lightbg = "#2e3135",
  pmenu_bg = "#b8c4b8",
  folder_bg = "#b4bcc4",
}

M.base_16 = {
  base00 = "#1a1d21",
  base01 = "#24272b",
  base02 = "#2e3135",
  base03 = "#515761",
  base04 = "#c0bdb8",
  base05 = "#e0dcd4",
  base06 = "#e0dcd4",
  base07 = "#e0dcd4",
  base08 = "#CDACAC",
  base09 = "#DBCDB3",
  base0A = "#DBCDB3",
  base0B = "#b8c4b8",
  base0C = "#b0bcc8",
  base0D = "#b4bcc4",
  base0E = "#c0b8bc",
  base0F = "#c8beb8",
}

M.type = "dark"

M.polish_hl = {
  ["@comment"] = { fg = M.base_30.grey, italic = true },
}

return M
