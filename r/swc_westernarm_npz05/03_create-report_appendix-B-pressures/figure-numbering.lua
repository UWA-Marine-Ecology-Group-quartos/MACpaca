--[[
  figure-numbering.lua

### NOTE ###
# YOU DO NOT NEED TO EDIT THIS FILE. THIS IS HERE SO THE HTML CAN PICK UP HOW
# TO NUMBER THE FIGURES.
#
# The PDF numbering is driven by LaTeX (the `metric` counter + \thefigure).
# This filter mirrors that logic for HTML so both formats agree.
#
# To add a letter prefix (e.g. Appendix B -> B1, B5.1) set this in the YAML
# header of the .qmd:
#
#   figure-number-prefix: "B"
#
# Leave it out (as Appendix A does) for plain 1, 2, 5.1 numbering.
--]]

-- Only run for HTML targets - PDF keeps its existing LaTeX-driven numbering.
if not FORMAT:match("html") then
  return {}
end

local prefix = ""     -- letter prefix, e.g. "B"
local fig    = 0      -- mirrors the LaTeX `figure` counter
local metric = 0      -- mirrors the LaTeX `metric` counter
local mode   = "flat" -- "flat" = <fig>, "sub" = <metric>.<fig>, "metric" = <metric>

-- Read figure-number-prefix from the document metadata.
local function get_meta(meta)
  if meta["figure-number-prefix"] ~= nil then
    prefix = pandoc.utils.stringify(meta["figure-number-prefix"])
  end
end

local function make_label()
  -- \stepcounter{figure} happens for every figure, whatever \thefigure prints.
  fig = fig + 1
  if mode == "sub" then
    return prefix .. tostring(metric) .. "." .. tostring(fig)
  elseif mode == "metric" then
    return prefix .. tostring(metric)
  else
    return prefix .. tostring(fig)
  end
end

local function prepend_number(inlines)
  local label = make_label()
  local out = pandoc.List()
  out:insert(pandoc.Str("Figure " .. label .. ":"))
  out:insert(pandoc.Space())
  for _, el in ipairs(inlines) do out:insert(el) end
  return out
end

-- Track the raw-LaTeX markers that drive PDF numbering.
local function RawBlock(el)
  if el.format ~= "tex" and el.format ~= "latex" then
    return el
  end
  local t = el.text

  -- \setcounter{metric}{...}  (does NOT reset the figure counter)
  if t:find("setcounter{metric}", 1, true) then
    local set = t:match("setcounter{metric}{(.-)}") or ""
    if set:find("value", 1, true) then
      metric = fig                      -- \setcounter{metric}{\value{figure}}
    else
      metric = tonumber(set) or metric  -- \setcounter{metric}{4}
    end
  end

  -- \stepcounter{metric}  (resets figure, via \counterwithin*{figure}{metric})
  if t:find("stepcounter{metric}", 1, true) then
    metric = metric + 1
    fig = 0
  end

  -- \renewcommand{\thefigure}{...} - work out which display format is now live.
  local i = t:find("thefigure}", 1, true)
  if i then
    local rest        = t:sub(i)
    local has_metric  = rest:find("arabic{metric}", 1, true) ~= nil
    local has_figure  = rest:find("arabic{figure}", 1, true) ~= nil
    if has_metric and has_figure then
      mode = "sub"
    elseif has_metric then
      mode = "metric"
    elseif has_figure then
      mode = "flat"
    end
  end

  return el
end

-- Modern pandoc AST: native Figure block with a Caption.
local function Figure(el)
  if el.caption and el.caption.long and #el.caption.long > 0 then
    local block = el.caption.long[1]
    if block and block.content then
      block.content = prepend_number(block.content)
    end
  end
  return el
end

-- Fallback for older pandoc AST: Para containing a single captioned Image.
local function Para(el)
  if #el.content == 1 and el.content[1].t == "Image" then
    local img = el.content[1]
    if img.caption and #img.caption > 0 then
      img.caption = prepend_number(img.caption)
    end
  end
  return el
end

return {
  { Meta = get_meta },
  { RawBlock = RawBlock, Figure = Figure, Para = Para }
}
