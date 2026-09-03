--[[
  figure-numbering.lua

### NOTE ###
# THIS IS HERE SO THE HTML CAN PICK UP HOW TO NUMBER THE FIGURES.
# The only thing worth editing is the prefix directly below - set it per
# appendix (A1, A2, B, ...) or leave it as "" for plain numbering.
# It can also be overridden from the qmd YAML with:
#   figure-number-prefix: "A2."
#
# Tables are NOT handled here. Quarto builds the table caption itself,
# after this filter runs, so the "Table A2" prefix is applied by the
# include-after-body script in the qmd instead.
--]]

-- Default for this appendix. YAML, if present, wins over this.
local fig_prefix = "A2."

-- Only run for HTML targets - PDF keeps its existing LaTeX-driven numbering.
if not FORMAT:match("html") then
  return {}
end

local flat_counter = 0
local metric = 0
local sub = 0
local grouped = false

local function Meta(meta)
  if meta["figure-number-prefix"] then
    fig_prefix = pandoc.utils.stringify(meta["figure-number-prefix"])
  end
  return meta
end

local function make_label()
  if grouped then
    sub = sub + 1
    return fig_prefix .. tostring(metric) .. "." .. tostring(sub)
  elseif metric > 0 then
    -- Flat figure after grouping has started (e.g. "Survey effort"): the
    -- qmd switches \thefigure back to a bare metric number for these.
    return fig_prefix .. tostring(metric)
  else
    flat_counter = flat_counter + 1
    return fig_prefix .. tostring(flat_counter)
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

-- Track the raw-LaTeX markers that currently drive PDF numbering. These
-- checks are independent (not elseif) because the qmd sometimes puts more
-- than one marker in the same {=latex} block (e.g. \stepcounter{metric}
-- next to a \renewcommand{\thefigure}{...}), and all of them need to apply.
local function RawBlock(el)
  if el.format == "tex" or el.format == "latex" then
    if el.text:find("setcounter{metric}", 1, true) then
      metric = flat_counter
    end
    if el.text:find("stepcounter{metric}", 1, true) then
      metric = metric + 1
      sub = 0
    end
    if el.text:find("arabic{metric}.\\arabic{figure}", 1, true) then
      grouped = true
    elseif el.text:find("arabic{metric}}", 1, true) then
      grouped = false
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
  { Meta = Meta },
  { RawBlock = RawBlock, Figure = Figure, Para = Para }
}
