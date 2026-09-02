--[[
  appendix-c-numbering.lua

### NOTE ###
# YOU DO NOT NEED TO EDIT THIS FILE. THIS IS HERE SO THE HTML CAN PICK UP HOW
# TO NUMBER THE FIGURES
#
# Appendix C numbers its figures flat as "C1.<n>" across the whole appendix,
# and the two habitat response-curve figures as "C1.<parent>.<n>". All of
# that is driven by raw LaTeX counters in 04_quarto.qmd, which pandoc
# discards for HTML. This filter watches the same raw-LaTeX markers and
# rebuilds the equivalent labels in the HTML captions.
--]]

-- Only run for HTML targets - PDF keeps its existing LaTeX-driven numbering.
if not FORMAT:match("html") then
  return {}
end

local fig       = 0      -- figures in the document (never resets)
local parent    = 0      -- number the split figure would have taken
local sub_mode  = false  -- inside the C1.y.z block?

local function make_label()
  fig = fig + 1
  if sub_mode then
    return "C1." .. tostring(parent) .. "." .. tostring(fig)
  else
    return "C1." .. tostring(fig)
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

-- Track the raw-LaTeX markers that drive PDF numbering. `find(..., 1, true)`
-- is a plain substring search, so the LaTeX backslashes and braces do not need
-- escaping as Lua patterns.
local function RawBlock(el)
  if el.format == "tex" or el.format == "latex" then
    if el.text:find("value{parentfig}", 1, true) then
      -- closing block: hand the count back to the parent figure
      fig      = parent
      sub_mode = false
    elseif el.text:find("setcounter{parentfig}", 1, true) then
      -- opening block: stash the number this figure would have taken, then
      -- start counting sub-figures from zero
      parent   = fig + 1
      fig      = 0
      sub_mode = true
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
  { RawBlock = RawBlock, Figure = Figure, Para = Para }
}
