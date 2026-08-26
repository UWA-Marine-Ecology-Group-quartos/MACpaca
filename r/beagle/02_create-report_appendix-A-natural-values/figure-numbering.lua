--[[
  figure-numbering.lua

### NOTE ###
# YOU DO NOT NEED TO EDIT THIS FILE. THIS IS HERE SO THE HTML CAN PICK UP HOW
# TO NUMBER THE FIGURES
--]]

-- Only run for HTML targets - PDF keeps its existing LaTeX-driven numbering.
if not FORMAT:match("html") then
  return {}
end

local flat_counter = 0
local metric = 0
local sub = 0
local grouped = false

local function make_label()
  if grouped then
    sub = sub + 1
    return tostring(metric) .. "." .. tostring(sub)
  else
    flat_counter = flat_counter + 1
    return tostring(flat_counter)
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

-- Track the raw-LaTeX markers that currently drive PDF numbering.
local function RawBlock(el)
  if el.format == "tex" or el.format == "latex" then
    if el.text:find("setcounter{metric}") then
      metric = flat_counter
    elseif el.text:find("stepcounter{metric}") then
      metric = metric + 1
      sub = 0
      grouped = true
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
