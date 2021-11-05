function paralist_items (el)
  local para = el.content

  for i=1,#para do
    if para[i].text == '§' then
      para[i] = pandoc.RawInline('tex', '\\item')
      el.content = para
      return el
    elseif para[i].text == '§§' then
      para[i] = pandoc.RawInline('tex', '\\itemtitle{')
      table.insert(para, pandoc.RawInline('tex', '}'))
      para:remove(i+1)
      el.content = para
      return el
    end
  end
end

function tabto (el)
  local classes = el.classes or el.attr.classes

  if not classes:includes("tabto") then
    return nil
  else
    return pandoc.RawInline('tex', "\\tabto{" .. el.content[1].text .. "}")
  end
end

return {{Para = paralist_items}, {Span = tabto}}
