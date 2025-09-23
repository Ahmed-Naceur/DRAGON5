-- Pandoc Lua filter to convert LaTeX references to MkDocs format

function Link(link)
    -- Convert LaTeX-style references like [1](#fig:multi){reference-type="ref" reference="fig:multi"}
    -- to simple markdown links like [Figure 1](#fig:multi)
    
    if link.attributes and link.attributes["reference-type"] == "ref" then
        local ref = link.attributes["reference"]
        if ref then
            -- Extract the type and label from reference like "fig:multi"
            local ref_type, label = ref:match("([^:]+):(.+)")
            
            if ref_type == "fig" then
                return pandoc.Link("Figure " .. pandoc.utils.stringify(link.content), "#" .. ref)
            elseif ref_type == "tab" then
                return pandoc.Link("Table " .. pandoc.utils.stringify(link.content), "#" .. ref)
            elseif ref_type == "eq" then
                return pandoc.Link("Equation " .. pandoc.utils.stringify(link.content), "#" .. ref)
            elseif ref_type == "sec" then
                return pandoc.Link("Section " .. pandoc.utils.stringify(link.content), "#" .. ref)
            else
                -- Generic reference
                return pandoc.Link(pandoc.utils.stringify(link.content), "#" .. ref)
            end
        end
    end
    
    return link
end

-- Optional: Clean up figure attributes to keep only the id
function Image(img)
    if img.identifier then
        -- Keep only the identifier, remove other attributes
        img.attributes = {}
    end
    return img
end

