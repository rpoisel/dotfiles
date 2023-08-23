local awful = require("awful")
local lyaml = require("lyaml")

local util = {}

function util.read_layout_cfg()
    local status, cfgfile = pcall(io.open,
        os.getenv("HOME") .. "/.config/awesome/config.yaml", "r")
    if not status or not cfgfile then
        error("Could not open config file: " .. tostring(cfgfile))
    end
    local content = cfgfile:read("*all")
    cfgfile:close()
    return lyaml.load(content)
end

function util.arrange_clients(cfg, matcher)
    local screen = awful.screen.focused()
    for _, c in ipairs(client.get()) do
        local found = false
        for tagnum = 1, 9 do
            -- careful: the tagnum may not exist in config.yaml
            local cfgtag = cfg["tags"][tagnum] or cfg["tags"][tostring(tagnum)]
            local tag = screen.tags[tagnum]
            for _, id in ipairs(cfgtag["clients"]) do
                if matcher(c, id) then
                    found = true
                    c:move_to_tag(tag)
                end
            end
        end
        if not found then c:move_to_tag(screen.tags[8]) end
    end
end

function util.arrange_clients_from_layout_config()
    local status, result = pcall(util.read_layout_cfg)
    if not status then
        error("Could not read layout config: " .. tostring(result))
    end

    util.arrange_clients(result, function(c, id) return string.match(c.name, id) end)
end

return util
