-- This file is put here for now.

local packages = {}

packages["ocap/weakmap"] = function (require)
  local iface = {}
  local mt = {}
  mt["__mode"] = "k"
  iface["make"] = function ()
    local ins = {}
    setmetatable(ins, mt)
    return ins
  end
  return iface
end

packages["ocap/shallow-read-only"] = function (require)
  local proxy2tab = {}
  local mt = {}
  mt["__newindex"] = function (proxy, idx, val) 
    -- simply ignored
  end
  mt["__index"] = function (proxy, idx)
    return proxy2tab[proxy][idx]
  end
end
