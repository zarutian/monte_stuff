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
  local proxy2tab = require("ocap/weakmap").make()
  local mt = {}
  mt["__newindex"] = function (proxy, idx, val) 
    -- simply ignored
  end
  mt["__index"] = function (proxy, idx)
    return proxy2tab[proxy][idx]
  end
  local iface = {}
  iface["make"] = function (tab)
    local proxy = {}
    setmetatable(proxy, mt)
    proxy2tab[proxy] = tab
    return proxy
  end
  return iface
end

packages["ocap/throwingTable"] = function (require)
  local throwingTable = {}
  local mt = {}
  mt["__newindex"] = function (self, idx, val)
    error("tried to assign to the throwingTable")
  end
  mt["__index"] = function (self, idx)
    error("tried to index into the throwingTable")
  end
  setmetatable(throwingTable, mt)
  local iface = {}
  iface["make"] = function ()
    return throwingTable
  end
  return iface
end

packages["ocap/care-taker"] = function (require)
  local throwingTable = require("ocap/throwingTable").make()
  local proxy2tab =     require("ocap/weakmap").make()
  local mt = {}
  mt["__newindex"] = function (proxy, idx, val)
    proxy2tab[proxy][idx] = val
  end
  mt["__index"] = function (proxy, idx)
    return proxy2tab[proxy][idx]
  end
  local iface = {}
  iface["make"] = function (tab)
    local proxy = {}
    setmetatable(proxy, mt)
    proxy2tab[proxy] = tab
    local revoker = function ()
      proxy2tab[proxy] = throwingTable
    end
    return proxy, revoker
  end
  return iface
end

packages["ocap/revokable-membrane"] = function (require)
  local throwingTable = require("ocap/throwingTable").make()
  
  local iface = {}
  iface["make"] = function (tabl)
    local proxy2tab     = require("ocap/weakmap").make()
    local tab2proxy     = require("ocap/weakmap").makeWeakKV()
    
    local mt = {}
    
    local makeProxy = function (tab)
      if tab2proxy[tab] ~= nil then
        return tab2proxy[tab]
      end
      if type(tab) == "Number" then
        return tab
      elseif type(tab) == "String" then
        return tab
      end
      local proxy = {}
      setmetatable(proxy, mt)
      tab2proxy[tab] = proxy
      proxy2tab[proxy] = tab
      return proxy
    end
    local unwrap = function (proxy)
      if proxy2tab[proxy] ~= nil then
        return proxy2tab[proxy]
      else
        return proxy
      end
    end
    local revoked = false
    local revoker = function ()
      revoked = true
    end
  
    mt["__newindex"] = function (proxy, idx, val)
      if revoked == true then
        return nil
      end
      local real_idx = unwrap(idx)
      local real_val = unwrap(val)
      local tab      = unwrap(proxy)
      tab[real_idx] = real_val
      return nil
    end
    mt["__index"] = function (proxy, idx)
      if revoked == true then
        return throwingTable
      end
      local real_idx = unwrap(idx)
      local tab      = unwrap(proxy)
      return makeProxy(tab[real_idx])
    end
    
    return makeProxy(tabl), revoker
  end
  return iface
end
