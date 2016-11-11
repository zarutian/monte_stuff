-- This file is put here for now.

local packages = []

packages["ocap/shallow-read-only"] = function (require)
  local weakmap = require("ocap/weakmap")
  local proxy2tab = weakmap.make()
  local mt = []
  mt["__newindex"] = function (proxy, idx, val) 
    -- simply ignored
  end
  mt["__index"] = function (
end
