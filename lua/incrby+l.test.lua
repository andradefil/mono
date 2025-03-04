local call_redis_script = require "./harness";

describe("incrby+l test", function()

  it("should incrby with limit", function()
    -- Call the incr-and-stor.lua script with the given keys and values
    local result = call_redis_script('incrby+l.lua',  { "key" }, { 10, 20 });
  end)

 end)