local value=redis.call("get", KEYS[1]) or 0
local increment=tonumber(ARGV[1])
local limit=tonumber(ARGV[2])

if (value + increment) > limit then value = redis.call("incrby", KEYS[1], limit - value)
else value = redis.call("incrby", KEYS[1], increment) end
return value