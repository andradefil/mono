redis = {}
KEYS = {}
ARGV = {}
redis.call = function(cmd, ...)
  print("Hello")
  return 2
end 

function call_redis_script(script, keys, argv)
  for k,v in pairs(keys) do table.insert(KEYS, v) end
  for k,v in pairs(argv) do table.insert(ARGV, v)  end

  return assert(dofile(script))
end

return call_redis_script;