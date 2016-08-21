DEBUG = true 
local print = _G.print
_G.print = function( ... )
    local var = ""
    for k, v in ipairs({...}) do
        var = var .. tostring(v)
    end

    var = string.gsub( var, '\n', '\r' )
    var = var .. '\r'

    print( var )
end

global_next = global_next or {}

function update_return(N)
   return {erlang.atom('update'), N}
end

function insert_return(N)
   return {erlang.atom('insert'), 0, N}
end

function select_return(N)
   return {erlang.atom('select'), N}
end

function delete_return(N)
   return {erlang.atom('delete'), N}
end


math.randomseed(tostring(os.time()):reverse():sub(1, 6))

function tt(t) 
   return t
end

--设置工作路径
function set_lua_path(path)
   	_G.print(path)
    package.path = [[/root/.luarocks/share/lua/5.1/?.lua;/root/.luarocks/share/lua/5.1/?/init.lua;/root/torch/install/share/lua/5.1/?.lua;/root/torch/install/share/lua/5.1/?/init.lua;./?.lua;/root/torch/install/share/luajit-2.1.0-beta1/?.lua;/usr/local/share/lua/5.1/?.lua;/usr/local/share/lua/5.1/?/init.lua;/usr/local/lib/lua/5.1/?.lua;/usr/local/lib/lua/5.1/?/init.lua]]
    package.cpath = [[/root/torch/install/lib/?.so;/root/.luarocks/lib/lua/5.1/?.so;/root/torch/install/lib/lua/5.1/?.so;/root/torch/install/lib/?.so;./?.so;/usr/local/lib/lua/5.1/?.so;/usr/local/lib/lua/5.1/loadall.so]]

    set_luapath(string.sub(path, 1, -6))
    local game_path = string.sub(path, 1, -6) .. '/?.lua'
    local game_cpath = string.sub(path, 1, -6) .. '/?.so'
    _G.print(game_path)
	package.path = package.path .. ';' .. path .. ';' .. game_path

    package.cpath = package.cpath .. ';' .. game_cpath

	_G.print(package.path)

end

function set_luapath(path)

   luapath = path

   game_luapath = path .. "game/"

end

function set_appenv(env, nodename)

	appenv = env

    dbhost = env.dbhost
    dbport = env.dbport

    global_node_name = nodename

	_G.print("appenv")
	_G.print(appenv)

	for k, v in pairs(appenv) do

		_G.print(k)
		_G.print(v)
	end	
end	

function reload()
end

function _G.print_r (t, name, indent)
  do
    --return
  end
  local tableList = {}
  function table_r (t, name, indent, full)
    local serial=string.len(full) == 0 and name
    or type(name)~="number" and '["'..tostring(name)..'"]' or '['..name..']'
    io.write(indent,serial,' = ')
    if type(t) == "table" then
      if tableList[t] ~= nil then io.write('{}; -- ',tableList[t],' (self reference)\n\r')
      else
        tableList[t]=full..serial
        if next(t) then -- Table not empty
          io.write('{\n\r')
          for key,value in pairs(t) do table_r(value,key,indent..'\t',full..serial) end
          io.write(indent,'};\n\r')
        else io.write('{};\n\r') end
      end
    else io.write(type(t)~="number" and type(t)~="boolean" and '"'..tostring(t)..'"'
      or tostring(t),';\n\r') end
    end
    table_r(t,name or '__unnamed__',indent or '','')
  end

