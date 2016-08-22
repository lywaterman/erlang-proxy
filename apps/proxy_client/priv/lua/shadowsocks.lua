function _G.print_r (t, name, indent)
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


crypto=require 'crypto'

local bxor, band, schar = (bit or bit32).bxor, (bit or bit32).band, string.char

function random_string(length)
	local buffer = {}
	for i = 1, length do buffer[i] = math.random(0, 255) end
	return schar(unpack(buffer))
end

function evp_bytestokey(password, key_len, iv_len)
	local key = string.format("%s-%d-%d", password, key_len, iv_len)
	local m, i = {}, 0
	while #(table.concat(m)) < key_len + iv_len do
		local data = password
		if i > 0 then data = m[i] .. password end
		m[#m + 1], i = crypto.digest("md5", data, true), i + 1
	end
	local ms = table.concat(m)
	local key = ms:sub(1, key_len)
	local iv = ms:sub(key_len + 1, iv_len)
	return key, iv
end

function newRC4MD5Stream(cipher, key, iv, enc)
    local wtf = crypto.encrypt
    local md5 = crypto.digest.new "md5"
    md5:update(key)
    md5:update(iv)
    return wtf.new("rc4", md5:final(nil, true), "")
end

function newAESStream(cipher, key, iv, enc)
    local wtf = enc and crypto.encrypt or crypto.decrypt
    return wtf.new(cipher, key, iv)
end

cipherMethod = {
    ["aes-128-cfb"] = {16, 16, newAESStream},
    ["aes-192-cfb"] = {24, 16, newAESStream},
    ["aes-256-cfb"] = {32, 16, newAESStream},
    --["des-cfb"]     = {8, 8, newDESStream},
    --["bf-cfb"]      = {16, 8, newBlowFishStream},
    --["cast5-cfb"]   = {16, 8, newCast5Stream},
    ["rc4-md5"]     = {16, 16, newRC4MD5Stream},
    --["chacha20"]    = {32, 8, newChaCha20Stream},
    --["salsa20"]     = {32, 8, newSalsa20Stream},
}

function create_cipher(cipher, password, enc, iv)
    local method = cipherMethod[cipher] 
    local keyLen = method[1]
    local ivLen = method[2]
    local key_ = evp_bytestokey(password, keyLen, ivLen)
    local cipher_iv = iv and iv or random_string(ivLen)

    return method[3](cipher, key_, cipher_iv, enc), cipher_iv
end

--cipher:update("12345")
--decipher=nil

function get_first_packet(ip, port)
    local a, b, c, d = ip:match "^(%d+)%.(%d+)%.(%d+)%.(%d+)$"
    a, b, c, d = tonumber(a), tonumber(b), tonumber(c), tonumber(d)
    return cipher_iv .. cipher:update(schar(1, a, b, c, d, band(port, 0xFF00) / 0x100, band(port, 0xFF)))
end

global_worker = {}

function worker_init(pid)
    local cipher, cipher_iv = create_cipher('aes-256-cfb', 'Mima4123', true)
    global_worker[pid] = {
        send_iv=false, 
        cipher = cipher,
        iv = cipher_iv,
        decipher = nil,
        create_decipher = function (self, iv)
            self.decipher = create_cipher('aes-256-cfb', 'Mima4123', false, iv)
        end
    }
end

function worker_terminate(pid)
    global_worker[pid] = nil
end


function send_data(pid, ssaddr, chunk)
    local worker = global_worker[pid]

    if worker == nil then
        assert(false) 
    end

    local cipherdata = worker.cipher:update(ssaddr .. chunk)
    --print('before')
    --print(ssaddr..chunk)
    if #cipherdata > 0 then
        if worker.send_iv then
            return cipherdata
        else
            --local tt = create_cipher('aes-256-cfb', 'Mima4123', false, worker.iv)
            --print('after')
            --print(tt:update(cipherdata))
            worker.send_iv = true
            return worker.iv .. cipherdata
        end
    end
end

function recv_data(pid, chunk)
    local worker = global_worker[pid]
    if worker == nil then
        assert(false) 
    end

    if worker.decipher then
        chunk = worker.decipher:update(chunk)
        if #chunk > 0 then return chunk end 
    else
        if #chunk >= 16 then
            worker:create_decipher(chunk:sub(1, 16))
            if #chunk > 16 then
                chunk = worker.decipher:update(chunk:sub(17, -1))
                if #chunk > 0 then return chunk end
            end
        end
    end
end

--decipher = create_cipher('aes-256-cfb', 'Mima4123', false, cipher_iv)
--
--print(chunk)
--print(decipher:update(chunk))


