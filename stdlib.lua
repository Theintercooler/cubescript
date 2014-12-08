
local string = require "string"
local math = require "math"
local bit = require "bit"
local table = require "table"

local cubescript = require "cubescript"

local api = {}

api["local"] = function (env, scope, trace, ...)
    for k, var in pairs({...}) do
--         if env.debug and type(scope[var]) ~= "nil" then
--             p("Warning: local variable overrides global", var)
--         end
        rawset(scope, var, "")
    end
    return ""
end

api["push"] = function(env, scope, trace, var, value, callback)
    scope = cubescript.makeScope(scope)
    rawset(scope, var, value)
    return env:executeCallback(callback, scope, trace)
end

api["alias"] = function(env, scope, trace, name, value)
    scope[name] = value
    return value
end

api["do"] = function(env, scope, trace, callback)
    return env:executeCallback(callback, scope, trace)
end

api["doargs"] = api["do"]

function api.echo(env, scope, trace, ...)
    local args = {...}
    for k, v in pairs(args) do
        args[k] = tostring(v)
    end
    local v = table.concat(args, " ")
    print (v)
    return v
end

local function parseList(list)
    local lexer = cubescript.Lexer:new(list)
    local buffer = cubescript.Buffer:new(list)

    local v = {}
    local offset = 0
    local t = lexer:lexize(buffer)
    for k, token in pairs(t) do
        if token.type == cubescript.tokenType.string or token.type == cubescript.tokenType.number then
            v[k-offset] = token.value
        elseif token.type == cubescript.tokenType.word then
            v[k-offset] = table.concat(token.value, "")
        elseif token.type == cubescript.tokenType.seperator then
            offset = offset + 1
        elseif token.type == cubescript.tokenType.endOfBuffer then
            break
        else
            p(token, t)
            error "unkown token type in list"
        end
    end

    return v
end

function api.looplistn (env, scope, trace, nvar, ...)
    local vars = {}
    local list, callback
    local nvar2 = nvar

    for k, v in pairs({...}) do
        if nvar > 0 then
            nvar = nvar - 1
            vars[#vars+1] = v
        elseif list == nil then
            list = v
        elseif callback == nil then
            callback = v
        end
    end

    list = parseList(list)
    scope = cubescript.makeScope(scope)
    local v = {}
    for k, val in pairs(list) do
        v[#v+1] = val

        if #v >= nvar2 then
            for j, varname in ipairs(vars) do
                rawset(scope, varname, v[j])
            end
            env:executeCallback(callback, scope, trace)
            v = {}
        end
    end
    return ""
end

api["looplist" ] = function(env, scope, trace, var1,                    list, callback) return api.looplistn(env, scope, trace, 1, var1,                    list, callback) end
api["looplist2"] = function(env, scope, trace, var1, var2,              list, callback) return api.looplistn(env, scope, trace, 2, var1, var2,              list, callback) end
api["looplist3"] = function(env, scope, trace, var1, var2, var3,        list, callback) return api.looplistn(env, scope, trace, 3, var1, var2, var3,        list, callback) end
api["looplist4"] = function(env, scope, trace, var1, var2, var3, var4,  list, callback) return api.looplistn(env, scope, trace, 4, var1, var2, var3, var4,  list, callback) end

function api.looplistconcat (env, scope, trace, var, list, callback)
    list = parseList(list)
    scope = cubescript.makeScope(scope)
    local r = {}
    for k, value in pairs(list) do
        rawset(scope, var, value)
        table.insert(r, env:executeCallback(callback, scope, trace))
    end
    return table.concat(r, " ")
end

function api.at(env, scope, trace, list, i, ...)
    list = parseList(list)
    if type(list[i+1]) == "nil" then
        if env.debug then
            p("warning out of list AT", list, i)
        end
        return ""
    else
        local v = type(list[i+1]) == "table" and table.concat(list[i+1], " ") or list[i+1]
        if #{...} > 0 then
            return api.at(env, scope, trace, v, ...)
        else
            return v
        end
    end
end

function api.listlen(env, scope, trace, list)
    return #parseList(list)
end

function api.prettylist(env, scope, trace, list, sep)
    local ret
    for k, v in pairs(parseList(list)) do
        if ret then
            ret = ret .. (sep or ", ") .. v
        else
            ret = v .. ""
        end
    end
    return ret
end

local function loop(env, scope, trace, var, offset, step, length, body)
    local loopScope = cubescript.makeScope(scope)

    offset = env:toNumber(offset)
    step= env:toNumber(step)
    length= env:toNumber(length)

    for i=0,length-1 do
        rawset(loopScope, var, offset + i * step)
        env:executeCallback(body, loopScope, trace)
    end

    return ""
end

local function loopConcat(env, scope, trace, var, offset, step, length, body, seperator)
    local loopScope = cubescript.makeScope(scope)
    local ret = {}

    offset = env:toNumber(offset)
    step= env:toNumber(step)
    length= env:toNumber(length)

    for i=0,length-1 do
        rawset(loopScope, var, offset + i * step)
        ret[i+1] = env:executeCallback(body, loopScope, trace)
    end

    return table.concat(ret, seperator or " ")
end

api["loop"]             = function(env, scope, trace, var,                length, body)   return loop      (env, scope, trace, var, 0,            1,    length, body)      end
api["loop+"]            = function(env, scope, trace, var, offset,        length, body)   return loop      (env, scope, trace, var, offset,       1,    length, body)      end
api["loop*"]            = function(env, scope, trace, var, step,          length, body)   return loop      (env, scope, trace, var, 0,            step, length, body)      end
api["loop+*"]           = function(env, scope, trace, var, offset, step,  length, body)   return loop      (env, scope, trace, var, offset,       step, length, body)      end

api["loopconcat"]       = function(env, scope, trace, var,                length, body)   return loopConcat(env, scope, trace, var, 0,            1,    length, body)      end
api["loopconcat+"]      = function(env, scope, trace, var, offset,        length, body)   return loopConcat(env, scope, trace, var, offset,       1,    length, body)      end
api["loopconcat*"]      = function(env, scope, trace, var, step,          length, body)   return loopConcat(env, scope, trace, var, 0,            step, length, body)      end
api["loopconcat+*"]     = function(env, scope, trace, var, offset, step,  length, body)   return loopConcat(env, scope, trace, var, offset,       step, length, body)      end

api["loopconcatword"]   = function(env, scope, trace, var,                length, body)   return loopConcat(env, scope, trace, var, 0,            1,    length, body, "")  end
api["loopconcatword+"]  = function(env, scope, trace, var, offset,        length, body)   return loopConcat(env, scope, trace, var, offset,       1,    length, body, "")  end
api["loopconcatword*"]  = function(env, scope, trace, var, step,          length, body)   return loopConcat(env, scope, trace, var, 0,            step, length, body, "")  end
api["loopconcatword+*"] = function(env, scope, trace, var, offset, step,  length, body)   return loopConcat(env, scope, trace, var, offset,       step, length, body, "")  end

function api.concatword (env, scope, trace, ...)
    local args = {...}
    for k, v in pairs(args) do
        args[k] = tostring(v)
    end
    return table.concat(args, "")
end

function api.concat(env, scope, trace, ...)
    local args = {...}
    for k, v in pairs(args) do
        args[k] = tostring(v)
    end
    return table.concat(args, " ")
end

function api.format (env, scope, trace, format, ...)
    for i, arg in ipairs({...}) do
        format = format:gsub("%%"..i, arg)
    end
    return format
end

function api.strcmp(env,scope,trace,a,b)
    return env:toBool(tostring(a) == tostring(b))
end

api[">"]        = function (env, scope, trace, ...)
    local args = {...}
    assert(#args >= 2, "Not engough arguments provided for comperator.")
    for k, value in ipairs(args) do
        if k ~= #args then
            if not (env:toNumber(value) > env:toNumber(args[k+1])) then
                return env:toBool(false)
            end
        end
    end

    return env:toBool(true)
end
api[">f"] = api[">"]

api[">="]       = function (env, scope, trace, ...)
    local args = {...}
    assert(#args >= 2, "Not engough arguments provided for comperator.")
    for k, value in ipairs(args) do
        if k ~= #args then
            if not (env:toNumber(value) >= env:toNumber(args[k+1])) then
                return env:toBool(false)
            end
        end
    end
    return env:toBool(true)
end
api[">=f"] = api[">="]

api["<"]        = function (env, scope, trace, ...)
    local args = {...}
    assert(#args >= 2, "Not engough arguments provided for comperator.")
    for k, value in ipairs(args) do
        if k ~= #args then
            if not (env:toNumber(value) < env:toNumber(args[k+1])) then
                return env:toBool(false)
            end
        end
    end

    return env:toBool(true)
end
api["<f"] = api["<"]

api["<="]        = function (env, scope, trace, ...)
    local args = {...}
    assert(#args >= 2, "Not engough arguments provided for comperator.")
    for k, value in ipairs(args) do
        if k ~= #args then
            if not (env:toNumber(value) <= env:toNumber(args[k+1])) then
                return env:toBool(false)
            end
        end
    end
    
    return env:toBool(true)
end
api["<=f"] = api["<="]

api["="]        = function (env, scope, trace, ...)
    local args = {...}
    assert(#args >= 2, "Not engough arguments provided for comperator.")
    for k, value in ipairs(args) do
        if k ~= #args then
            if not (env:toNumber(value) == env:toNumber(args[k+1])) then
                return env:toBool(false)
            end
        end
    end
    
    return env:toBool(true)
end
api["=f"] = api["="]

api["!="]        = function (env, scope, trace, ...)
    local args = {...}
    assert(#args >= 2, "Not engough arguments provided for comperator.")
    for k, value in ipairs(args) do
        if k ~= #args then
            if not (env:toNumber(value) ~= env:toNumber(args[k+1])) then
                return env:toBool(false)
            end
        end
    end
    
    return env:toBool(true)
end
api["!=f"] = api["!="]

api["=s"]       = function(env, scope, trace, a, b)  return env:toBool(tostring(a) == tostring(b))      end
api["!=s"]      = function(env, scope, trace, a, b)  return env:toBool(tostring(a) ~= tostring(b))      end


api["divf"]     = function(env, scope, trace, a, b)
    a = env:toNumber(a)
    b = env:toNumber(b)
    assert(type(a) ~= "nil" and type(b) ~= "nil", "Passing non number to =f operator.")
    return a / b
end

api["div"]      = function(...)
    return math.floor(api["divf"](...) + 0.5)
end

api["!"]        = function(env, scope, trace, a)
    if env:isTrue(a) then
        return env:toBool(false)
    else
        return env:toBool(true)
    end
end

api["?"]        = function(env, scope, trace, a, b, c)
    if env:isTrue(a) then
        return b
    else
        return c or ""
    end
end

local function plus(env, scope, trace, v, ...)
    v = env:toNumber(v)
    for k, number in pairs({...}) do
        v = v + env:toNumber(number)
    end
    return v
end

api["+"]        = plus
api["+f"]       = plus

local function times(env, scope, trace, v, ...)
    v = env:toNumber(v)
    for k, number in pairs({...}) do
        v = v * env:toNumber(number)
    end
    return v
end

api["*"]        = times
api["*f"]       = times

local function minus(env, scope, trace, v, ...)
    v = env:toNumber(v)
    for k, number in pairs({...}) do
        v = v - env:toNumber(number)
    end
    return v
end

api["-"]        = minus
api["-f"]       = minus

local function mod(env, scope, trace, a, b)
    a = env:toNumber(a)
    b = env:toNumber(b)
    return a % b
end

api["mod"] = mod

api["<<"] = function(env, scope, trace, a, b, ...)
    assert(#({...}) == 0, "Operator << does only support 2 arguments.")
    a = env:toNumber(a)
    b = env:toNumber(b)
    return bit.lshift(a, b)
end

api[">>"] = function(env, scope, trace, a, b, ...)
    assert(#({...}) == 0, "Operator >> does only support 2 arguments.")
    a = env:toNumber(a)
    b = env:toNumber(b)
    return bit.rshift(a, b)
end

api["&"]       = function(env, scope, trace, a, b, ...)
    assert(#({...}) == 0, "Operator & does only support 2 arguments.")
    a = env:toNumber(a)
    b = env:toNumber(b)
    return bit.band(a, b)
end

api["&~"]       = function(env, scope, trace, a, b, ...)
    assert(#({...}) == 0, "Operator &~ does only support 2 arguments.")
    a = env:toNumber(a)
    b = env:toNumber(b)
    return bit.band(a, bit.bnot(b))
end

api["min"]      = function(env, scope, trace, ...)
    local args = {...}
    assert(#args > 0, "Operator min requires more than 0 arguments.")

    for k, v in pairs(args) do
        args[k] = env:toNumber(v)
    end

    return math.min(unpack(args))
end
api["minf"]     = api["min"]

api["max"]      = function(env, scope, trace, ...)
    local args = {...}
    assert(#args > 0, "Operator min requires more than 0 arguments.")

    for k, v in pairs(args) do
        args[k] = env:toNumber(v)
    end

    return math.max(unpack(args))
end
api["maxf"]     = api["max"]

api["abs"]      = function(env, scope, trace, a)
    return math.abs(env:toNumber(a))
end

api["||"]       = function(env, scope, trace, ...)
    local argMeta = rawget(scope, "#args")
    for k, cond in pairs({...}) do
        if argMeta[k].type == cubescript.tokenType.string then
            cond = env:executeCallback(cond, scope, trace)
        end

        if env:isTrue(cond) then
            return env:toBool(true)
        end
    end
    return env:toBool(false)
end

api["&&"]       = function(env, scope, trace, ...)
    local argMeta = rawget(scope, "#args")
    for k, cond in pairs({...}) do
        if argMeta[k].type == cubescript.tokenType.string then
            cond = env:executeCallback(cond, scope, trace)
        end

        if not env:isTrue(cond) then
            return env:toBool(false)
        end
    end
    return env:toBool(true)
end

api["if"]       = function(env, scope, trace, cond, yes, no)
    local argMeta = rawget(scope, "#args")
    if argMeta[1].type == cubescript.tokenType.string then
        cond = env:executeCallback(cond, scope, trace)
    end

    if env:isTrue(cond) then
        return env:executeCallback(yes, scope, trace)
    elseif no then
        return env:executeCallback(no, scope, trace)
    else
        return ""
    end
end

api["while"]       = function(env, scope, trace, cond, yes)
    local argMeta = rawget(scope, "#args")

    local function isTrue()
        return env:isTrue(cond)
    end

    if argMeta[1].type == cubescript.tokenType.string then
        function isTrue()
            return env:isTrue(env:executeCallback(cond, scope, trace))
        end
    end

    local maxLoop = 100
    local lastResult
    while isTrue() and maxLoop > 0 do
        maxLoop = maxLoop - 1
        lastResult = env:executeCallback(yes, scope, trace)
    end

    if maxLoop <= 0 then
        error("Loop exceeds maximum loopyness.")
    end

    return lastResult
end

api["result"] = function(env, scope, trace, value)
    return value
end

api["cond"] = function(env, scope, trace, ...)
    local argMeta = rawget(scope, "#args")
    local cond
    for k, arg in pairs({...}) do
        if type(cond) == "nil" then
            if argMeta[1].type == cubescript.tokenType.string then
                cond = env:executeCallback(arg, scope, trace) or "0"
            else
                cond = arg
            end
        else
            if env:isTrue(cond) then
                return env:executeCallback(arg, scope, trace)
            end

            cond = nil
        end
    end

    if cond then
        --Default value
        env:executeCallback(cond, scope, trace)
    end

    return ""
end

api["strstr"] = function(env, scope, trace, a, b)
    return (string.find(a, b, 1, true) or 0) - 1
end

local function escapeNeedle(str)
    return str:gsub("[%(%)%.%%%+%-%*%?%[%]%^%$]", function(c) return "%" .. c end)
end

api["strreplace"] = function(env, scope, trace, str, needle, replacement)
    return str:gsub(escapeNeedle(needle), replacement)
end

return {
    api = api
}
