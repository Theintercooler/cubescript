
local math = require "math"
local bit = require "bit"
local table = require "table"

local cubescript = require "cubescript"

local api = {}

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
    local t = lexer:lexize(buffer)
    for k, token in pairs(t) do
        if token.type == cubescript.tokenType.string or token.type == cubescript.tokenType.number then
            v[k-1] = token.value
        elseif token.type == cubescript.tokenType.word then
            v[k-1] = table.concat(token.value, "")
        else
            p(token)
            error "unkown token type in list"
        end
    end

    return v
end

function api.looplist (env, scope, trace, var, list, callback)
    list = parseList(list)
    scope = cubescript.makeScope(scope)
    for k, value in pairs(list) do
        rawset(scope, var, value)
        env:executeCallback(callback, scope, trace)
    end
    return ""
end

function api.at(env, scope, trace, list, i)
    list = parseList(list)
    if type(list[i]) == "nil" then
        if env.debug then
            p("warning out of list AT", list, i)
        end
        return ""
    else
        return list[i]
    end
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

    for i=0,length-1 do
        rawset(loopScope, var, offset + i * step)
        env:executeCallback(body, loopScope, trace)
    end

    return ""
end

local function loopConcat(env, scope, trace, var, offset, step, length, body, seperator)
    local loopScope = cubescript.makeScope(scope)
    local ret = {}
    
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

function api.format (env, scope, trace, format, ...)
    for i, arg in ipairs({...}) do
        format = format:gsub("%%"..i, arg)
    end
    return format
end

api[">"]        = function (env, scope, trace, a, b) return tonumber(a) >       tonumber(b) end
api[">="]       = function (env, scope, trace, a, b) return tonumber(a) >=      tonumber(b) end
api["<"]        = function (env, scope, trace, a, b) return tonumber(a) <       tonumber(b) end
api["<="]       = function (env, scope, trace, a, b) return tonumber(a) <=      tonumber(b) end

api["="]        = function(env, scope, trace, a, b)  return a == b                          end
api["=s"]       = function(env, scope, trace, a, b)  return tostring(a) == tostring(b)      end

api["=f"]       = function(env, scope, trace, a, b)
    a = tonumber(a)
    b = tonumber(b)
    assert(type(a) ~= "nil" and type(b) ~= "nil", "Passing non number to =f operator.")
    return a == b
end

api["divf"]     = function(env, scope, trace, a, b)
    a = tonumber(a)
    b = tonumber(b)
    assert(type(a) ~= "nil" and type(b) ~= "nil", "Passing non number to =f operator.")
    return a / b
end

api["!"]        = function(env, scope, trace, a)
    if a and a ~= "" and a ~= "0" then
        return "0"
    else
        return "1"
    end
end

api["?"]        = function(env, scope, trace, a, b, c)
    if a and a ~= "" and a ~= "0" then
        return b
    else
        return c or ""
    end
end

api["+"]        = function(env, scope, trace, ...)
    local v = 0
    for k, number in pairs({...}) do
        number = tonumber(number)
        if type(number) == "nil" then
            p(...)
            error("Arg #"..tostring(k).." is not a number: "..tostring(({...})[k]))
        end
        v = v + number
    end
    return v
end

local function times(env, scope, trace, ...)
    local v = 0
    for k, number in pairs({...}) do
        number = tonumber(number)
        if type(number) == "nil" then
            p(...)
            error("Arg #"..tostring(k).." is not a number: "..tostring(({...})[k]))
        end
        v = v * number
    end
    return v
end

api["*"]        = times

local function minus(env, scope, trace, ...)
    local v = 0
    for k, number in pairs({...}) do
        number = tonumber(number)
        if type(number) == "nil" then
            p(...)
            error("Arg #"..tostring(k).." is not a number: "..tostring(({...})[k]))
        end
        v = v - number
    end
    return v
end

api["-"]        = minus
api["-f"]       = minus

api["&~"]       = function(env, scope, trace, a, b, ...)
    assert(#({...}) == 0, "Operator &~ does only support 2 arguments.")
    a = tonumber(a)
    b = tonumber(b)
    assert(type(a) ~= "nil" and type(b) ~= "nil", "Passing non number to &~ operator.")
    return bit.band(a, bit.bnot(b))
end

api["min"]      = function(env, scope, trace, ...)
    return math.min(...)
end

api["||"]       = function(env, scope, trace, ...)
    for k, cond in pairs({...}) do
        if cond and cond ~= "" and cond ~= "0" then
            return "1"
        end
    end
    return "0"
end

api["if"]       = function(env, scope, trace, cond, yes, no)
    if cond and cond ~= "" and cond ~= "0" then
        return env:executeCallback(yes, scope, trace)
    elseif no then
        return env:executeCallback(no, scope, trace)
    else
        return ""
    end
end

api["result"] = function(env, scope, trace, value)
    return value
end

return {
    api = api
}