
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
    local offset = 1
    local t = lexer:lexize(buffer)
    for k, token in pairs(t) do
        if token.type == cubescript.tokenType.string or token.type == cubescript.tokenType.number then
            v[k-offset] = token.value
        elseif token.type == cubescript.tokenType.word then
            v[k-offset] = table.concat(token.value, "")
        elseif token.type == cubescript.tokenType.seperator then
            offset = offset + 1
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

api[">"]        = function (env, scope, trace, ...)
    local args = {...}
    assert(#args >= 2, "Not engough arguments provided for comperator.")
    for k, value in ipairs(args) do
        if k ~= #args then
            if not (tonumber(value) > tonumber(args[k+1])) then
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
            if not (tonumber(value) >= tonumber(args[k+1])) then
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
            if not (tonumber(value) < tonumber(args[k+1])) then
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
            if not (tonumber(value) <= tonumber(args[k+1])) then
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
            if not (tonumber(value) == tonumber(args[k+1])) then
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
            if not (tonumber(value) ~= tonumber(args[k+1])) then
                return env:toBool(false)
            end
        end
    end
    
    return env:toBool(true)
end
api["!=f"] = api["!="]

api["=s"]       = function(env, scope, trace, a, b)  return env:toBool(tostring(a) == tostring(b))      end



api["divf"]     = function(env, scope, trace, a, b)
    a = tonumber(a)
    b = tonumber(b)
    assert(type(a) ~= "nil" and type(b) ~= "nil", "Passing non number to =f operator.")
    return a / b
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

local function plus(env, scope, trace, ...)
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

api["+"]        = plus
api["+f"]       = plus

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
api["*f"]       = times

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

local function mod(env, scope, trace, a, b)
    return a % b
end

api["mod"] = mod

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

api["max"]      = function(env, scope, trace, ...)
    return math.max(...)
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
        p("default", cond)
        env:executeCallback(cond, scope, trace)
    end

    return ""
end

return {
    api = api
}