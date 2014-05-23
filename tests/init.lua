local traceback = require "debug".traceback
local string = require "string"
local math = require "math"
local os = require "os"
local ffi = require "ffi"
local cubescript = require "cubescript"

local padName = 20
local function pad(name, i)
    name = tostring(name)
    local pad = (i or padName)-#name
    pad = math.max(0, pad)
    if not i then
        padName = math.max(padName, pad)
    end
    return name .. string.rep(" ", pad)
end

local total = 0
local fails = 0

local function assertEquals(n, args)
    local a
    local argc = 0
    local failed = false
    local function fail()
        if not failed then
            failed = true
            fails = fails + 1
        end
    end
    for k, value in pairs(args) do
        if argc == 0 then
            a = value
        elseif value ~= a then
            fail()
            print (("[✖ %s] %s (%s) does not equal %s (%s)"):format(pad(k), a, type(a), value, type(value)))
        else
            local k = tostring(a)
            local v = tostring(value)
            
            k = k:gsub("\n", "\\n")
            k = k:gsub("\f", "\\f")
            k = k:gsub("\r", "\\r")
            
            v = v:gsub("\n", "\\n")
            v = v:gsub("\f", "\\f")
            v = v:gsub("\r", "\\r")

            if k:len() > 15 then
                k = k:sub(1, 12).."..."
            end

            k = pad(k, 15)

            if v:len() > 15 then
                v = v:sub(1, 12).."..."
            end

            v = pad(v, 15)

            print (("[✓ %s] %s (%s) does equal %s (%s)"):format(pad(k), k, pad(type(a), 6), v, pad(type(value), 6)))
        end
        argc = argc + 1
    end
    if n ~= argc then
        fail()
        print(("[✖ %s] Some arguments are missing (%i ~= %i)"):format(pad("unkown"), n, argc))
    elseif n == 0 then
        print(("[✓ %s] No arguments were given, none were expected."):format(pad("unkown")))
    end
    total = total + 1
end

local tests = {}

function tests:buffer()
    local buf = "10"

    for k, b in pairs({
        cubescript.Buffer:new (buf),
        cubescript.Buffer:new (ffi.new("char [?]", #buf+1, buf)),
    }) do
    assertEquals(2, { ["0"] = 0, ["buffer.i"] = b.i})
    assertEquals(2, { ["char <0> "] = string.byte("1"), peekChar = b:peekChar()})       --i = 0
    assertEquals(2, { ["0"] = 0, ["buffer.i"] = b.i})
    assertEquals(2, { ["char <0> "] = string.byte("1"), getChar = b:getChar()})         --i = 0 -> i = 1
    assertEquals(2, { ["1"] = 1, ["buffer.i"] = b.i})
    assertEquals(2, { ["char <1> "] = string.byte("0"), peekChar = b:peekChar()})       --i = 1
    assertEquals(2, { ["1"] = 1, ["buffer.i"] = b.i})
    b:undoChar()                                                                        --i = 1 -> i = 0
    assertEquals(2, { ["0"] = 0, ["buffer.i"] = b.i})
    assertEquals(2, { ["char <0> "] = string.byte("1"), peekChar = b:peekChar()})       --i = 0
    assertEquals(2, { ["0"] = 0, ["buffer.i"] = b.i})
    b:skipChar()                                                                        --i = 0 -> i = 1
    assertEquals(2, { ["1"] = 1, ["buffer.i"] = b.i})
    assertEquals(2, { ["char <1> "] = string.byte("0"), peekChar = b:peekChar()})       --i = 1
    assertEquals(2, { ["1"] = 1, ["buffer.i"] = b.i})
    b:skipChar()                                                                        --i = 1 -> i = 2
    assertEquals(2, { ["2"] = 2, ["buffer.i"] = b.i})
    assertEquals(0, { ["nil"] = nil, getChar = b:getChar()})                            --i = 2 -> i = 3
    assertEquals(2, { ["3"] = 3, ["buffer.i"] = b.i})
    assertEquals(0, { ["nil"] = nil, getChar = b:getChar()})                            --i = 3 -> i = 4
    assertEquals(2, { ["4"] = 4, ["buffer.i"] = b.i})
    assertEquals(0, { ["nil"] = nil, getChar = b:getChar()})                            --i = 4 -> i = 5
    assertEquals(2, { ["5"] = 5, ["buffer.i"] = b.i})
    assertEquals(0, { ["nil"] = nil, getChar = b:getChar()})                            --i = 5 -> i = 6
    assertEquals(2, { ["6"] = 6, ["buffer.i"] = b.i})
    assertEquals(0, { ["nil"] = nil, getChar = b:getChar()})                            --i = 6 -> i = 7
    assertEquals(2, { ["7"] = 7, ["buffer.i"] = b.i})
    assertEquals(0, { ["nil"] = nil, getChar = b:getChar()})                            --i = 7 -> i = 8
    assertEquals(2, { ["8"] = 8, ["buffer.i"] = b.i})
    end
end

function tests:lexer()
    local values = {
        { in_ = "10",    out = 10,       type = "number"},
        { in_ = "0",     out = 0,        type = "number"},
        { in_ = "-10",   out = -10,      type = "number"},
        { in_ = "-0x2F", out = -0x2f,    type = "number"},
        { in_ = "-0x2f", out = -0x2f,    type = "number"},
        { in_ = "0x2F",  out = 0x2f,     type = "number"},
        { in_ = "0x2f",  out = 0x2f,     type = "number"},
        { in_ = ".23",   out = .23,      type = "number"},
        { in_ = "0.23",  out = 0.23,     type = "number"},
        { in_ = "0b011", out = 3,        type = "number"},
        { in_ = "aaa",   out = {"aaa"},  type = "word"  },
        { in_ = "a0010", out = {"a0010"},type = "word"  },
        { in_ = "=",     out = "=",      type = "operator"},
        { in_ = "=s",    out = {"=s"},   type = "word"  },
        { in_ = "\"^n^f^r\"", out = "\n\f\r",   type = "string"  },
    }
    for k, v in pairs(values) do
        local lex = cubescript.Lexer:new()
        local buf = cubescript.Buffer:new(v.in_)
        local token = lex:lexizeSingleToken(buf)

        assertEquals(2, { ["token.type"] = token.type, ["tokenType.".. v.type] = cubescript.tokenType[v.type]})

        if type(v.out) == "table" then
            for key, value in pairs(v.out) do
                assertEquals(2, { ["token.value"] = token.value[key], [v.type.." <"..tostring(v.in_)..">"] = value})
            end
        else
            assertEquals(2, { ["token.value"] = token.value, [v.type.." <"..tostring(v.in_)..">"] = v.out})
        end
    end
end

function tests:lexer_string()
    local lex = cubescript.Lexer:new()
    local string =  " Hello world :) [ substring ] \"Another string part\" "
    local buf = cubescript.Buffer:new("["..string.."]")
    local token = lex:lexizeSingleToken(buf)
    assertEquals(2, { ["token.type"] = token.type, ["tokenType.string"] = cubescript.tokenType.string})
    assertEquals(2, { ["token.value[1]"] = token.value[1], ["string"] = string})
end

function tests:lexer_assignment()
    local lex = cubescript.Lexer:new()
    local buf = cubescript.Buffer:new "a = [hello world]"
    local tokens = lex:lexize(buf)
    assertEquals(2, { ["tokens[1].type"] = tokens[1].type, ["tokenType.word"] = cubescript.tokenType.word})
    assertEquals(2, { ["tokens[1].value[1]"] = tokens[1].value[1], ["a"] = "a"})
end

function tests:tokenStack()
    local lex = cubescript.Lexer:new()
    local buf = cubescript.Buffer:new("10")
    local token = lex:parseNumber(buf)
    local lexerStack = cubescript.LexerStack:new({token})
    assertEquals(2, { ["tokens [0] "] = token, peek = lexerStack:peek()})
    assertEquals(2, { ["tokens [0] "] = token, peek = lexerStack:next()})
    assertEquals(0, { peek = lexerStack:peek()})
    assertEquals(0, { next = lexerStack:next()})
end

function tests:parser()
    local lex = cubescript.Lexer:new()
    local buf = cubescript.Buffer:new("10")
    local token = lex:parseNumber(buf)
    local lexerStack = cubescript.LexerStack:new({token})
    local parser = cubescript.Parser:new()
    local statements = parser:parse(lexerStack)
    assertEquals(2, {["1"] = 1, ["#statements"] = #statements})
    assertEquals(2, {["statementType.call"] = cubescript.statementType.call, ["statements[1].type"] = statements[1].type})
    assertEquals(2, {["1"] = 1, ["#statements[1].arguments"] = #statements[1].arguments})
    assertEquals(2, {["token"] = token, ["statements[1].arguments[1]"] = statements[1].arguments[1]})
end

function tests:environment()
    local env = cubescript.createEnvironment()
    local res = env:run("10")
    assertEquals(2, {["10"] = 10, ["env:run(\"10\")"] = res})
end

function tests:environment_assignment()
    local env = cubescript.createEnvironment()
    local string = "hello world"
    local res = env:run("a = ["..string.."]")
    assertEquals(2, {["string"] = string, ["env:run(...)"] = res})
    assertEquals(2, {["string"] = string, ["env:lookup(a)"] = env:lookup("a", env.globalScope) })
end

function tests:utf8()
    local env = cubescript.createEnvironment()
    local func = "aäś΅¨"
    local ran = 0
    env:register(func, function(a, b, c, arg1)
        assertEquals(2, {["0x20"] = 0x20, ["arg1"] = env:toNumber(arg1)})
        ran = ran + 1
        return ran
    end)
    local runResult = env:run(func .. "  0x20")
    assertEquals(3, {["numCalls"] = ran, ["env:run(func)"] = runResult, ["1"] = 1})
end


local startTime = os.time()
for k, test in pairs(tests) do
    local res, err = xpcall(function()
        print (("Running tests in: %s"):format(k))
        test()
    end, traceback)
    if not res then
        print ("Error while running test: ", err)
    end
end
local endTime = os.time()
local timeDiff = endTime - startTime
print (("Ran %i/%i tests succeeded %i seconds"):format(total-fails, total, timeDiff))