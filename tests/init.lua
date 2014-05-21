local traceback = require "debug".traceback
local string = require "string"
local math = require "math"
local os = require "os"
local cubescript = require "cubescript"

local padName = 20
local function pad(name)
    name = tostring(name)
    local pad = padName-#name
    pad = math.max(0, pad)
    padName = math.max(padName, pad)
    return name .. string.rep(" ", pad)
end

local function assertEquals(n, args)
    local a
    local argc = 0
    for k, value in pairs(args) do
        if argc == 0 then
            a = value
        elseif value ~= a then
            print (("[✖ %s] %s (%s) does not equal %s (%s)"):format(pad(k), a, type(a), value, type(value)))
        else
            print (("[✓ %s] %s (%s) does equal %s (%s)"):format(pad(k), a, type(a), value, type(value)))
        end
        argc = argc + 1
    end
    if n ~= argc then
        print(("[✖ %s] Some arguments are missing (%i ~= %i)"):format(pad("unkown"), n, argc))
    end
end

local tests = {}

function tests:buffer()
    local buf = "10"

    local b = cubescript.Buffer:new (buf)
    assertEquals(2, { ["char <0> "] = string.byte("1"), peekChar = b:peekChar()})
    assertEquals(2, { ["char <0> "] = string.byte("1"), getChar = b:getChar()})
    assertEquals(2, { ["char <1> "] = string.byte("0"), peekChar = b:peekChar()})
    b:undoChar()
    assertEquals(2, { ["char <0> "] = string.byte("1"), peekChar = b:peekChar()})
    b:skipChar()
    assertEquals(2, { ["char <1> "] = string.byte("0"), peekChar = b:peekChar()})
end

function tests:lexer()
    local lex = cubescript.Lexer:new()
    local buf = cubescript.Buffer:new("10")
    local token = lex:parseNumber(buf)

    assertEquals(2, { ["token.type"] = token.type, ["tokenType.number"] = cubescript.tokenType.number})
    assertEquals(2, { ["token.value"] = token.value, ["number <10>"] = 10})
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
print (("Ran %i tests in %i seconds"):format(#tests, timeDiff))