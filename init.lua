local string = require "string"
local table = require "table"
local utils = require "utils"
local fs = require "fs"
local core = require "core"

function string.split(str, pat)
    local t = {} -- NOTE: use {n = 0} in Lua-5.0
            local fpat = "(.-)" .. pat
    local last_end = 1
    local s, e, cap = str:find (fpat, 1)
    
    while s do
        if s ~= 1 or cap ~= "" then
            table.insert(t, cap)
        end
        last_end = e+1
        s, e, cap = str:find(fpat, last_end)
    end
    if last_end <= #str then
        cap = str:sub(last_end)
        table.insert(t, cap)
    end
    return t
end

local Line = core.Emitter:extend()

function Line:initialize(line)
    self.i = 0
    self.chars = {}
    local i = 1
    for c in line:gmatch(".") do
        self.chars[i] = c
        i = i + 1
    end
end

function Line:getChar()
   self.i = self.i + 1
   return self.chars[self.i]
end

function Line:peekChar()
    return self.chars[self.i+1]
end

local Lexer = core.Emitter:extend()

function Lexer:initialize(contents)
    self.i = 0
    self.lines = ""
end

function Lexer:getNext()
    self.i = self.i + 1
    return self.lines[self.i]
end

function Lexer:process(code)
    self.i = 0
    self.lines = code:split("\n")
    
    local line = self:getNext()
    local calls = {}
    
    while line do
        local curLine = Line:new(line)
        local subject = ""
        local operation = "call"
        local char
        local c
        
        ::restart::
        
        c = curLine:getChar()
        while c do
            if c ~= " " and c ~= "\t" then
                if c == "/" then
                    local c2 = curLine:peekChar()

                    if c2 == "/" then
                        goto skip
                        --Skip this line
                    elseif c2 == "*" then
                        while line do
                            c = curLine:getChar()
                            while c do
                                
                                if c == "*" and curLine:peekChar() == "/" then
                                   --Comment is finished
                                    curLine:getChar() -- Skip /
                                    goto restart
                                end
                                
                                c = curLine:getChar() 
                            end
                            
                            line = self:getNext()
                            curLine = Line:new(line)
                        end
                        --Multi-line comment
                        -- TODO: implement
                    end
                else
                    curLine. i = curLine. i - 1
                end
                break
            end
            c = curLine:getChar()
        end
        
        --empty line
        if c == nil then
           goto skip 
        end

        c = curLine:getChar()
        
        while c do
            if c ~= " " and c ~= "\t" then
                subject = subject .. c
            else
                break
            end
            c = curLine:getChar()
        end
        
        
        do
            local starti = curLine.i
            
            c = curLine:getChar()
            while c do
                if c == "=" then
                    operation = "set"
                elseif c ~= " " and c ~= "\t" then
                    starti = curLine.i
                    break
                end
                c = curLine:getChar()
            end
            curLine.i = starti - 1
        end
        
        if operation == "set" then
            line = "set \"" .. subject .."\" ".. line:sub(curLine.i)
            curLine = Line:new(line)
            subject = "set"
            operation = "call"
            curLine.i = 3
        end
        
        if operation == "call" then
            --Parse arguments
            
            local callArgs = {}
            c = curLine:getChar()
            while c do
                if c == " " or c == "\t" then
                    --Ignore
                elseif c == "(" then
                    local command = ""
                    
                    -- ( in ( in ( ...
                    local count = 1
                    local d
                    
                    d = curLine:getChar()
                    while d do
                        
                       if d == ")" then
                           count = count - 1
                       elseif d == "(" then
                           count = count + 1
                       else
                           command = command .. d
                       end
                       
                       if count == 0 then
                           break
                       end
                       
                       d = curLine:getChar()
                    end
                    
                    if count ~= 0 then
                       error("Missing ) on line: "..line) 
                    end
                    
                    --Pass by result
                    table.insert(callArgs, {true, command})
                
                --Single line string
                elseif c == "\"" then
                    local argument = ""
                    local d
                    
                    d = curLine:getChar()
                    while d do
                        if d == "\"" then
                            break
                        else
                            argument = argument .. d
                        end
                        d = curLine:getChar()
                    end
                    table.insert(callArgs, {false, argument})
                
                -- Multi line string
                elseif c == "[" then
                    local argument = ""
                    
                    -- ( in ( in ( ...
                    local count = 1
                    
                    repeat
                        local d
                        
                        d = curLine:getChar()
                        while d do
                            
                            if d == "]" then
                                count = count - 1
                            elseif d == "[" then
                                count = count + 1
                            else
                                argument = argument .. d
                            end
                            
                            if count == 0 then
                                break
                            end
                            d = curLine:getChar()
                        end
                    
                        --Read on to the next line
                        if count ~= 0 then
                            argument = argument .. "\n"
                           line = self:getNext()
                           curLine = Line:new(line)
                           if not line then
                               error("Unfihished string near EOF")
                           end
                        end
                    until count == 0
                    
                    table.insert(callArgs, {false, argument})
                elseif c == "$" or c == "@" then
                    local argument = ""
                    local d
                    
                    d = curLine:getChar()
                    while d do
                        
                        if d == " " or d == "\t" then
                            break
                        else
                            argument = argument .. d
                        end
                        d = curLine:getChar()
                    end
                    
                    argument = "get \""..argument.."\""
                    
                    table.insert(callArgs, {true, argument})
                end
                --Todo add support for strings without "
                c = curLine:getChar()
            end
            
            table.insert(calls, {subject, callArgs})
        end
    
        
        ::skip::
        line = self:getNext()
    end
    
    return calls
end

local Environment = core.Emitter:extend()

function Environment:initialize()
   self.globals = {} 
   self.lexer = Lexer:new()
end

function Environment:getGlobal(name)
    
    if type(self.globals[name]) == "nil" then
        p("WARNING: unkown global: "..name)
    end
    
   return self.globals[name] or ""
end

function Environment:setGlobal(name, value)
    self.globals[name] = tostring(value or "")
end

function Environment:setGlobalFunction(name, value)
    self.globals[name] = value
end

function Environment:run(string)
    local calls = self.lexer:process(string)
    local lastRes
    
    self:setGlobalFunction("result", function(env, result)
        lastRes = result
    end)
    
    for i, call in pairs(calls) do
        local func = self:getGlobal(call[1])
        
        for j, arg in pairs(call[2]) do
           if arg[1] then
              
              call[2][j] = self:run(arg[2]) 
           else
               call[2][j] = arg[2]
           end
        end
        
        local res
        if type(func) == "string" then
            res = self:run(func)
        else --Global function
            res = func(self, unpack(call[2]))
        end
        
        if res then
            lastRes = res
        end
    end
    
    return lastRes
end

local env = Environment:new()
env:setGlobalFunction("if", function(env, cond, case1, case2)
    if tonumber(cond) then
        env:run(case1)
    elseif case2 then
        env:run(case2)
    end
end)

env:setGlobalFunction("+", function(env, ...)
    local args = {...}
    local res = 0
    
    for i, value in pairs(args) do
        res = res + (tonumber(value) or 0)
    end
    
    return res
end)

env:setGlobalFunction("set", function(env, name, value)
    if not name then
        env:argError("Missing argument 1 for set.") 
    end

    if not value then
        env:argError("Missing argument 2 for set.") 
    end
    
    p("SET", name, value)
    env:setGlobal(name, value)
end)

env:setGlobalFunction("get", function(env, name)
    if not name then
       env:argError("Missing argument 1 for get.") 
    end

    p("GET", name)
    return env:getGlobal(name)
end)

env:setGlobalFunction("true", function(env, name)
    return "1"
end)

env:setGlobalFunction("false", function(env, name)
    return "0"
end)

env:setGlobalFunction("echo", function(env, ...)
    print(...)
end)

env:setGlobalFunction("concatword", function(env, ...)
    local args = {...}
    local res = ""
    
    for i, arg in pairs(args) do
       res = res .. arg 
    end
    
    return res
end)

env:setGlobalFunction("concat", function(env, ...)
    local args = {...}
    local res
             
    for i, arg in pairs(args) do
        if res then
            res = res .. " " .. arg 
        else
            res = arg
        end
    end
             
    return res
end)

local function lexitize(file, callback)
    fs.readFile(file, function (err, data)
        env:run(data, callback)
    end)
end

local function exec(file)
    lexitize(file, function()
        
    end)
end

exec("test.cs")

