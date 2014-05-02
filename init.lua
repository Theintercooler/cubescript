local string = require "string"
local table = require "table"
local core = require "luvit.core"

local utils = require "luvit.utils"
utils.DUMP_MAX_DEPTH = 100

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

local function isAlpha(char)
    if not char then return end
    local c = char:byte()
    return (c >= 65 and c <= 90) or (c >= 97 and c <= 122)
end

local function isNumeric(char)
    if not char then return end
    local c = char:byte()
    return (c >= 48 and c <= 57)
end

local function isAlphaNumeric(char)
    if not char then return end
    local c = char:byte()
    return (c >= 48 and c <= 57) or (c >= 65 and c <= 90) or (c >= 97 and c <= 122)
end

local function isLineSeperator(char)
    if not char then return end
    local c = char:byte()
    return c == 10 or c == 13
end

local function isWhiteSpace(char)
    if not char then return end
    local c = char:byte()
    return c == 32 or c == 10 or c == 13 or c == 9 or c == 11 or c == 12 -- space, \n, \r, \t, \v, \f
end

local Buffer = core.Object:extend()

function Buffer:initialize(string)
    if type(string) ~= "string" then
        error("No string was given for argument #1")
    end
    self.i = 0
    self.line = 1
    self.chars = string
end

function Buffer:skipChar()
    self.i = self.i + 1 
end

function Buffer:getChar()
   self:skipChar()

   if self.i  > self.chars:len() or self.i < 0 then
       return nil
   end

   local c = self.chars:sub(self.i, self.i)
   if isLineSeperator(c) then
       self.line = self.line + 1
   end
   return c
end

function Buffer:peekChar(peek)
    peek = peek or 1
    if self.i + peek > self.chars:len() or self.i < 0 then
        return nil
    end

    return self.chars:sub(self.i + peek, self.i + peek)
end

function Buffer:undoChar()
    self.i = self.i - 1
    return self:peekChar(0)
end

local tokenType = {
    comment     = "comment",
    string      = "string",
    word        = "word",
    alias       = "alias",
    macro       = "macro",
    endStatement= "endStatement",
    operator    = "operator",
    seperator   = "seperator",
    endOfBuffer = "endOfBuffer",
    call        = "call",
    number      = "number"
}

local Token = core.Object:extend()

function Token:initialize(tokenType)
    self.type = tokenType
end

local Lexer = core.Object:extend()

function Lexer:initialize()

end

function Lexer:error(message, wrongChar, buffer)
    local location = buffer.i

    local line = ""

    while not isLineSeperator(buffer:undoChar()) do 
        if not buffer:peekChar() then
            break
        end
    end

    local offset = 0
    local char = buffer:getChar()
    while char and not isLineSeperator(char) do
        line = line .. char
        char = buffer:getChar()
        if buffer.i <= location then
            offset = offset + 1
        end
    end

    local arrow = ("."):rep(offset) .. "^"

    if wrongChar then
        if isLineSeperator(wrongChar) then
            char = "<eol>"
        end
        message = message .." got: "..tostring(wrongChar)
    end
    message = message .. "\n"..line .. "\n" .. arrow
    error (message)
end

function Lexer:skipWhiteSpace(buffer)
    while isWhiteSpace(buffer:peekChar()) and not isLineSeperator(buffer:peekChar()) do
        buffer:skipChar()
    end
end

function Lexer:readUntilWhiteSpace(buffer)
    local s = ""
    while not isWhiteSpace(buffer:peekChar()) do
        s = s .. buffer:getChar()
    end
    return s
end

function Lexer:parseString(buffer)
    local stringType = buffer:getChar()
    if stringType == "\"" then
        local token = Token:new(tokenType.string)
        token.value = ""

        local char = buffer:getChar()
        while char do
            if char == "^" then
                char = self:readEscapeSequence(buffer)
            elseif char == stringType then
                break
            end

            token.value = token.value .. char

            char = buffer:getChar()
        end

        if char ~= stringType then
            self:error("Expected closing "..stringType, char, buffer)
        end

        return token
    else
        local token = Token:new(tokenType.string)
        local parts = {}
        local stack = 1

        local char = buffer:peekChar()
        while char do
            if char == "@" then
                local oldI = buffer.i
                char = self:parseMacro(buffer)
                if char.stack < stack then
                    buffer.i = oldI
                    char = buffer:getChar()
                end
            else
                char = buffer:getChar()
                if char == "]" then
                    stack = stack - 1

                    if stack == 0 then
                        break
                    end
                elseif char == "[" then
                    stack = stack + 1
                end
            end

            parts[#parts+1] = char

            char = buffer:peekChar()
        end

        token.value = {}

        local currentString = nil
        for k, v in pairs(parts) do
            if type(v) == "string" then
                currentString = (currentString or "") .. v
            else
                if currentString then
                    table.insert(token.value, currentString)
                    currentString = nil
                end

                table.insert(token.value, v)
            end
        end

        if currentString then
            table.insert(token.value, currentString)
            currentString = nil
        end

        return token
    end
end

function Lexer:parseAlias(buffer)
    local token = Token:new(tokenType.alias)
    local char = buffer:getChar()

    local word = self:parseWord(buffer)

    token.value = word.value

    return token
end

function Lexer:parseWord(buffer)
    local token = Token:new(tokenType.word)
    local char = buffer:peekChar()

    token.value = {}

    local stringValue = ""
    while char and not isWhiteSpace(char) do
        char = buffer:getChar()
        --buffer:skipChar()

        if char == "@" then
            table.insert(token.value, stringValue)
            stringValue = nil
            buffer:undoChar()
            table.insert(token.value, self:parseMacro(buffer))
        else
            stringValue = (stringValue or "") .. char
        end

        char = buffer:peekChar()
    end

    if stringValue then
        table.insert(token.value, stringValue)
    end

    return token
end

function Lexer:parseNumber(buffer)
    local token = Token:new(tokenType.number)
    local char = buffer:getChar()
    token.value = 0

    local negative = false

    if char == "-" or char == "+" then
        negative = char == "-"
        char = buffer:getChar()
    end

    local base = 10
    local numberType = "d"

    if char == "0" and isAlpha(buffer:peekChar()) then
        numberType = buffer:getChar()
        char = buffer:getChar()
    end

    if numberType == "d" then
    elseif numberType == "x" then
        base = 16
    elseif numberType == "b" then
        base = 2
    end
    local noPeek = false
    while char and not isWhiteSpace(char) do
        if noPeek then
            buffer:skipChar()
        end

        local value = ({
            ["0"] = 0,
            ["1"] = 1,
            ["2"] = 2,
            ["3"] = 3,
            ["4"] = 4,
            ["5"] = 5,
            ["6"] = 6,
            ["7"] = 7,
            ["8"] = 8,
            ["9"] = 9,
            ["a"] = 10,
            ["b"] = 11,
            ["c"] = 12,
            ["d"] = 13,
            ["e"] = 14,
            ["f"] = 15
        })[char:lower()]

        if type(value) == "nil" then
            self:error("Invalid number", char, buffer)
        end

        token.value = token.value * base + value

        char = buffer:peekChar()
        noPeek = true
    end

    if negative then
        token.value = -token.value
    end

    return token
end

function Lexer:parseMacro(buffer)
    local char = buffer:peekChar()
    local token = Token:new(tokenType.macro)

    token.stack = 0

    while char == "@" do
        token.stack = token.stack + 1
        buffer:skipChar()
        char = buffer:peekChar()
    end

    if token.stack == 0 then
        self:error("Expecting start of macro", char, buffer)
    end

    if char == "[" then
        token.value = self:parseString(buffer)
    else
        token.value = ""
        char = buffer:peekChar()

        while char and isAlphaNumeric(char) do
            char = buffer:getChar()

            token.value = token.value .. char

            char = buffer:peekChar()
        end
    end

    return token
end

function Lexer:parseCall(buffer)
    --TODO: macros!
    local open = buffer:getChar()
    assert(open == "(")

    local token = Token:new(tokenType.call)
    token.value = ""
    local stack = 1
    local char = buffer:getChar()
    while char do
        if char == "(" then
            stack = stack + 1
        elseif char == ")" then
            stack = stack - 1

            if stack == 0 then
                break
            end
        end

        token.value = token.value .. char

        char = buffer:getChar()
    end

    if char ~= ")" then
        self:error("Expected closing )", char, buffer)
    end

    return token
end

Lexer.operators = {
    ["="] = "=",
}
function Lexer:isOperator(buffer)
    local i = 1
    local char = buffer:peekChar(i)
    local str = ""

    while char and not isWhiteSpace(char) do
        str = str .. char
        i = i + 1
        char = buffer:peekChar(i)
    end

    return self.operators[str]
end

function Lexer:parseOperator(buffer)
    local op = self:readUntilWhiteSpace(buffer)
    if op ~= "=" then
        self:error("Unkown operator: ", op, buffer)
    end

    local token = Token:new(tokenType.operator)
    token.value = "="
    return token
end

function Lexer:parseSeperator(buffer)
    local seperator = buffer:getChar()
    assert(seperator == ";" or seperator == "\n")
    return Token:new(tokenType.seperator)
end

function Lexer:parseComment(buffer)
    local char = buffer:getChar()

    local token = Token:new(tokenType.comment)
    token.value = ""

    if char == "#" or char == "/" and buffer:peekChar() == "/" then 
        if char == "/" then
            buffer:getChar() -- skip the second slash
        end

        char = buffer:getChar()

        while char and char ~= "\n" and char ~= "\r" do
            token.value = token.value .. char
            if isLineSeperator(buffer:peekChar()) then
                break
            end
            char = buffer:getChar()
        end
    elseif char == "/" and buffer:peekChar() == "*" then
        buffer:getChar() -- skip the *
        char = buffer:getChar()

        while char and not (char == "*" and buffer:peekChar() == "/") do
            token.value = token.value .. char
            char = buffer:getChar()
        end
        buffer:getChar() -- skip the ending /
    else
        self:error ("Malformatted comment", char, buffer)
    end

    return token
end

function Lexer:parseEndOfBuffer()
    return Token:new(tokenType.endOfBuffer)
end

local _valid_var_chars = {
    ["+"] = true,
    ["-"] = true,
    ["*"] = true,
    ["="] = true,
    ["<"] = true,
    [">"] = true,
    ["_"] = true,
}
function Lexer:validVariableChar(buffer)
    local char = buffer:peekChar()
    return isAlpha(char) or _valid_var_chars[char]
end

function Lexer:lexizeSingleToken(buffer)
    self:skipWhiteSpace(buffer)
    local char = buffer:peekChar()

    if char == "[" or char == "\"" then
        return self:parseString(buffer)
    elseif char == "$" then
        return self:parseAlias(buffer)
    elseif char == "@" then
        return self:parseMacro(buffer)
    elseif char == "(" then
        return self:parseCall(buffer)
    elseif char == "/" and (buffer:peekChar(2) == "/" or buffer:peekChar(2) == "*") or char == "#" then
        return self:parseComment(buffer)
    elseif char == ";" or char == "\n" then
        return self:parseSeperator(buffer)
    elseif self:isOperator(buffer) then
        return self:parseOperator(buffer)
    elseif isNumeric(char) or char == "-" then
        return self:parseNumber(buffer)
    elseif self:validVariableChar(buffer) then
        return self:parseWord(buffer)
    elseif char then
        return self:error("Unexpected character ", char, buffer)
    else
        return self:parseEndOfBuffer()
    end
end

function Lexer:lexize(buffer)
    local tokens = {}

    while buffer:peekChar() do
        local line = buffer.line
        local token = self:lexizeSingleToken(buffer)
        token.line = line
        table.insert(tokens, token)
    end

    return tokens
end

local LexerStack = core.Object:extend()

function LexerStack:initialize(lex)
    self.lex = lex
    self.i = 0
end

function LexerStack:next()
    self.i = self.i + 1
    return self.lex[self.i]
end

function LexerStack:peek(amount)
    amount = amount or 1
    return self.lex[self.i + amount]
end

function LexerStack:backPeek(amount)
    return self:peek(-amount)
end

local Statement = core.Object:extend()

function Statement:initialize()
    self.type = nil
    self.arguments = {}
end

local statementType = {
    assignment = "assignment",
    call = "call"
}

local Parser = core.Object:extend()

function Parser:initialize()

end

function Parser:skipComments(lex)
    while lex:peek() do
        local token = lex:peek()
        if token.type == tokenType.comment or token.type == tokenType.seperator or token.type == tokenType.endOfBuffer then
            lex:next()
        else
            break
        end
    end
end

function Parser:parseArguments(lex, statement)
    while lex:peek() do
        local token = lex:peek()
        if token and token.type ~= tokenType.seperator and token.type ~= tokenType.endOfBuffer then
            if token.type ~= tokenType.comment then
                table.insert(statement.arguments, lex:next())
            else
                lex:next()
            end
        else
            break
        end
    end
end

function Parser:parseOne(lex)
    self:skipComments(lex)

    local token = lex:next()

    if not token then
        return false
    end

    local statement = Statement:new()
    statement.type = statementType.call
    statement.line = token.line
    table.insert(statement.arguments, token)

    if token.type == tokenType.word or token.type == tokenType.string then
        if lex:peek() and lex:peek().type == tokenType.operator then
            local operator = lex:next()
            if operator.value == "=" then
                statement.type = statementType.assignment
            end
        end
        self:parseArguments(lex, statement)
    else
        error("Unexpected type: "..token.type)
    end
    
    return statement
end

function Parser:parse(lex)
    local statements = {}
    while lex:peek() do
        local statement = self:parseOne(lex)
        if statement then
            table.insert(statements, statement)
        end
    end
    return statements
end

local Trace = core.Object:extend()

function Trace:initialize()
    self.i = 1
    self.trace = {}
end

function Trace:add(obj)
    self.trace[self.i] = obj
    self.i = self.i + 1
end

function Trace:pop()
    self.i = self.i - 1
    return self.trace[self.i]
end

local metaScope = {}
function metaScope:__index(key)
    if key:sub(1, 1) == "#" then return end
    local v = rawget(self, key)
    local parent = rawget(self, "#parent")
    if type(v) == "nil" and parent then
        return parent[key]
    else
        return v
    end
end

function metaScope:__newindex(key, value)
    if key:sub(1, 1) == "#" then return end
    local haveLocal = rawget(self, key)
    local isNil = type(value) == "nil"
    local parent = rawget(self, "#parent")
    local preferlocal = rawget(self, "preferlocal")
    if haveLocal or preferlocal then
        if isNil then
            rawset(self, key, "") -- Keep locals
        else
            rawset(self, key, value)
        end
    elseif parent then
        parent[key] = value
    else
        rawset(self, key, value)
    end
end

local function makeScope(parent, preferlocal)
    return setmetatable({["#parent"] = parent, ["#preferlocal"] = preferlocal}, metaScope)
end

local Environment = core.Object:extend()

function Environment:initialize(scope)
    self.lexer = nil
    self.parser = nil
    self.globalScope = scope or makeScope(nil)
end

function Environment:createTokenLocationTrace(token, shift)
    local line = token.line
    if shift.line > 0 then
        line = line + shift.line - 1
    end
    return shift.file..":"..tostring(line)
end

function Environment:createTraceMessage(msg, shift)
    if type(msg) == "string" then
        return msg
    elseif type(msg) == "table" then
        if msg.argument then
            return self:createTokenLocationTrace(msg.argument, shift).." in argument preprocessing"
        elseif msg.macro then
            return self:createTokenLocationTrace(msg.macro, shift).." in macro preprocessing"
        elseif msg.statement then
            if msg.type == "assignment" then
                return self:createTokenLocationTrace(msg.statement, shift).." in assginment "..tostring(msg.name)
            elseif msg.type == "run.args" then
                return self:createTokenLocationTrace(msg.statement, shift).." in argument calling."
            else
                return self:createTokenLocationTrace(msg.statement, shift).." in call to function "..tostring(msg.name)
            end
        elseif msg.fileShift or type(msg.lineShift) == "number" or type(msg.relativelineShift) == "number" then
            if type(msg.lineShift) == "number" then
                shift.line = msg.lineShift
            end
            if msg.fileShift then
                shift.file = msg.fileShift
            end
            return shift
        elseif msg.callbackFunction then
            return self:createTokenLocationTrace({line = 1}, shift).. " in callback function"
        elseif msg.globalScope then
            return self:createTokenLocationTrace({line = 1}, shift).. " in global scope"
        else
            local str = ""
            for k, v in pairs(msg) do
                str = str .. tostring(k) .. "=" .. tostring(v).. ", "
            end
            return str
        end
    else
        return "Weird trace msg of type "..type(msg)
    end
end

function Environment:throwError(message, trace)
    local err = setmetatable({}, errorMeta)
    err.message = message
    local back = {}

    local shift = {file = "<unkown file>", line = 0}
    local traceMessage = trace:pop()
    while traceMessage do
        for k, msg in pairs(trace.trace) do
            if msg.fileShift or type(msg.lineShift) == "number" or type(msg.relativelineShift) == "number" then
                if type(msg.lineShift) == "number" then
                    shift.line = msg.lineShift
                end
                if msg.fileShift then
                    shift.file = msg.fileShift
                end
                if msg.relativelineShift then
                    shift.line = shift.line + msg.relativelineShift - 1
                end
            end
            if traceMessage == msg then
                break
            end
        end
        local v = self:createTraceMessage(traceMessage, shift)
        if type(v) == "table" then
            shift = v
        else
            table.insert(back, v)
        end
        traceMessage = trace:pop()
    end
    err.trace = back
    err.message = err.message .."\n\t"..table.concat(back, "\n\t")
    error(err)
end

function Environment:makeBuffer(string)
    return Buffer:new(string)
end

function Environment:tokenize(buf)
    return self.lexer:lexize(buf)
end

function Environment:makeTokenStack(tokens)
    return LexerStack:new(tokens)
end

function Environment:parse(stack)
    return self.parser:parse(stack)
end

function Environment:executeCallback(callback, scope, trace)
    local x = trace:pop()
    trace:add(x)
    trace:add({relativelineShift = x.statement.line + 1})
    trace:add({callbackFunction = true})
        local ret = self:run(callback, scope, trace)
    trace:pop()
    trace:pop()

    return ret
end

function Environment:run(code, scope, trace, meta)
    if not core.instanceof(Buffer, code) then
        if type(code) == "table" then
            meta = meta or {fileShift = code.file, lineShift = code.line}
            code = code.value
        end
        code = self:makeBuffer(code)
    end

    local tokens = self:tokenize(code)
    local statements = self:parse(self:makeTokenStack(tokens))

    if not trace then
        trace = Trace:new()
        if meta then
            trace:add(meta)
        end
        trace:add({globalScope = true})
    else
        if meta then
            trace:add(meta)
        end
    end

    local ret = self:runStatements(statements, scope or self.globalScope, trace)
    if meta then
        trace:pop()
    end

    return ret
end

function Environment:runMacros(parent, scope, trace)
    local result = ""

    trace:add({macro = parent})

    local tokens = parent.value
    for k, value in pairs(type(tokens) == "table" and tokens or {tokens}) do
        if type(value) ~= "string" then
            assert(value.type == tokenType.macro, "Value is not a macro")

            -- @[ ]
            if type(value.value) == "table" then
                assert(value.value.type == tokenType.string, "Macro table is not string")
                value = self:run(value.value.value[1], scope)

            -- @n
            elseif not scope[value.value] then
                self:throwError("Invalid value lookup: "..value.value, trace)
            else
                value = scope[value.value]
            end

            result = result .. value --TODO better scope lookup 
        else
            result = result .. value
        end
    end

    trace:pop()

    return result
end

function Environment:runArgument(token, scope, trace)
    trace:add({argument = token})
    trace:add({relativelineShift = token.line})

    local r
    if token.type == tokenType.string then
        r = self:runMacros(token, scope, trace)
    elseif token.type == tokenType.word then
        r = self:runMacros(token, scope, trace)
    elseif token.type == tokenType.number then
        r = token.value
    elseif token.type == tokenType.alias then
        local v = self:runMacros(token, scope, trace)

        if not scope[v] then
            self:throwError("Unkown alias lookup: "..v, trace)
        end

        r = scope[v]
    elseif token.type == tokenType.call then
        r = self:run(token.value, scope, trace)
    elseif token.type == tokenType.macro then
        error "Macro argument not preprocessed"
    else
        error("Unkown token type: "..token.type)
    end

    trace:pop()
    trace:pop()
    return r
end

function Environment:runAllArguments(tokens, scope, trace)
    local result = {}

    for k, v in pairs(tokens) do
        result[k] = self:runArgument(v, scope, trace)
    end

    return result
end

function Environment:runStatements(statements, scope, trace)
    for k, statement in pairs(statements) do
        if not statement then
        elseif statement.type == statementType.assignment then
            assert(#statement.arguments == 2, "Too many arguments for assignment operator")

            trace:add({statement = statement, type="assignment", name = statement.arguments[1]})
                local var = self:runArgument(statement.arguments[1], scope, trace)
            trace:pop()
            trace:add({statement = statement, type="run.args", name = var})
                local val = self:runArgument(statement.arguments[2], scope, trace)
            trace:pop()

            if type(scope[var]) == "function" then
                self:throwError("Cannot overwrite function type: "..var, trace)
            end

            trace:add({statement = statement, type="assignment", name = var})
                scope[var] = {value=val, line=statement.line, file=statement.file}
            trace:pop()
            scope.result = val
        elseif statement.type == statementType.call then
            local func
            local args = {}

            for k, v in pairs(statement.arguments) do
                if k == 1 then
                    func = v
                else
                    args[k-1] = v
                end
            end

            trace:add({statement = statement, type = "run.args"})
                func = self:runArgument(func, scope, trace)
                args = self:runAllArguments(args, scope, trace)
            trace:pop()

            trace:add({statement = statement, type = "run", name = func})
                local f = scope[func]
                if type(f) == "function" then
                    scope.result = f(self, scope, trace, unpack(args))
                elseif type(f) == "string" or type(f) == "table" then
                    local subScope = makeScope(scope)

                    for k, v in ipairs(args) do
                        rawset(subScope, "arg"..k, v) --Locals
                    end

                    scope.result = self:run(f, subScope, trace)
                elseif type(f) == "number" or type(f) == "boolean" then
                    scope.result = f
                else
                    self:throwError("Trying to call nonexisting function: "..func, trace)
                end
            trace:pop()
        else
            error("Unkown statement type")
        end
    end
    
    return scope.result
end

function Environment:register(name, cb, scope)
    scope = scope or self.globalScope
    scope[name] = cb
end


return {
    Environment = Environment,
    Lexer = Lexer,
    Parser = Parser,
    createEnvironment = function()
        local env = Environment:new()
        env.lexer = Lexer:new()
        env.parser = Parser:new()
        return env
    end,
    makeScope = makeScope
}