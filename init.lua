local ffi = require "ffi"
local math = require "math"
local string = require "string"
local table = require "table"
local bit = require "bit"
local core = require "luvit.core"
local debug = require "debug"
local traceback = debug.traceback


local function isAlpha(c)
    if type(c) == "nil" then return false end
    return (c >= 65 and c <= 90) or (c >= 97 and c <= 122)
end

local function isNumeric(c)
    if type(c) == "nil" then return false end
    return (c >= 48 and c <= 57)
end

local function isAlphaNumeric(c)
    if type(c) == "nil" then return false end
    return (c >= 48 and c <= 57) or (c >= 65 and c <= 90) or (c >= 97 and c <= 122)
end

local function isLineSeperator(c)
    if type(c) == "nil" then return false end
    return c == 10 or c == 13
end

local function isWhiteSpace(c)
    if type(c) == "nil" then return false end
    return c == 20 or c == 32 or c == 10 or c == 13 or c == 9 or c == 11 or c == 12 -- space, \n, \r, \t, \v, \f
end

local function ___cut_trace___(f, ...)
    return f(...)
end

local Buffer = core.Object:extend()

function Buffer:initialize(string, startLine, fileInfo)
    if type(string) ~= "string" then
        error (("No string was given for argument #1 (given: %s (%s))"):format(tostring(string), type(string)))
    end
    self.i = 0
    self.length = #string
    self.buffer = ffi.new("char[?]", self.length + 1, string)
    self.line = startLine or 1
    self.file = fileInfo
end

function Buffer:getCharPointer(i)
    return self.buffer + (i or self.i) - 1
end

function Buffer:skipChar()
    self.i = self.i + 1
end

function Buffer:getChar()
   self:skipChar()

   if self.i  > self.length or self.i < 0 then
       return nil
   end

   local c = self.buffer[self.i-1]
   if isLineSeperator(c) and (c ~= '\r' or self:peekChar() ~= '\n') then
       self.line = self.line + 1
   end
   return c
end

function Buffer:peekChar(peek)
    peek = peek or 1
    if self.i + peek > self.length or self.i < 0 then
        return nil
    end

    return self.buffer[self.i + peek - 1]
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
    local fileLocation = buffer.file or "<unkown file>"
    local lineLocation = buffer.line or 0

    local line = ""

    while not isLineSeperator(buffer:undoChar()) do 
        if not buffer:peekChar() then
            break
        end
    end

    local offset = 0
    local char = buffer:getChar()
    while char and not isLineSeperator(char) do
        line = line .. self:getChar(char, buffer)
        char = buffer:getChar()
        if buffer.i <= location then
            offset = offset + 1
        end
    end

    local arrow = ("."):rep(offset) .. "^"

    if wrongChar then
        local char
        if type(wrongChar) ~= "string" then
            wrongChar = tonumber(wrongChar)

            if isLineSeperator(wrongChar) then
                char = "<eol>"
            elseif wrongChar < 0 then
                char = "<utf8>"
            else
                char = string.char(tonumber(wrongChar) or 0)
            end
        end
        message = ("%s:%i %s got: %s (%i)"):format(fileLocation, lineLocation, message, char, wrongChar)
    end
    message = message .. "\n"..line .. "\n" .. arrow
    error (message)
end

function Lexer:isSeperator(c)
    return (c == 59 or isLineSeperator(c)) -- ; \n or \r
end

function Lexer:skipWhiteSpace(buffer)
    while isWhiteSpace(buffer:peekChar()) and not isLineSeperator(buffer:peekChar()) do
        buffer:skipChar()
    end
end

function Lexer:readUntilWhiteSpace(buffer)
    local s = ""
    while not isWhiteSpace(buffer:peekChar()) do
        s = s .. self:getChar(nil, buffer)
    end
    return s
end

function Lexer:readEscapeSequence(buffer)
    local char = buffer:getChar()
    local escaped = ({
        [102] = "\f",
        [110] = "\n",
        [114] = "\r"
    })[char]
    
    if not escaped then
        self:error("Unkown escape sequence ^"..char, char, buffer)
    end

    return escaped
end

function Lexer:getChar(char, buffer)
    if not char then
        char = buffer:getChar()
    end

    if char < 0 then --utf8 char
        local len
        local modus = bit.band(char, 224)
        if modus == 192 then --2 byte
            len = 2
        else
            modus = bit.band(char, 240)
            if modus == 224 then -- 3 byte
                len = 3
            else
                modus = bit.band(char, 248)
                if modus == 240 then -- 4 byte
                    len = 4
                end
            end
        end

        if not len then
            self:error("Invalid UTF8 char", char, buffer)
        end

        local v = ffi.string(buffer:getCharPointer(), len)
        buffer.i = buffer.i + len - 1
        return v
    else
        return string.char(char)
    end
end

function Lexer:parseString(buffer)
    local stringType = buffer:getChar()
    if stringType == 34 then -- "
        local token = Token:new(tokenType.string)
        local value = {}
        local char = buffer:getChar()
        while char do
            if char == 94 then -- ^
                char = self:readEscapeSequence(buffer)
            elseif char == stringType then
                break
            end

            value[#value + 1] = self:getChar(char, buffer)

            char = buffer:getChar()
        end

        token.value = table.concat(value, "")

        if char ~= stringType then
            self:error("Expected closing "..string.char(stringType), char, buffer)
        end

        return token
    else
        local token = Token:new(tokenType.string)
        local parts = {}
        local stack = 1

        local char = buffer:peekChar()
        while char do
            if char == 64 then -- @
                local oldI = buffer.i
                local oldLine = buffer.line
                char = self:parseMacro(buffer)
                if char.stack < stack then
                    buffer.i = oldI
                    buffer.line = oldLine
                    char = buffer:getChar()
                end
            else
                char = buffer:getChar()
                if char == 93 then -- ]
                    stack = stack - 1

                    if stack == 0 then
                        break
                    end
                elseif char == 91 then -- [
                    stack = stack + 1
                end
            end

            if type(char) ~= "table" then
                parts[#parts+1] = self:getChar(char, buffer)
            else
                parts[#parts+1] = char
            end

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
    while char and not isWhiteSpace(char) and not self:isSeperator(char) do
        char = buffer:getChar()
        --buffer:skipChar()

        if char == 64 then -- @
            table.insert(token.value, stringValue)
            stringValue = nil
            buffer:undoChar()
            table.insert(token.value, self:parseMacro(buffer))
        else
            stringValue = (stringValue or "") .. self:getChar(char, buffer)
        end

        char = buffer:peekChar()
    end

    if stringValue then
        table.insert(token.value, stringValue)
    end

    return token
end

function Lexer:isNumber(buffer)
    local char = buffer:peekChar()
    local next = buffer:peekChar(2)
    if isNumeric(char) then
        --check if the first 2 chars are valid number chars, bit hacky, don't use numbers at the start of a  word...
        return not next or isWhiteSpace(next) or isNumeric(next) or (
                char == 48 --"0"
                and (
                    next == 102 or next == 88 --x or X
                    or next == 100 or next == 68 -- d or D
                    or next == 98 or next == 66 -- b or B
                ) and isNumeric(buffer:peekChar(3))
            ) or (
                next == 46 -- .
                and isNumeric(buffer:peekChar(3))
            )
    elseif char == 45  then -- -
        return isNumeric(next)
    else
        return false
    end
end

function Lexer:parseNumber(buffer)
    local token = Token:new(tokenType.number)
    local char = buffer:getChar()
    token.value = 0

    local negative = false

    if char == 45 or char == 43 then -- - or +
        negative = char == 45
        char = buffer:getChar()
    end

    local base = 10
    local numberType = "d"

    if char == 48 and isAlpha(buffer:peekChar()) then
        numberType = self:getChar(nil, buffer)
        char = buffer:getChar()
    end

    if numberType == "d" then
    elseif numberType == "x" then
        base = 16
    elseif numberType == "b" then
        base = 2
    else
        self:error("Invalid base", string.byte(numberType), buffer)
    end
    local finalShift = 0
    local noPeek = false
    while char and not isWhiteSpace(char) do
        if noPeek then
            buffer:skipChar()
        end

        local value = ({
            [48] = 0,
            [49] = 1,
            [50] = 2,
            [51] = 3,
            [52] = 4,
            [53] = 5,
            [54] = 6,
            [55] = 7,
            [56] = 8,
            [57] = 9,
            [97] = 10, --a
            [65] = 10, --A
            [98] = 11, --b
            [66] = 11, --B
            [99] = 12, --c
            [67] = 12, --C
            [100] = 13,--d
            [68] = 13,--D
            [101] = 14,--e
            [69] = 14,--E
            [102] = 15, --f
            [70] = 15 --F
        })[char]

        if char == 46 then -- .
            finalShift = 1
        elseif type(value) == "nil" then
            self:error("Invalid number", char, buffer)
        end

        if finalShift > 0 then
            finalShift = finalShift + 1
        end

        if value then
            token.value = token.value * base + value
        end

        char = buffer:peekChar()
        noPeek = true
    end

    if not noPeek then
        self:error("Invalid number (premature end)", char, buffer)
    end
    

    if negative then
        token.value = -token.value
    end

    if finalShift > 0 then
        token.value = token.value / (math.pow(base, finalShift-2))
    end

    return token
end

function Lexer:parseMacro(buffer)
    local char = buffer:peekChar()
    local token = Token:new(tokenType.macro)

    token.stack = 0

    while char == 64 do --@
        token.stack = token.stack + 1
        buffer:skipChar()
        char = buffer:peekChar()
    end

    if token.stack == 0 then
        self:error("Expecting start of macro", char, buffer)
    end

    if char == 91 then -- [
        token.value = self:parseString(buffer)
    elseif char == 40 then -- (
        token.value = self:parseCall(buffer)
    else
        token.value = ""
        char = buffer:peekChar()

        while char and isAlphaNumeric(char) do
            char = buffer:getChar()

            token.value = token.value .. self:getChar(char, buffer)

            char = buffer:peekChar()
        end
    end

    return token
end

function Lexer:parseCall(buffer)
    --TODO: macros!
    local open = buffer:getChar()
    assert(open == 40) --(

    local token = Token:new(tokenType.call)
    token.value = ""
    local stack = 1
    local char = buffer:getChar()
    while char do
        if char == 40 then -- (
            stack = stack + 1
        elseif char == 41 then -- )
            stack = stack - 1

            if stack == 0 then
                break
            end
        end

        token.value = token.value .. self:getChar(char, buffer)

        char = buffer:getChar()
    end

    if char ~= 41 then -- )
        self:error("Expected closing )", char, buffer)
    end

    return token
end

Lexer.operators = {
    ["61"] = "=",
}
function Lexer:isOperator(buffer)
    local i = 1
    local char = buffer:peekChar(i)
    local op = {}

    while char and not isWhiteSpace(char) do
        op[#op + 1] = tostring(char)
        i = i + 1
        char = buffer:peekChar(i)
    end
    
    op = table.concat(op, "-")

    return self.operators[op]
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
    assert(self:isSeperator(seperator))
    return Token:new(tokenType.seperator)
end

function Lexer:parseComment(buffer)
    local char = buffer:getChar()

    local token = Token:new(tokenType.comment)
    token.value = ""

    if --[[char == "#" or]] char == 47 and buffer:peekChar() == 47 then  -- /
        token.inline = true
        if char == 47 then
            buffer:getChar() -- skip the second slash
        end

        char = buffer:getChar()

        while char and not isLineSeperator(char) do
            token.value = token.value .. self:getChar(char, buffer)
            char = buffer:getChar()
        end
    elseif char == 47 and buffer:peekChar() == 42 then
        buffer:skipChar() -- skip the *
        char = buffer:getChar()

        while char and not (char == "*" and buffer:peekChar() == "/") do
            token.value = token.value .. char
            char = buffer:getChar()
        end
        buffer:skipChar() -- skip the ending /
    else
        self:error ("Malformatted comment", char, buffer)
    end

    return token
end

function Lexer:parseEndOfBuffer()
    return Token:new(tokenType.endOfBuffer)
end

local _valid_var_chars = {
    [33] = true, -- !
    [38] = true, -- &
    [42] = true, -- *
    [43] = true, -- +
    [45] = true, -- -
    [46] = true, -- .
    [47] = true, -- /
    [60] = true, -- <
    [61] = true, -- =
    [62] = true, -- >
    [63] = true, -- ?
    [95] = true, -- _
    [124] = true, -- |
}
function Lexer:validVariableChar(buffer)
    local char = buffer:peekChar()
    return isAlpha(char) or isNumeric(char) or _valid_var_chars[char]
end

function Lexer:lexizeSingleToken(buffer)
    self:skipWhiteSpace(buffer)
    local char = buffer:peekChar()

    if char == 91 or char == 34 then -- [ or "
        return self:parseString(buffer)
    elseif char == 36 then -- $
        return self:parseAlias(buffer)
    elseif char == 64 then -- @
        return self:parseMacro(buffer)
    elseif char == 40 then -- (
        return self:parseCall(buffer)
    elseif char == 47 and (buffer:peekChar(2) == 47 or buffer:peekChar(2) == 42) then -- / and (/ or *)
        return self:parseComment(buffer)
    elseif self:isSeperator(char) then
        return self:parseSeperator(buffer)
    elseif self:isOperator(buffer) then
        return self:parseOperator(buffer)
    elseif self:isNumber(buffer) then
        return self:parseNumber(buffer)
    elseif self:validVariableChar(buffer) then
        return self:parseWord(buffer)
    elseif char then
        return self:error("Unexpected character", char, buffer)
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
        token.file = buffer.file
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
    call = "call",
    lookup = "lookup"
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
        if token and token.type ~= tokenType.seperator and token.type ~= tokenType.endOfBuffer and (token.type ~= tokenType.comment and not token.inline) then
            if token.type ~= tokenType.comment then
                table.insert(statement.arguments, lex:next())
            else
                lex:next() -- skip non inline comments
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
    statement.file = token.file
    table.insert(statement.arguments, token)

    -- Check if argument 1 would be valid in a call
    local nextIsCall = token.type == tokenType.number or token.type == tokenType.word or token.type == tokenType.string or token.type == tokenType.operator or token.type == tokenType.macro or token.type == tokenType.call

    if token.type == tokenType.alias then
        local i = 1
        repeat
            local nextToken = lex:peek(i)
            i = i + 1
            if not nextToken or nextToken.type == tokenType.seperator or nextToken.type ==  tokenType.endOfBuffer then
                nextIsCall = false
                break
            elseif nextToken.type == tokenType.comment then
                --skip
            else
                nextIsCall = true
                break
            end
        until false
    end

    if nextIsCall then -- (= ....)
        if token.type == tokenType.operator then
            token.type = tokenType.word --Convert
        end
        if lex:peek() and lex:peek().type == tokenType.operator then
            local operator = lex:next()
            if operator.value == "=" then
                statement.type = statementType.assignment
            end
        end
        self:parseArguments(lex, statement)
    elseif token.type == tokenType.alias then
        statement.type = statementType.lookup
    else
        p(lex)
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
    local ret = self.trace[self.i]
    self.trace[self.i] = nil
    return ret
end

function Trace:copy()
    local new = Trace:new()
    new.i = self.i
    for i = 1, self.i do
        new.trace[i] = self.trace[i]
    end
    return new
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

function Environment:createTokenLocationTrace(token)
    return tostring(token.file)..":"..tostring(token.line)
end

function Environment:createTraceMessage(msg)
    if type(msg) == "string" then
        return msg
    elseif type(msg) == "table" then
        if msg.argument then
            return self:createTokenLocationTrace(msg.argument).." in argument preprocessing"
        elseif msg.macro then
            return self:createTokenLocationTrace(msg.macro).." in macro preprocessing"
        elseif msg.statement then
            if msg.type == "assignment" then
                return self:createTokenLocationTrace(msg.statement).." in assginment "..tostring(msg.name)
            elseif msg.type == "run.args" then
                return self:createTokenLocationTrace(msg.statement).." in argument calling."
            else
                return self:createTokenLocationTrace(msg.statement).." in call to function "..tostring(msg.name)
            end
        elseif msg.callbackFunction then
            return self:createTokenLocationTrace(msg).. " in callback function"
        elseif msg.globalScope then
            return self:createTokenLocationTrace(msg).. " in global scope"
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

    local traceMessage = trace:pop()
    while traceMessage do
        local v = self:createTraceMessage(traceMessage)
        if type(v) == "table" then
            shift = v
        else
            table.insert(back, v)
        end
        traceMessage = trace:pop()
    end
    err.trace = back
    err.message = err.message .."\n\t"..table.concat(back, "\n\t")
    if self.debug == true then
        err.message = err.message .. "\n\nDebug info: \n"..traceback()
    end
    error(err)
end

function Environment:makeBuffer(string, startLine, fileInfo)
    return Buffer:new(string, startLine, fileInfo)
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

function Environment:executeCallback(callback, scope, trace, meta)
    if type(callback) == "number" then
        return callback --TODO: is this the place where we should put this?
    elseif type(callback) == "string" and type(meta) == "number" then
        local metaArg = rawget(scope, "#args")[meta]

        if metaArg then
            callback = {
                value = callback,
                file = metaArg.file,
                line = metaArg.line
            }
        end
        meta = nil
    elseif type(callback) == "table" and type(meta) == "number" then
        meta = nil -- callback contains the data
    elseif type(meta) == "number" then
        meta = trace.meta
    else
        meta = meta or trace.meta
    end
    
    local info = debug.getinfo(2)
    trace:add({callbackFunction = true, file = info.short_src, line = info.currentline})
        local ret = self:run(callback, scope, trace, meta)
    trace:pop()

    return ret
end

function Environment:run(code, scope, trace, meta)
    if not core.instanceof(Buffer, code) then
        if type(code) == "table" then
            meta = meta or {fileShift = code.file, lineShift = code.line}
            code = code.value
        end
        meta = meta or {}
        code = self:makeBuffer(code, meta.lineShift, meta.fileShift)
    end

    local tokens = self:tokenize(code)
    local statements = self:parse(self:makeTokenStack(tokens))

    if not trace then
        trace = Trace:new()
        trace:add({globalScope = true, file = meta.fileShift, line = meta.lineShift})
    end

    local ret = self:runStatements(statements, scope or self.globalScope, trace)

    return ret
end

function Environment:lookup(value, scope, trace)
    local r
    if not scope[value] then
        p(value, scope[value])
        self:throwError("Unkown alias lookup: "..value, trace)
    elseif type(scope[value]) == "function" then --TODO: seperate getter/setter table function
        local res, err = xpcall(function()
            r = scope[value](self, scope, trace)
        end, traceback)

        if not res then
            self:throwError(tostring(err).."\ncaused by alias lookup (native getter): "..value, trace)
        end
    elseif type(scope[value]) == "table" and type(scope[value].line) == "number" then
        r = scope[value].value
    else
        r = scope[value]
    end
    return r
end

function Environment:runMacros(parent, scope, trace)
    local result = ""

    trace:add({macro = parent})

    local tokens = parent.value
    for k, value in pairs(type(tokens) == "table" and tokens or {tokens}) do
        if type(value) ~= "string" then
            assert(value.type == tokenType.macro, "Value is not a macro")

            -- @[ ] or @( )
            if type(value.value) == "table" then
                assert(value.value.type == tokenType.string or value.value.type == tokenType.call, "Macro table is not string or call")
                if value.value.type == tokenType.string then
                    --Lookup
                    --TODO: more error checking
                    local v = self:runMacros(value.value, scope, trace)
                    value = self:lookup(v, scope, trace)
                else
                    if type(value.value.value) == "string" then
                        value = self:run(value.value.value, scope, trace, {lineShift = parent.line, fileShift = parent.file})
                    else
                        --TODO: run everything
                        p(value)
                        assert(false)
                        value = self:run(value.value.value[1], scope)
                    end
                end

            -- @n
            else
                value = self:lookup(value.value, scope, trace)
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

    local r
    if token.type == tokenType.string then
        r = self:runMacros(token, scope, trace)
    elseif token.type == tokenType.word then
        r = self:runMacros(token, scope, trace)
    elseif token.type == tokenType.number then
        r = token.value
    elseif token.type == tokenType.alias then
        local v = self:runMacros(token, scope, trace)
        r = self:lookup(v, scope, trace)
    elseif token.type == tokenType.call then
        r = self:run(token.value, scope, trace, {fileShift = token.file, lineShift = token.line})
    elseif token.type == tokenType.macro then
        return self:runMacros(token.value, scope, trace)
    else
        p(token)
        error("Unkown token type: "..token.type)
    end

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
            scope.__result__ = val
        elseif statement.type == statementType.lookup then
            trace:add({statement = statement, type = "lookup", name = statement.arguments[1]})
                scope.__result__ = self:runArgument(statement.arguments[1], scope, trace)
            trace:pop()
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

            --Ignore executement of numbers
            if func.type == tokenType.number then
                scope.__result__ = func.value
            else
                local file = func.file
                local line = func.line

                local argumentMeta = {}

                for k, token in pairs(args) do
                    argumentMeta[k] = {type = token.type, file = token.file, line = token.line}
                end

                trace:add({statement = statement, type = "run.args"})
                    func = self:runArgument(func, scope, trace)
                    args = self:runAllArguments(args, scope, trace)
                trace:pop()

                for k, v in ipairs(args) do
                    if type(v) == "table" and v.line then
                        argumentMeta[k].file = v.file
                        argumentMeta[k].line = v.line
                        args[k] = v.value
                    end
                end
                
                rawset(scope, "#args", argumentMeta)

                trace:add({statement = statement, type = "run", name = func})
                    local f = scope[func]
                    if type(f) == "function" then
                        local oldMeta = trace.meta
                        trace.meta = {fileShift = file, lineShift = line}
                        local res, err = xpcall(function()
                            scope.__result__ = ___cut_trace___(f, self, scope, trace, unpack(args))
                            if type(scope.__result__) == "nil" then
                                error("Native command returns nil")
                            end
                        end, traceback)
                        trace.meta = oldMeta

                        if not res then
                            err = tostring(err)
                            err = err:gsub("(in function '___cut_trace___')(.*)", function(a, b) return "in cubescript call to native function: "..func end)
                            err = err:gsub("stack traceback:\n", "")
                            self:throwError(err, trace)
                        end
                    elseif type(f) == "string" or type(f) == "table" then
                        local subScope = makeScope(scope)
                        local lastI = 0
                        for k, v in ipairs(args) do
                            rawset(subScope, "arg"..k, v) --Locals
                            lastI = k
                        end

                        rawset(subScope, "numargs", lastI)

                        for k = lastI + 1, 25 do
                            rawset(subScope, "arg"..k, "")
                        end
                        rawset(subScope, "__result__", "") -- force local

                        scope.__result__ = self:run(f, subScope, trace)
                    elseif type(f) == "number" or type(f) == "boolean" then
                        scope.__result__ = f
                    else
                        self:throwError("Trying to call nonexisting function: "..func, trace)
                    end
                trace:pop()
            end
        else
            error("Unkown statement type")
        end
    end
    
    return scope.__result__
end

function Environment:register(name, cb, scope, override)
    scope = scope or self.globalScope
    if not override and type(scope[name]) ~= "nil" then
        error("Native function overriding already existing function: "..name .. " = ".. tostring(scope[name]))
    end
    scope[name] = cb
end

function Environment:isBool(value)
    return value == "" or value == "1" or value == "0"
end

function Environment:isTrue(value)
    if type(value) == "nil" or value == 0 or value == false or value == "0" or value == "" then
        return false
    else
        return true
    end
end

function Environment:toBool(value)
    if not self:isTrue(value) then
        return "0"
    else
        return "1"
    end
end

function Environment:toNumber(value, default)
    local optional = type(default) ~= "nil"
    if (type(value) == "nil" or value == "") and optional then
        return default
    end

    if type(value) == "number" then
        return value
    else
        value = tostring(value)
        local buffer = self:makeBuffer(value)
        local v = self.lexer:parseNumber(buffer)
        return v.value
    end
end

return {
    Environment = Environment,
    Lexer = Lexer,
    Parser = Parser,
    Buffer = Buffer,
    LexerStack = LexerStack,
    tokenType = tokenType,
    statementType = statementType,
    createEnvironment = function()
        local env = Environment:new()
        env.lexer = Lexer:new()
        env.parser = Parser:new()
        return env
    end,
    makeScope = makeScope
}