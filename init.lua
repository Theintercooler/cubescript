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

local Buffer = core.Object:extend()

function Buffer:initialize(string)
    if type(string) ~= "string" then
        error("No string was given for argument #1")
    end
    self.i = 0
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

   return self.chars:sub(self.i, self.i)
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

local Lexer = core.Object:extend()

function Lexer:initialize()

end

function Lexer:error(message, wrongChar, buffer)
    local location = buffer.i

    local line = ""

    while not isLineSeperator(buffer:undoChar()) do 
        if not buffer:peekChar() then
            p(buffer:getChar())
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

        if not isWhiteSpace(char) then
            char = buffer:getChar()
            self:error("Expecting end of macro", char, buffer)
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
    [">"] = true
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
    elseif char == "=" then
        return self:parseOperator(buffer)
    elseif char == ";" or char == "\n" then
        return self:parseSeperator(buffer)
    elseif self:validVariableChar(buffer) then
        return self:parseWord(buffer)
    elseif isNumeric(char) then
        return self:parseNumber(buffer)
    elseif char == "/" and (buffer:peekChar(2) == "/" or buffer:peekChar(2) == "*") or char == "#" then
        return self:parseComment(buffer)
    elseif char then
        return self:error("Unexpected character ", char, buffer)
    else
        return self:parseEndOfBuffer()
    end
end

function Lexer:lexize(buffer)
    local tokens = {}

    while buffer:peekChar() do
        table.insert(tokens, self:lexizeSingleToken(buffer))
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
            table.insert(statement.arguments, lex:next())
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
    table.insert(statement.arguments, token)

    if token.type == tokenType.word then
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

local function makeScope(parent)
    return setmetatable({}, {__index = parent})
end


local Environment = core.Object:extend()

function Environment:initialize(scope)
    self.lexer = nil
    self.parser = nil
    self.globalScope = scope or makeScope(nil)
end

function Environment:makeBuffer(string)
    return Buffer:new(string)
end

function Environment:tokenize(buf)
    p ":tokenize"
    return self.lexer:lexize(buf)
end

function Environment:makeTokenStack(tokens)
    return LexerStack:new(tokens)
end

function Environment:parse(stack)
    return self.parser:parse(stack)
end

function Environment:executeCallback(callback, scope)
    return self:run(callback, scope)
end

function Environment:run(code, scope)
    if not core.instanceof(Buffer, code) then
        code = self:makeBuffer(code)
    end
    
    p "got buffer"

    local tokens = self:tokenize(code)
    
    p "tokenizing"
    
    local statements = self:parse(self:makeTokenStack(tokens))
    
    p "running statements"
    
    return self:runStatements(statements, scope or self.globalScope)
end

function Environment:runMacros(tokens, scope)
    local result = ""

    for k, value in pairs(type(tokens) == "table" and tokens or {tokens}) do
        if type(value) ~= "string" then
            assert(value.type == tokenType.macro, "Value is not a macro")

            -- @[ ]
            if type(value.value) == "table" then
                assert(value.value.type == tokenType.string, "Macro table is not string")
                value = self:run(value.value.value[1], scope)

            -- @n
            elseif not scope[value.value] then
                p(value.value)
                error("Invalid value lookup: "..value.value)
            else
                value = scope[value.value]
            end

            p("Lookup", value)

            result = result .. value --TODO better scope lookup 
        else
            result = result .. value
        end
    end
    
    p("after macro run", result)

    return result
end

function Environment:runArgument(token, scope)
    p(token.type)
    if token.type == tokenType.string then
        return self:runMacros(token.value, scope)
    elseif token.type == tokenType.word then
        return self:runMacros(token.value, scope)
    elseif token.type == tokenType.number then
        return token.value
    elseif token.type == tokenType.alias then
        local v = self:runMacros(token.value, scope)

        if not scope[v] then
            error("Unkown alias lookup: "..v)
        end

        return scope[v]
    elseif token.type == tokenType.call then
        p(token.value)
        return self:run(token.value, scope)
    elseif token.type == tokenType.macro then
        p(token, token.value)
        error "macro ?"
    else
        error("Unkown token type: "..token.type)
    end
end

function Environment:runAllArguments(tokens, scope)
    local result = {}

    for k, v in pairs(tokens) do
        result[k] = self:runArgument(v, scope)
    end

    return result
end

function Environment:runStatements(statements, scope)
    for k, statement in pairs(statements) do
        p("Running statement:", statement)
        if not statement then
        elseif statement.type == statementType.assignment then
            p(statement.arguments)
            assert(#statement.arguments == 2, "Too many arguments for assignment operator")

            local var = self:runArgument(statement.arguments[1], scope)
            local val = self:runArgument(statement.arguments[2], scope)

            p(var, "=", val)
            scope[var] = val
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

            p("preargs")
            func = self:runArgument(func, scope)
            p("func")
            args = self:runAllArguments(args, scope)
            p("args")

            if type(scope[func]) == "function" then
                scope.result = scope[func](self, scope, unpack(args))
            elseif type(scope[func]) == "string" then
                local subScope = makeScope(scope)

                for k, v in ipairs(args) do
                    subScope["arg"..k] = v
                end

                scope.result = self:run(scope[func], subScope)
            elseif type(scope[func]) == "number" or type(scope[func]) == "boolean" then
                scope.result = scope[func]
            else
                p(statement, func)
                error("Trying to call nonexisting function: "..func)
            end
        else
            p(statement)
            error("Unkown statement type")
        end
    end
    
    return scope.result
end

function Environment:register(name, cb, scope)
    scope = scope or self.globalScope
    scope[name] = cb
end



-- TEST:
do
    local fs = require "luvit.fs"
    fs.readFile("test.cs", function(err, data)
        if err then error(err) end
        local env = Environment:new()
        env.lexer = Lexer:new()
        env.parser = Parser:new()

        env.globalScope["true"] = true
        
        env:register("echo", print)

        env:register("+", function(env, scope, ...)
            local value = 0

            for k, v in pairs({...}) do
                value = value + tonumber(v) --TODO: use the lexer number parser
            end

            return value
        end)

        env:register("<", function(env, scope, a, b)
            return tonumber(a or 0) < tonumber(b or 0)
        end)

        env:register("if", function(env, scope, a, b, c)
            if a then
                return env:executeCallback(b)
            else
                return env:executeCallback(c)
            end
        end)

        env:register("concat", function(env, scope, ...)
            return table.concat({...}, " ")
        end)

        env:run(data)

        p "done"
    end)
end