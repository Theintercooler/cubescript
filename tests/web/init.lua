local table = require "table"

local cubescript = require "cubescript"
local stdlib = require "cubescript.stdlib"

local url = require "luvit.url"
local querystring = require "luvit.querystring"
local http = require "luvit.http"
local form = require "cubescript.tests.web.form"

local server = http.createServer(function(req, res)
    res:on("error", function(...) p ("response error:", ...); res:destroy() end)
    local reqUrl = url.parse(req.url, true)
    if reqUrl.pathname == "/run" then
        local i = 0
        local dataBuffer = {}
        req:on("data", function(chunk)
            i = i + 1
            dataBuffer[i] = chunk
        end)

        req:on("end", function()
            local buffer = table.concat(dataBuffer, "")
            local data = querystring.parse(buffer)
            local code = tostring(data.code)
            print (req.socket._handle:getpeername().address, "executes", (("%q"):format(code)):gsub("\\\n", "\\n"))

            res:setCode(200)
            res:setHeader("Content-Type", "text/plain")

            local err, data = pcall(function()
                local env = cubescript.createEnvironment()
                for k, v in pairs(stdlib.api) do
                    env:register(k, v)
                end

                env:register("echo", function(env, scope, trace, ...)
                    local args = {...}
                    for k, v in pairs(args) do
                        args[k] = tostring(v)
                    end
                    local v = table.concat(args, " ").."\n"
                    res:write(v)
                    return v
                end, nil, true)

                return env:run({value = code, file = "<form>", line = 1})
            end)

            if data then
                if not err then
                    res:finish(tostring(data))
                else
                    res:finish("Result of last statement: "..tostring(data))
                end
            else
                res:finish("No cubes were hurt during the execution ...")
            end
        end)
    elseif reqUrl.pathname == "/" then
        return res:finish(form(reqUrl.query.script or
[[
a = [
    echo "Hello world" $arg1
    result "Return"
]

a "in cubescript!"
]]
            ))
    else
        res:writeHead(301, {
            location = "/",
        })
        return res:finish("Redirecting ...")
    end
end)
server:listen(8123, "0.0.0.0")

print "Listening on 8123"