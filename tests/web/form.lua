return [[
<!DOCTYPE html>
<html>
    <head>
        <link rel="stylesheet" href="http://s.ibts.me/codemirror/4.1/lib/codemirror.css">
        <link href="http://s.ibts.me/bootstrap/3.1.1/css/bootstrap.min.css" rel="stylesheet">
    </head>
    <body>
        <div class="container">
            <h1>Cubescript runner <small>I CAN HAZ CUBESCRIPT ONLINE<small></h1>
            <form role="form" class="form-horizontal">
                <div class="form-group">
                    <textarea id="codeform">
a = [
    echo "Hello world" $arg1
    result "Return"
]

a "in cubescript!"
                    </textarea>
                </div>
                <div class="form-group">
                    <button type="button" class="btn btn-primary" id="button">Get the cube rolling!</button>
                </div>
            </form>
            <pre id="result"></pre>
        </div>
        <script src="http://s.ibts.me/jquery/jquery-1.11.1.min.js"></script>
        <script src="http://s.ibts.me/bootstrap/3.1.1/3.1.1/js/bootstrap.min.js"></script>
        <script src="http://s.ibts.me/codemirror/4.1/lib/codemirror.js"></script>
        <script>
            var textarea = $("#codeform")[0]
            var editor = CodeMirror.fromTextArea(textarea, {
                lineNumbers: true,
            });

            jQuery(function($)
            {
                $("#button").on("click", function(event)
                {
                    event.preventDefault()
                    var code = editor.getValue()
                    $.post("/run", { "code": code , "request": "yes"}, function(a, b)
                    {
                        $("#result").text(a)
                    })
                })
            })
        </script>
    </body>
</html>
]]