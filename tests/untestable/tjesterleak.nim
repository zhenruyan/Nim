import jester, asyncdispatch, os, winlean
from strutils import formatSize

const testHtml = """
<html>
<head>
<script>

var counter = 0;

function removeImg(id) {
    var req = new XMLHttpRequest();
    req.open("GET", "/delimg/" + id);
    req.send();
}

function genNextImage() {
    //removeImg(counter);
    ++counter;
    if (counter > 10) return;
    var req = new XMLHttpRequest();
    req.addEventListener("load", function(){
        getNextFile();
    });
    req.open("GET", "/genimg/" + counter);
    req.send();
}

function getNextFile() {
    document.getElementById("myImg").src = "img" + counter + ".png";
    document.getElementById("req").innerHTML = "Requests made: " + counter;
    setTimeout(genNextImage, 10);
}

setTimeout(function(){
    genNextImage();
}, 500);

</script>
</head>
<body>
<span id="req"></span>
<img id="myImg"></img>
</body>
</html>
"""

let settings = newSettings(staticDir = ".")

var counter = 0

routes:
    get "/":
        resp Http200, testHtml

    get "/genimg/@id":
        #dumpNumberOfInstances()
        #copyFile("img.png", "img" & @"id" & ".png")
        inc counter
        #if counter > 30: quit(1)

        echo "total Mem ", formatSize getTotalMem(), " occupied Mem ", formatSize getOccupiedMem(), " free mem ", formatSize getFreeMem()
        for i in 0 ..< traceLen:
          let p = cast[PCustomOverlapped](traceA[i][0])
          echo cast[int](p.internal), " ", cast[int](p.internalHigh)
          echo "state ", traceA[i][1], " ", p[]
        resp Http200, "ok"

    get "/delimg/@id":
        removeFile("img" & @"id" & ".png")
        resp Http200, "ok"

runForever()
