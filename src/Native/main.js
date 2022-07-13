import { Elm } from "./elmappesm.js"
import { initCanvas } from "./canvas.js"
import { initRpc } from "./rpc.js"
import { initApp } from "./init.js"


initApp(Elm, initCanvas, initRpc, "#canvas", "#minimap")
