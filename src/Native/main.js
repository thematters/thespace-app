import { Elm } from "./elmappesm.js"
import { registerRender } from "./canvas.js"
import { registerRpc } from "./rpc.js"
import { initApp } from "./init.js"


initApp(Elm, registerRender, registerRpc, "#canvas", "#minimap")
