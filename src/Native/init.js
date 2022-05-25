import Elm from "../Main.elm"
import { registerRender } from "./canvas.js"
import { registerRpc } from "./rpc.js"

const observer = new MutationObserver(mutations => {
    if (document.querySelector(selector)) {
        resolve(document.querySelector(selector))
        observer.disconnect()
    }
})

const waitElement = (selector) => {
    return new Promise(resolve => {
        if (document.querySelector(selector)) {
            return resolve(document.querySelector(selector))
        }
        observer.observe(
            document.body, { childList: true, subtree: true }
        )
    })
}

const param = (name) => {
    return new URLSearchParams(window.location.search).get(name)
}

const initCenterCell = () => {
    const centerX = param("cx")
    const centerY = param("cy")
    if (centerX !== null && centerY !== null) {
        return { x: centerX >> 0, y: centerY >> 0 }
    } else {
        return null
    }
}

var app = Elm.Main.init({
    node: document.querySelector("main"),
    flags: {
        winW: window.innerWidth,
        winH: window.innerHeight,
        // cx&cy&z for init canvas transform
        centerCell: initCenterCell(),
        zoom: param("z") >> 0
    }
})

// register canvas render and rpc ports
// this will be cleaner using async/await, but uglify-js doesn't agree,
// seems some babel stuff can fix it, but just seems not worth it...
waitElement("#canvas").then(
    cvs => {
        waitElement("#minimap").then(
            mmmap => {
                registerRender(app, cvs, mmmap)
                registerRpc(app)
                observer.disconnect()
            }
        )
    }
)
