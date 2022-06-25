/* canvas.js: rendering engine port ///////////////////////////////////////////

    elmapp ------ Rendering Commands ------> canvas.js
    elmapp <----- Ack Messages ------------- canvas.js

    rendering commands:
        see Canvas.elm
        and switch statement in ```registerCanvasPort``` function

    ack messages:
        inited      realtime initialisation finished
        pbInited    playback initialisation finished
        tick        playback step finished
*/

// ployfill for createImageBitmap on Safari ///////////////////////////////////

if (!('createImageBitmap' in window)) {
    window.createImageBitmap = async function(data) {
        return new Promise((resolve, reject) => {
            let dataURL
            const canvas = document.createElement('canvas')
            const ctx = canvas.getContext('2d')
            canvas.width = data.width
            canvas.height = data.height
            ctx.putImageData(data, 0, 0)
            dataURL = canvas.toDataURL()
            const img = document.createElement('img')
            img.onload = () => resolve(this)
            img.src = dataURL
        })
    }
}

// constants //////////////////////////////////////////////////////////////////

// speed settings in ms
const speedNormal = 1000
const speed1X = 1000 / 8
const speed2X = speed1X / 2
const speed4X = speed2X / 2

// states /////////////////////////////////////////////////////////////////////

// passed by Elm app canvas port registration and realtime init command
let // elm app ref
    app,
    // map canvas context ref
    ctx,
    // minimap canvas context ref
    mmctx

// passed by realtime init command
let
    // map width/height
    mapW, mapH,
    // color table: colorId (zero based) -> RGB
    RGBs,
    // reverse color table: RGB -> colorId (for reverse time generation)
    reverseRGBs

// map RGBA value array and objects for drawing to canvas
let // realtime map RGBA array
    mapData,
    // starting map data for playback
    playbackInitMapData,
    // playback map RGBA array
    playbackMapData,
    // imageData object for creating the final image (DOM API matching)
    mapImageData,
    // the actual image object drawn to canvas drawImage (DOM API matching)
    mapBitmap

// last tick timestamp
let lastTick
// map need redraw?
let mapNeedRedraw = false
// map image need re-generated?
let mapNeedRegen = false
// currently in playback mode?
let playingBack = false
// image re-generation interval
let mapRegenInterval = speedNormal

// Helpers ////////////////////////////////////////////////////////////////////

function has(x) { return typeof x !== "undefined" }

function cc2rgb(colorcode) { return RGBs[colorcode >> 0] }

function parseArgs(xs) { return Array.from(xs.values()).map(i => i >> 0) }

function updateImageData(data, i, rgb) {
    const idx = i * 4
    data[idx] = rgb >> 16 & 0xFF        // R
    data[idx + 1] = rgb >> 8 & 0xFF     // G
    data[idx + 2] = rgb & 0xFF          // B
    data[idx + 3] = 255                 // A
}

function swapImageData(data, i, rgb) {
    const idx = i * 4
    const old = data[idx] << 16 | data[idx + 1] << 8 | data[idx + 2]
    updateImageData(data, i, rgb)
    return old
}

function setTranslate(t, dx, dy) { t.e = dx; t.f = dy }

function setScale(t, s) { t.a = s; t.d = s }

function updateContext(ctx, w, h) {
    ctx.canvas.width = w
    ctx.canvas.height = h
    ctx.imageSmoothingEnabled = false
}

function changeSpeed(spd) {
    switch (spd) {
        case 1:
            mapRegenInterval = speed1X
            break

        case 2:
            mapRegenInterval = speed2X
            break

        case 4:
            mapRegenInterval = speed4X
            break

        default:
            break
    }
}

// function to redraw map and minimap
function redraw() {
    ctx.save()
    ctx.setTransform(1, 0, 0, 1, 0, 0)
    ctx.clearRect(0, 0, ctx.canvas.width, ctx.canvas.height)
    ctx.restore()
    ctx.drawImage(mapBitmap, 0, 0, mapW, mapH)
    mmctx.drawImage(mapBitmap, 0, 0, mmctx.canvas.width, mmctx.canvas.height)
}

// async update map image
async function asyncUpdateMapBitmap() {
    mapBitmap = await createImageBitmap(mapImageData, 0, 0, mapW, mapH)
    mapNeedRedraw = true
}

// init a off-screen canvas context
function offScreenContext() {
    const osctx = document.createElement('canvas').getContext('2d')
    updateContext(osctx, mapW, mapH)
    return osctx
}

// load image from url to off screen context and call loading callback
function loadImage(imgUrl, callback) {
    let img = new Image()
    img.crossOrigin = "Anonymous"
    img.src = imgUrl
    img.onload = () => {
        let osctx = offScreenContext()
        osctx.drawImage(img, 0, 0, mapW, mapH)
        callback(osctx)
        osctx.canvas.remove()
    }
}

// render loop: all redraw happens here unless force redraw commands invoked
// we ONLY clear mapNeedRedraw/mapNeedRegen flags here
async function tick(timestamp) {
    if (lastTick === undefined) lastTick = timestamp
    if (mapNeedRegen && timestamp - lastTick > mapRegenInterval) {
        await asyncUpdateMapBitmap()
        redraw()
        mapNeedRegen = false
        mapNeedRedraw = false
        if (playingBack) { app.ports.canvasIn.send("tick") }
        lastTick = timestamp
    }
    if (mapNeedRedraw) {
        redraw()
        mapNeedRedraw = false
    }
    window.requestAnimationFrame(tick)
}

// render command handler
async function render(cmd) {
    const args = cmd.split(",")
    switch (args[0]) {

        case "initMapSnapshot":
            // init realtime: map snapshot
            RGBs = args.slice(8, 25).map(i => `0x${i}` >> 0)
            reverseRGBs = RGBs.reduce((acc, v, i) => (acc[v] = i, acc), {})
            var [dx, dy, zoom, wW, wH, mW, mH] = parseArgs(args.slice(1, 8))
            mapW = mW
            mapH = mH
            var trans = ctx.getTransform()
            setTranslate(trans, dx, dy)
            setScale(trans, zoom)
            updateContext(ctx, wW, wH)
            ctx.setTransform(trans)
            loadImage(
                args[24],
                (osctx) => {
                    mapImageData = osctx.getImageData(0, 0, mapW, mapH)
                    mapData = new Uint8ClampedArray(mapImageData.data)
                    mapImageData.data.set(mapData)
                    app.ports.canvasIn.send("initedMapSnapshot")
                })
            break

        case "initLatestColors":
            // init realtime: latest color changes after map snapshot
            if (args.length > 1) {
                for (let i = 1; i < args.length; i += 2) {
                    updateImageData(mapData, args[i] >> 0, cc2rgb(args[i + 1]))
                }
            }
            mapImageData.data.set(mapData)
            asyncUpdateMapBitmap().then(
                () => {
                    app.ports.canvasIn.send("inited")
                    window.requestAnimationFrame(tick)
                }
            )
            break

        case "trans":
            // trans: move / scale / reset
            var [dx, dy, zoom, wW, wH] = parseArgs(args.slice(1, 6))
            var trans = ctx.getTransform()
            setTranslate(trans, dx, dy)
            if (has(zoom)) setScale(trans, zoom)
            if (has(wW) && has(wH)) updateContext(ctx, wW, wH)
            ctx.setTransform(trans)
            mapNeedRedraw = true
            break

        case "update":
            // update realtime
            for (let i = 1; i < args.length; i += 2) {
                updateImageData(mapData, args[i] >> 0, cc2rgb(args[i + 1]))
            }
            if (!playingBack) {
                mapImageData.data.set(mapData)
                mapNeedRegen = true
            }
            break

        case "redrawmm":
            // force redraw minimap
            mmctx.drawImage(
                mapBitmap, 0, 0, mmctx.canvas.width, mmctx.canvas.height)
            break

        case "pbInit":
            // init playback
            loadImage(
                args[1],
                (osctx) => {
                    const tmp = osctx.getImageData(0, 0, mapW, mapH)
                    playbackInitMapData = new Uint8ClampedArray(tmp.data)
                    app.ports.canvasIn.send("pbInited")
                })
            break

        case "pbStart":
            // start playback
            playbackMapData = new Uint8ClampedArray(playbackInitMapData)
            mapImageData.data.set(playbackInitMapData)
            await asyncUpdateMapBitmap()
            if (mapRegenInterval === speedNormal) mapRegenInterval = speed1X
            redraw()
            playingBack = true
            app.ports.canvasIn.send("pbStarted")
            break

        case "pbPlayAgain":
            // jump to start and start playback
            playbackMapData = new Uint8ClampedArray(playbackInitMapData)
            mapImageData.data.set(playbackInitMapData)
            await asyncUpdateMapBitmap()
            redraw()
            app.ports.canvasIn.send("tick")
            break

        case "pbForward":
            // forward playback
            for (let i = 1; i < args.length; i += 2) {
                updateImageData(
                    playbackMapData, args[i] >> 0, cc2rgb(args[i + 1]))
            }
            mapImageData.data.set(playbackMapData)
            mapNeedRegen = true
            break

            // rewind playback
        case "pbRewind":
            args.reverse()
            for (let i = 0; i < args.length - 1; i += 2) {
                updateImageData(
                    playbackMapData, args[i + 1] >> 0, cc2rgb(args[i]))
            }
            mapImageData.data.set(playbackMapData)
            mapNeedRegen = true
            break

        case "pbSpeed":
            // change playback speed
            changeSpeed(args[1] >> 0)
            break

        case "pbEnd":
            // end playback
            mapImageData.data.set(mapData)
            await asyncUpdateMapBitmap()
            mapRegenInterval = speedNormal
            redraw()
            playingBack = false
            break

        case "pbReverse":
            // build reverse timeline
            const timelineBackwards = []
            const tmpMapData = new Uint8ClampedArray(playbackInitMapData)
            for (let i = 1; i < args.length; i += 2) {
                var old = swapImageData(
                    tmpMapData, args[i] >> 0, cc2rgb(args[i + 1]))
                timelineBackwards.push(old == 0 ? 0 : reverseRGBs[old])
            }
            app.ports.canvasIn.send(timelineBackwards.join(","))
            break

        default:
            break
    }
}

// canvas port registration ///////////////////////////////////////////////////

export function registerRender(elmapp, canvas, minimap) {
    app = elmapp
    ctx = canvas.getContext("2d")
    mmctx = minimap.getContext("2d")
    app.ports.canvasOut.subscribe(render)
}