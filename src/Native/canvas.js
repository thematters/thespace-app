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
    window.createImageBitmap = async function (data) {
        return new Promise((resolve,reject) => {
            let dataURL
            const canvas = document.createElement('canvas')
            const ctx = canvas.getContext('2d')
            canvas.width = data.width
            canvas.height = data.height
            ctx.putImageData(data,0,0)
            dataURL = canvas.toDataURL()
            const img = document.createElement('img')
            img.addEventListener('load',function () { resolve(this) })
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
    // map width/height
    mapW, mapH,
    // map/minimap canvas context ref
    ctx, mmctx,
    // color table
    RGBs,
    // reverse color table: colorCode -> RAB
    reverseRGBs

// map RGBA value array and objects for drawing on canvas
let // realtime map RGBA array
    mapData,
    // starting map data for playback
    playbackInitMapData,
    // playback map RGBA array
    playbackMapData,
    // imageData object for creating the final image (DOM API matching)
    mapImageData,
    // the actual image object drawn to canvas by drawImage API
    mapBitmap

// last tick timestamp
let lastTick
// map need redraw?
let mapNeedRedraw = false
// map image need re-generated?
let mapNeedRegen = false
// playback inited?
let playbackInited = false
// currently in playback mode?
let playingBack = false
// image re-generation interval
let mapRegenInterval = speedNormal

// Helpers ////////////////////////////////////////////////////////////////////

function parseArgs(xs) { return Array.from(xs.values()).map(i => i >> 0) }

function setTranslate(t, dx, dy) {
    t.e = dx
    t.f = dy
}

function setScale(t, s) {
    t.a = s
    t.d = s
}

function updateContext(w, h) {
    ctx.canvas.width = w
    ctx.canvas.height = h
    ctx.imageSmoothingEnabled = false
}

function cc2rgb(colorcode) { return RGBs[colorcode >> 0] }

function swapImageData(data, i, rgb) {
    const idx = i * 4
    const old = data[idx] << 16 | data[idx + 1] << 8 | data[idx + 2]
    updateImageData(data, i, rgb)
    return old
}

function updateImageData(data, i, rgb) {
    const idx = i * 4
    data[idx    ] = rgb >> 16 & 0xFF  // R
    data[idx + 1] = rgb >> 8  & 0xFF  // G
    data[idx + 2] = rgb       & 0xFF  // B
    data[idx + 3] = 255               // A
}

function initImageDataFromColorIdString(s) {
    for (let i = 0; i < s.length; i++)
        { updateImageData(mapData, i, cc2rgb(s.charAt(i))) }
}

function initPlaybackMapDataToBlankMap() {
    for (let i = 0; i < mapW * mapH; i++)
        { updateImageData(playbackMapData, i, 0xffffff) }
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

// render loop
// all redraw happens here unless force redraw commands invoked
// we ONLY clear mapNeedRedraw/mapNeedRegen flags here
async function tick(timestamp) {
    if (lastTick === undefined) { lastTick = timestamp }
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

function LoadImageT(image) {
    const tmpcvs = document.createElement('canvas')
    const tmpctx = tmpcvs.getContext('2d')
    tmpctx.canvas.width = mapW
    tmpctx.canvas.height = mapH
    tmpctx.imageSmoothingEnabled = false
    tmpctx.drawImage(image, 0, 0, mapW, mapH)
    return tmpctx
}

// render command handler
async function render(cmd) {
    const args = cmd.split(",")
    switch (args[0]) {

        // init realtime: map snapshot
        case "initMapSnapshot":
            RGBs = args.slice(8, 25).map(i => `0x${i}` >> 0)
            reverseRGBs = RGBs.reduce((acc, v, i) => (acc[v] = i, acc), {})
            var [dx, dy, zoom, wW, wH, mW, mH] = parseArgs(args.slice(1, 8))
            mapW = mW
            mapH = mH
            var trans = ctx.getTransform()
            setTranslate(trans, dx, dy)
            setScale(trans, zoom)
            updateContext(wW, wH)
            ctx.setTransform(trans)
            let img = new Image()
            img.crossOrigin = "Anonymous"
            img.src = args[24]
            img.onload = () => {
                const tmpctx = LoadImageT(img)
                mapImageData = tmpctx.getImageData(0, 0, mapW, mapH)
                mapData = new Uint8ClampedArray(mapImageData.data)
                mapImageData.data.set(mapData)
                tmpctx.canvas.remove()
                app.ports.canvasIn.send("initedMapSnapshot")
            }
            break

        // init realtime: latest color changes after map snapshot
        case "initLatestColors":
            if ( args.length > 1 ) {
                for (let i = 1; i < args.length; i += 2) {
                    updateImageData(mapData, args[i]>>0, cc2rgb(args[i+1]))
                }
            }
            mapImageData.data.set(mapData)
            asyncUpdateMapBitmap()
                .then(
                    () => {
                        app.ports.canvasIn.send("inited")
                        window.requestAnimationFrame(tick)
                    }
                )
            break            

        // trans: move / scale / reset
        case "trans":
            var [dx, dy, zoom, wW, wH] = parseArgs(args.slice(1, 6))
            var trans = ctx.getTransform()
            // move
            setTranslate(trans, dx, dy)
            // scale
            if (typeof zoom !== "undefined")
                setScale(trans, zoom)
            // reset
            if (typeof wW !== "undefined" && typeof wH !== "undefined")
                updateContext(wW, wH)
            ctx.setTransform(trans)
            mapNeedRedraw = true
            break

        // update realtime
        case "update":
            for (let i = 1; i < args.length; i += 2) {
                updateImageData(mapData, args[i]>>0, cc2rgb(args[i+1]))
            }
            if (!playingBack) {
                mapImageData.data.set(mapData)
                mapNeedRegen = true
            }
            break

        // force redraw minimap
        case "redrawmm":
            mmctx.drawImage(mapBitmap, 0, 0,
                mmctx.canvas.width, mmctx.canvas.height)
            break

        // init playback
        case "pbInit":
            let pbImg = new Image()
            pbImg.crossOrigin = "Anonymous"
            pbImg.src = args[1]
            pbImg.onload = async () => {
                const tmpctx = LoadImageT(pbImg)
                const size = mapW * mapH * 4
                const playbackInitMapImageData = tmpctx.getImageData(0, 0, mapW, mapH)
                playbackInitMapData = 
                    new Uint8ClampedArray(playbackInitMapImageData.data)
                tmpctx.canvas.remove()
                playbackInited = true
                app.ports.canvasIn.send("pbInited")
            }
            break

        // start playback
        case "pbStart":
            playbackMapData = new Uint8ClampedArray(playbackInitMapData)
            mapImageData.data.set(playbackInitMapData)
            await asyncUpdateMapBitmap()
            if (mapRegenInterval === speedNormal) {
                mapRegenInterval = speed1X
            }
            redraw()
            playingBack = true
            app.ports.canvasIn.send("pbStarted")
            break

        // jump to start and start playback
        case "pbPlayAgain":
            playbackMapData = new Uint8ClampedArray(playbackInitMapData)
            mapImageData.data.set(playbackInitMapData)
            await asyncUpdateMapBitmap()
            redraw()
            app.ports.canvasIn.send("tick")
            break

        // forward playback
        case "pbForward":
            for (let i = 1; i < args.length; i += 2) {
                updateImageData(playbackMapData, args[i]>>0, cc2rgb(args[i+1]))
            }
            mapImageData.data.set(playbackMapData)
            mapNeedRegen = true
            break

        // rewind playback
        case "pbRewind":
            args.reverse()
            for (let i = 0; i < args.length - 1; i += 2) {
            // for (let i = 1; i < args.length - 1; i += 2) {
                updateImageData(playbackMapData, args[i+1]>>0, cc2rgb(args[i]))
            }
            mapImageData.data.set(playbackMapData)
            mapNeedRegen = true
            break

        // change playback speed
        case "pbSpeed":
            changeSpeed(args[1]>>0)
            break

        // end playback
        case "pbEnd":
            mapImageData.data.set(mapData)
            await asyncUpdateMapBitmap()
            mapRegenInterval = speedNormal
            redraw()
            playingBack = false
            break

        // build reverse timeline
        case "reverse":
            let reverseTimeline = []
            if (playbackInited) {
                let tmpMapData = new Uint8ClampedArray(playbackInitMapData)
                let reverseTimeline = []
                for (let i = 1; i < args.length; i += 2) {
                    var old = swapImageData(
                        tmpMapData, args[i]>>0, cc2rgb(args[i+1]))
                    reverseTimeline.push(old == 0 ? 0 : reverseRGBs[old])
                }
                app.ports.canvasIn.send(reverseTimeline.join(","))
            }
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