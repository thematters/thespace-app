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
const speed1X = 500
const speed2X = 250
const speed4X = 125

// states /////////////////////////////////////////////////////////////////////

// passed by Elm app canvas port registration and realtime init command
let // elm app ref
    app,
    // map width/height
    mapW, mapH,
    // map/minimap canvas context ref
    ctx, mmctx,
    // color table
    RGBAs

// map RGBA value array and objects for drawing on canvas
let // realtime map RGBA array
    mapData,
    // playback map RGBA array
    playbackMapData,
    // backup RGBA array for faster skip to start in playback
    blankMapData,
    // backup RGBA array for faster skip to end in playback
    backupMapData,
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

function cc2rgb(colorcode) { return RGBAs[colorcode >> 0] }

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

function skip(reverse) {
    const skipToBackup = reverse ? backupMapData : blankMapData
    playbackMapData = new Uint8ClampedArray(skipToBackup)
    mapImageData.data.set(playbackMapData)
    mapNeedRegen = true
}

function skipToStart() { skip(false) }

function skipToEnd() { skip(true) }

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


// render command handler
async function render(cmd) {
    const args = cmd.split(",")
    switch (args[0]) {

        // init realtime: map snapshot
        case "initMapSnapshot":
            RGBAs = args.slice(8, 25).map(i => `0x${i}` >> 0)
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
                const tmpcvs = document.createElement('canvas')
                const tmpctx = tmpcvs.getContext('2d')
                tmpctx.canvas.width = mW
                tmpctx.canvas.height = mH
                tmpctx.imageSmoothingEnabled = false
                tmpctx.drawImage(img, 0, 0, mW, mH)
                mapImageData = tmpctx.getImageData(0, 0, mW, mH)
                mapData = new Uint8ClampedArray(mapImageData.data)
                mapImageData.data.set(mapData)
                tmpcvs.remove()
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
            const size = mapW * mapH * 4
            backupMapData = new Uint8ClampedArray(mapData)
            playbackMapData = new Uint8ClampedArray(size)
            initPlaybackMapDataToBlankMap()
            blankMapData = new Uint8ClampedArray(playbackMapData)
            app.ports.canvasIn.send("pbInited")
            break

        // start playback
        case "pbStart":
            if (!playingBack) {
                mapImageData.data.set(playbackMapData)
                await asyncUpdateMapBitmap()
                playingBack = true
                if (mapRegenInterval === speedNormal) {
                    mapRegenInterval = speed1X
                }
                redraw()
            }
            app.ports.canvasIn.send("tick")
            break

        // skip to start of playback then start again
        case "pbStartAgain":
            playbackMapData = new Uint8ClampedArray(blankMapData)
            mapImageData.data.set(playbackMapData)
            mapNeedRegen = true
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
                updateImageData(playbackMapData, args[i+1]>>0, cc2rgb(args[i]))
            }
            mapImageData.data.set(playbackMapData)
            mapNeedRegen = true
            break

        // change playback speed
        case "pbSpeed":
            changeSpeed(args[1]>>0)
            break

        // skip to start of playback
        case "pbSkipToStart":
            skipToStart()
            break

        // skip to end of playback
        case "pbSkipToEnd":
            skipToEnd()
            break

        // end playback
        case "pbEnd":
            mapImageData.data.set(mapData)
            await asyncUpdateMapBitmap()
            mapRegenInterval = speedNormal
            redraw()
            playingBack = false
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