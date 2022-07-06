const observer = new MutationObserver(mutations => {
    if (document.querySelector(selector)) {
        resolve(document.querySelector(selector))
        observer.disconnect()
    }
})

const waitElement = (selector) => {
    return new Promise(resolve => {
        if (document.querySelector(selector))
            return resolve(document.querySelector(selector))
        observer.observe(document.body, { childList: true, subtree: true })
    })
}

export function initApp(elmModule, renderRegFunc, rpcRegFunc, canvasId, mmapId) {
    const app = elmModule.Main.init({
        node: document.querySelector("main"),
        flags: { winW: window.innerWidth, winH: window.innerHeight }
    })
    // this will be cleaner using async/await, but uglify-js doesn't agree,
    // seems some babel stuff can fix it, but just seems not worth it...
    waitElement(canvasId).then(
        cvs => {
            waitElement(mmapId).then(
                mmap => {
                    renderRegFunc(app, cvs, mmap)
                    rpcRegFunc(app)
                    observer.disconnect()
                }
            )
        }
    )
}