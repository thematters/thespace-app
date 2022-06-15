let env = "prod"
let debug = false
let wsClosed = false
let reconnecting = false

const reconnectTimeout = 2000

function onOpen(app) {
    return () => {
        wsClosed = false
        if (reconnecting) {
            app.ports.rpcSocketControl.send("reconnected")
            reconnecting = false
        } else {
            app.ports.rpcSocketControl.send("opened")
        }
    }
}

function onSend(socket) {
    return msg => {
        if (debug) {
            console.log("Out:", msg)
            if (typeof msg.id !== "undefined") console.time(msg.id)
        }
        socket.send(JSON.stringify(msg))
    }
}

function onMessage(app) {
    return evt => {
        const msg = JSON.parse(evt.data)
        if (debug) {
            console.log("In:", msg)
            if (typeof msg.id !== "undefined") console.timeEnd(msg.id)
        }
        app.ports.rpcSocketIn.send(msg)
    }
}

const fastReconnect = (app, rpc) => {
    try {
        registerSocket(app, rpc)
    } catch (err) {
        app.ports.rpcSocketControl.send("closed")
    }
}

function onClose(app, rpc) {
    return () => {
        wsClosed = true
        if (!reconnecting && document.visibilityState === "visible") {
            reconnecting = true
            app.ports.rpcSocketControl.send("reconnecting")
            setTimeout(registerSocket, reconnectTimeout, app, rpc)
        } else {
            app.ports.rpcSocketControl.send("closed")
        }
    }
}

function registerSocket(app, rpc) {
    var ws = new WebSocket(rpc)
    ws.onopen = onOpen(app)
    ws.onclose = onClose(app, rpc)
    ws.onmessage = onMessage(app)
    app.ports.rpcSocketOut.subscribe(onSend(ws))
}

async function registerWallet(app) {
    const send = (msg) => {
        if (debug) console.log("Wallet In:", msg)
        app.ports.walletIn.send(msg)
    }

    const noMetaMask = () => {
        return typeof window.ethereum === "undefined" || !ethereum.isMetaMask
    }

    const metaMaskLocked = async () => {
        if (typeof window.ethereum._metamask !== "undefined") {
            return !await ethereum._metamask.isUnlocked()
        } else {
            return false
        }
    }

    const _getOrReqAcct = async (method) => {
        const addrs = await ethereum.request({ method: method })
        if (addrs.length === 0) {
            if (await metaMaskLocked()) {
                return { type: "wallet-locked" }
            } else {
                return {
                    type: "wallet",
                    data: {
                        chainId: ethereum.chainId,
                        address: null,
                        balance: null,
                        allowance: null
                    }
                }
            }
        } else {
            return {
                type: "wallet",
                data: {
                    chainId: ethereum.chainId,
                    address: addrs[0],
                    balance: null,
                    allowance: null
                }
            }
        }
    }

    const getAcct = async () => { return _getOrReqAcct("eth_accounts") }

    const reqAcct = async () => { return _getOrReqAcct("eth_requestAccounts") }

    const sendTx = async (msg) => {
        msg.method = "eth_sendTransaction"
        return await ethereum.request(msg)
    }

    const walletHandler = async (msg) => {
        if (debug) console.log("Wallet Out:", msg)

        // refresh app
        if (msg.method === "reinitApp") {
            window.location.reload()
            return
        }

        // check if MetaMask is installed?
        if (noMetaMask()) {
            send({ type: "no-wallet" })
            return
        }

        // check if MetaMask is currently locked?
        if (await metaMaskLocked()) {
            send({ type: "wallet-locked" })
            return
        }

        switch (msg.method) {

            case "wallet_addEthereumChain":
                try {
                    // try switch first
                    await ethereum.request({
                        method: "wallet_switchEthereumChain",
                        params: [{ chainId: msg.params[0].chainId }],
                    })
                } catch (switchErr) {
                    if (switchErr.code === 4902 || switchErr.code === -32603)
                        try {
                            await ethereum.request(msg)
                        } catch (addErr) {
                            // ignore
                        }
                }
                break

            case "wallet_getAccounts":
                // try to get account address and balance,
                // but do not request if got none
                send(await getAcct())
                break

            case "wallet_requestAccounts":
                // request account address and balance
                // prompt user to connect if got none
                send(await reqAcct())
                break

            case "approveAllowance":
                // approve all balance as allowance
                try {
                    const tx = await sendTx(msg)
                    const txRes = await tx
                    // succeed
                    const blcMsg = {
                        method: "eth_call",
                        params: msg.getBalanceParams
                    }
                    const blcTx = await ethereum.request(blcMsg)
                    const blcTxRes = await blcTx
                    send({ type: "allowance", balance: blcTxRes })
                } catch (err) {
                    send({ type: "allowance", balance: null })
                }
                break

            case "setPixel":
                // bid/update pixel
                try {
                    const tx = await sendTx(msg)
                    const txRes = await tx
                    send({ type: "tx-send", data: msg.index })
                } catch (err) {
                    if (err.code === 4001)
                        send({ type: "tx-rejected", data: msg.index })
                }
                break

            case "collectUbi":
                // collect pixel income
                try {
                    const tx = await sendTx(msg)
                    const txRes = await tx
                    send({ type: "income", data: true })
                } catch (err) {
                    send({ type: "income", data: false })
                }
                break

            default:
                break
        }
    }

    if (!noMetaMask()) {
        // register MetaMask account/chain change events
        ethereum.on('accountsChanged', async () => send(await getAcct()))
        ethereum.on('chainChanged', async () => send(await getAcct()))
    }
    app.ports.walletOut.subscribe(walletHandler)
}

export function registerRpc(app) {

    addEventListener('visibilitychange', event => {
        if (wsClosed && document.visibilityState === "visible")
            window.location.reload()
    })

    app.ports.openRpcSocket.subscribe(
        (data) => {
            env = data.env
            debug = data.debug
            registerSocket(app, data.rpc)
            registerWallet(app)
        }
    )
}