let env = "prod"
let debug = false

function registerSocket(app, ws) {
    const debugOnSend = msg => {
        console.log("Out:", msg)
        if (typeof msg.id !== "undefined")
            console.time(msg.id)
        ws.send(JSON.stringify(msg))
    }

    const debugOnMessage = evt => {
        const msg = JSON.parse(evt.data)
        console.log("In:", msg)
        if (typeof msg.id !== "undefined")
            console.timeEnd(msg.id)
        app.ports.rpcSocketIn.send(msg)
    }

    if (debug) {
        ws.onmessage = debugOnMessage
        app.ports.rpcSocketOut.subscribe(debugOnSend)
    } else {
        ws.onmessage = evt => app.ports.rpcSocketIn.send(JSON.parse(evt.data))
        app.ports.rpcSocketOut.subscribe(msg => ws.send(JSON.stringify(msg)))
    }

    ws.onopen = () => app.ports.rpcSocketControl.send(true)
    ws.onclose = () => app.ports.rpcSocketControl.send(false)
}

async function registerWallet(app) {
    const send = (msg) => {
        if (debug) console.log("Wallet In:", msg)
        app.ports.walletIn.send(msg)
    }

    const noMetaMask = () => {
        return typeof window.ethereum === "undefined"
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
                // switch to Polygon, add it if not added.
                try {
                    // try switch first
                    await ethereum.request({
                        method: "wallet_switchEthereumChain",
                        params: [{ chainId: msg.params[0].chainId }],
                    })
                } catch (err) {
                    if (err.code === 4902)
                        // add it then switch
                        // ... and Yes, MetaMask is THIS stupid:
                        msg.params[0].chainId =
                            `0x${msg.params[0].chainId.toString(16)}`
                        await ethereum.request(msg)
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
                    if (err.code === -32603) {
                        send({ type: "tx-underpriced", data: msg.index })
                    } else {
                        send({ type: "tx-rejected", data: msg.index })
                    }
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
    app.ports.openRpcSocket.subscribe(
        (data) => {
            env = data.env
            debug = data.debug
            registerSocket(app, new WebSocket(data.rpc))
            registerWallet(app)
        }
    )
}