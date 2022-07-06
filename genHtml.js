"use strict"

const fs = require("fs")
const crypto = require("crypto")

const prodJs = "current/prod/app.min.js"
const stagJs = "current/stag/app.min.js"

const js = process.argv[2]

if (js !== prodJs && js !== stagJs) {
    console.log("[Error] Unrecognized env:", js)
    process.exit(1)
}

const isProd = js === prodJs ? true : false

const title = "The Space: World’s First NFT Pixel Art Game Governed by Radical Markets"
const desc = "The Space is the first NFT pixel art game where players own, color, and trade pixels as ERC721 tokens under Harberger Tax and earn a Universal Basic Income."
const keywords = "universal basic income,r/place,graffiti wall,pixel nft,nft pixel art,reddit place,pixel art nft,radical markets,fractional nft,pixel map,harberger tax,dynamic nft,reddit pixel canvas,nft pixel art grid,nft fractionalization,nft fractional ownership,fractionalized nft,online graffiti wall,fractionalized ownership nft,collaborative nft,TheSpace,SpaceDAO,pixels"
const imgUrl = "/shareThumb.jpg?v=1"
const host = isProd ? "app.thespace.game" : "moon.thespace.game"
const robot = isProd ? "" : `
    <meta name="robots" content="noindex, nofollow"/><meta name="googlebot" content="noindex, nofollow"/>`
const seo = isProd ? `
    <meta name="description" content="${desc}"/>
    <meta name="keywords" content="${keywords}"/>
    <meta property="og:title" content="${title}"/>
    <meta property="og:type" content="website"/>
    <meta property="og:image" content="${imgUrl}"/>
    <meta property="og:url" content="https://${host}/"/>
    <meta property="og:description" content="${desc}"/>
    <meta name="twitter:card" content="summary_large_image"/>
    <meta name="twitter:image" content="${imgUrl}"/>
    <meta name="twitter:image:alt" content="${desc}"/>
    <meta name="twitter:site" content="@TheSpace2022"/>
    <meta name="twitter:creator" content="@Mattersw3b"/>` : ""
const md5sum = crypto.createHash("md5").update(fs.readFileSync(js)).digest("hex")
const ga = isProd ? "G-Z5GX99N9XF" : "G-BDVY2DY2QW"

const html = `<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <title>${title}</title>
    <link rel="shortcut icon" href="/favicon.svg" type="image/x-icon" />
    <meta name="viewport" content="width=device-width, initial-scale=0.7">${seo}${robot}
    <style>body{margin:0;overflow:hidden;}</style>
    <script async src="https://www.googletagmanager.com/gtag/js?id=${ga}"></script>
    <script>window.dataLayer=window.dataLayer||[]; function gtag(){dataLayer.push(arguments);} gtag('js', new Date); gtag('config', '${ga}', {page_path: window.location.pathname});</script>
</head>
<body>
    <script src="/app.min.js?v=${md5sum}"></script>
</body>
</html>`

const output = isProd ? "current/prod/index.html" : "current/stag/index.html"

fs.writeFile(output, html, (err) => {
    if (err) return console.log(err)
    console.log('>>> Generated html to:', output)
})
