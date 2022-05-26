"use strict"

const fs = require("fs")
const crypto = require("crypto")

const text = fs.readFileSync(process.argv[2])
const md5sum = crypto.createHash("md5").update(text).digest("hex")

const title = "TheSpace App | World’s First NFT Pixel Art Game in Radical Markets"
const description = "The Space is the first NFT pixel art game where players own, color, and trade pixels as ERC721 tokens under Harberger Tax and earn a Universal Basic Income."
const imageUrl = "https://thespace.game/img/thumb.jpg?v=1"
const template = `<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=0.5, maximum-scale=5.0, minimum-scale=0.2">
  <title>${title}</title>
  <meta name="description" content="${description}"/>
  <meta name="keywords" content="universal basic income,r/place,graffiti wall,pixel nft,nft pixel art,reddit place,pixel art nft,radical markets,fractional nft,pixel map,harberger tax,dynamic nft,reddit pixel canvas,nft pixel art grid,nft fractionalization,nft fractional ownership,fractionalized nft,online graffiti wall,fractionalized ownership nft,collaborative nft,TheSpace,SpaceDAO,pixels"/>
  <meta property="og:title" content="${title}"/>
  <meta property="og:type" content="website"/>
  <meta property="og:image" content="${imageUrl}"/>
  <meta property="og:url" content="https://app.thespace.game/"/>
  <meta property="og:description" content="${description}"/>
  <meta name="twitter:card" content="summary_large_image"/>
  <meta name="twitter:image" content="${imageUrl}"/>
  <meta name="twitter:image:alt" content="${description}"/>
  <meta name="twitter:site" content="@TheSpace2022"/>
  <meta name="twitter:creator" content="@Mattersw3b"/>
  <link rel="icon" href="https://thespace.game/favicon.ico" type="image/x-icon" />
  ${process.env.NEXT_PUBLIC_RUNTIME_ENV !== 'production' ? `<meta name="robots" content="noindex, nofollow"/><meta name="googlebot" content="noindex, nofollow"/>` : ''}
  <style>body{margin:0;overflow:hidden;}</style>
</head>
<body>
  <main></main>
  <script src="./app.min.js?v=${md5sum}"></script>
${process.env.NEXT_PUBLIC_GOOGLE_ANALYTICS ? `<script async src="https://www.googletagmanager.com/gtag/js?id=${process.env.NEXT_PUBLIC_GOOGLE_ANALYTICS}"></script><script>window.dataLayer=window.dataLayer||[]; function gtag(){dataLayer.push(arguments);} gtag('js', new Date); gtag('config', '${process.env.NEXT_PUBLIC_GOOGLE_ANALYTICS}', {page_path: window.location.pathname});</script>` : ''}
</body>
</html>`

fs.writeFile('build/index.html', template, (err) => {
    if (err) return console.log(err)
    console.log('>>> Writtten html to build/index.html')
})
