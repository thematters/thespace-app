"use strict"

const fs = require("fs")
const crypto = require("crypto")

const text = fs.readFileSync(process.argv[2])
const md5sum = crypto.createHash("md5").update(text).digest("hex")
const template = `<!DOCTYPE html>
<html>
<head>
    <meta charset="UTF-8">
    <title>TheSpace</title>
    <head>
        <style>
        body {
            margin: 0;
            overflow: hidden;
        }
        </style>
    </head>
<body>
    <main></main>
    <script src="./app.min.js?v=${md5sum}"></script>
    <script async src="https://www.googletagmanager.com/gtag/js?id=G-1E9WY5EQX9"></script>
    <script>window.dataLayer = window.dataLayer || []; function gtag(){dataLayer.push(arguments);} gtag('js', new Date); gtag('config', 'G-1E9WY5EQX9', {page_path: window.location.pathname});</script>
</body>
</html>`

fs.writeFile('public/release.html', template, (err) => {
    if (err) return console.log(err)
    console.log('>>> Writtten html to public/release.html')
})
