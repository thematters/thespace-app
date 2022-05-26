import elm from 'rollup-plugin-elm'

export default {
    input: 'src/Native/init.js',
    output: {
        file: `build/app.js`,
        format: 'iife'
    },
    plugins: [
        elm({
            exclude: 'elm_stuff/**',
            compiler: {
                optimize: true,
                debug: false
            }
        })
    ]
}