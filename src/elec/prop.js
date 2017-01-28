'use strict'


// prevent navigation; not only drop
document.addEventListener("drop", (e) => e.preventDefault(), false)
document.addEventListener("dragover", (e) =>  e.preventDefault(), false)

const electron = require('electron').remote
const app = electron.app

require('./output/Main').main(app.getAppPath())()
//require('./output/Main').dropIv()
// cmh.TranslationHelper is deprecated

