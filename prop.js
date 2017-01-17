'use strict'

// prevent navigation; not only drop
document.addEventListener("drop", (e) => e.preventDefault(), false)
document.addEventListener("dragover", (e) =>  e.preventDefault(), false)

require('./output/Main').main()
require('./output/Main').dropIv()



require('./output/Main').testP()
