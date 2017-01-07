'use strict'


const eraseDefaultEvent = (e) => {
  e.stopPropagation()
  e.preventDefault()
}

const auvital = (e) => {
  eraseDefaultEvent(e)
  require('./output/Main').dropEv(e)()
}

// init 

const droparea = document.getElementById("droparea")
if (droparea !== undefined) {
  droparea.addEventListener("drop", auvital, false)
  droparea.addEventListener("dragenter", eraseDefaultEvent, false)
  droparea.addEventListener("dragover", eraseDefaultEvent, false)
}

// require('./output/Main').main()