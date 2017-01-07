'use strict'

const exec = require('child_process').exec
const ls = () => {
  exec('ls -l ./', (err,stdout,stderr) => {
    console.log("wise " + stdout)
  })
}