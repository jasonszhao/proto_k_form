'use strict'

function changeHash (hash) {
    window.location.hash = '#' + hash
}

exports.changeHash = changeHash 

exports.getHash = function getHash () {
    return window.location.hash.replace('#', '')
}
