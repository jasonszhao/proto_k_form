'use strict'



exports.changeHash = function changeHash (hash) {
    console.log("changeHash called")

    // causes window to scroll to the top
// window.location.hash = '#' + hash

    // doesn't trigger onhashchange
    window.history.pushState(null, null, '#' + hash)  
    

    // so we do it manually
    const e = new HashChangeEvent('hashchange') 
    window.dispatchEvent(e)

    return () => {} // this is necessary, probably to workaround a bug in PureScript
}

exports.getHash = function getHash () {
    return decodeURIComponent(window.location.hash.replace('#', ''))
}

