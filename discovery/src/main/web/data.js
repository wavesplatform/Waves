Array.prototype.except = function(arr) {
    var self = this.slice()
    return this.filter(x => !arr.includes(x))
}

var peerSet = {}
var peerStats = {}
var peerHops = {}

function peers(peer) {
    if(!peerSet[peer])
        return []

    return peerSet[peer]
}

function addPeerWithPeers(peer, peers) {
    peerSet[peer] = peers
}

function isEmpty(obj) {
    for(var key in obj) {
        if(obj.hasOwnProperty(key))
            return false;
    }
    return true;
}

function calculateHops() {
    var result = []
    for(peer in peerSet) {
       var byLevel = hopsByLevel(calculatePeerHops(peer, 0, 1, 4))
       byLevel.peer = peer
       result.push(byLevel)
    }

    return result
}

function calculatePeerHops(peer, peerHops = {}, depth = 1, max = 1) {

    if(depth > max)
        return peerHops

    if(isEmpty(peerHops)) {
        peerHops = {}
        peers(peer).forEach(hop => peerHops[hop] = depth)
    } else {
        for(hop in peerHops) {
            if(peerHops[hop] == depth - 1) {
                peers(hop).filter(p => peerHops[p] ? false : true).forEach(hop => peerHops[hop] = depth)
            }
        }
    }

    return calculatePeerHops(peer, peerHops, depth + 1, max)
}

function hopsByLevel(peerHops) {
    byLevel = {}
    for(hop in peerHops) {
       var level = peerHops[hop]

       if(!byLevel[level])
         byLevel[level] = []

       byLevel[level].push(hop)
    }

    return byLevel
}

function firstHopPeers(peer) {
    return peers(peer).length
}

