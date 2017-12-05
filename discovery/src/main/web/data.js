Array.prototype.except = function(arr) {
    var self = this.slice()
    return this.filter(x => !arr.includes(x))
}

function isEmpty(obj) {
    for(var key in obj) {
       if(obj.hasOwnProperty(key))
           return false;
    }
    return true;
}

function PeersInfo()
{
   var peerSet = {}
   var peerStats = {}
   var peerHops = {}

   this.peers = function(peer) {
       if(!peerSet[peer])
           return []

       return peerSet[peer]
   }


   this.hopsByLevel = function(peerHops) {
       byLevel = {}
       for(hop in peerHops) {
          var level = peerHops[hop]

          if(!byLevel[level])
            byLevel[level] = []

          byLevel[level].push(hop)
       }

       return byLevel
   }

   this.addPeerWithPeers = function(peer, peers) {
       peerSet[peer] = peers
   }

   this.calculatePeerHops = function(peer, peerHops = {}, depth = 1, max = 1) {

       if(depth > max)
           return peerHops

       if(isEmpty(peerHops)) {
           peerHops = {}
           this.peers(peer).forEach(hop => peerHops[hop] = depth)
       } else {
           for(hop in peerHops) {
               if(peerHops[hop] == depth - 1) {
                   this.peers(hop).filter(p => peerHops[p] ? false : true).forEach(hop => peerHops[hop] = depth)
               }
           }
       }

       return this.calculatePeerHops(peer, peerHops, depth + 1, max)
   }

   this.calculateHops = function() {
       var result = []
       for(peer in peerSet) {
          var byLevel = this.hopsByLevel(this.calculatePeerHops(peer, 0, 1, 6))
          byLevel.peer = peer
          result.push(byLevel)
       }

       return result
   }
}

