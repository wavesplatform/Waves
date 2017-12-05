
function print(text) {
    var p = document.createElement("P")
    p.appendChild(document.createTextNode(text))
    document.getElementById("root").appendChild(p)
    //window.scrollTo(0,document.body.scrollHeight);
}

function Init(chainId) {
    $(document).ready(function() {
    var table = $('#' + chainId + 'Table').DataTable({
          paging: false,
          columnDefs: columnDefinitions
        });

        Connect(chainId, table, new PeersInfo())
    });
}

function Connect(chainId, table, peersInfo) {

   if ("WebSocket" in window)
   {
      var ws = new WebSocket("ws://localhost:8080/" + chainId);

      ws.onopen = function() {
         wasConnected = true
         print("WebSocket [" + chainId + "] connected...")
      };

      ws.onmessage = function (evt) {
         var received_msg = evt.data
         var msg = JSON.parse(evt.data);

         for(var peer in msg) {
            peersInfo.addPeerWithPeers(peer, msg[peer])
         }

         var hops = peersInfo.calculateHops()

         table.clear()

         for (var i = 0; i < hops.length; i++){
               table.row.add(hops[i])
         }

         table.draw()

         print("[" + chainId + "]: " + received_msg)
      };

      ws.onclose = function() {
         Connect(chainId, table, peersInfo)
      };

      window.onbeforeunload = function(event) {
         socket.close()
      };
   }
   else {
      print("WebSocket NOT supported by your Browser!");
   }
}

var columnDefinitions = [{
    title: "node",
    targets: 0,
    data: "peer",
    render: function ( data, type, row, meta ) {
      return data
    }
}]

for(var i = 1; i <= 5; i++) {
    columnDefinitions.push({
        title: "hop#" + i,
        targets: i,
        data: i,
        render: function ( data, type, row, meta ) {
          if(!data)
            return 0
          return data.length
        }
    })
}


print("Waiting for hosts...")

Init("w");
Init("t");
