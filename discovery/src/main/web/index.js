var table

function print(text) {
    var p = document.createElement("P")
    p.appendChild(document.createTextNode(text))
    document.getElementById("root").appendChild(p)
    //window.scrollTo(0,document.body.scrollHeight);
}

function WebSocketTest()
{
   if ("WebSocket" in window)
   {
      var ws = new WebSocket("ws://localhost:8080/");
      var wasConnected = false

      ws.onopen = function()
      {
         wasConnected = true
         print("WebSocket connected...")
      };

      ws.onmessage = function (evt)
      {
         var received_msg = evt.data
         var msg = JSON.parse(evt.data);

         for(var peer in msg) {
            addPeerWithPeers(peer, msg[peer])
         }

         var hops = calculateHops()

         table.clear()

         for (var i = 0; i < hops.length; i++){
               table.row.add(hops[i])
         }

         table.draw()

         $('.label')
           .popup()
         ;

         print(received_msg)
      };

      ws.onclose = function()
      {
         if(!wasConnected) {
           WebSocketTest()
         }
         else {
           print("WebSocket closed.")
         }
      };

      window.onbeforeunload = function(event) {
         socket.close()
      };
   }

   else
   {
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

for(var i = 1; i <= 3; i++) {
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


$(document).ready(function() {
table = $('#example').DataTable({
                                  paging: false,
                                  columnDefs: columnDefinitions
                                });

print("Waiting for host...")
WebSocketTest();
});




