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

                  restart()

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

         print("Waiting for host...")
         WebSocketTest();
