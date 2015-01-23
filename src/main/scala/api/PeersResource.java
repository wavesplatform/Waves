package api;

import java.util.List;
import java.util.Map;

import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;

import network.ConnectedPeer;
import network.Peer;

import org.json.simple.JSONArray;
import org.json.simple.JSONObject;

import controller.Controller;
import scala.collection.Iterator;
import scala.collection.mutable.Buffer;

@Path("peers")
@Produces(MediaType.APPLICATION_JSON)
public class PeersResource 
{
	@SuppressWarnings("unchecked")
	@GET
	public String getPeers()
	{
		Iterator<ConnectedPeer> peers = Controller.getActivePeers().toIterator();
		JSONArray array = new JSONArray();
		
		while(peers.hasNext()){
			array.add(peers.next().address().getHostAddress());
		}
		
		return array.toJSONString();
	}
	
	@SuppressWarnings("unchecked")
	@GET
	@Path("height")
	public String getTest()
	{
		Map<ConnectedPeer,Integer> peers = Controller.getPeerHeights();
		JSONArray array = new JSONArray();
		
		for(Map.Entry<ConnectedPeer, Integer> peer: peers.entrySet())
		{
			JSONObject o = new JSONObject();
			o.put("peer", peer.getKey().address().getHostAddress());
			o.put("height", peer.getValue());
			array.add(o);
		}
		
		return array.toJSONString();
	}
}
