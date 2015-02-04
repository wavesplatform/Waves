package api;

/*


import java.security.SecureRandom;
import java.util.Random;

import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;

import org.json.simple.JSONObject;

import scorex.crypto.Base58;

@Path("seed")
@Produces(MediaType.APPLICATION_JSON)
public class SeedResource 
{
	private Random random = new SecureRandom();
	
	@GET
	public String getSeed()
	{
		return this.getSeed(32);
	}
	
	@SuppressWarnings("unchecked")
	@GET
	@Path("/{length}")
	public String getSeed(@PathParam("length") int length)
	{
		byte[] seed = new byte[length];
		this.random.nextBytes(seed);	
		
		JSONObject jsonObject = new JSONObject();
		jsonObject.put("seed", Base58.encode(seed));
		return jsonObject.toJSONString();
	}
}

*/