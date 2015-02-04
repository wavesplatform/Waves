package api;

/*
import javax.ws.rs.Consumes;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;

import org.json.simple.JSONObject;
import org.json.simple.JSONValue;

import scorex.crypto.Base58;
import controller.Controller;

@Path("wallet")
@Produces(MediaType.APPLICATION_JSON)
public class WalletResource {

	@SuppressWarnings("unchecked")
	@GET
	public String getWallet()
	{
		JSONObject jsonObject = new JSONObject();
		
		jsonObject.put("exists", Controller.doesWalletExists());
		jsonObject.put("isunlocked", Controller.isWalletUnlocked());
		
		return jsonObject.toJSONString();
	}
	
	@GET
	@Path("/seed")
	public String getSeed()
	{
		//CHECK IF WALLET EXISTS
		if(!Controller.doesWalletExists())
		{
			throw ApiErrorFactory.getInstance().createError(ApiErrorFactory.ERROR_WALLET_NO_EXISTS);
		}
		
		//CHECK WALLET UNLOCKED
		if(!Controller.isWalletUnlocked())
		{
			throw ApiErrorFactory.getInstance().createError(ApiErrorFactory.ERROR_WALLET_LOCKED);
		}
				
		byte[] seed = Controller.exportSeed();
		return Base58.encode(seed);
	}
	
	@GET
	@Path("/synchronize")
	public String synchronize()
	{
		//CHECK IF WALLET EXISTS
		if(!Controller.doesWalletExists())
		{
			throw ApiErrorFactory.getInstance().createError(ApiErrorFactory.ERROR_WALLET_NO_EXISTS);
		}
				
		Controller.synchronizeWallet();
		return String.valueOf(true);
	}
	
	@GET
	@Path("/lock")
	public String lock()
	{
		//CHECK IF WALLET EXISTS
		if(!Controller.doesWalletExists())
		{
			throw ApiErrorFactory.getInstance().createError(ApiErrorFactory.ERROR_WALLET_NO_EXISTS);
		}
				
		return String.valueOf(Controller.lockWallet());
	}
	
	@POST 
	@Consumes(MediaType.WILDCARD)
	public String createWallet(String x)
	{
		try
		{
			//READ JSON
			JSONObject jsonObject = (JSONObject) JSONValue.parse(x);
			boolean recover = (Boolean) jsonObject.get("recover");
			String seed = (String) jsonObject.get("seed");
			String password = (String) jsonObject.get("password");
			int amount = ((Long) jsonObject.get("amount")).intValue();
		
			//CHECK IF WALLET EXISTS
			if(Controller.doesWalletExists())
			{
				throw ApiErrorFactory.getInstance().createError(ApiErrorFactory.ERROR_WALLET_ALREADY_EXISTS);
			}
			
			//DECODE SEED
			byte[] seedBytes;
			try
			{
				seedBytes = Base58.decode(seed);
			}
			catch(Exception e)
			{
				throw ApiErrorFactory.getInstance().createError(ApiErrorFactory.ERROR_INVALID_SEED);
			}
					
			//CHECK SEED LENGTH
			if(seedBytes.length != 32)
			{
				throw ApiErrorFactory.getInstance().createError(ApiErrorFactory.ERROR_INVALID_SEED);
			}
			
			//CHECK AMOUNT
			if(amount < 1)
			{
				throw ApiErrorFactory.getInstance().createError(ApiErrorFactory.ERROR_INVALID_AMOUNT);
			}
					
			//CREATE WALLET
			if(recover)
			{
				return String.valueOf(Controller.recoverWallet(seedBytes, password, amount));
			}
			else
			{
				return String.valueOf(Controller.createWallet(seedBytes, password, amount));
			}
		}
		catch(NullPointerException e)
		{
			e.printStackTrace();
			//JSON EXCEPTION
			throw ApiErrorFactory.getInstance().createError(ApiErrorFactory.ERROR_JSON);
		}
		catch(ClassCastException e)
		{
			e.printStackTrace();
			//JSON EXCEPTION
			throw ApiErrorFactory.getInstance().createError(ApiErrorFactory.ERROR_JSON);
		}
	}
	
	@POST
	@Path("/unlock")
	@Consumes(MediaType.WILDCARD)	
	public String unlock(String x)
	{
		String password = x;
		
		//CHECK IF WALLET EXISTS
		if(!Controller.doesWalletExists())
		{
			throw ApiErrorFactory.getInstance().createError(ApiErrorFactory.ERROR_WALLET_NO_EXISTS);
		}
		
		return String.valueOf(Controller.unlockWallet(password));
	}
	
}
*/