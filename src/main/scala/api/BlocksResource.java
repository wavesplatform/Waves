package api;

/*
import java.util.List;

import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;

import org.json.simple.JSONArray;

import scorex.BlockGenerator;
import scorex.account.Account;
import scorex.block.Block;
import scorex.block.GenesisBlock;
import scorex.crypto.Base58;
import scorex.crypto.Crypto;
import utils.Pair;
import controller.Controller;
import database.DBSet;

@Path("blocks")
@Produces(MediaType.APPLICATION_JSON)
public class BlocksResource 
{

	@SuppressWarnings("unchecked")
	@GET
	@Path("/address/{address}")	
	public String getBlocks(@PathParam("address") String address)
	{
		//CHECK IF WALLET EXISTS
		if(!Controller.doesWalletExists())
		{
			throw ApiErrorFactory.getInstance().createError(ApiErrorFactory.ERROR_WALLET_NO_EXISTS);
		}
				
		//CHECK ADDRESS
		if(!Crypto.isValidAddress(address))
		{
			throw ApiErrorFactory.getInstance().createError(ApiErrorFactory.ERROR_INVALID_ADDRESS);
		}
				
		//CHECK ACCOUNT IN WALLET
		Account account = Controller.getAccountByAddress(address);
		if(account == null)
		{
			throw ApiErrorFactory.getInstance().createError(ApiErrorFactory.ERROR_WALLET_ADDRESS_NO_EXISTS);
		}
		
		JSONArray array = new JSONArray();
		for(Block block: Controller.getLastBlocks(account))
		{
			array.add(block.toJson());
		}
		
		return array.toJSONString();
	}

	
}
  */