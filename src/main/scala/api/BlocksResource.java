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

	@GET
	@Path("/child/{signature}")	
	public String getChild(@PathParam("signature") String signature)
	{
		//DECODE SIGNATURE
		byte[] signatureBytes;
		try
		{
			signatureBytes = Base58.decode(signature);
		}
		catch(Exception e)
		{
			throw ApiErrorFactory.getInstance().createError(ApiErrorFactory.ERROR_INVALID_SIGNATURE);
		}
				
		Block block = Controller.getBlock(signatureBytes);
				
		//CHECK IF BLOCK EXISTS
		if(block == null)
		{
			throw ApiErrorFactory.getInstance().createError(ApiErrorFactory.ERROR_BLOCK_NO_EXISTS);
		}
		
		Block child = block.getChild();
		
		//CHECK IF CHILD EXISTS
		if(child == null)
		{
			throw ApiErrorFactory.getInstance().createError(ApiErrorFactory.ERROR_BLOCK_NO_EXISTS);
		}
		
		return child.toJson().toJSONString();
	}
	
	@GET
	@Path("/generatingbalance")
	public String getGeneratingBalance()
	{
		long generatingBalance = Controller.getNextBlockGeneratingBalance();
		return String.valueOf(generatingBalance);
	}
	
	@GET
	@Path("/generatingbalance/{signature}")
	public String getGeneratingBalance(@PathParam("signature") String signature)
	{
		//DECODE SIGNATURE
		byte[] signatureBytes;
		try
		{
			signatureBytes = Base58.decode(signature);
		}
		catch(Exception e)
		{
			throw ApiErrorFactory.getInstance().createError(ApiErrorFactory.ERROR_INVALID_SIGNATURE);
		}
		
		Block block = Controller.getBlock(signatureBytes);
		
		//CHECK IF BLOCK EXISTS
		if(block == null)
		{
			throw ApiErrorFactory.getInstance().createError(ApiErrorFactory.ERROR_BLOCK_NO_EXISTS);
		}
		
		long generatingBalance = Controller.getNextBlockGeneratingBalance(block);
		return String.valueOf(generatingBalance);
	}
	
	@GET
	@Path("/time")
	public String getTimePerBlock()
	{
		Block block = Controller.getLastBlock();
		long timePerBlock = BlockGenerator.getBlockTime(block.generatingBalance());
		return String.valueOf(timePerBlock);
	}
	
	@GET
	@Path("/time/{generatingbalance}")
	public String getTimePerBlock(@PathParam("generating") long generatingbalance)
	{
		long timePerBlock = BlockGenerator.getBlockTime(generatingbalance);
		return String.valueOf(timePerBlock);
	}
	
	@GET
	@Path("/height")
	public String getHeight() 
	{
		return String.valueOf(Controller.getHeight());
	}
	
	@GET
	@Path("/height/{signature}")
	public String getHeight(@PathParam("signature") String signature) 
	{
		//DECODE SIGNATURE
		byte[] signatureBytes;
		try
		{
			signatureBytes = Base58.decode(signature);
		}
		catch(Exception e)
		{
			throw ApiErrorFactory.getInstance().createError(ApiErrorFactory.ERROR_INVALID_SIGNATURE);
		}

		Block block = DBSet.getInstance().getBlockMap().get(signatureBytes);
				
		//CHECK IF BLOCK EXISTS
		if(block == null)
		{
			throw ApiErrorFactory.getInstance().createError(ApiErrorFactory.ERROR_BLOCK_NO_EXISTS);
		}
		
		return String.valueOf(block.getHeight());
	}
	
}
  */