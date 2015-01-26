package api;

import java.nio.charset.StandardCharsets;
import java.util.List;

import javax.ws.rs.Consumes;
import javax.ws.rs.DELETE;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;

import org.json.simple.JSONArray;
import org.json.simple.JSONObject;
import org.json.simple.JSONValue;

import scorex.account.Account;
import scorex.account.PrivateKeyAccount;
import scorex.account.PublicKeyAccount;
import scorex.crypto.Base58;
import scorex.crypto.Crypto;
import controller.Controller;

@Path("addresses")
@Produces(MediaType.APPLICATION_JSON)
public class AddressesResource 
{
	@SuppressWarnings("unchecked")
	@GET
	public String addresses()
	{
		//CHECK IF WALLET EXISTS
		if(!Controller.doesWalletExists())
		{
			throw ApiErrorFactory.getInstance().createError(ApiErrorFactory.ERROR_WALLET_NO_EXISTS);
		}
		
		//GET ACCOUNTS
		List<Account> accounts = Controller.getAccounts();
		
		//CONVERT TO LIST OF ADDRESSES
		JSONArray addresses = new JSONArray();
		for(Account account: accounts)
		{
			addresses.add(account.address());
		}
		
		//RETURN
		return addresses.toJSONString();		
	}
	
	@GET
	@Path("/validate/{address}")
	public String validate(@PathParam("address") String address)
	{
		//CHECK IF VALID ADDRESS
		return String.valueOf(Crypto.isValidAddress(address));
	}
	
	@GET
	@Path("/seed/{address}")
	public String getSeed(@PathParam("address") String address)
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
		
		//CHECK IF VALID ADDRESS
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
		
		byte[] seed = Controller.exportAccountSeed(address).get();
		return Base58.encode(seed);
	}
	
	@GET
	@Path("/new")
	public String generateNewAccount()
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
		
		return Controller.generateNewAccount();
	}
	
	@POST
	@Consumes(MediaType.WILDCARD)
	public String createNewAddress(String x)
	{
		//CHECK IF CONTENT IS EMPTY
		if(x.isEmpty())
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
					
			return Controller.generateNewAccount();
		}
		else
		{
			String seed = x;
			
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
			if(seedBytes == null || seedBytes.length != 32)
			{
				throw ApiErrorFactory.getInstance().createError(ApiErrorFactory.ERROR_INVALID_SEED);
				
			}
					
			//CONVERT TO BYTE
			return Controller.importAccountSeed(seedBytes);
		}
	}
	
	@DELETE
	@Path("/{address}")
	public String deleteAddress(@PathParam("address") String address)
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
				
		//CHECK IF VALID ADDRESS
		if(!Crypto.isValidAddress(address))
		{
			throw ApiErrorFactory.getInstance().createError(ApiErrorFactory.ERROR_INVALID_ADDRESS);
		}
				
		//DELETE
		PrivateKeyAccount account = Controller.getPrivateKeyAccountByAddress(address).get();
		return String.valueOf(Controller.deleteAccount(account));
	}
	
	@GET
	@Path("/generatingbalance/{address}")
	public String getGeneratingBalanceOfAddress(@PathParam("address") String address)
	{
		//CHECK IF VALID ADDRESS
		if(!Crypto.isValidAddress(address))
		{
			throw ApiErrorFactory.getInstance().createError(ApiErrorFactory.ERROR_INVALID_ADDRESS);
		}
		
		return new Account(address).getGeneratingBalance().toPlainString();
	}
	
	@GET
	@Path("balance/{address}")
	public String getGeneratingBalance(@PathParam("address") String address)
	{
		return this.getGeneratingBalance(address, 1);
	}
	
	@GET
	@Path("balance/{address}/{confirmations}")
	public String getGeneratingBalance(@PathParam("address") String address, @PathParam("confirmations") int confirmations)
	{
		//CHECK IF VALID ADDRESS
		if(!Crypto.isValidAddress(address))
		{
			throw ApiErrorFactory.getInstance().createError(ApiErrorFactory.ERROR_INVALID_ADDRESS);
		}
				
		return new Account(address).getBalance(confirmations).toPlainString();
	}
	
	@SuppressWarnings("unchecked")
	@POST
	@Path("sign/{address}")
	public String sign(String x, @PathParam("address") String address)
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
		
		//CHECK IF VALID ADDRESS
		if(!Crypto.isValidAddress(address))
		{
			throw ApiErrorFactory.getInstance().createError(ApiErrorFactory.ERROR_INVALID_ADDRESS);
		}
		
		//GET OWNER
		PrivateKeyAccount account = Controller.getPrivateKeyAccountByAddress(address).get();
		if(account == null)
		{
			throw ApiErrorFactory.getInstance().createError(ApiErrorFactory.ERROR_WALLET_ADDRESS_NO_EXISTS);
		}
		
		JSONObject signatureJSON = new JSONObject();
		signatureJSON.put("message", x);
		signatureJSON.put("publickey", Base58.encode(account.publicKey()));
		signatureJSON.put("signature", Base58.encode(Crypto.sign(account, x.getBytes(StandardCharsets.UTF_8))));
				
		return signatureJSON.toJSONString();
	}
	
	@POST
	@Path("verify/{address}")
	public String verify(String x, @PathParam("address") String address)
	{
		try
		{	
			//READ JSON
			JSONObject jsonObject = (JSONObject) JSONValue.parse(x);		
			String message = (String) jsonObject.get("message");
			String signature = (String) jsonObject.get("signature");
			String publicKey = (String) jsonObject.get("publickey");
			
			//CHECK IF VALID ADDRESS
			if(!Crypto.isValidAddress(address))
			{
				throw ApiErrorFactory.getInstance().createError(ApiErrorFactory.ERROR_INVALID_ADDRESS);
			}
			
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
			
			//DECODE PUBLICKEY
			byte[] publicKeyBytes;
			try
			{
				publicKeyBytes = Base58.decode(publicKey);
			}
			catch(Exception e)
			{
				throw ApiErrorFactory.getInstance().createError(ApiErrorFactory.ERROR_INVALID_PUBLIC_KEY);
			}
			
			PublicKeyAccount account = new PublicKeyAccount(publicKeyBytes);
			
			//CHECK IF ADDRESS MATCHES
			if(!account.address().equals(address))
			{
				return String.valueOf(false);
			}
					
			return String.valueOf(Crypto.verify(publicKeyBytes, signatureBytes, message.getBytes(StandardCharsets.UTF_8)));
		}
		catch(NullPointerException e)
		{
			//JSON EXCEPTION
			throw ApiErrorFactory.getInstance().createError(ApiErrorFactory.ERROR_JSON);
		}
		catch(ClassCastException e)
		{
			//JSON EXCEPTION
			throw ApiErrorFactory.getInstance().createError(ApiErrorFactory.ERROR_JSON);
		}
	}
}
