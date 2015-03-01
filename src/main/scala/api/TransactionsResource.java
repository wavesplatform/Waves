package api;

/*

@Path("transactions")
@Produces(MediaType.APPLICATION_JSON)
public class TransactionsResource {

	@GET
	public String getTransactions()
	{
		return this.getTransactionsLimited(50);
	}
	
	@GET
	@Path("/{address}")
	public String getTransactions(@PathParam("address") String address)
	{
		return this.getTransactionsLimited(address, 50);
	}
	
	@GET
	@Path("address/{address}")
	public String getTransactionsTwo(@PathParam("address") String address)
	{
		return this.getTransactions(address);
	}
	

	@SuppressWarnings("unchecked")
	@GET
	@Path("address/{address}/limit/{limit}")
	public String getTransactionsLimited(@PathParam("address") String address, @PathParam("limit") int limit)
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
		for(Transaction transaction: Controller.getLastTransactions(account, limit))
		{
			array.add(transaction.toJson());
		}
		
		return array.toJSONString();
	}
	
	@GET
	@Path("signature/{signature}")
	public String getTransactionsBySignature(@PathParam("signature") String signature) throws Exception
	{

		//GET TRANSACTION
		Transaction transaction = Controller.getTransaction(signatureBytes);
		
		//CHECK IF TRANSACTION EXISTS
		if(transaction == null)
		{
			throw ApiErrorFactory.getInstance().createError(ApiErrorFactory.ERROR_TRANSACTION_NO_EXISTS);
		}
		
		return transaction.toJson().toString();
	}
	

	@SuppressWarnings("unchecked")
	@POST
	@Path("/scan")
	public String scanTransactions(String x)
	{
		try
		{
			//READ JSON
			JSONObject jsonObject = (JSONObject) JSONValue.parse(x);
			
			//GET BLOCK
			Block block = null;
			if(jsonObject.containsKey("start"))
			{
				byte[] signatureBytes;
				try
				{
					String signature = (String) jsonObject.get("start");
					signatureBytes = Base58.decode(signature);
				}
				catch(Exception e)
				{
					throw ApiErrorFactory.getInstance().createError(ApiErrorFactory.ERROR_INVALID_SIGNATURE);
				}
						
				block = Controller.getBlock(signatureBytes);
				
				//CHECK IF BLOCK EXISTS
				if(block == null)
				{
					throw ApiErrorFactory.getInstance().createError(ApiErrorFactory.ERROR_BLOCK_NO_EXISTS);
				}	
			}
			
			//CHECK FOR BLOCKLIMIT
			int blockLimit = -1;
			try
			{
				 blockLimit = ((Long) jsonObject.get("blocklimit")).intValue();
			}
			catch(NullPointerException e)
			{
				//OPTION DOES NOT EXIST
			}
			catch(ClassCastException e)
			{
				//JSON EXCEPTION
				throw ApiErrorFactory.getInstance().createError(ApiErrorFactory.ERROR_JSON);
			}
			
			//CHECK FOR TRANSACTIONLIMIT
			int transactionLimit = -1;
			try
			{
				transactionLimit = ((Long) jsonObject.get("transactionlimit")).intValue();
			}
			catch(NullPointerException e)
			{
				//OPTION DOES NOT EXIST
			}
			catch(ClassCastException e)
			{
				//JSON EXCEPTION
				throw ApiErrorFactory.getInstance().createError(ApiErrorFactory.ERROR_JSON);
			}
			
			//CHECK FOR TYPE
			int type = -1;
			try
			{
				 type = ((Long) jsonObject.get("type")).intValue();
			}
			catch(NullPointerException e)
			{
				//OPTION DOES NOT EXIST
			}
			catch(ClassCastException e)
			{
				//JSON EXCEPTION
				throw ApiErrorFactory.getInstance().createError(ApiErrorFactory.ERROR_JSON);
			}
			
			//CHECK FOR SERVICE
			int service = -1;
			try
			{
				 service = ((Long) jsonObject.get("service")).intValue();
			}
			catch(NullPointerException e)
			{
				//OPTION DOES NOT EXIST
			}
			catch(ClassCastException e)
			{
				//JSON EXCEPTION
				throw ApiErrorFactory.getInstance().createError(ApiErrorFactory.ERROR_JSON);
			}
			
			//CHECK FOR ACCOUNT
			Account account = null;
			try
			{
				if(jsonObject.containsKey("address"))
				{
					String address = (String) jsonObject.get("address");
					 
					//CHECK ADDRESS
					if(!Crypto.isValidAddress(address))
					{
						throw ApiErrorFactory.getInstance().createError(ApiErrorFactory.ERROR_INVALID_ADDRESS);
					}
					
					account = new Account(address);
				}
				 
			}
			catch(NullPointerException e)
			{
				//OPTION DOES NOT EXIST
			}
			catch(ClassCastException e)
			{
				//JSON EXCEPTION
				throw ApiErrorFactory.getInstance().createError(ApiErrorFactory.ERROR_JSON);
			}
			
			//SCAN
			Pair<Block, List<Transaction>> result = Controller.scanTransactions(block, blockLimit, transactionLimit, type, service, account);
			
			//CONVERT RESULT TO JSON
			JSONObject json = new JSONObject();
			
			json.put("lastscanned", Base58.encode(result.getA().signature()));
			
			
			if(block != null)
			{
				json.put("amount", result.getA().getHeight() - block.getHeight() + 1);
			}
			else
			{
				json.put("amount", result.getA().getHeight());
			}
			
			JSONArray transactions = new JSONArray();
			for(Transaction transaction: result.getB())
			{
				transactions.add(transaction.toJson());
			}
			json.put("transactions", transactions);
			
			//RETURN
			return json.toJSONString();
		}
		catch(NullPointerException e)
		{
			//JSON EXCEPTION
			e.printStackTrace();
			throw ApiErrorFactory.getInstance().createError(ApiErrorFactory.ERROR_JSON);
		}
		catch(ClassCastException e)
		{
			//JSON EXCEPTION
			e.printStackTrace();
			throw ApiErrorFactory.getInstance().createError(ApiErrorFactory.ERROR_JSON);
		}
	}
}
  */