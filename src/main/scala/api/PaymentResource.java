package api;

import java.math.BigDecimal;

import javax.ws.rs.Consumes;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;

import org.json.simple.JSONObject;
import org.json.simple.JSONValue;

import scorex.account.Account;
import scorex.account.PrivateKeyAccount;
import scorex.crypto.Crypto;
import scorex.transaction.Transaction;
import utils.Pair;
import controller.Controller;

@Path("payment")
@Produces(MediaType.APPLICATION_JSON)
public class PaymentResource 
{
	@POST
	@Consumes(MediaType.WILDCARD)
	public String createPayment(String x)
	{
		try
		{
			//READ JSON
			JSONObject jsonObject = (JSONObject) JSONValue.parse(x);
			String amount = (String) jsonObject.get("amount");
			String fee = (String) jsonObject.get("fee");
			String sender = (String) jsonObject.get("sender");
			String recipient = (String) jsonObject.get("recipient");
			
			//PARSE AMOUNT
			BigDecimal bdAmount;
			try
			{
				bdAmount = new BigDecimal(amount);
				bdAmount = bdAmount.setScale(8);
			}
			catch(Exception e)
			{
				throw ApiErrorFactory.getInstance().createError(ApiErrorFactory.ERROR_INVALID_AMOUNT);
			}
				
			//PARSE FEE
			BigDecimal bdFee;
			try
			{
				bdFee = new BigDecimal(fee);
				bdFee = bdFee.setScale(8);
			}
			catch(Exception e)
			{
				throw ApiErrorFactory.getInstance().createError(ApiErrorFactory.ERROR_INVALID_FEE);
			}	
				
			//CHECK ADDRESS
			if(!Crypto.isValidAddress(sender))
			{
				throw ApiErrorFactory.getInstance().createError(ApiErrorFactory.ERROR_INVALID_SENDER);
			}
				
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
				
			//GET ACCOUNT
			PrivateKeyAccount account = Controller.getPrivateKeyAccountByAddress(sender).getOrElse(null);
			if(account == null)
			{
				throw ApiErrorFactory.getInstance().createError(ApiErrorFactory.ERROR_INVALID_SENDER);
			}
				
			//SEND PAYMENT
			Pair<Transaction, Integer> result = Controller.sendPayment(account, new Account(recipient), bdAmount, bdFee);
				
			switch(result.getB())
			{
			case Transaction.VALIDATE_OKE:
				
				return result.getA().toJson().toJSONString();
			
			case Transaction.INVALID_NAME_LENGTH:
				
				throw ApiErrorFactory.getInstance().createError(ApiErrorFactory.ERROR_INVALID_NAME_LENGTH);	
			
			case Transaction.INVALID_VALUE_LENGTH:
				
				throw ApiErrorFactory.getInstance().createError(ApiErrorFactory.ERROR_INVALID_VALUE_LENGTH);	
				
			case Transaction.INVALID_ADDRESS:
					
				throw ApiErrorFactory.getInstance().createError(ApiErrorFactory.ERROR_INVALID_RECIPIENT);
					
			case Transaction.NAME_ALREADY_REGISTRED:
				
				throw ApiErrorFactory.getInstance().createError(ApiErrorFactory.ERROR_NAME_ALREADY_EXISTS);
			
			case Transaction.NEGATIVE_FEE:
					
				throw ApiErrorFactory.getInstance().createError(ApiErrorFactory.ERROR_INVALID_FEE);
					
			case Transaction.NO_BALANCE:	
					
				throw ApiErrorFactory.getInstance().createError(ApiErrorFactory.ERROR_NO_BALANCE);
			
			default:
				
				throw ApiErrorFactory.getInstance().createError(ApiErrorFactory.ERROR_UNKNOWN);	
			}
		}
		catch(NullPointerException | ClassCastException e)
		{
			//JSON EXCEPTION
			//e.printStackTrace();
			throw ApiErrorFactory.getInstance().createError(ApiErrorFactory.ERROR_JSON);
		}
	}
}
