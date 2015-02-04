package api;

/*
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
			scala.Tuple2<Transaction, scala.Enumeration.Value> result = Controller.sendPayment(account, new Account(recipient), bdAmount, bdFee);
				
			switch(result._2().id()){
				case 1: return result._1().toJson().toString();
				default: throw ApiErrorFactory.getInstance().createError(ApiErrorFactory.ERROR_UNKNOWN);
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
  */