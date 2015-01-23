package api;

import java.util.HashMap;
import java.util.Map;

import javax.ws.rs.WebApplicationException;
import javax.ws.rs.core.Response;

import org.json.simple.JSONObject;

public class ApiErrorFactory 
{
	//COMMON
	public static final int ERROR_UNKNOWN = 0;
	public static final int ERROR_JSON = 1;
	public static final int ERROR_NO_BALANCE = 2;
	public static final int ERROR_NOT_YET_RELEASED = 3;
	
	//VALIDATION
	public static final int ERROR_INVALID_SIGNATURE = 101;
	public static final int ERROR_INVALID_ADDRESS = 102;
	public static final int ERROR_INVALID_SEED = 103;
	public static final int ERROR_INVALID_AMOUNT = 104;
	public static final int ERROR_INVALID_FEE = 105;
	public static final int ERROR_INVALID_SENDER = 106;
	public static final int ERROR_INVALID_RECIPIENT = 107;
	public static final int ERROR_INVALID_NAME_LENGTH = 108;
	public static final int ERROR_INVALID_VALUE_LENGTH = 109;
	public static final int ERROR_INVALID_NAME_OWNER = 110;
	public static final int ERROR_INVALID_BUYER = 111;
	public static final int ERROR_INVALID_PUBLIC_KEY = 112;
	public static final int ERROR_INVALID_OPTIONS_LENGTH = 113;
	public static final int ERROR_INVALID_OPTION_LENGTH = 114;
	public static final int ERROR_INVALID_DATA = 115;
	public static final int ERROR_INVALID_DATA_LENGTH = 116;
	
	//WALLET
	public static final int ERROR_WALLET_NO_EXISTS = 201;
	public static final int ERROR_WALLET_ADDRESS_NO_EXISTS = 202;
	public static final int ERROR_WALLET_LOCKED = 203;
	public static final int ERROR_WALLET_ALREADY_EXISTS = 204;
	
	//BLOCKS
	public static final int ERROR_BLOCK_NO_EXISTS = 301;
	
	//TRANSACTIONS
	public static final int ERROR_TRANSACTION_NO_EXISTS = 311;
	
	//NAMING
	public static final int ERROR_NAME_NO_EXISTS = 401;
	public static final int ERROR_NAME_ALREADY_EXISTS = 402;
	public static final int ERROR_NAME_ALREADY_FOR_SALE = 403;
	public static final int ERROR_NAME_NOT_LOWER_CASE = 404;
	public static final int ERROR_NAME_SALE_NO_EXISTS = 410;
	public static final int ERROR_BUYER_ALREADY_OWNER = 411;
	
	//POLLS
	public static final int ERROR_POLL_NO_EXISTS = 501;
	public static final int ERROR_POLL_ALREADY_EXISTS = 502;
	public static final int ERROR_DUPLICATE_OPTION = 503;
	public static final int ERROR_POLL_OPTION_NO_EXISTS = 504;
	public static final int ERROR_ALREADY_VOTED_FOR_THAT_OPTION = 505;
	
	private static ApiErrorFactory  instance;
	
	public static ApiErrorFactory  getInstance()
	{
		if(instance == null)
		{
			instance = new ApiErrorFactory();
		}
		
		return instance;
	}
	
	private Map<Integer, String> errorMessages;
	
	public ApiErrorFactory()
	{
		this.errorMessages = new HashMap<Integer, String>();
		
		//COMMON
		this.errorMessages.put(ERROR_UNKNOWN, "unknown error");
		this.errorMessages.put(ERROR_JSON, "failed to parse json message");
		this.errorMessages.put(ERROR_NO_BALANCE, "not enough balance");
		this.errorMessages.put(ERROR_NOT_YET_RELEASED, "that feature is not yet released");
		
		//VALIDATION		
		this.errorMessages.put(ERROR_INVALID_SIGNATURE, "invalid signature");
		this.errorMessages.put(ERROR_INVALID_ADDRESS, "invalid address");
		this.errorMessages.put(ERROR_INVALID_SEED, "invalid seed");
		this.errorMessages.put(ERROR_INVALID_AMOUNT, "invalid amount");
		this.errorMessages.put(ERROR_INVALID_FEE, "invalid fee");
		this.errorMessages.put(ERROR_INVALID_SENDER, "invalid sender");
		this.errorMessages.put(ERROR_INVALID_RECIPIENT, "invalid recipient");
		this.errorMessages.put(ERROR_INVALID_NAME_LENGTH, "invalid name length");
		this.errorMessages.put(ERROR_INVALID_VALUE_LENGTH, "invalid value length");
		this.errorMessages.put(ERROR_INVALID_NAME_OWNER, "invalid name owner");
		this.errorMessages.put(ERROR_INVALID_BUYER, "invalid buyer");
		this.errorMessages.put(ERROR_INVALID_PUBLIC_KEY, "invalid public key");
		this.errorMessages.put(ERROR_INVALID_OPTIONS_LENGTH, "invalid options length");
		this.errorMessages.put(ERROR_INVALID_OPTION_LENGTH, "invalid option length");
		this.errorMessages.put(ERROR_INVALID_DATA, "invalid data");
		this.errorMessages.put(ERROR_INVALID_DATA_LENGTH, "invalid data length");
		
		//WALLET
		this.errorMessages.put(ERROR_WALLET_NO_EXISTS, "wallet does not exist");
		this.errorMessages.put(ERROR_WALLET_ADDRESS_NO_EXISTS, "address does not exist in wallet");
		this.errorMessages.put(ERROR_WALLET_LOCKED, "wallet is locked");
		this.errorMessages.put(ERROR_WALLET_ALREADY_EXISTS, "wallet already exists");
		
		//BLOCK
		this.errorMessages.put(ERROR_BLOCK_NO_EXISTS, "block does not exist");
		
		//TRANSACTIONS
		this.errorMessages.put(ERROR_TRANSACTION_NO_EXISTS, "transactions does not exist");
		
		//NAMING
		this.errorMessages.put(ERROR_NAME_NO_EXISTS, "name does not exist");
		this.errorMessages.put(ERROR_NAME_ALREADY_EXISTS, "name already exists");
		this.errorMessages.put(ERROR_NAME_ALREADY_FOR_SALE, "name already for sale");
		this.errorMessages.put(ERROR_NAME_NOT_LOWER_CASE, "name must be lower case");
		this.errorMessages.put(ERROR_NAME_SALE_NO_EXISTS, "namesale does not exist");
		this.errorMessages.put(ERROR_BUYER_ALREADY_OWNER, "buyer is already owner");
		
		//POLLS
		this.errorMessages.put(ERROR_POLL_NO_EXISTS, "poll does not exist");
		this.errorMessages.put(ERROR_POLL_ALREADY_EXISTS, "poll already exists");
		this.errorMessages.put(ERROR_DUPLICATE_OPTION, "not all options are unique");
		this.errorMessages.put(ERROR_POLL_OPTION_NO_EXISTS, "option does not exist");
		this.errorMessages.put(ERROR_ALREADY_VOTED_FOR_THAT_OPTION, "already voted for that option");
	}
	
	@SuppressWarnings("unchecked")
	public WebApplicationException createError(int error)
	{
		JSONObject jsonObject = new JSONObject();
		
		jsonObject.put("error", error);
		jsonObject.put("message", this.errorMessages.get(error));
		
		return new WebApplicationException(Response.status(Response.Status.BAD_REQUEST).entity(jsonObject.toJSONString()).build());
	}
}
