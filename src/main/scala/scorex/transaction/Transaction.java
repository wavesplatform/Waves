package scorex.transaction;

import java.math.BigDecimal;
import java.math.MathContext;
import java.util.Arrays;
import java.util.List;

import org.json.simple.JSONObject;

import database.DBSet;
import scorex.account.Account;
import scorex.crypto.Base58;
import settings.Settings;

public abstract class Transaction {
	
	//VALIDATION CODE
	public static final int VALIDATE_OKE = 1;
	public static final int INVALID_ADDRESS = 2;
	public static final int NEGATIVE_AMOUNT = 3;
	public static final int NEGATIVE_FEE = 4;
	public static final int NO_BALANCE = 5;
	public static final int INVALID_REFERENCE = 6;
	
	public static final int INVALID_NAME_LENGTH = 7;
	public static final int INVALID_VALUE_LENGTH = 8;
	public static final int NAME_ALREADY_REGISTRED = 9;

	
	//TYPES
	public static final int GENESIS_TRANSACTION = 1;
	public static final int PAYMENT_TRANSACTION = 2;
	
	//MINIMUM FEE
	public static final BigDecimal MINIMUM_FEE = BigDecimal.ONE;

	//PROPERTIES LENGTH
	protected static final int TYPE_LENGTH = 4;
	public static final int TIMESTAMP_LENGTH = 8;

	protected byte[] reference;
	protected BigDecimal fee;
	protected int type;
	protected byte[] signature;
	protected long timestamp;
	
	protected Transaction(int type, BigDecimal fee, long timestamp, byte[] reference, byte[] signature)
	{
		this.fee = fee;
		this.type = type;
		this.signature = signature;
		this.timestamp = timestamp;
		this.reference = reference;
	}
	
	//GETTERS/SETTERS
	
	public int getType()
	{
		return this.type;
	}
	
	public long getTimestamp()
	{
		return this.timestamp;
	}
	
	public long getDeadline()
	{
		//24HOUR DEADLINE TO INCLUDE TRANSACTION IN BLOCK
		return this.timestamp + (1000*60*60*24);
	}
	
	public BigDecimal getFee()
	{
		return this.fee;
	}
	
	public byte[] getSignature()
	{
		return this.signature;
	}
	
	public byte[] getReference()
	{
		return this.reference;
	}
	
	public BigDecimal feePerByte()
	{
		return this.fee.divide(new BigDecimal(this.getDataLength()), MathContext.DECIMAL32);
	}
	
	public boolean hasMinimumFee()
	{
		return this.fee.compareTo(MINIMUM_FEE) >= 0;
	}
	
	public boolean hasMinimumFeePerByte()
	{
		BigDecimal minFeePerByte = BigDecimal.ONE.divide(BigDecimal.valueOf(Settings.getMaxBytePerFee()), MathContext.DECIMAL32);
		
		return this.feePerByte().compareTo(minFeePerByte) >= 0;
	}
	
	//PARSE/CONVERT
	
	@SuppressWarnings("unchecked")
	protected JSONObject getJsonBase()
	{
		JSONObject transaction = new JSONObject();
		
		transaction.put("type", this.type);
		transaction.put("fee", this.fee.toPlainString());
		transaction.put("timestamp", this.timestamp);
		transaction.put("reference", Base58.encode(this.reference));
		transaction.put("signature", Base58.encode(this.signature));
		transaction.put("confirmations", this.getConfirmations());
		
		return transaction;
	}
	
	public abstract JSONObject toJson();
	
	public abstract byte[] toBytes();
	
	public abstract int getDataLength();
	
	//VALIDATE
	
	public abstract boolean isSignatureValid();
	
	public int isValid()
	{
		return this.isValid(DBSet.getInstance());
	}
	
	public abstract int isValid(DBSet db);
	
	//PROCESS/ORPHAN
	
	public void process()
	{
		this.process(DBSet.getInstance());
	}
		
	public abstract void process(DBSet db);

	public void orphan()
	{
		this.orphan(DBSet.getInstance());
	}
	
	public abstract void orphan(DBSet db);
	
	//REST
	
	public abstract Account getCreator();
	
	public abstract List<Account> getInvolvedAccounts();
		
	public abstract boolean isInvolved(Account account);
	
	public abstract BigDecimal getAmount(Account account);
	
	@Override 
	public boolean equals(Object object)
	{
		if(object instanceof Transaction)
		{
			Transaction transaction = (Transaction) object;
			
			return Arrays.equals(this.getSignature(), transaction.getSignature());
		}
		
		return false;
	}

	public boolean isConfirmed()
	{
		return this.isConfirmed(DBSet.getInstance());
	}
	
	public boolean isConfirmed(DBSet db)
	{
		return DBSet.getInstance().getTransactionMap().contains(this);
	}
	
	public int getConfirmations()
	{
		//CHECK IF IN TRANSACTIONDATABASE
		if(DBSet.getInstance().getTransactionMap().contains(this))
		{
			return 0;
		}
		
		//CALCULATE CONFIRMATIONS
		int lastBlockHeight = DBSet.getInstance().getHeightMap().get(DBSet.getInstance().getBlockMap().getLastBlockSignature());
		int transactionBlockHeight = DBSet.getInstance().getHeightMap().get(DBSet.getInstance().getTransactionParentMap().getParent(this.signature));
		
		//RETURN
		return 1 + lastBlockHeight - transactionBlockHeight;
	}

}
