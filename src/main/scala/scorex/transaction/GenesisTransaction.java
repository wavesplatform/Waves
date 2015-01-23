package scorex.transaction;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Arrays;
import java.util.List;

import org.json.simple.JSONObject;

import scorex.account.Account;
import scorex.crypto.Base58;
import scorex.crypto.Crypto;

import com.google.common.primitives.Bytes;
import com.google.common.primitives.Ints;
import com.google.common.primitives.Longs;

import database.DBSet;

public class GenesisTransaction extends Transaction {

	private static final int RECIPIENT_LENGTH = Account.ADDRESS_LENGTH;
	private static final int AMOUNT_LENGTH = 8;
	private static final int BASE_LENGTH = TIMESTAMP_LENGTH + RECIPIENT_LENGTH + AMOUNT_LENGTH;
	
	private Account recipient;
	private BigDecimal amount;
	
	public GenesisTransaction(Account recipient, BigDecimal amount, long timestamp)
	{
		super(GENESIS_TRANSACTION, BigDecimal.ZERO, timestamp, new byte[]{}, generateSignature(recipient, amount, timestamp));
		
		this.recipient = recipient;
		this.amount = amount;
	}

	//GETTERS/SETTERS
	
	@Override
	public byte[] getSignature() {
		
		return generateSignature(this.recipient, this.amount, this.timestamp);
	}
	
	public Account getRecipient()
	{
		return this.recipient;
	}
	
	public BigDecimal getAmount()
	{
		return this.amount;
	}

	//PARSE/CONVERT
	
	public static Transaction Parse(byte[] data) throws Exception{
		
		//CHECK IF WE MATCH BLOCK LENGTH
		if(data.length < BASE_LENGTH)
		{
			throw new Exception("Data does not match block length");
		}
		
		int position = 0;
		
		//READ TIMESTAMP
		byte[] timestampBytes = Arrays.copyOfRange(data, position, position + TIMESTAMP_LENGTH);
		long timestamp = Longs.fromByteArray(timestampBytes);	
		position += TIMESTAMP_LENGTH;
		
		//READ RECIPIENT
		byte[] recipientBytes = Arrays.copyOfRange(data, position, position + RECIPIENT_LENGTH);
		Account recipient = new Account(Base58.encode(recipientBytes));
		position += RECIPIENT_LENGTH;
		
		//READ AMOUNT
		byte[] amountBytes = Arrays.copyOfRange(data, position, position + AMOUNT_LENGTH);
		BigDecimal amount = new BigDecimal(new BigInteger(amountBytes), 8);
		
		return new GenesisTransaction(recipient, amount, timestamp);	
	}	
	
	@SuppressWarnings("unchecked")
	@Override
	public JSONObject toJson() {
		
		//GET BASE
		JSONObject transaction = this.getJsonBase();
		
		//ADD RECIPIENT/AMOUNT
		transaction.put("recipient", this.recipient.getAddress());
		transaction.put("amount", this.amount.toPlainString());
		
		return transaction;	
	}
	
	@Override
	public byte[] toBytes() 
	{
		byte[] data = new byte[0];
		
		//WRITE TYPE
		byte[] typeBytes = Ints.toByteArray(GENESIS_TRANSACTION);
		typeBytes = Bytes.ensureCapacity(typeBytes, TYPE_LENGTH, 0);
		data = Bytes.concat(data, typeBytes);
		
		//WRITE TIMESTAMP
		byte[] timestampBytes = Longs.toByteArray(this.timestamp);
		timestampBytes = Bytes.ensureCapacity(timestampBytes, TIMESTAMP_LENGTH, 0);
		data = Bytes.concat(data, timestampBytes);
		
		//WRITE RECIPIENT
		data = Bytes.concat(data, Base58.decode(this.recipient.getAddress()));
		
		//WRITE AMOUNT
		byte[] amountBytes = this.amount.unscaledValue().toByteArray();
		byte[] fill = new byte[AMOUNT_LENGTH - amountBytes.length];
		amountBytes = Bytes.concat(fill, amountBytes);
		data = Bytes.concat(data, amountBytes);
		
		return data;
	}

	@Override
	public int getDataLength() 
	{
		return TYPE_LENGTH + BASE_LENGTH;
	}

	//VALIDATE
	
	public boolean isSignatureValid()
	{
		byte[] data = new byte[0];
		
		//WRITE TYPE
		byte[] typeBytes = Ints.toByteArray(GENESIS_TRANSACTION);
		typeBytes = Bytes.ensureCapacity(typeBytes, TYPE_LENGTH, 0);
		data = Bytes.concat(data, typeBytes);
		
		//WRITE TIMESTAMP
		byte[] timestampBytes = Longs.toByteArray(this.timestamp);
		timestampBytes = Bytes.ensureCapacity(timestampBytes, TIMESTAMP_LENGTH, 0);
		data = Bytes.concat(data, timestampBytes);
		
		//WRITE RECIPIENT
		data = Bytes.concat(data, Base58.decode(this.recipient.getAddress()));
				
		//WRITE AMOUNT
		byte[] amountBytes = this.amount.unscaledValue().toByteArray();
		byte[] fill = new byte[AMOUNT_LENGTH - amountBytes.length];
		amountBytes = Bytes.concat(fill, amountBytes);
		data = Bytes.concat(data, amountBytes);
		
		//DIGEST
		byte[] digest = Crypto.sha256(data);
		digest = Bytes.concat(digest, digest);
				
		//CHECK IF EQUAL
		return Arrays.equals(digest, this.signature);
	}
	
	@Override
	public int isValid(DBSet db) 
	{	
		//CHECK IF AMOUNT IS POSITIVE
		if(this.amount.compareTo(BigDecimal.ZERO) == -1)
		{
			return NEGATIVE_AMOUNT;
		}
		
		//CHECK IF ADDRESS IS VALID
		if(!Crypto.isValidAddress(this.recipient.getAddress()))
		{
			return INVALID_ADDRESS;
		}
		
		return VALIDATE_OKE;
	}
	
	//PROCESS/ORPHAN
	
	@Override
	public void process(DBSet db) {
		
		//UPDATE BALANCE
		this.recipient.setConfirmedBalance(this.amount, db);
		
		//SET AS REFERENCE
		recipient.setLastReference(this.signature, db);
		
	}
	
	@Override
	public void orphan(DBSet db) 
	{
		//UNDO BALANCE
		this.recipient.setConfirmedBalance(BigDecimal.ZERO, db);
		
		//UNDO REFERENCE
		this.recipient.removeReference(db);
	}
	
	//REST

	private static byte[] generateSignature(Account recipient, BigDecimal amount, long timestamp)
	{
		byte[] data = new byte[0];
		
		//WRITE TYPE
		byte[] typeBytes = Ints.toByteArray(GENESIS_TRANSACTION);
		typeBytes = Bytes.ensureCapacity(typeBytes, TYPE_LENGTH, 0);
		data = Bytes.concat(data, typeBytes);
				
		//WRITE TIMESTAMP
		byte[] timestampBytes = Longs.toByteArray(timestamp);
		timestampBytes = Bytes.ensureCapacity(timestampBytes, TIMESTAMP_LENGTH, 0);
		data = Bytes.concat(data, timestampBytes);
				
		//WRITE RECIPIENT
		data = Bytes.concat(data, Base58.decode(recipient.getAddress()));
				
		//WRITE AMOUNT
		byte[] amountBytes = amount.unscaledValue().toByteArray();
		byte[] fill = new byte[AMOUNT_LENGTH - amountBytes.length];
		amountBytes = Bytes.concat(fill, amountBytes);
		data = Bytes.concat(data, amountBytes);
		
		//DIGEST
		byte[] digest = Crypto.sha256(data);
		digest = Bytes.concat(digest, digest);
				
		return digest;
	}
	
	@Override
	public Account getCreator()
	{
		return null;
	}
	
	@Override
	public List<Account> getInvolvedAccounts()
	{
		return Arrays.asList(this.recipient);
	}

	@Override
	public boolean isInvolved(Account account) 
	{	
		return this.recipient.getAddress().equals(account.getAddress());		
	}

	@Override
	public BigDecimal getAmount(Account account) 
	{		
		if(this.recipient.getAddress().equals(account.getAddress()))
		{
			return this.amount;
		}
		
		return BigDecimal.ZERO;
	}
}
