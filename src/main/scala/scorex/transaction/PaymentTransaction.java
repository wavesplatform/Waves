package scorex.transaction;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Arrays;
import java.util.List;

import org.json.simple.JSONObject;

import scorex.account.Account;
import scorex.account.PrivateKeyAccount;
import scorex.account.PublicKeyAccount;
import scorex.crypto.Base58;
import scorex.crypto.Crypto;

import com.google.common.primitives.Bytes;
import com.google.common.primitives.Ints;
import com.google.common.primitives.Longs;

import database.DBSet;

public class PaymentTransaction extends Transaction {

	private static final int REFERENCE_LENGTH = 64;
	private static final int SENDER_LENGTH = 32;
	private static final int RECIPIENT_LENGTH = Account.ADDRESS_LENGTH;
	private static final int AMOUNT_LENGTH = 8;
	private static final int FEE_LENGTH = 8;
	private static final int SIGNATURE_LENGTH = 64;
	private static final int BASE_LENGTH = TIMESTAMP_LENGTH + REFERENCE_LENGTH + SENDER_LENGTH + RECIPIENT_LENGTH + AMOUNT_LENGTH + FEE_LENGTH + SIGNATURE_LENGTH;

	private PublicKeyAccount sender;
	private Account recipient;
	private BigDecimal amount;
	
	public PaymentTransaction(PublicKeyAccount sender, Account recipient, BigDecimal amount, BigDecimal fee, long timestamp, byte[] reference, byte[] signature) 
	{
		super(PAYMENT_TRANSACTION, fee, timestamp, reference, signature);
		
		this.sender = sender;
		this.recipient = recipient;
		this.amount = amount;
	}
	
	//GETTERS/SETTERS
	
	public Account getSender()
	{
		return this.sender;
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
		
		//READ REFERENCE
		byte[] reference = Arrays.copyOfRange(data, position, position + REFERENCE_LENGTH);
		position += REFERENCE_LENGTH;
		
		//READ SENDER
		byte[] senderBytes = Arrays.copyOfRange(data, position, position + SENDER_LENGTH);
		PublicKeyAccount sender = new PublicKeyAccount(senderBytes);
		position += SENDER_LENGTH;
		
		//READ RECIPIENT
		byte[] recipientBytes = Arrays.copyOfRange(data, position, position + RECIPIENT_LENGTH);
		Account recipient = new Account(Base58.encode(recipientBytes));
		position += RECIPIENT_LENGTH;
		
		//READ AMOUNT
		byte[] amountBytes = Arrays.copyOfRange(data, position, position + AMOUNT_LENGTH);
		BigDecimal amount = new BigDecimal(new BigInteger(amountBytes), 8);
		position += AMOUNT_LENGTH;
		
		//READ FEE
		byte[] feeBytes = Arrays.copyOfRange(data, position, position + FEE_LENGTH);
		BigDecimal fee = new BigDecimal(new BigInteger(feeBytes), 8);
		position += FEE_LENGTH;		
		
		//READ SIGNATURE
		byte[] signatureBytes = Arrays.copyOfRange(data, position, position + SIGNATURE_LENGTH);
		
		return new PaymentTransaction(sender, recipient, amount, fee, timestamp, reference, signatureBytes);	
	}	
	
	@SuppressWarnings("unchecked")
	@Override
	public JSONObject toJson() 
	{
		//GET BASE
		JSONObject transaction = this.getJsonBase();
				
		//ADD SENDER/RECIPIENT/AMOUNT
		transaction.put("sender", this.sender.getAddress());
		transaction.put("recipient", this.recipient.getAddress());
		transaction.put("amount", this.amount.toPlainString());
				
		return transaction;	
	}
	
	@Override
	public byte[] toBytes() 
	{
		byte[] data = new byte[0];
		
		//WRITE TYPE
		byte[] typeBytes = Ints.toByteArray(PAYMENT_TRANSACTION);
		typeBytes = Bytes.ensureCapacity(typeBytes, TYPE_LENGTH, 0);
		data = Bytes.concat(data, typeBytes);
		
		//WRITE TIMESTAMP
		byte[] timestampBytes = Longs.toByteArray(this.timestamp);
		timestampBytes = Bytes.ensureCapacity(timestampBytes, TIMESTAMP_LENGTH, 0);
		data = Bytes.concat(data, timestampBytes);
		
		//WRITE REFERENCE
		data = Bytes.concat(data, this.reference);
		
		//WRITE SENDER
		data = Bytes.concat(data , this.sender.getPublicKey());
		
		//WRITE RECIPIENT
		data = Bytes.concat(data, Base58.decode(this.recipient.getAddress()));
		
		//WRITE AMOUNT
		byte[] amountBytes = this.amount.unscaledValue().toByteArray();
		byte[] fill = new byte[AMOUNT_LENGTH - amountBytes.length];
		amountBytes = Bytes.concat(fill, amountBytes);
		data = Bytes.concat(data, amountBytes);
		
		//WRITE FEE
		byte[] feeBytes = this.fee.unscaledValue().toByteArray();
		fill = new byte[FEE_LENGTH - feeBytes.length];
		feeBytes = Bytes.concat(fill, feeBytes);
		data = Bytes.concat(data, feeBytes);

		//SIGNATURE
		data = Bytes.concat(data, this.signature);
		
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
		//CHECK SIGNATURE
		byte[] data = new byte[0];
		
		//WRITE TYPE
		byte[] typeBytes = Ints.toByteArray(PAYMENT_TRANSACTION);
		typeBytes = Bytes.ensureCapacity(typeBytes, TYPE_LENGTH, 0);
		data = Bytes.concat(data, typeBytes);
		
		//WRITE TIMESTAMP
		byte[] timestampBytes = Longs.toByteArray(this.timestamp);
		timestampBytes = Bytes.ensureCapacity(timestampBytes, TIMESTAMP_LENGTH, 0);
		data = Bytes.concat(data, timestampBytes);
		
		//WRITE REFERENCE
		data = Bytes.concat(data, this.reference);
		
		//WRITE SENDER
		data = Bytes.concat(data , this.sender.getPublicKey());
		
		try
		{
			//WRITE RECIPIENT
			data = Bytes.concat(data, Base58.decode(this.recipient.getAddress()));
		}
		catch(Exception e)
		{
			//ERROR DECODING ADDRESS
		}
		
		//WRITE AMOUNT
		byte[] amountBytes = this.amount.unscaledValue().toByteArray();
		byte[] fill = new byte[AMOUNT_LENGTH - amountBytes.length];
		amountBytes = Bytes.concat(fill, amountBytes);
		data = Bytes.concat(data, amountBytes);
		
		//WRITE FEE
		byte[] feeBytes = this.fee.unscaledValue().toByteArray();
		fill = new byte[FEE_LENGTH - feeBytes.length];
		feeBytes = Bytes.concat(fill, feeBytes);
		data = Bytes.concat(data, feeBytes);
				
		return Crypto.verify(this.sender.getPublicKey(), this.signature, data);
	}
	
	@Override
	public int isValid(DBSet db) 
	{
		//CHECK IF RECIPIENT IS VALID ADDRESS
		if(!Crypto.isValidAddress(this.recipient.getAddress()))
		{
			return INVALID_ADDRESS;
		}
		
		//CHECK IF SENDER HAS ENOUGH MONEY
		if(this.sender.getBalance(1, db).compareTo(this.amount.add(this.fee)) == -1)
		{
			return NO_BALANCE;
		}
		
		//CHECK IF REFERENCE IS OKE
		if(!Arrays.equals(this.sender.getLastReference(db), this.reference))
		{
			return INVALID_REFERENCE;
			//todo: causes network stuck if one account sumbits two txs to be included into one block with older tx fee
			//todo: more than newer's one

		}
		
		//CHECK IF AMOUNT IS POSITIVE
		if(this.amount.compareTo(BigDecimal.ZERO) <= 0)
		{
			return NEGATIVE_AMOUNT;
		}
		
		//CHECK IF FEE IS POSITIVE
		if(this.fee.compareTo(BigDecimal.ZERO) <= 0)
		{
			return NEGATIVE_FEE;
		}
		
		return VALIDATE_OKE;
	}

	//PROCESS/ORPHAN
	
	@Override
	public void process(DBSet db) 
	{
		//UPDATE SENDER
		this.sender.setConfirmedBalance(this.sender.getConfirmedBalance(db).subtract(this.amount).subtract(this.fee), db);
						
		//UPDATE RECIPIENT
		this.recipient.setConfirmedBalance(this.recipient.getConfirmedBalance(db).add(this.amount), db);
		
		//UPDATE REFERENCE OF SENDER
		this.sender.setLastReference(this.signature, db);
		
		//UPDATE REFERENCE OF RECIPIENT
		if(Arrays.equals(this.recipient.getLastReference(db), new byte[0]))
		{
			this.recipient.setLastReference(this.signature, db);
		}		
	}

	@Override
	public void orphan(DBSet db) 
	{
		//UPDATE SENDER
		this.sender.setConfirmedBalance(this.sender.getConfirmedBalance(db).add(this.amount).add(this.fee), db);
						
		//UPDATE RECIPIENT
		this.recipient.setConfirmedBalance(this.recipient.getConfirmedBalance(db).subtract(this.amount), db);
		
		//UPDATE REFERENCE OF SENDER
		this.sender.setLastReference(this.reference, db);
		
		///UPDATE REFERENCE OF RECIPIENT
		if(Arrays.equals(this.recipient.getLastReference(db), this.signature))
		{
			this.recipient.removeReference(db);
		}	
	}

	//REST
	
	@Override
	public Account getCreator()
	{
		return this.sender;
	}
	
	@Override
	public List<Account> getInvolvedAccounts()
	{
		return Arrays.asList(this.sender, this.recipient);
	}
	
	@Override
	public boolean isInvolved(Account account) {
		String address = account.getAddress();
		return address.equals(sender.getAddress()) || address.equals(recipient.getAddress());
	}

	@Override
	public BigDecimal getAmount(Account account) 
	{
		String address = account.getAddress();
		
		//CHECK OF BOTH SENDER AND RECIPIENT
		if(address.equals(sender.getAddress()) && address.equals(recipient.getAddress()))
		{
			return BigDecimal.ZERO.setScale(8).subtract(this.fee);
		}
		
		//CHECK IF ONLY SENDER
		if(address.equals(sender.getAddress()))
		{
			return BigDecimal.ZERO.setScale(8).subtract(this.amount).subtract(this.fee);
		}
		
		//CHECK IF ONLY RECIPIENT
		if(address.equals(recipient.getAddress()))
		{
			return this.amount;
		}
		
		return BigDecimal.ZERO;
	}
	
	public static byte[] generateSignature(PrivateKeyAccount sender, Account recipient, BigDecimal amount, BigDecimal fee, long timestamp) 
	{
		return generateSignature(DBSet.getInstance(), sender, recipient, amount, fee, timestamp);
	}
	
	public static byte[] generateSignature(DBSet db, PrivateKeyAccount sender, Account recipient, BigDecimal amount, BigDecimal fee, long timestamp) 
	{
		byte[] data = new byte[0];
		
		//WRITE TYPE
		byte[] typeBytes = Ints.toByteArray(PAYMENT_TRANSACTION);
		typeBytes = Bytes.ensureCapacity(typeBytes, TYPE_LENGTH, 0);
		data = Bytes.concat(data, typeBytes);
		
		//WRITE TIMESTAMP
		byte[] timestampBytes = Longs.toByteArray(timestamp);
		timestampBytes = Bytes.ensureCapacity(timestampBytes, TIMESTAMP_LENGTH, 0);
		data = Bytes.concat(data, timestampBytes);
		
		//WRITE REFERENCE
		data = Bytes.concat(data, sender.getLastReference(db));
		
		//WRITE SENDER
		data = Bytes.concat(data , sender.getPublicKey());
		
		try
		{
			//WRITE RECIPIENT
			data = Bytes.concat(data, Base58.decode(recipient.getAddress()));
		}
		catch(Exception e)
		{
			//ERROR DECODING ADDRESS
		}
		
		//WRITE AMOUNT
		byte[] amountBytes = amount.unscaledValue().toByteArray();
		byte[] fill = new byte[AMOUNT_LENGTH - amountBytes.length];
		amountBytes = Bytes.concat(fill, amountBytes);
		data = Bytes.concat(data, amountBytes);
		
		//WRITE FEE
		byte[] feeBytes = fee.unscaledValue().toByteArray();
		fill = new byte[FEE_LENGTH - feeBytes.length];
		feeBytes = Bytes.concat(fill, feeBytes);
		data = Bytes.concat(data, feeBytes);
		
		//SIGN
		return Crypto.sign(sender, data);
	}

	@Override
	public String toString() {
		return "PaymentTransaction{" +
				"sender=" + sender +
				", recipient=" + recipient +
				", amount=" + amount +
				", fee=" + fee +
				", timestamp=" + timestamp +
				'}';
	}
}
