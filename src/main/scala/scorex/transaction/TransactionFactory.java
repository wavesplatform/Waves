package scorex.transaction;

import java.util.Arrays;

import com.google.common.primitives.Ints;

public class TransactionFactory {

	private static TransactionFactory instance;
	
	public static TransactionFactory getInstance()
	{
		if(instance == null)
		{
			instance = new TransactionFactory();
		}
		
		return instance;
	}
	
	private TransactionFactory()
	{
		
	}
	
	public Transaction parse(byte[] data) throws Exception
	{
		//READ TYPE
		byte[] typeBytes = Arrays.copyOfRange(data, 0, Transaction.TYPE_LENGTH);
		int type = Ints.fromByteArray(typeBytes);
		
		switch(type)
		{
		case Transaction.GENESIS_TRANSACTION:
					
			//PARSE GENESIS TRANSACTION
			return GenesisTransaction.Parse(Arrays.copyOfRange(data, 4, data.length));
			
		case Transaction.PAYMENT_TRANSACTION:
			
			//PARSE PAYMENT TRANSACTION
			return PaymentTransaction.Parse(Arrays.copyOfRange(data, 4, data.length));
		}

		throw new Exception("Invalid transaction type");
	}
	
}
