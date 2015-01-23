package utils;

import java.util.AbstractList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import database.DBSet;
import scorex.transaction.Transaction;

public class TransactionsList extends AbstractList<Transaction> 
{
	private List<byte[]> transactionSignatures;
	private Map<byte[], Transaction> transactions;
	
	public TransactionsList(List<byte[]> transactionSignatures)
	{
		this.transactionSignatures = transactionSignatures;
		this.transactions = new HashMap<>();
	}
	
	
	@Override
	public Transaction get(int index) 
	{
		if(!this.transactions.containsKey(this.transactionSignatures.get(index)))
		{
			this.transactions.put(this.transactionSignatures.get(index), DBSet.getInstance().getTransactionMap().get(this.transactionSignatures.get(index)));
		}
		
		return this.transactions.get(this.transactionSignatures.get(index));
	}

	@Override
	public int size() 
	{
		return this.transactionSignatures.size();
	}

}
