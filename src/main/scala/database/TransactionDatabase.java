package database;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Observable;
import java.util.Observer;
import java.util.TreeMap;

import ntp.NTP;

import org.mapdb.BTreeKeySerializer;
import org.mapdb.DB;

import com.google.common.primitives.UnsignedBytes;

import scorex.transaction.Transaction;
import scorex.transaction.TransactionFactory;
import utils.ObserverMessage;
import utils.TransactionsList;

public class TransactionDatabase extends Observable implements Observer {
	
	private TransactionDatabase parent;
	private DBSet databaseSet;	
	private Map<byte[], byte[]> transactionMap;	
	
	public TransactionDatabase(DBSet databaseSet, DB database)
	{
		this.databaseSet = databaseSet;
		
		//OPEN MAP
		this.transactionMap = database.createTreeMap("transactions")
			.keySerializer(BTreeKeySerializer.BASIC)
			.comparator(UnsignedBytes.lexicographicalComparator())
			.makeOrGet();
	}
	
	public TransactionDatabase(TransactionDatabase parent)
	{
		this.parent = parent;
	    
	    //OPEN MAP
	    this.transactionMap = new TreeMap<byte[], byte[]>(UnsignedBytes.lexicographicalComparator());
	}
	
	public List<Transaction> getTransactions()
	{
		try
		{
			//GET ALL TRANSACTIONS IN MAP
			List<byte[]> keyList = this.getKeys();
			
			if(this.parent != null)
			{
				keyList.addAll(this.parent.getKeys());
				
				//TODO REMOVE DUPLICATES
			}
			
			//RETURN
			return new TransactionsList(keyList);
		}
		catch(Exception e)
		{
			e.printStackTrace();
			return new ArrayList<Transaction>();
		}		
	}
	
	private List<byte[]> getKeys()
	{
		//GET ALL KEYS
		List<byte[]> keyList = new ArrayList<byte[]>();
		
		for(byte[] key: this.transactionMap.keySet())
		{
			keyList.add(key);
		}
		
		return keyList;
	}
	
	public Transaction getTransaction(byte[] signature)
	{
		try
		{
			if(this.transactionMap.containsKey(signature))
			{
				return TransactionFactory.getInstance().parse(this.transactionMap.get(signature));
			}
			else
			{
				if(this.parent != null)
				{
					return this.parent.getTransaction(signature);
				}
			}
			
			return null;
		}
		catch(Exception e)
		{
			//NO BLOCK FOUND
			return null;
		}	
	}
	
	public void addTransaction(Transaction transaction)
	{
		try
		{			
			this.transactionMap.put(transaction.getSignature(), transaction.toBytes());
			
			//COMMIT
			if(this.databaseSet != null)
			{
				this.databaseSet.commit();
			}
		}
		catch(Exception e)
		{
			e.printStackTrace();
		}	
	}
	
	public void remove(Transaction transaction)
	{
		//REMOVE TRANSACTION FROM 0 CONFIRMS
		this.transactionMap.remove(transaction.getSignature());
		
		//COMMIT
		if(this.databaseSet != null)
		{
			this.databaseSet.commit();
		}		
	}
	
	public boolean contains(Transaction transaction) 
	{
		if(this.transactionMap.containsKey(transaction.getSignature()))
		{
			return true;
		}
		else
		{
			if(this.parent != null)
			{
				return this.parent.contains(transaction);
			}
		}
		
		return false;
	}

	@Override
	public void update(Observable o, Object arg) 
	{	
		ObserverMessage message = (ObserverMessage) arg;
		
		//ON NEW BLOCK
		if(message.getType() == ObserverMessage.ADD_BLOCK_TYPE)
		{			
			//CLEAN UP
			for(Transaction transaction: this.getTransactions())
			{
				//CHECK IF DEADLINE PASSED
				if(transaction.getDeadline() < NTP.getTime())
				{
					this.remove(transaction);
					
					//NOTIFY
					this.setChanged();
					this.notifyObservers(new ObserverMessage(ObserverMessage.REMOVE_TRANSACTION_TYPE, transaction));
				}
			}
		}
	}
}
