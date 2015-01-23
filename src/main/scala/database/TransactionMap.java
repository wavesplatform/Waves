package database;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.NavigableSet;
import java.util.Observable;
import java.util.Observer;
import java.util.TreeMap;

import ntp.NTP;

import org.mapdb.BTreeKeySerializer;
import org.mapdb.DB;
import org.mapdb.Fun;
import org.mapdb.Fun.Tuple2;
import org.mapdb.Fun.Tuple2Comparator;

import com.google.common.primitives.UnsignedBytes;

import scorex.transaction.Transaction;
import utils.ObserverMessage;
import utils.ReverseComparator;
import database.serializer.TransactionSerializer;

public class TransactionMap extends DBMap<byte[], Transaction> implements Observer
{
	public static final int TIMESTAMP_INDEX = 1;
	
	private Map<Integer, Integer> observableData = new HashMap<Integer, Integer>();
	
	public TransactionMap(DBSet databaseSet, DB database)
	{
		super(databaseSet, database);
		
		this.observableData.put(DBMap.NOTIFY_ADD, ObserverMessage.ADD_TRANSACTION_TYPE);
		this.observableData.put(DBMap.NOTIFY_REMOVE, ObserverMessage.REMOVE_TRANSACTION_TYPE);
		this.observableData.put(DBMap.NOTIFY_LIST, ObserverMessage.LIST_TRANSACTION_TYPE);
	}

	public TransactionMap(TransactionMap parent) 
	{
		super(parent);
	}
	
	@SuppressWarnings({ "unchecked", "rawtypes" })
	protected void createIndexes(DB database)
	{
		//TIMESTAMP INDEX
		Tuple2Comparator<Long, byte[]> comparator = new Fun.Tuple2Comparator<Long, byte[]>(Fun.COMPARATOR, UnsignedBytes.lexicographicalComparator());
		NavigableSet<Tuple2<Integer, byte[]>> heightIndex = database.createTreeSet("transactions_index_timestamp")
				.comparator(comparator)
				.makeOrGet();
				
		NavigableSet<Tuple2<Integer, byte[]>> descendingHeightIndex = database.createTreeSet("transactions_index_timestamp_descending")
				.comparator(new ReverseComparator(comparator))
				.makeOrGet();
				
		createIndex(TIMESTAMP_INDEX, heightIndex, descendingHeightIndex, new Fun.Function2<Long, byte[], Transaction>() {
		   	@Override
		    public Long run(byte[] key, Transaction value) {
		   		return value.getTimestamp();
		    }
		});
	}

	@Override
	protected Map<byte[], Transaction> getMap(DB database) 
	{
		//OPEN MAP
		return database.createTreeMap("transactions")
				.keySerializer(BTreeKeySerializer.BASIC)
				.comparator(UnsignedBytes.lexicographicalComparator())
				.valueSerializer(new TransactionSerializer())
				.counterEnable()
				.makeOrGet();
	}

	@Override
	protected Map<byte[], Transaction> getMemoryMap() 
	{
		return new TreeMap<byte[], Transaction>(UnsignedBytes.lexicographicalComparator());
	}

	@Override
	protected Transaction getDefaultValue() 
	{
		return null;
	}
	
	@Override
	protected Map<Integer, Integer> getObservableData() 
	{
		return this.observableData;
	}
	
	@Override
	public void update(Observable o, Object arg) 
	{	
		ObserverMessage message = (ObserverMessage) arg;
		
		//ON NEW BLOCK
		if(message.getType() == ObserverMessage.ADD_BLOCK_TYPE)
		{			
			//CLEAN UP
			for(Transaction transaction: this.getValues())
			{
				//CHECK IF DEADLINE PASSED
				if(transaction.getDeadline() < NTP.getTime())
				{
					this.delete(transaction.getSignature());
					
					//NOTIFY
					/*this.setChanged();
					this.notifyObservers(new ObserverMessage(ObserverMessage.REMOVE_TRANSACTION_TYPE, transaction));*/
				}
			}
		}
	}

	public void add(Transaction transaction) {
		this.set(transaction.getSignature(), transaction);
	}

	public List<Transaction> getTransactions() {
		return new ArrayList<Transaction>(this.getValues());
	}

	public void delete(Transaction transaction) {
		this.delete(transaction.getSignature());		
	}

	public boolean contains(Transaction transaction) {
		return this.contains(transaction.getSignature());
	}
}
