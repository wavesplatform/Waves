package database.wallet;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.NavigableSet;
import java.util.TreeMap;

import org.mapdb.BTreeKeySerializer;
import org.mapdb.DB;
import org.mapdb.Fun;
import org.mapdb.Fun.Tuple2;
import org.mapdb.BTreeMap;

import scorex.account.Account;
import scorex.transaction.Transaction;
import utils.ObserverMessage;
import utils.Pair;
import utils.ReverseComparator;
import database.DBMap;
import database.serializer.TransactionSerializer;

public class TransactionMap extends DBMap<Tuple2<String, String>, Transaction>
{
	public static final int TIMESTAMP_INDEX = 1;
	public static final int ADDRESS_INDEX = 2;
	public static final int AMOUNT_INDEX = 3;
	
	private Map<Integer, Integer> observableData = new HashMap<Integer, Integer>();
	
	public TransactionMap(WalletDatabase walletDatabase, DB database)
	{
		super(walletDatabase, database);
		
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
		NavigableSet<Tuple2<Long, Tuple2<String, String>>> timestampIndex = database.createTreeSet("transactions_index_timestamp")
				.comparator(Fun.COMPARATOR)
				.makeOrGet();
		
		NavigableSet<Tuple2<Long, Tuple2<String, String>>> descendingTimestampIndex = database.createTreeSet("transactions_index_timestamp_descending")
				.comparator(new ReverseComparator(Fun.COMPARATOR))
				.makeOrGet();
		
		createIndex(TIMESTAMP_INDEX, timestampIndex, descendingTimestampIndex, new Fun.Function2<Long, Tuple2<String, String>, Transaction>() {
		   	@Override
		    public Long run(Tuple2<String, String> key, Transaction value) {
		   		return value.getTimestamp();
		    }
		});
		
		//ADDRESS INDEX
		NavigableSet<Tuple2<String, Tuple2<String, String>>> addressIndex = database.createTreeSet("transactions_index_address")
				.comparator(Fun.COMPARATOR)
				.makeOrGet();
		
		NavigableSet<Tuple2<String, Tuple2<String, String>>> descendingAddressIndex = database.createTreeSet("transactions_index_address_descending")
				.comparator(new ReverseComparator(Fun.COMPARATOR))
				.makeOrGet();
		
		createIndex(ADDRESS_INDEX, addressIndex, descendingAddressIndex, new Fun.Function2<String, Tuple2<String, String>, Transaction>() {
		   	@Override
		    public String run(Tuple2<String, String> key, Transaction value) {
		   		return key.a;
		    }
		});	
		
		//AMOUNT INDEX
		NavigableSet<Tuple2<BigDecimal, Tuple2<String, String>>> amountIndex = database.createTreeSet("transactions_index_amount")
				.comparator(Fun.COMPARATOR)
				.makeOrGet();
		
		NavigableSet<Tuple2<BigDecimal, Tuple2<String, String>>> descendingAmountIndex = database.createTreeSet("transactions_index_amount_descending")
				.comparator(new ReverseComparator(Fun.COMPARATOR))
				.makeOrGet();
		
		createIndex(AMOUNT_INDEX, amountIndex, descendingAmountIndex, new Fun.Function2<BigDecimal, Tuple2<String, String>, Transaction>() {
		   	@Override
		    public BigDecimal run(Tuple2<String, String> key, Transaction value) {
		   		Account account = new Account(key.a);
		   		return value.getAmount(account);
		    }
		});		
	}

	@Override
	protected Map<Tuple2<String, String>, Transaction> getMap(DB database) 
	{
		//OPEN MAP
		return database.createTreeMap("transactions")
				.keySerializer(BTreeKeySerializer.TUPLE2)
				.valueSerializer(new TransactionSerializer())
				.counterEnable()
				.makeOrGet();
	}

	@Override
	protected Map<Tuple2<String, String>, Transaction> getMemoryMap() 
	{
		return new TreeMap<Tuple2<String, String>, Transaction>(Fun.TUPLE2_COMPARATOR);
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

	@SuppressWarnings({ "unchecked", "rawtypes" })
	public List<Transaction> get(Account account, int limit)
	{
		List<Transaction> transactions = new ArrayList<Transaction>();
		
		try
		{
			//GET ALL TRANSACTIONS THAT BELONG TO THAT ADDRESS
			/*Map<Tuple2<String, String>, Transaction> accountTransactions = ((BTreeMap) this.map).subMap(
					Fun.t2(null, account.getAddress()),
					Fun.t2(Fun.HI(), account.getAddress()));*/
			
			Map<Tuple2<String, String>, Transaction> accountTransactions = ((BTreeMap) this.map).subMap(
					Fun.t2(account.getAddress(), null),
					Fun.t2(account.getAddress(), Fun.HI()));
			
			//GET ITERATOR
			Iterator<Transaction> iterator = accountTransactions.values().iterator();
			
			//RETURN {LIMIT} TRANSACTIONS
			int counter = 0;
			while(iterator.hasNext() && counter < limit)
			{
				transactions.add(iterator.next());
				counter++;
			}
		}
		catch(Exception e)
		{
			//ERROR
			e.printStackTrace();
		}
		
		return transactions;
	}
	
	public List<Pair<Account, Transaction>> get(List<Account> accounts, int limit)
	{
		List<Pair<Account, Transaction>> transactions = new ArrayList<Pair<Account, Transaction>>();
		
		try
		{
			//FOR EACH ACCOUNTS
			synchronized(accounts)
			{
				for(Account account: accounts)
				{
					List<Transaction> accountTransactions = get(account, limit);
					for(Transaction transaction: accountTransactions)
					{
						transactions.add(new Pair<Account, Transaction>(account, transaction));
					}
				}
			}
		}
		catch(Exception e)
		{
			//ERROR
			e.printStackTrace();
		}
		
		return transactions;
	}
	
	@SuppressWarnings({ "unchecked", "rawtypes" })
	public void delete(Account account)
	{
		//GET ALL TRANSACTIONS THAT BELONG TO THAT ADDRESS
		Map<Tuple2<String, String>, Transaction> accountTransactions = ((BTreeMap) this.map).subMap(
				Fun.t2(account.getAddress(), null),
				Fun.t2(account.getAddress(), Fun.HI()));
		
		//DELETE TRANSACTIONS
		for(Tuple2<String, String> key: accountTransactions.keySet())
		{
			this.delete(key);
		}
	}
	
	public void delete(Account account, Transaction transaction)
	{
		this.delete(new Tuple2<String, String>(account.getAddress(), new String(transaction.getSignature())));
	}
	
	public void deleteAll(List<Account> accounts)
	{
		for(Account account: accounts)
		{
			this.delete(account);
		}
	}
	
	public boolean add(Account account, Transaction transaction)
	{
		return this.set(new Tuple2<String, String>(account.getAddress(), new String(transaction.getSignature())), transaction);
	}
	
	public void addAll(Map<Account, List<Transaction>> transactions)
	{
		//FOR EACH ACCOUNT
	    for(Account account: transactions.keySet())
	    {
	    	//FOR EACH TRANSACTION
	    	for(Transaction transaction: transactions.get(account))
	    	{
	    		this.add(account, transaction);
	    	}
	    }
	}
}
