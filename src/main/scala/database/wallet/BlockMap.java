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
import scorex.block.Block;
import utils.ObserverMessage;
import utils.Pair;
import utils.ReverseComparator;
import database.DBMap;
import database.serializer.BlockSerializer;

public class BlockMap extends DBMap<Tuple2<String, String>, Block>
{
	public static final int TIMESTAMP_INDEX = 1;
	public static final int GENERATOR_INDEX = 2;
	public static final int BALANCE_INDEX = 3;
	public static final int TRANSACTIONS_INDEX = 4;
	public static final int FEE_INDEX = 5;
	
	private Map<Integer, Integer> observableData = new HashMap<Integer, Integer>();
	
	public BlockMap(WalletDatabase walletDatabase, DB database)
	{
		super(walletDatabase, database);
		
		this.observableData.put(DBMap.NOTIFY_ADD, ObserverMessage.ADD_BLOCK_TYPE);
		this.observableData.put(DBMap.NOTIFY_REMOVE, ObserverMessage.REMOVE_BLOCK_TYPE);
		this.observableData.put(DBMap.NOTIFY_LIST, ObserverMessage.LIST_BLOCK_TYPE);
	}

	public BlockMap(BlockMap parent) 
	{
		super(parent);
	}
	
	@SuppressWarnings({ "unchecked", "rawtypes" })
	protected void createIndexes(DB database)
	{
		//TIMESTAMP INDEX
		NavigableSet<Tuple2<Long, Tuple2<String, String>>> timestampIndex = database.createTreeSet("blocks_index_timestamp")
				.comparator(Fun.COMPARATOR)
				.makeOrGet();
		
		NavigableSet<Tuple2<Long, Tuple2<String, String>>> descendingTimestampIndex = database.createTreeSet("blocks_index_timestamp_descending")
				.comparator(new ReverseComparator(Fun.COMPARATOR))
				.makeOrGet();
		
		createIndex(TIMESTAMP_INDEX, timestampIndex, descendingTimestampIndex, new Fun.Function2<Long, Tuple2<String, String>, Block>() {
		   	@Override
		    public Long run(Tuple2<String, String> key, Block value) {
		   		return value.timestamp();
		    }
		});
		
		//GENERATOR INDEX
		NavigableSet<Tuple2<String, Tuple2<String, String>>> generatorIndex = database.createTreeSet("blocks_index_generator")
				.comparator(Fun.COMPARATOR)
				.makeOrGet();
		
		NavigableSet<Tuple2<String, Tuple2<String, String>>> descendingGeneratorIndex = database.createTreeSet("blocks_index_generator_descending")
				.comparator(new ReverseComparator(Fun.COMPARATOR))
				.makeOrGet();
		
		createIndex(GENERATOR_INDEX, generatorIndex, descendingGeneratorIndex, new Fun.Function2<String, Tuple2<String, String>, Block>() {
		   	@Override
		    public String run(Tuple2<String, String> key, Block value) {
		   		return key.a;
		    }
		});	
		
		//BALANCE INDEX
		NavigableSet<Tuple2<Long, Tuple2<String, String>>> balanceIndex = database.createTreeSet("blocks_index_balance")
				.comparator(Fun.COMPARATOR)
				.makeOrGet();
		
		NavigableSet<Tuple2<Long, Tuple2<String, String>>> descendingBalanceIndex = database.createTreeSet("blocks_index_balance_descending")
				.comparator(new ReverseComparator(Fun.COMPARATOR))
				.makeOrGet();
		
		createIndex(BALANCE_INDEX, balanceIndex, descendingBalanceIndex, new Fun.Function2<Long, Tuple2<String, String>, Block>() {
		   	@Override
		    public Long run(Tuple2<String, String> key, Block value) {
		   		return value.generatingBalance();
		    }
		});
		
		//TRANSACTIONS INDEX
		NavigableSet<Tuple2<Integer, Tuple2<String, String>>> transactionsIndex = database.createTreeSet("blocks_index_transactions")
				.comparator(Fun.COMPARATOR)
				.makeOrGet();
		
		NavigableSet<Tuple2<Integer, Tuple2<String, String>>> descendingTransactionsIndex = database.createTreeSet("blocks_index_transactions_descending")
				.comparator(new ReverseComparator(Fun.COMPARATOR))
				.makeOrGet();
		
		createIndex(TRANSACTIONS_INDEX, transactionsIndex, descendingTransactionsIndex, new Fun.Function2<Integer, Tuple2<String, String>, Block>() {
		   	@Override
		    public Integer run(Tuple2<String, String> key, Block value) {
		   		return value.transactions().size();
		    }
		});
		
		//FEE INDEX
		NavigableSet<Tuple2<BigDecimal, Tuple2<String, String>>> feeIndex = database.createTreeSet("blocks_index_fee")
				.comparator(Fun.COMPARATOR)
				.makeOrGet();
		
		NavigableSet<Tuple2<BigDecimal, Tuple2<String, String>>> descendingFeeIndex = database.createTreeSet("blocks_index_fee_descending")
				.comparator(new ReverseComparator(Fun.COMPARATOR))
				.makeOrGet();
		
		createIndex(FEE_INDEX, feeIndex, descendingFeeIndex, new Fun.Function2<BigDecimal, Tuple2<String, String>, Block>() {
		   	@Override
		    public BigDecimal run(Tuple2<String, String> key, Block value) {
		   		return value.getTotalFee();
		    }
		});
	}

	@Override
	protected Map<Tuple2<String, String>, Block> getMap(DB database) 
	{
		//OPEN MAP
		return database.createTreeMap("blocks")
				.keySerializer(BTreeKeySerializer.TUPLE2)
				.valueSerializer(new BlockSerializer())
				.counterEnable()
				.makeOrGet();
	}

	@Override
	protected Map<Tuple2<String, String>, Block> getMemoryMap() 
	{
		return new TreeMap<Tuple2<String, String>, Block>(Fun.TUPLE2_COMPARATOR);
	}

	@Override
	protected Block getDefaultValue() 
	{
		return null;
	}
	
	@Override
	protected Map<Integer, Integer> getObservableData() 
	{
		return this.observableData;
	}

	@SuppressWarnings({ "unchecked", "rawtypes" })
	public List<Block> get(Account account)
	{
		List<Block> blocks = new ArrayList<Block>();
		
		try
		{
			Map<Tuple2<String, String>, Block> accountBlocks = ((BTreeMap) this.map).subMap(
					Fun.t2(account.address(), null),
					Fun.t2(account.address(), Fun.HI()));
			
			//GET ITERATOR
			Iterator<Block> iterator = accountBlocks.values().iterator();
			
			while(iterator.hasNext())
			{
				blocks.add(iterator.next());
			}
		}
		catch(Exception e)
		{
			//ERROR
			e.printStackTrace();
		}
		
		return blocks;
	}
	
	public List<Pair<Account, Block>> get(List<Account> accounts)
	{
		List<Pair<Account, Block>> blocks = new ArrayList<Pair<Account, Block>>();
		
		try
		{
			//FOR EACH ACCOUNTS
			synchronized(accounts)
			{
				for(Account account: accounts)
				{
					List<Block> accountBlocks = get(account);
					for(Block block: accountBlocks)
					{
						blocks.add(new Pair<Account, Block>(account, block));
					}
				}
			}
		}
		catch(Exception e)
		{
			//ERROR
			e.printStackTrace();
		}
		
		return blocks;
	}
	
	@SuppressWarnings({ "unchecked", "rawtypes" })
	public void delete(Account account)
	{
		//GET ALL TRANSACTIONS THAT BELONG TO THAT ADDRESS
		Map<Tuple2<String, String>, Block> accountBlocks = ((BTreeMap) this.map).subMap(
				Fun.t2(account.address(), null),
				Fun.t2(account.address(), Fun.HI()));
		
		//DELETE TRANSACTIONS
		for(Tuple2<String, String> key: accountBlocks.keySet())
		{
			this.delete(key);
		}
	}
	
	public void delete(Block block)
	{
		this.delete(new Tuple2<String, String>(block.generator().address(), new String(block.signature())));
	}
	
	public void deleteAll(List<Account> accounts)
	{
		for(Account account: accounts)
		{
			this.delete(account);
		}
	}
	
	public boolean add(Block block)
	{
		return this.set(new Tuple2<String, String>(block.generator().address(), new String(block.signature())), block);
	}
	
	public void addAll(Map<Account, List<Block>> blocks)
	{
		//FOR EACH ACCOUNT
	    for(Account account: blocks.keySet())
	    {
	    	//FOR EACH TRANSACTION
	    	for(Block block: blocks.get(account))
	    	{
	    		this.add(block);
	    	}
	    }
	}
}
