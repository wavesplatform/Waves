package database;

import java.math.BigDecimal;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.TreeMap;

import org.mapdb.BTreeKeySerializer;
import org.mapdb.BTreeMap;
import org.mapdb.Bind;
import org.mapdb.DB;
import org.mapdb.Fun;
import org.mapdb.Fun.Tuple2;
import org.mapdb.Fun.Tuple3;

import scorex.account.Account;
import utils.ObserverMessage;

public class BalanceMap extends DBMap<Tuple2<String, Long>, BigDecimal> 
{
	public static final long QORA_KEY = 0l;
	
	private Map<Integer, Integer> observableData = new HashMap<Integer, Integer>();
	
	@SuppressWarnings("rawtypes")
	private BTreeMap assetKeyMap;
	
	public BalanceMap(DBSet databaseSet, DB database)
	{
		super(databaseSet, database);
		
		this.observableData.put(DBMap.NOTIFY_ADD, ObserverMessage.ADD_BALANCE_TYPE);
		this.observableData.put(DBMap.NOTIFY_REMOVE, ObserverMessage.REMOVE_BALANCE_TYPE);
		//this.observableData.put(DBMap.NOTIFY_LIST, ObserverMessage.LIST_BALANCE_TYPE);
	}

	public BalanceMap(BalanceMap parent) 
	{
		super(parent);
	}
	
	protected void createIndexes(DB database){}

	@SuppressWarnings({ "unchecked"})
	@Override
	protected Map<Tuple2<String, Long>, BigDecimal> getMap(DB database) 
	{
		//OPEN MAP
		BTreeMap<Tuple2<String, Long>, BigDecimal> map =  database.createTreeMap("balances")
				.keySerializer(BTreeKeySerializer.TUPLE2)
				.counterEnable()
				.makeOrGet();
		
		//HAVE/WANT KEY
		this.assetKeyMap = database.createTreeMap("balances_key_asset")
				.comparator(Fun.COMPARATOR)
				.counterEnable()
				.makeOrGet();
		
		//BIND ASSET KEY
		Bind.secondaryKey(map, this.assetKeyMap, new Fun.Function2<Tuple3<Long, BigDecimal, String>, Tuple2<String, Long>, BigDecimal>() {
			@Override
			public Tuple3<Long, BigDecimal, String> run(Tuple2<String, Long> key, BigDecimal value) {
				return new Tuple3<Long, BigDecimal, String>(key.b, value.negate(), key.a);
			}	
		});
		
		//RETURN
		return map;
	}

	@Override
	protected Map<Tuple2<String, Long>, BigDecimal> getMemoryMap() 
	{
		return new TreeMap<Tuple2<String, Long>, BigDecimal>(Fun.TUPLE2_COMPARATOR);
	}

	@Override
	protected BigDecimal getDefaultValue() 
	{
		return BigDecimal.ZERO.setScale(8);
	}
	
	@Override
	protected Map<Integer, Integer> getObservableData() 
	{
		return this.observableData;
	}
	
	public void set(String address, BigDecimal value)
	{
		this.set(address, QORA_KEY, value);
	}
	
	public void set(String address, long key, BigDecimal value)
	{
		this.set(new Tuple2<String, Long>(address, key), value);
	}
	
	public BigDecimal get(String address)
	{
		return this.get(address, QORA_KEY);
	}
	
	public BigDecimal get(String address, long key)
	{
		return this.get(new Tuple2<String, Long>(address, key));
	}
	
	@SuppressWarnings({ "unchecked", "rawtypes" })
	public SortableList<Tuple2<String, Long>, BigDecimal> getBalancesSortableList(long key)
	{
		//FILTER ALL KEYS
		Collection<Tuple2<String, Long>> keys = ((BTreeMap<Tuple3, Tuple2<String, Long>>) this.assetKeyMap).subMap(
				Fun.t3(key, null, null),
				Fun.t3(key, Fun.HI(), Fun.HI())).values();
		
		//RETURN
		return new SortableList<Tuple2<String, Long>, BigDecimal>(this, keys);
	}

	@SuppressWarnings({ "unchecked", "rawtypes" })
	public SortableList<Tuple2<String, Long>, BigDecimal> getBalancesSortableList(Account account) 
	{
		BTreeMap map = (BTreeMap) this.map;
		
		//FILTER ALL KEYS
		Collection keys = ((BTreeMap<Tuple2, BigDecimal>) map).subMap(
				Fun.t2(account.address(), null),
				Fun.t2(account.address(), Fun.HI())).keySet();
		
		//RETURN
		return new SortableList<Tuple2<String, Long>, BigDecimal>(this, keys);
	}
}
