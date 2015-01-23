package database;

import java.util.HashMap;
import java.util.Map;

import org.mapdb.DB;

import scorex.account.Account;

public class ReferenceMap extends DBMap<String, byte[]> 
{
	private Map<Integer, Integer> observableData = new HashMap<Integer, Integer>();
	
	public ReferenceMap(DBSet databaseSet, DB database)
	{
		super(databaseSet, database);
	}

	public ReferenceMap(ReferenceMap parent) 
	{
		super(parent);
	}
	
	protected void createIndexes(DB database){}

	@Override
	protected Map<String, byte[]> getMap(DB database) 
	{
		//OPEN MAP
		return database.getTreeMap("references");
	}

	@Override
	protected Map<String, byte[]> getMemoryMap() 
	{
		return new HashMap<String, byte[]>();
	}

	@Override
	protected byte[] getDefaultValue() 
	{
		return new byte[0];
	}
	
	@Override
	protected Map<Integer, Integer> getObservableData() 
	{
		return this.observableData;
	}

	public byte[] get(Account account) 
	{
		return this.get(account.getAddress());
	}
	
	public void set(Account account, byte[] reference)
	{
		this.set(account.getAddress(), reference);
	}
	
	public void delete(Account account)
	{
		this.delete(account.getAddress());
	}
}
