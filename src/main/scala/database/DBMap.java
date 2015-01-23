package database;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.NavigableMap;
import java.util.NavigableSet;
import java.util.Observable;
import java.util.Observer;
import java.util.Set;

import org.mapdb.BTreeMap;
import org.mapdb.Bind;
import org.mapdb.DB;
import org.mapdb.Fun.Function2;
import org.mapdb.Fun.Tuple2;

import utils.ObserverMessage;

public abstract class DBMap<T, U> extends Observable {
	
	protected static final int NOTIFY_ADD = 1;
	protected static final int NOTIFY_REMOVE = 2;
	protected static final int NOTIFY_LIST = 3;
	
	public static final int DEFAULT_INDEX = 0;
	
	protected DBMap<T, U> parent;
	protected IDB databaseSet;
	protected Map<T, U> map;
	protected List<T> deleted;
	private Map<Integer, NavigableSet<Tuple2<?, T>>> indexes;

	public DBMap(IDB databaseSet, DB database)
	{
		this.databaseSet = databaseSet;
		
		//OPEN MAP
	    this.map = this.getMap(database);
	    
	    //CREATE INDEXES
	    this.indexes = new HashMap<Integer, NavigableSet<Tuple2<?, T>>>();
	    this.createIndexes(database);
	}
	
	public DBMap(DBMap<T, U> parent)
	{
		this.parent = parent;
	    
	    //OPEN MAP
	    this.map = this.getMemoryMap();
	    this.deleted = new ArrayList<T>();
	}
	
	protected abstract Map<T, U> getMap(DB database);
	
	protected abstract Map<T, U> getMemoryMap();
	
	protected abstract U getDefaultValue();
	
	protected abstract Map<Integer, Integer> getObservableData();
	
	protected abstract void createIndexes(DB database);
	
	@SuppressWarnings("unchecked")
	protected <V> void createIndex(int index, NavigableSet<?> indexSet, NavigableSet<?> descendingIndexSet, Function2<V, T, U> function) 
	{
		Bind.secondaryKey((BTreeMap<T, U>) this.map, (NavigableSet<Tuple2<V, T>>) indexSet, function);
		this.indexes.put(index, (NavigableSet<Tuple2<?, T>>) indexSet);
		
		Bind.secondaryKey((BTreeMap<T, U>) this.map, (NavigableSet<Tuple2<V, T>>) descendingIndexSet, function);
		this.indexes.put(index + 10000, (NavigableSet<Tuple2<?, T>>) descendingIndexSet);
	}
	
	@SuppressWarnings("unchecked")
	protected <V> void createIndexes(int index, NavigableSet<?> indexSet, NavigableSet<?> descendingIndexSet, Function2<V[], T, U> function) 
	{
		Bind.secondaryKeys((BTreeMap<T, U>) this.map, (NavigableSet<Tuple2<V, T>>) indexSet, function);
		this.indexes.put(index, (NavigableSet<Tuple2<?, T>>) indexSet);
		
		Bind.secondaryKeys((BTreeMap<T, U>) this.map, (NavigableSet<Tuple2<V, T>>) descendingIndexSet, function);
		this.indexes.put(index + 10000, (NavigableSet<Tuple2<?, T>>) descendingIndexSet);
	}
	
	public int size() {
		return this.map.size();
	}
	
	public U get(T key)
	{
		try
		{
			if(this.map.containsKey(key))
			{
				return this.map.get(key);
			}
			else
			{
				if(this.deleted == null || !this.deleted.contains(key))
				{
					if(this.parent != null)
					{
						return this.parent.get(key);
					}
				}
			}
			
			return this.getDefaultValue();
		}
		catch(Exception e)
		{
			e.printStackTrace();
			
			return this.getDefaultValue();
		}			
	}
	
	public Set<T> getKeys()
	{
		return this.map.keySet();
	}
	
	public Collection<U> getValues()
	{
		return this.map.values();
	}
	
	public boolean set(T key, U value)
	{
		try
		{
			U old = this.map.put(key, value);
			
			if(this.deleted != null)
			{
				this.deleted.remove(key);
			}
			
			//COMMIT
			if(this.databaseSet != null)
			{
				this.databaseSet.commit();
			}
			
			//NOTIFY ADD
			if(this.getObservableData().containsKey(NOTIFY_ADD))
			{
				this.setChanged();
				this.notifyObservers(new ObserverMessage(this.getObservableData().get(NOTIFY_ADD), value));
			}
			
			//NOTIFY LIST
			if(this.getObservableData().containsKey(NOTIFY_LIST))
			{
				this.setChanged();
				this.notifyObservers(new ObserverMessage(this.getObservableData().get(NOTIFY_LIST), new SortableList<T, U>(this)));
			}
			
			return old != null;
		}
		catch(Exception e)
		{
			e.printStackTrace();
		}
		
		return false;
	}
	
	public void delete(T key) 
	{
		try
		{
			//REMOVE
			if(this.map.containsKey(key))
			{
				U value = this.map.remove(key);
				
				//NOTIFY REMOVE
				if(this.getObservableData().containsKey(NOTIFY_REMOVE))
				{
					this.setChanged();
					this.notifyObservers(new ObserverMessage(this.getObservableData().get(NOTIFY_REMOVE), value));
				}
				
				//NOTIFY LIST
				/*if(this.getObservableData().containsKey(NOTIFY_LIST))
				{
					this.setChanged();
					this.notifyObservers(new ObserverMessage(this.getObservableData().get(NOTIFY_LIST), new SortableList<T, U>(this)));
				}*/
			}
			
			if(this.deleted != null)
			{
				this.deleted.add(key);
			}
			
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
	
	public boolean contains(T key)
	{
		if(this.map.containsKey(key))
		{
			return true;
		}
		else
		{
			if(this.deleted == null || !this.deleted.contains(key))
			{
				if(this.parent != null)
				{
					return this.parent.contains(key);
				}
			}
		}
		
		return false;
	}
	
	@Override
	public void addObserver(Observer o) 
	{
		//ADD OBSERVER
		super.addObserver(o);	
		
		//NOTIFY LIST
		if(this.getObservableData().containsKey(NOTIFY_LIST))
		{
			//CREATE LIST
			SortableList<T, U> list = new SortableList<T, U>(this);
			
			//UPDATE
			o.update(null, new ObserverMessage(this.getObservableData().get(NOTIFY_LIST), list));
		}
	}
	
	public Iterator<T> getIterator(int index, boolean descending)
	{
		if(index == DEFAULT_INDEX)
		{
			if(descending)
			{
				return ((NavigableMap<T, U>) this.map).descendingKeySet().iterator();
			}
			
			return ((NavigableMap<T, U>) this.map).keySet().iterator();
		}
		else
		{
			if(descending)
			{
				index += 10000;
			}
			
			return new IndexIterator<T>(this.indexes.get(index));
		}
	}
	
	public void reset() 
	{
		//RESET MAP
		this.map.clear();
		
		//RESET INDEXES
		for(Set<Tuple2<?, T>> set: this.indexes.values())
		{
			set.clear();
		}
		
		//NOTIFY LIST
		if(this.getObservableData().containsKey(NOTIFY_LIST))
		{
			//CREATE LIST
			SortableList<T, U> list = new SortableList<T, U>(this);
			
			//UPDATE
			this.setChanged();
			this.notifyObservers(new ObserverMessage(this.getObservableData().get(NOTIFY_LIST), list));
		}
	}
}
