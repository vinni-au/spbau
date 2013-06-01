package ru.spbau.storozhev.patterns.lights;

import java.util.Collection;
import java.util.Iterator;

public class BulbIterator implements Iterator<LightBulb> {
	
	public BulbIterator(LightBulb bulb) {
		this.bulb = bulb;
	}
	
	public BulbIterator(Iterator<LightBulb> inner) {
		this.inner = inner;
	}
	
	public BulbIterator(Collection<Illuminant> illums) {
		
	}
	
	@Override
	public boolean hasNext() {
		if (inner != null)
			return inner.hasNext();
		else 
			return bulb != null;
	}

	@Override
	public LightBulb next() {
		LightBulb result = null;
		if (inner != null) {
			result = inner.next();
			if (!inner.hasNext())
				inner = null;
			return result;
		}
		if (bulb != null) {
			if (bulb.isOk())
				result = bulb;
			bulb = null;
		}
		return result;
	}

	@Override
	public void remove() {
	}
	
	private LightBulb bulb;
	private Iterator<LightBulb> inner;
}
