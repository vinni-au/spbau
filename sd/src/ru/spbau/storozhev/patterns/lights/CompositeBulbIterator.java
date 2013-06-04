package ru.spbau.storozhev.patterns.lights;

import java.util.Iterator;

public class CompositeBulbIterator implements Iterator<LightBulb> {
	public CompositeBulbIterator(CompositeLightingSystem system) {
		Visitor visitor = new Visitor();
		visitor.visit(system);
		iter = visitor.iterators().iterator();
		current = iter.next();
	}

	@Override
	public boolean hasNext() {
		if (current == null)
			return false;
		
		while (!current.hasNext()) {
			if (!iter.hasNext())
				return false;
			else
				current = iter.next();
		}
		return current.hasNext();
	}

	@Override
	public LightBulb next() {
		return current.next();
	}

	@Override
	public void remove() {
	}
	
	private Iterator<Iterator<LightBulb>> iter;
	private Iterator<LightBulb> current;
}
