package ru.spbau.storozhev.patterns.streams;

import java.util.Set;
import java.util.HashSet;

public abstract class AbstractIntegerStream implements IntegerStream {

	@Override
	public int getNext() {
		int value;
		do {
			value = getNextValue();
		} while (!isSatisfiesAllFilters(value));
		return value;
	}
	
	@Override
	public void installFilter(IntegerStreamFilter filter) {
		filters.add(filter);
	}

	@Override
	public void removeFilter(IntegerStreamFilter filter) {
		filters.remove(filter);
	}
	
	private boolean isSatisfiesAllFilters(int value) {
		for (IntegerStreamFilter filter : filters) {
			if (!filter.isSatisfies(value))
				return false;
		}
		return true;
	}
	
	private Set<IntegerStreamFilter> filters = new HashSet<>();

	protected abstract int getNextValue();

}
