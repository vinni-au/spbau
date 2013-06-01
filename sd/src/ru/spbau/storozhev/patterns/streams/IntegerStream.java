package ru.spbau.storozhev.patterns.streams;

public interface IntegerStream {
	int getNext();
	
	void installFilter(IntegerStreamFilter filter);
	void removeFilter(IntegerStreamFilter filter);
}
