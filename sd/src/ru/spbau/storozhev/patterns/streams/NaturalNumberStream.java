package ru.spbau.storozhev.patterns.streams;

public class NaturalNumberStream extends AbstractIntegerStream {

	@Override
	public int getNextValue() {
		return ++current;
	}

	int current = 0;

}
