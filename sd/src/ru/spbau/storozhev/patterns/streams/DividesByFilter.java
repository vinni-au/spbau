package ru.spbau.storozhev.patterns.streams;

public class DividesByFilter implements IntegerStreamFilter {
	
	public DividesByFilter(int divisor) {
		if (divisor == 0) {
			throw new IllegalArgumentException("Divisor can't be 0");
		}
		this.divisor = divisor;
	}

	@Override
	public boolean isSatisfies(int value) {
		return value % divisor == 0;
	}
	
	private int divisor;

}
