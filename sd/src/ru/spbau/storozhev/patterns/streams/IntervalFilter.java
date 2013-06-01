package ru.spbau.storozhev.patterns.streams;

public class IntervalFilter implements IntegerStreamFilter {
	
	public IntervalFilter(int lower, int upper) {
		if (lower > upper) {
			throw new IllegalArgumentException("Lower bound should be less or equal" +
					"to upper");
		}
		lowerBound = lower;
		upperBound = upper;
	}

	@Override
	public boolean isSatisfies(int value) {
		return value >= lowerBound && value <= upperBound;
	}

	private int lowerBound;
	private int upperBound;
}
