package ru.spbau.storozhev.patterns.streams;

public class FibonacciFilter implements IntegerStreamFilter {

	@Override
	public boolean isSatisfies(int value) {
		int f1 = 0;
		int f2 = 1;
		int tmp = 1;
		while (f2 < value) {
			tmp = f1 + f2;
			f1 = f2;
			f2 = tmp;
		}
		if (f2 == value)
			return true;
		return false;
	}

}
