package ru.spbau.storozhev.patterns.streams;

public class Main {

	public static void main(String[] args) {
		performTest("----------- Simple stream --------------",
				createNaturalStream(), new IntegerStreamFilter[] {});
		
		performTest("----------- Simple filter :3 -----------",
				createNaturalStream(), new IntegerStreamFilter[] {
			createDividesBy3Filter()
		});
		
		performTest("----------- Fibonacci filter -----------",
				createNaturalStream(), new IntegerStreamFilter[] {
			createFibonacciFilter()
		});
		
		performTest("---------- Filter combination ----------",
				createNaturalStream(), new IntegerStreamFilter[] {
			createDividesBy3Filter(),
			createDividesBy5Filter()
		});
	}
	
	private static void performTest(
			String message, 
			IntegerStream stream,
			IntegerStreamFilter[] filters) 
	{
		System.out.println(message);
		for (IntegerStreamFilter filter : filters) {
			stream.installFilter(filter);
		}
		for (int i = 0; i < 10; ++i) {
			System.out.println(stream.getNext());
		}
		System.out.println("");
	}
	
	private static IntegerStream createNaturalStream() {
		return new NaturalNumberStream();
	}
	
	private static IntegerStreamFilter createDividesBy3Filter() {
		return new DividesByFilter(3);
	}
	
	private static IntegerStreamFilter createDividesBy5Filter() {
		return new DividesByFilter(5);
	}
	
	private static IntegerStreamFilter createFibonacciFilter() {
		return new FibonacciFilter();
	}

}
