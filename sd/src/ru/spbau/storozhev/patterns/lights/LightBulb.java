package ru.spbau.storozhev.patterns.lights;

import java.util.Iterator;
import java.util.Random;

public class LightBulb extends AbstractIlluminant implements Iterable<LightBulb>{

	@Override
	public void turnOn() throws BurnedOutException {
		if (!isOk)
			return;
		
		System.out.println("LightBulb : trying to turn");
		
		int r = Math.abs(rand.nextInt()) % 20;
		if (r == 5) {
			isOk = false;
			System.out.println("LightBulb : burned out");
			throw new BurnedOutException();
		} else {
			System.out.println("LightBulb : turned on");
			isOn = true;
		}
	}

	@Override
	public void turnOff() {
		isOn = false;
	}

	private static Random rand = new Random();

	@Override
	public Iterator<LightBulb> iterator() {
		return new BulbIterator(this);
	}
}
