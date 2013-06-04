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
		return new Iterator<LightBulb>() {
			private LightBulb bulb = LightBulb.this;
			
			@Override
			public boolean hasNext() {
				return bulb != null && bulb.isOk();
			}

			@Override
			public LightBulb next() {
				LightBulb result = null;
				if (bulb != null && bulb.isOk()) {
					result = bulb;
					bulb = null;
				}
				return result;
			}

			@Override
			public void remove() {
			}
		};
	}

	@Override
	public void visit(Visitor visitor) {
		visitor.visit(this);
	}
}
