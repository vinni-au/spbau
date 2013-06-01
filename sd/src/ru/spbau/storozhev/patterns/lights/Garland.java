package ru.spbau.storozhev.patterns.lights;

import java.util.Collection;

public class Garland extends BulbCompositeIlluminant {
	
	public Garland(Collection<LightBulb> bulbs) {
		super(bulbs);
	}

	@Override
	public void turnOn() throws BurnedOutException {
		System.out.println("Garland: trying to turn on");
		super.turnOn();
		
		if (isOn()) {
			System.out.println("Garland: turned on");
		} else {
			throw new BurnedOutException();
		}
	}

	@Override
	public boolean isOn() {
		for (LightBulb bulb : bulbs) {
			if (!bulb.isOn())
				return false;
		}
		return true;
	}
	
}
