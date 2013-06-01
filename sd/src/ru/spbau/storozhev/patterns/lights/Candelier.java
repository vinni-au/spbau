package ru.spbau.storozhev.patterns.lights;

import java.util.Collection;

public class Candelier extends BulbCompositeIlluminant {
	
	public Candelier(Collection<LightBulb> bulbs) {
		super(bulbs);
	}

	@Override
	public void turnOn() throws BurnedOutException {
		System.out.println("Candelier: trying to turn on");
		super.turnOn();
		
		if (isOn()) {
			System.out.println("Candelier: turned on");
		} else {
			throw new BurnedOutException();
		}
	}

	@Override
	public void turnOff() {
		for (LightBulb bulb : bulbs) {
			bulb.turnOff();
		}
		isOn = false;
	}
	
	@Override
	public boolean isOn() {
		for (LightBulb bulb : bulbs) {
			if (bulb.isOn())
				return true;
		}
		return false;
	}
	
}
