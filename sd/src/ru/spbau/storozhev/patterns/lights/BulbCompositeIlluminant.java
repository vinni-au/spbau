package ru.spbau.storozhev.patterns.lights;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;

public class BulbCompositeIlluminant extends AbstractIlluminant {
	
	public BulbCompositeIlluminant(Collection<LightBulb> bulbs) {
		for (LightBulb bulb : bulbs) {
			this.bulbs.add(bulb);
		}		
	}

	@Override
	public void turnOn() throws BurnedOutException {
		for (LightBulb bulb : bulbs) {
			try {
				bulb.turnOn();
			} catch (BurnedOutException e) {
				//swallow
			}
		}
	}
	
	@Override
	public void turnOff() {
		for (LightBulb bulb : bulbs) {
			bulb.turnOff();
		}
	}

	@Override
	public Iterator<LightBulb> iterator() {
		return bulbs.iterator();		
	}
	
	protected List<LightBulb> bulbs = new ArrayList<>();

	@Override
	public void visit(Visitor visitor) {
		visitor.visit(this);
	}

}
