package ru.spbau.storozhev.patterns.lights;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;

public class CompositeLightingSystem extends AbstractIlluminant {
	
	public CompositeLightingSystem(Collection<? extends Illuminant> illums) {
		for (Illuminant illuminant : illums) {
			illuminants.add(illuminant);
		}		
	}

	@Override
	public void turnOn() {
		for (Illuminant illuminant : illuminants) {
			try {
				illuminant.turnOn();
			} catch (BurnedOutException e) {
			}
		}
	}

	@Override
	public void turnOff() {
		for (Illuminant illuminant : illuminants) {
			illuminant.turnOff();
		}
	}
	
	@Override
	public boolean isOn() {
		for (Illuminant illuminant : illuminants) {
			if (illuminant.isOn())
				return true;
		}
		return false;
	}
	
	private List<Illuminant> illuminants = new ArrayList<>();

	@Override
	public Iterator<LightBulb> iterator() {
		return new CompositeBulbIterator(this);
	}
	
	public Iterator<Illuminant> compositeIterator() {
		return illuminants.iterator();
	}

	@Override
	public void visit(Visitor visitor) {
		visitor.visit(this);
	}
	
	


}
