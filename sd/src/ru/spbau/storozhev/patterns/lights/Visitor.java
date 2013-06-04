package ru.spbau.storozhev.patterns.lights;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

public class Visitor {
	public void visit(LightBulb bulb) {
		iters.add(bulb.iterator());
	}
	
	public void visit(BulbCompositeIlluminant bulbComposite) {
		iters.add(bulbComposite.iterator());
	}
	
	public void visit(CompositeLightingSystem system) {
		Iterator<Illuminant> illIt = system.compositeIterator();
		while (illIt.hasNext()) {
			illIt.next().visit(this);
		}
	}
	
	public List<Iterator<LightBulb>> iterators() {
		return iters;
	}
	
	private List<Iterator<LightBulb>> iters = new ArrayList<>();
}
