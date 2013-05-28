package ru.spbau.storozhev.drunksim.objects;

import ru.spbau.storozhev.drunksim.ai.PathFinding;
import ru.spbau.storozhev.drunksim.core.Cell;
import ru.spbau.storozhev.drunksim.stepdecisions.AbstractStepDecision;
import ru.spbau.storozhev.drunksim.stepdecisions.BeggarGoesToCollectorStepDecision;
import ru.spbau.storozhev.drunksim.stepdecisions.BeggarTakesBottleStepDecision;
import ru.spbau.storozhev.drunksim.stepdecisions.MoveStepDecision;

public class BeggarCellObject extends AbstractCellObject {

	public BeggarCellObject(Cell c, GlassCollectorCellObject glassC) {
		super(c);
		glassCollector = glassC;
	}

	@Override
	public boolean isWalkableThru() {
		return false;
	}

	@Override
	public char toChar() {
		if (bottle == null)			
			return 'h';
		
		return 'H';
	}

	@Override
	public AbstractStepDecision makeStep(int no) {
		glassCollector.setBeggar(null);
		
		if (bottle == null) {
			Cell c = PathFinding.findCellWithStuff(cell, BottleStuffObject.class);
			if (c != null) {
				return new BeggarTakesBottleStepDecision(c.getX(), c.getY(), cell);
			}
			
			Cell target = PathFinding.randomSearchStep(cell);
			if (target == null)
				return null;
			
			return new MoveStepDecision(target.getX(), target.getY(), cell); 
		} else {
			Cell c = PathFinding.findCellWithObject(cell, GlassCollectorCellObject.class);
			if (c != null) 
				return new BeggarGoesToCollectorStepDecision(glassCollector.getX(), glassCollector.getY(), cell);
			
			return PathFinding.stepTo(cell, glassCollector.getCell());
		}
	}
	
	public void setBottle(BottleStuffObject b) {
		bottle = b;
	}
	
	public GlassCollectorCellObject getCollector() {
		return glassCollector;
	}
	
	private BottleStuffObject bottle;
	private GlassCollectorCellObject glassCollector;

}
