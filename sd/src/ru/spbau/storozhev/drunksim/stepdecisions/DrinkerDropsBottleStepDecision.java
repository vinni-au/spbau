package ru.spbau.storozhev.drunksim.stepdecisions;

import ru.spbau.storozhev.drunksim.core.Cell;
import ru.spbau.storozhev.drunksim.objects.BottleStuffObject;

public class DrinkerDropsBottleStepDecision extends AbstractStepDecision {

	public DrinkerDropsBottleStepDecision(int x, int y, Cell c) {
		super(x, y, c);
	}

	@Override
	public void doIt() {
		cell.setStuff(new BottleStuffObject());
		(new MoveStepDecision(targetX, targetY, cell)).doIt();
	}

	@Override
	public boolean isConflictedWith(AbstractStepDecision other) {
		return false;
	}

}
