package ru.spbau.storozhev.drunksim.stepdecisions;

import ru.spbau.storozhev.drunksim.core.Cell;
import ru.spbau.storozhev.drunksim.objects.DrinkerCellObject;
import ru.spbau.storozhev.drunksim.objects.EmptyCellObject;

public class DrinkerGoOutStepDecision extends AbstractStepDecision {
	public DrinkerGoOutStepDecision(int x, int y, Cell c) {
		super(x, y, c);
	}

	@Override
	public void doIt() {
		Cell target = cell.getField().getCell(targetX, targetY);
		if (target.getStuffObject() == null 
				&& target.getObject().getClass().equals(EmptyCellObject.class)) {
			cell.getField().setCellObject(new DrinkerCellObject(target));
		}
	}

	@Override
	public boolean isConflictedWith(AbstractStepDecision other) {
		return true;
	}

}
