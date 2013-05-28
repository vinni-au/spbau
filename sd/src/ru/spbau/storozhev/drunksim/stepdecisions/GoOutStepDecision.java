package ru.spbau.storozhev.drunksim.stepdecisions;

import ru.spbau.storozhev.drunksim.core.Cell;
import ru.spbau.storozhev.drunksim.objects.AbstractCellObject;
import ru.spbau.storozhev.drunksim.objects.EmptyCellObject;

public class GoOutStepDecision extends AbstractStepDecision {

	public GoOutStepDecision(int x, int y, Cell c, AbstractCellObject who) {
		super(x, y, c);
		object = who;
	}

	@Override
	public void doIt() {
		Cell target = cell.getField().getCell(targetX, targetY);
		if (target.getStuff() == null && 
				target.getObject().getClass().equals(EmptyCellObject.class)) {
			object.setCell(target);
			cell.getField().setCellObject(object);
		}		
	}

	private AbstractCellObject object;
}
