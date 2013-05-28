package ru.spbau.storozhev.drunksim.stepdecisions;

import ru.spbau.storozhev.drunksim.core.Cell;
import ru.spbau.storozhev.drunksim.objects.DrinkerCellObject;
import ru.spbau.storozhev.drunksim.objects.EmptyCellObject;
import ru.spbau.storozhev.drunksim.objects.PolicemanCellObject;

public class PolicemanTakesDrinkerStepDecision extends AbstractStepDecision {

	public PolicemanTakesDrinkerStepDecision(int x, int y, Cell c) {
		super(x, y, c);
	}

	@Override
	public void doIt() {
		Cell target = cell.getField().getCell(targetX, targetY);
		if (target != null) {
			if (target.getObject().getClass().equals(DrinkerCellObject.class)) {
				DrinkerCellObject drinker = (DrinkerCellObject)target.getObject();
				PolicemanCellObject policeman = (PolicemanCellObject)cell.getObject();
				policeman.setDrinker(drinker);
				EmptyCellObject newObj = new EmptyCellObject(target);
				cell.getField().setCellObject(newObj);
			}
		}
	}

}
