package ru.spbau.storozhev.drunksim.stepdecisions;

import ru.spbau.storozhev.drunksim.core.Cell;
import ru.spbau.storozhev.drunksim.objects.EmptyCellObject;
import ru.spbau.storozhev.drunksim.objects.PolicemanCellObject;

public class PolicemanReturnsStepDecision extends AbstractStepDecision {

	public PolicemanReturnsStepDecision(int x, int y, Cell c) {
		super(x, y, c);
	}

	@Override
	public void doIt() {
		PolicemanCellObject policeman = (PolicemanCellObject)cell.getObject();
		policeman.getPoliceStation().setPolicemanIsOut(false);
		EmptyCellObject newObj = new EmptyCellObject(cell);
		cell.getField().setCellObject(newObj);		
	}

}
