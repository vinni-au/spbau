package ru.spbau.storozhev.drunksim.stepdecisions;

import ru.spbau.storozhev.drunksim.core.Cell;
import ru.spbau.storozhev.drunksim.objects.BeggarCellObject;
import ru.spbau.storozhev.drunksim.objects.EmptyCellObject;

public class BeggarGoesToCollectorStepDecision extends AbstractStepDecision {

	public BeggarGoesToCollectorStepDecision(int x, int y, Cell c) {
		super(x, y, c);
	}

	@Override
	public void doIt() {
		BeggarCellObject beggar = (BeggarCellObject)cell.getObject();
		beggar.setBottle(null);
		beggar.getCollector().setBeggar(beggar);
		EmptyCellObject newObj = new EmptyCellObject(cell);
		cell.getField().setCellObject(newObj);
	}

}
