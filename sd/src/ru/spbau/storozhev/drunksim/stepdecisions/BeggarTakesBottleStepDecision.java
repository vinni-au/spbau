package ru.spbau.storozhev.drunksim.stepdecisions;

import ru.spbau.storozhev.drunksim.core.Cell;
import ru.spbau.storozhev.drunksim.objects.BeggarCellObject;
import ru.spbau.storozhev.drunksim.objects.BottleStuffObject;

public class BeggarTakesBottleStepDecision extends AbstractStepDecision {

	public BeggarTakesBottleStepDecision(int x, int y, Cell c) {
		super(x, y, c);
	}

	@Override
	public void doIt() {
		BeggarCellObject beggar = (BeggarCellObject)cell.getObject();
		Cell target = cell.getField().getCell(targetX, targetY);
		BottleStuffObject bottle = (BottleStuffObject)target.getStuff();
		beggar.setBottle(bottle);
		target.setStuff(null);
	}

}
