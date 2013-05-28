package ru.spbau.storozhev.drunksim.objects;

import ru.spbau.storozhev.drunksim.core.Cell;
import ru.spbau.storozhev.drunksim.stepdecisions.AbstractStepDecision;
import ru.spbau.storozhev.drunksim.stepdecisions.GoOutStepDecision;

public class GlassCollectorCellObject extends AbstractCellObject {

	public GlassCollectorCellObject(Cell c) {
		super(c);
	}

	@Override
	public boolean isWalkableThru() {
		return false;
	}

	@Override
	public char toChar() {
		return 'G';
	}

	@Override
	public AbstractStepDecision makeStep(int no) {
		if (beggar != null) {
			counter--;
			if (counter <= 0) {
				return new GoOutStepDecision(getX() + 1, getY(), cell, beggar);
			}
		}
		return null;
	}
	
	public void setBeggar(BeggarCellObject b) {
		beggar = b;
		if (beggar != null) {
			counter = 30;
		}
	}
	
	private BeggarCellObject beggar;
	private int counter;

}
